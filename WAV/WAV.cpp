#include "WAV.h"
#include <iostream>



void WAV::read(const char* filename) {
    FILE* f;
    fopen_s(&f, filename, "rb");
    if (f == nullptr) {
        printf("Cannot open file.\n");
        exit(1);
    }
    auto check = [](char* a, const char* b, int kol) {
        for (int i = 0; i < kol; i++) {
            if (a[i] != b[i]) return true;
        }
        return false;
    };


    fread(chunkId, 1, 4, f);
    if (check(chunkId, "RIFF", 4)) exit(2);

    fread(&chunkSize, 4, 1, f);

    fread(format, 1, 4, f);
    if (check(format, "WAVE", 4)) exit(3);

    fread(subchunk1Id, 1, 4, f);
    if (check(subchunk1Id, "fmt ", 4)) exit(4);

    fread(&subchunk1Size, 4, 1, f);

    fread(&audioFormat, 2, 1, f);

    fread(&numChannels, 2, 1, f);

    fread(&sampleRate, 4, 1, f);

    fread(&byteRate, 4, 1, f);

    fread(&blockAlign, 2, 1, f);

    fread(&bitsPerSample, 2, 1, f);
    bytesPerSample = bitsPerSample / 8;

    char temp;

    if (subchunk1Size != 16 && audioFormat != 1) { // not PCM
        fread(&extraParamSize, 2, 1, f);
        for (int i = 0; i < extraParamSize; i++) {
            fread(&temp, 1, 1, f);
            extraParam.push_back(temp);
        }

    }
    fread(subchunk2Id, 1, 4, f);

    if (!check(subchunk2Id, "LIST", 4)) {
        listId = true;
        fread(&listSize, 4, 1, f);
        for (unsigned int i = 0; i < listSize; i++) {
            fread(&temp, 1, 1, f);
            listParam.push_back(temp);
        }
        fread(subchunk2Id, 1, 4, f);
    }

    if (!check(subchunk2Id, "data", 4)) {
        fread(&subchunk2Size, 4, 1, f);
    } else exit(5);

    for (int i = 0; i < numChannels; i++)
        data.push_back(std::vector<char>());
    for (unsigned long readed = 0; readed < subchunk2Size; readed += bytesPerSample * numChannels)
        for (int j = 0; j < numChannels; j++) 
            for (int i = 0; i < bytesPerSample; i++) {
                fread(&temp, 1, 1, f);
                data[j].push_back(temp);
            }
    fclose(f);
}

void WAV::write(const char* filename) {
    FILE* f;
    fopen_s(&f, filename, "wb");
    if (f == nullptr) {
        printf("Cannot open file.\n");
        exit(1);
    }

    fwrite(chunkId, 1, 4, f);

    fwrite(&chunkSize, 4, 1, f);

    fwrite(format, 1, 4, f);

    fwrite(subchunk1Id, 1, 4, f);

    fwrite(&subchunk1Size, 4, 1, f);

    fwrite(&audioFormat, 2, 1, f);

    fwrite(&numChannels, 2, 1, f);

    fwrite(&sampleRate, 4, 1, f);

    fwrite(&byteRate, 4, 1, f);

    fwrite(&blockAlign, 2, 1, f);

    fwrite(&bitsPerSample, 2, 1, f);


    if (subchunk1Size != 16 && audioFormat != 1) { // not PCM
        fwrite(&extraParamSize, 2, 1, f);
        for (int i = 0; i < extraParamSize; i++) 
            fwrite(&extraParam[i], 1, 1, f);
    }

    if (listId) {
        fwrite("LIST", 1, 4, f);

        fwrite(&listSize, 4, 1, f);

        for (unsigned int i = 0; i < listParam.size(); i++)
            fwrite(&listParam[i], 1, 1, f);
    }

    fwrite(subchunk2Id, 1, 4, f);

    fwrite(&subchunk2Size, 4, 1, f);

    for (unsigned long writed = 0; writed < data[0].size(); writed += bytesPerSample)
        for (int j = 0; j < numChannels; j++)
            for (short i = 0; i < bytesPerSample; i++) {
                fwrite(&data[j][writed + i], 1, 1, f);
            }
    fclose(f);
}
