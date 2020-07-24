#include <cstdio>
#include <vector>

class WAV {
public:
    char chunkId[4];
    unsigned long chunkSize;
    char format[4];
    char subchunk1Id[4];
    unsigned long subchunk1Size;
    unsigned short audioFormat;
    unsigned short numChannels;
    unsigned long sampleRate;
    unsigned long byteRate;
    unsigned short blockAlign;
    unsigned short bitsPerSample;
    unsigned short bytesPerSample;

    [[maybe_unused]] unsigned short extraParamSize;
    [[maybe_unused]] std::vector<char> extraParam;
    [[maybe_unused]] bool listId;
    [[maybe_unused]] unsigned long listSize;
    [[maybe_unused]] std::vector<char> listParam;

    char subchunk2Id[4];
    unsigned long subchunk2Size;
    std::vector<std::vector<char>> data;


    void read(const char* filename);
    void write(const char* filename);

    WAV(const char *filename) {
        read(filename);
    }

    WAV() {}
};

