#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <stdint.h>

uint64_t uncorrectCode(uint64_t x) { return 0xDC00 + x; }

uint64_t UTF8ToUnicode(const unsigned char *in, int *pos) {
    uint64_t res;
    if (in[*pos] >> 7 == 0) { // 1b
        res = in[(*pos)];
    } else if (in[*pos] >> 5 == 6) { //2b
        if (in[*pos + 1] >> 6 == 2) {
            uint64_t byte1 = in[(*pos)++] - 0b11000000;
            uint64_t byte2 = in[*pos] - 0b10000000;
            res = (byte1 << 6) + byte2;
        } else res = uncorrectCode(in[*pos]);

    } else if (in[*pos] >> 4 == 14) { //3b

        if (in[*pos + 1] >> 6 == 2) {
            if (in[*pos + 2] >> 6 == 2) {
                uint64_t byte1 = in[(*pos)++] - 0b11100000;
                uint64_t byte2 = in[(*pos)++] - 0b10000000;
                uint64_t byte3 = in[*pos] - 0b10000000;
                res = (byte1 << 12) + (byte2 << 6) + byte3;
            } else res = uncorrectCode(in[*pos]);
        } else res = uncorrectCode(in[*pos]);

    } else if (in[*pos] >> 3 == 30) { //4b
        if (in[*pos + 1] >> 6 == 2) {
            if (in[*pos + 2] >> 6 == 2) {
                if (in[*pos + 3] >> 6 == 2) {
                    uint64_t byte1 = in[(*pos)++] - 0b11110000;
                    uint64_t byte2 = in[(*pos)++] - 0b10000000;               //змейка
                    uint64_t byte3 = in[(*pos)++] - 0b10000000;
                    uint64_t byte4 = in[*pos] - 0b10000000;
                    res = (byte1 << 18) + (byte2 << 12) + (byte3 << 6) + byte4;
                } else res = uncorrectCode(in[*pos]);
            } else res = uncorrectCode(in[*pos]);
        } else res = uncorrectCode(in[*pos]);

    } else {
        res = uncorrectCode(in[*pos]);
    }
    return res;
}

uint64_t UTF16LEToUnicode(const unsigned char *in, int *pos) {
    uint64_t a = in[(*pos)] + (in[*pos + 1] << 8);
    if (a <= 0xD7FF || a >= 0xE000 || (a >= 0xDC80 && a <= 0xDCFF)) {
        *pos += 1;
        return a;
    } else {
        uint64_t b = in[(*pos) + 2] + (in[(*pos) + 3] << 8);
        a -= 0xD800;
        a = a << 10;
        b -= 0xDC00;
        *pos += 3;
        return a + b + 0x10000;
    }
}

uint64_t UTF16BEToUnicode(const unsigned char *in, int *pos) {
    uint64_t a = (in[(*pos)] << 8) + in[*pos + 1];
    if (a <= 0xD7FF || a >= 0xE000 || (a >= 0xDC80 && a <= 0xDCFF)) {
        *pos += 1;
        return a;
    } else {
        uint64_t b = (in[(*pos) + 2] << 8) + in[(*pos) + 3];
        a -= 0xD800;
        a = a << 10;
        b -= 0xDC00;
        *pos += 3;
        return a + b + 0x10000;
    }
}

uint64_t UTF32LEToUnicode(const unsigned char *in, int *pos) {
    uint64_t res = 0;
    res = in[*pos] + (in[*pos + 1] << 8) + (in[*pos + 2] << 16) + (in[*pos + 3] << 24);
    *pos += 3;
    return res;
}

uint64_t UTF32BEToUnicode(const unsigned char *in, int *pos) {
    uint64_t res = 0;
    res = in[*pos] + (in[*pos + 1] << 8) + (in[*pos + 2] << 16) + (in[*pos + 3] << 24);
    *pos += 3;
    return res;
}

uint64_t toUnicode(const unsigned char *in, uint64_t inEncoding, int *pos) {
    int prev = 0;
    if (inEncoding == 7) return UTF8ToUnicode(in, pos);
    if (inEncoding == 8) return UTF8ToUnicode(in, pos);
    if (inEncoding == 15) return UTF16LEToUnicode(in, pos);
    if (inEncoding == 17) return UTF16BEToUnicode(in, pos);
    if (inEncoding == 31) return UTF32LEToUnicode(in, pos);
    return UTF32BEToUnicode(in, pos);
}

int UnicodeToUTF32BE(unsigned char *res, const uint64_t unicode) {
    uint64_t byte1 = (unicode << 32) >> 56;
    uint64_t byte2 = (unicode << 40) >> 56;
    uint64_t byte3 = (unicode << 48) >> 56;
    uint64_t byte4 = (unicode << 56) >> 56;
    res[0] = byte1;
    res[1] = byte2;
    res[2] = byte3;
    res[3] = byte4;
    return 4;
}


int UnicodeToUTF32LE(unsigned char *res, const uint64_t unicode) {
    uint64_t byte1 = (unicode << 32) >> 56;
    uint64_t byte2 = (unicode << 40) >> 56;
    uint64_t byte3 = (unicode << 48) >> 56;
    uint64_t byte4 = (unicode << 56) >> 56;
    res[3] = byte1;
    res[2] = byte2;
    res[1] = byte3;
    res[0] = byte4;
    return 4;
}

int UnicodeToUTF16BE(unsigned char *res, const uint64_t unicode) {
    if (unicode <= 0xD7FF || (unicode >= 0xE000 && unicode <= 0xFFFF)) {
        uint64_t byte1 = (unicode << 48) >> 56;
        uint64_t byte2 = (unicode << 56) >> 56;
        res[0] = byte1;
        res[1] = byte2;
        return 2;
    } else if (unicode >= 0x10000 && unicode <= 0x10FFFF) {
        uint64_t temp = unicode - 0x10000;
        uint64_t word1 = ((temp << 44) >> 54) + 0xD800;
        uint64_t word2 = ((temp << 54) >> 54) + 0xDC00;
        res[0] = (word1 << 48) >> 56;
        res[1] = (word1 << 56) >> 56;
        res[2] = (word2 << 48) >> 56;
        res[3] = (word2 << 56) >> 56;
        return 4;
    } else {
        res[0] = (unicode << 48) >> 56;
        res[1] = (unicode << 56) >> 56;
        return 2;
    }
}

int UnicodeToUTF16LE(unsigned char *res, const uint64_t unicode) {
    if (unicode <= 0xD7FF || (unicode >= 0xE000 && unicode <= 0xFFFF)) {
        uint64_t byte1 = (unicode << 48) >> 56;
        uint64_t byte2 = (unicode << 56) >> 56;
        res[0] = byte2;
        res[1] = byte1;
        return 2;
    } else if (unicode >= 0x10000 && unicode <= 0x10FFFF) {
        uint64_t temp = unicode - 0x10000;
        uint64_t word1 = ((temp << 44) >> 54) + 0xD800;
        uint64_t word2 = ((temp << 54) >> 54) + 0xDC00;
        res[1] = (word1 << 48) >> 56;
        res[0] = (word1 << 56) >> 56;
        res[3] = (word2 << 48) >> 56;
        res[2] = (word2 << 56) >> 56;
        return 4;
    } else {
        res[1] = (unicode << 48) >> 56;
        res[0] = (unicode << 56) >> 56;
        return 2;
    }
}

int UnicodeToUTF8(unsigned char *res, const uint64_t unicode) {
    if (unicode <= 0x7F) {
        res[0] = unicode;
        return 1;
    } else if (unicode <= 0x7FF) {
        res[0] = 0xC0 | (unicode >> 6);
        res[1] = 0x80 | (unicode & 0x3F);
        return 2;
    } else if (unicode >= 0xDC80 && unicode <= 0xDCFF) {
        res[0] = unicode - 0xDC00;
        return 1;
    } else if (unicode <= 0xFFFF) {
        res[0] = 0xE0 | (unicode >> 12);
        res[1] = 0x80 | ((unicode >> 6) & 0x3F);
        res[2] = 0x80 | (unicode & 0x3F);
        return 3;
    } else if (unicode <= 0x10FFFF) {
        res[0] = 0xF0 | (unicode >> 18);
        res[1] = 0x80 | ((unicode >> 12) & 0x3F);
        res[2] = 0x80 | ((unicode >> 6) & 0x3F);
        res[3] = 0x80 | (unicode & 0x3F);
        return 4;
    } else {
        res[0] = 0xE0 | 0xF;
        res[1] = 0x80 | 0x3F;
        res[2] = 0x80 | 0x3D;
        return 3;
    }
}

int toOutEncoding(uint64_t unicode, const unsigned int outEncoding, unsigned char *res) {
    for (int i = 0; i < 5; i++) res[i] = '\0';
    if (outEncoding == 7) return UnicodeToUTF8(res, unicode);
    if (outEncoding == 8) return UnicodeToUTF8(res, unicode);
    if (outEncoding == 15) return UnicodeToUTF16LE(res, unicode);
    if (outEncoding == 17) return UnicodeToUTF16BE(res, unicode);
    if (outEncoding == 31) return UnicodeToUTF32LE(res, unicode);
    return UnicodeToUTF32BE(res, unicode);
}

uint64_t check_BOM(unsigned char BOM[4]) {
    if (memcmp(BOM, "\x00\x00\xFE\xFF", 4) == 0) { // UTF-32 big-endian
        return 33;
    } else if (memcmp(BOM, "\xFF\xFE\x00\x00", 4) == 0) { // UTF-32 little-endian
        return 31;
    } else if (memcmp(BOM, "\xFE\xFF", 2) == 0) { // UTF-16 big-endian
        return 17;
    } else if (memcmp(BOM, "\xFF\xFE", 2) == 0) { // UTF-16 little-endian
        return 15;
    } else if (memcmp(BOM, "\xEF\xBB\xBF", 3) == 0) { // UTF-8 with BOM
        return 8;
    } else { // UTF-8 without BOM
        return 7;
    }
}

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("\nInput file name not passed\n");
        return 1;
    }
    if (argc < 3) {
        printf("\nOutput file name not passed\n");
        return 2;
    }
    if (argc < 4) {
        printf("\nOutput encoding not passed\n");
        return 4;
    }
    FILE *in;
    in = fopen(argv[1], "rb");
    if (!in) {
        printf("\nInput file not found\n");
        return 3;
    }
    unsigned char BOM[4];
    BOM[0] = '\0';
    BOM[1] = '\0';
    BOM[2] = '\0';
    BOM[3] = '\0';  // на всякий если вообще ничего не считается (вдруг UTF-8 без BOM пустой)
    int e = 1;
    e = fread(BOM, sizeof(unsigned char), 4, in);
    if (e == 0) {
        printf("Input file is empty");
        fclose(in);
        return 0;
    }
    unsigned int inEncoding = check_BOM(BOM);
    FILE *out;
    out = fopen(argv[2], "wb");
    unsigned char *arr;
    int capacity = 1;
    int size = 0;
    arr = (unsigned char *) malloc(capacity * sizeof(unsigned char));
    if (arr == NULL) {
        error:
        printf("Failed to allocate memory");
        fclose(in);
        return 4;
    }
    unsigned char *support = arr;
    capacity = 8;
    arr = (unsigned char *) realloc(arr, sizeof(unsigned char) * capacity);
    if (arr == NULL) {
        free(support);
        goto error;
    }
    if (inEncoding == 7) {
        size = e;
        for (int i = 0; i < e; i++) {
            arr[i] = BOM[i];
        }
    }
    if (inEncoding == 8) {
        size = 1;
        arr[0] = BOM[3];
    }
    if (inEncoding == 15 || inEncoding == 17) {
        if (e > 2) {
            arr[0] = BOM[2];
            size++;
        }
        if (e > 3) {
            arr[1] = BOM[3];
            size++;
        }
    }
    int block = 2048;
    while (!ferror(in) && !feof(in) && e != 0) {
        if (capacity <= size + block) {
            capacity += block + 1;
            unsigned char *support = arr;
            arr = (unsigned char *) realloc(arr, sizeof(unsigned char) * capacity);
            if (arr == NULL) {
                free(support);
                goto error;
            }
            e = fread(arr + size, sizeof(unsigned char), block, in);
            size += e;
        }
    }
    fclose(in);
    int outEncoding = -1;
    if (strcmp(argv[3], "UTF-8") == 0) outEncoding = 7;  //пишем BOM
    if (strcmp(argv[3], "UTF-8BOM") == 0) {
        fwrite("\xEF\xBB\xBF", sizeof(char), 3, out);
        outEncoding = 8;
    }
    if (strcmp(argv[3], "UTF-16LE") == 0) {
        fwrite("\xFF\xFE", sizeof(char), 2, out);
        outEncoding = 15;
    }
    if (strcmp(argv[3], "UTF-16BE") == 0) {
        fwrite("\xFE\xFF", sizeof(char), 2, out);
        outEncoding = 17;
    }
    if (strcmp(argv[3], "UTF-32LE") == 0) {
        fwrite("\xFF\xFE\x00\x00", sizeof(char), 4, out);
        outEncoding = 31;
    }
    if (strcmp(argv[3], "UTF-32BE") == 0) {
        fwrite("\x00\x00\xFE\xFF", sizeof(char), 4, out);
        outEncoding = 33;
    }
    unsigned char *res;
    int temp = 0;
    res = (unsigned char *) malloc(sizeof(unsigned char) * 5);
    for (int i = 0; i < 5; i++) res[i] = '\0';
    for (int i = 0; i < size; i++) {
        temp = toOutEncoding(toUnicode(arr, inEncoding, &i), outEncoding, res);
        fwrite(res, sizeof(unsigned char), temp, out);
    }
    fclose(out);
    return 0;
}
