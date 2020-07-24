//#include "Load.cpp"
#include "WAV.h"
#include <string>
#include <iostream>




int main(int argc, char** argv) {
    WAV wav;
    if (argc < 2) {
        std::cout << "Enter input filename.";
        std::string s;
        std::cin >> s;
        if (s[0] == '\"') s = s.substr(1, s.length() - 2);
        wav = WAV(s.data());
    }
    else wav = WAV(argv[1]);
    int key = 1;
    std::cout << "Succsesfull read\n";
    if (key == 0) {
        std::cout << "Enter output filename.\n";
        std::string s;
        std::cin >> s;
        if (s[0] == '\"') s = s.substr(1, s.length() - 2);
        wav.write(s.data());
    }
    else wav.write("C:\\Users\\Gleb\\Desktop\\out.wav");

    std::cout << "Succsesfull write\n";

    system("pause");
    return 0;
}

