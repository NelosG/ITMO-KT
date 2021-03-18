#include <vector>
#include <string>
#include <iostream>
#include <fstream>
//#include <time.h>

class big_integer {
    static const int dig = 4;
    static const int base = 10000;

    bool nan = false;

    bool negative;

    void remove_zeros();

    void rightShift();

    big_integer(float);

public:

    big_integer();

    big_integer(int);

    big_integer(std::string);


    explicit operator std::string();

    friend int comp(const big_integer &, const big_integer &);

    big_integer operator-() const;

    friend big_integer sqrt(const big_integer &bigInteger);

    friend bool operator==(const big_integer &, const big_integer &);

    friend bool operator<(const big_integer &, const big_integer &);

    friend bool operator!=(const big_integer &, const big_integer &);

    friend bool operator<=(const big_integer &, const big_integer &);

    friend bool operator>(const big_integer &, const big_integer &);

    friend bool operator>=(const big_integer &, const big_integer &);

    friend big_integer operator+(const big_integer &, const big_integer &);

    friend big_integer operator-(const big_integer &, const big_integer &);

    friend big_integer operator*(const big_integer &, const big_integer &);

    friend big_integer operator/(const big_integer &, big_integer);

    // правый не константный что бы избавиться от лишних копий
    friend big_integer operator%(const big_integer &, big_integer);

    std::vector<int> digits;
};

big_integer::big_integer() {
    this->negative = false;
}

big_integer::big_integer(float a) {
    this->negative = false;
    this->nan = true;
}

big_integer::big_integer(int i) {
    this->negative = i < 0;
    this->digits.push_back(std::abs(i) % base);
    i /= base;
    if (i != 0) this->digits.push_back(std::abs(i));
}

big_integer::big_integer(std::string str) {
    if (str.length() == 0) {
        this->negative = false;
    } else {
        if (str[0] == '-') {
            str = str.substr(1);
            this->negative = true;
        } else this->negative = false;

        for (auto i = static_cast<long long>(str.length()); i > 0; i -= dig) {
            if (i < dig) this->digits.push_back(atoi(str.substr(0, i).c_str()));
            else this->digits.push_back(atoi(str.substr(i - dig, dig).c_str()));
        }

        this->remove_zeros();
    }
}

void big_integer::remove_zeros() {
    while (this->digits.size() > 1 && this->digits.back() == 0)
        this->digits.pop_back();
    if (this->digits.size() == 1 && this->digits[0] == 0 || this->digits.empty())
        this->negative = false;
}


big_integer big_integer::operator-() const {
    big_integer copy(*this);
    copy.negative = !copy.negative;
    return copy;
}


int comp(const big_integer &left, const big_integer &right) {
    if (left.negative) {
        if (right.negative) return comp(-right, -left);
        else return -1;
    } else if (right.negative) return 1;
    else {
        if (left.digits.size() < right.digits.size()) return -1;
        else if (left.digits.size() > right.digits.size()) return 1;
        else {
            for (long long i = static_cast<long long>(left.digits.size()) - 1; i >= 0; --i) {
                if (left.digits[i] < right.digits[i]) return -1;
                if (left.digits[i] > right.digits[i]) return 1;
            }
            return 0;
        }
    }
}

bool operator==(const big_integer &left, const big_integer &right) {
    return comp(left, right) == 0;
}

bool operator<(const big_integer &left, const big_integer &right) {
    return comp(left, right) == -1;
}

bool operator>(const big_integer &left, const big_integer &right) {
    return comp(left, right) == 1;
}

bool operator!=(const big_integer &left, const big_integer &right) {
    return !(left == right);
}

bool operator<=(const big_integer &left, const big_integer &right) {
    return !(left > right);
}

bool operator>=(const big_integer &left, const big_integer &right) {
    return !(left < right);
}

big_integer operator+(const big_integer &left, const big_integer &right) {
    big_integer result(left);
    if (result.negative) {
        if (right.negative) return -(-result + (-right));
        else return right - (-result);
    } else if (right.negative) return result - (-right);
    int carry = 0;
    for (long long i = 0; i < static_cast<long long>(
            result.digits.size() > right.digits.size() ? result.digits.size() : right.digits.size()
    ) || carry != 0; ++i) {
        if (i == result.digits.size()) result.digits.push_back(0);
        result.digits[i] += carry + (i < right.digits.size() ? right.digits[i] : 0);
        carry = result.digits[i] >= big_integer::base;
        if (carry != 0) result.digits[i] -= big_integer::base;
    }

    return result;
}

big_integer operator-(const big_integer &left, const big_integer &right) {
    big_integer result(left);
    if (right.negative) return result + (-right);
    else if (result.negative) return -(-result + right);
    else if (result < right) return -(right - result);
    int carry = 0;
    for (long long i = 0; i < static_cast<long long>(right.digits.size()) || carry != 0; ++i) {
        result.digits[i] -= carry + (i < right.digits.size() ? right.digits[i] : 0);
        carry = result.digits[i] < 0;
        if (carry != 0) result.digits[i] += big_integer::base;
    }

    result.remove_zeros();
    return result;
}

big_integer operator*(const big_integer &left, const big_integer &right) {
    big_integer result;
    result.digits.resize(left.digits.size() + right.digits.size());
    for (long long i = 0; i < static_cast<long long>(left.digits.size()); ++i) {
        int carry = 0;
        for (long long j = 0; j < static_cast<long long>(right.digits.size()) || carry != 0; ++j) {
            long long cur = result.digits[i + j] +
                            left.digits[i] * (j < right.digits.size() ? right.digits[j] : 0) * 1LL + carry;
            result.digits[i + j] = static_cast<int>(cur % big_integer::base);
            carry = static_cast<int>(cur / big_integer::base);
        }
    }

    result.negative = left.negative != right.negative;
    result.remove_zeros();
    return result;
}

void big_integer::rightShift() {
    if (this->digits.empty()) {
        this->digits.push_back(0);
        return;
    }
    this->digits.push_back(this->digits[this->digits.size() - 1]);
    for (long long i = static_cast<long long>(this->digits.size()) - 2; i > 0; --i)
        this->digits[i] = this->digits[i - 1];
    this->digits[0] = 0;
}

big_integer operator/(const big_integer &left, big_integer right) {
    if (right == 0) return NAN;
    bool neg = right.negative;
    right.negative = false;
    big_integer result, current;
    result.digits.resize(left.digits.size());
    for (long long i = static_cast<long long>(left.digits.size()) - 1; i >= 0; --i) {
        current.rightShift();
        current.digits[0] = left.digits[i];
        current.remove_zeros();
        int locRes = 0, l = 0, r = big_integer::base;
        while (l <= r) {
            int m = (l + r) / 2;
            big_integer t = right * m;
            if (t <= current) {
                locRes = m;
                l = m + 1;
            } else r = m - 1;
        }
        result.digits[i] = locRes;
        current = current - right * locRes;
    }
    right.negative = neg;
    result.negative = left.negative != right.negative;
    result.remove_zeros();
    return result;
}

big_integer operator%(const big_integer &left, big_integer right) {
    if (right == 0) return NAN;
    big_integer result = left - (left / right) * right;
    if (result.negative) result = result + right;
    return result;
}


big_integer::operator std::string() {
    if (digits.empty()) {
        if (nan) return "NAN";
        else return "0";
    }
    std::string res;
    if (negative) res = "-";
    res += std::to_string(digits.back());
    for (long long i = static_cast<long long>(digits.size()) - 2; i >= 0; --i) {
        std::string temp = std::to_string(digits[i]);
        for (int j = 0; j < dig - temp.length(); j++)
            res += '0';
        res += temp;
    }
    return res;

}


big_integer sqrt(const big_integer &bigInteger) {
    if (bigInteger <= 0) return NAN;
    big_integer x0 = bigInteger, x1 = (bigInteger + 1) / 2;
    while (x1 < x0) {
        x0 = x1;
        x1 = (x1 + bigInteger / x1) / 2;
    }
    return x0;
}

int main(int argc, char **argv) {
    if (argc < 2) {
        std::cout << "Error: please pass the input and output file names.";
        return 3;
    }
    if (argc < 3) {
        std::cout << "Error: please pass the output file name.";
        return 3;
    }
    std::ifstream in(argv[1]);

    if (!in) {
        std::cout << "Error: the input file can't be opened.";
        return 4;
    }
    std::ofstream out(argv[2]);
    if (!out) {
        std::cout << "Error: the output file can't be opened.";
        in.close();
        return 5;
    }

    std::string fir, operation;
    in >> fir >> operation;
    if (fir.length() == 0) {
        std::cout << "Error: file is empty";
        in.close();
        out.close();
        return 6;
    }
    if (operation.length() == 0) {
        std::cout << "Error: can't read operation";
        in.close();
        out.close();
        return 8;
    }
    big_integer first(fir);
    fir.clear();
//    clock_t start, end;
//    start = clock();
    if (operation == "#") out << std::string((sqrt(first)));
    else {
        in >> fir;
        if (fir.length() == 0) {
            std::cout << "Error: can't read second argument of expression";
            in.close();
            out.close();
            return 8;
        }
        big_integer second(fir);
        fir.clear();
        if (operation == "-")       out << std::string((first - second));
        else if (operation == "*")  out << std::string((first * second));
        else if (operation == "/")  out << std::string((first / second));
        else if (operation == "%")  out << std::string((first % second));
        else if (operation == "+")  out << std::string((first + second));
        else if (operation == "==") out << (first == second ? "true" : "false");
        else if (operation == "<=") out << (first <= second ? "true" : "false");
        else if (operation == ">=") out << (first >= second ? "true" : "false");
        else if (operation == "<")  out << (first < second ? "true" : "false");
        else if (operation == ">")  out << (first > second ? "true" : "false");
        else if (operation == "!=") out << (first != second ? "true" : "false");
        else printf("Error: operation not recognized");
    }
    in.close();
    out.close();
    return 0;
}
