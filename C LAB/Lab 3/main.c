#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>



void swap(double *a, double *b) {
    double temp;
    temp = *a;
    *a = *b;
    *b = temp;
}

int equals(double left, double right, double k) {
    return fabs(left - right) <= 1e-8 * k;
}

int gauss(int n, double *A, double *ans, double k) {
    int n1 = n + 1;
    double div, max;
    int Rang = 0, RangA = 0, maxInd;

    for (int j = 0; j < n; j++) {
        max = fabs(A[j * n1 + j]);
        maxInd = j;
        for (int i = j; i < n; i++)
            if (fabs(A[i * n1 + j]) > max) {
                max = fabs(A[i * n1 + j]);
                maxInd = i;
            }

        if (j != maxInd)
            for (int i = 0; i <= n; i++)
                swap(&A[j * n1 + i], &A[maxInd * n1 + i]);

        for (int i = j; i < n; i++) {
            if (equals(A[i * n1 + j], 1.0, k) == 0) {
                if (equals(A[i * n1 + j], 0.0, k) == 1) {
                    A[i * n1 + j] = 0.0;
                    continue;
                }
                div = A[i * n1 + j];
                for (int col = 0; col <= n; col++) {
                    if (equals(A[i * n1 + col], 0.0, k) == 1) A[i * n1 + col] = 0.0;
                    else {
                        A[i * n1 + col] /= div;
                        if (equals(A[i * n1 + col], 0.0, k) == 1) A[i * n1 + col] = 0.0;
                        if (equals(A[i * n1 + col], 1.0, k) == 1) A[i * n1 + col] = 1.0;
                    }
                }
            }
            if (i == j) continue;
            for (int col = 0; col <= n; col++) {
                if (equals(A[i * n1 + col], A[j * n1 + col], k) == 1) A[i * n1 + col] = 0.0;
                else A[i * n1 + col] -= A[j * n1 + col];
            }
        }
    }
    for (int j = n - 1; j >= 0; j--) {
        ans[j] = A[j * n1 + n];
        for (int i = 0; i < j; i++)
            A[i * n1 + n] -= A[i * n1 + j] * ans[j];
    }

    for (int i = n - 1; i >= 0; i--)
        for (int j = 0; j <= n; j++)
            if (equals(A[i * n1 + j], 0.0, k) == 0) {
                RangA++;
                if (j != n) {
                    Rang++;
                    break;
                }
            }
    if (Rang != RangA) return 0;
    else if (Rang < n) return 2;
    else return 1;
}

int main(int argc, char **argv) {
    int n;
    double MaxValue = -1e+32;
    double MinValue = 1e+32;
    if (argc < 2) {
        printf("Error: please pass the input and output file names.");
        return 11;
    }
    if (argc < 3) {
        printf("Error: please pass the output file name.");
        return 11;
    }
    FILE *inp = fopen(argv[1], "r");
    if (inp == NULL) {
        printf("Error: the input file cannot be opened.");
        fclose(inp);
        return 13;
    }
    FILE *out = fopen(argv[2], "w");
    if (out == NULL) {
        printf("Error: the output file cannot be opened.");
        fclose(inp);
        fclose(out);
        return 13;
    }
    fscanf(inp, "%i", &n);
    double *res = (double *) malloc(sizeof(double) * n);
    if (res == NULL) {
        printf("Error: can't allocate memory");
        fclose(inp);
        fclose(out);
        return 17;
    }
    double *in = (double *) malloc(sizeof(double) * n * (n + 1));
    if (in == NULL) {
        printf("Error: can't allocate memory");
        fclose(inp);
        fclose(out);
        free(res);
        return 17;
    }

    for (int i = 0; i < n; i++) {
        res[i] = 0;
    }

    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n + 1; j++) {
            if(fscanf(inp,"%lf", &in[i * (n + 1) + j]) != 1){
                printf("Error: read");
                fclose(inp);
                fclose(out);
                free(res);
                free(in);
                return 15;
            }
            if (in[i * (n + 1) + j] > MaxValue) MaxValue = in[i * (n + 1) + j];
            if (in[i * (n + 1) + j] < MinValue) MinValue = in[i * (n + 1) + j];
        }
    }
    double k = MaxValue - MinValue;
    int flag = gauss(n, in, res, k);
    free(in);
    if (flag == 1)
        for (int i = 0; i < n; i++)
            fprintf(out, "%lf\n", res[i]);
    free(res);
    if (flag == 0) fprintf(out, "no solution");
    if (flag == 2) fprintf(out, "many solutions");
    fclose(inp);
    fclose(out);
    return 0;
}