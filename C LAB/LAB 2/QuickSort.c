#include <stdio.h>
#include <string.h>
#include <stdlib.h>

struct Person {
    char surname[21];
    char name[21];
    char patronymic[21];
    long long number;
};
int compare(const struct Person a, const struct Person b) {
    int temp = 0;
    if ((temp = strcmp(a.surname, b.surname)) == 0)
        if ((temp = strcmp(a.name, b.name)) == 0)
            if ((temp = strcmp(a.patronymic, b.patronymic)) == 0)
                temp = (a.number > b.number) ? 1 : 0;
    return (temp > 0 ? 1 : 0);
}

struct Stack {
    size_t size;
    size_t capacity;
    int *data;
};

struct Stack *createStack() {
    struct Stack *tmp = (struct Stack *) malloc(sizeof(struct Stack));
    if (tmp == NULL)
        return NULL;
    tmp->capacity = 1;
    tmp->size = 0;
    tmp->data = (int *) malloc(tmp->capacity * sizeof(int));
    if (tmp->data == NULL)
        return NULL;
    return tmp;
}

int push(struct Stack *s, int x) {
    if (s->size >= s->capacity) {
        s->capacity *= 2;
        int *sub = s->data;
        s->data = (int *) realloc(s->data, s->capacity * sizeof(int));
        if (s->data == NULL) {
            free(sub);
            return -1;
        }
    }
    s->data[s->size++] = x;
    return 0;
}

int pop(struct Stack *s) {
    return s->data[--s->size];
}

int isEmpty(struct Stack *pStack) {
    if (pStack->size == 0) return 0;
    return 1;
}

int qsit(struct Person *a, long size) {
    long i, j;
    long l, r;
    struct Stack *lbstack = createStack();
    struct Stack *ubstack = createStack();
    long mid;
    struct Person middle;
    struct Person temp;
    push(lbstack, 0);
    push(ubstack, size - 1);
    while (isEmpty(lbstack) == 1 && isEmpty(ubstack) == 1) {
        l = pop(lbstack);
        r = pop(ubstack);
        while (l < r) {
            mid = (l + r) / 2;
            i = l;
            j = r;
            middle = a[mid];
            while (i <= j) {
                while (compare(middle, a[i])) i++;
                while (compare(a[j], middle)) j--;
                if (i <= j) {
                    temp = a[i];
                    a[i] = a[j];
                    a[j] = temp;
                    i++;
                    j--;
                }
            }
            if (i < mid) {
                if (i < r) {
                    push(lbstack, i);
                    push(ubstack, r);
                }
                r = j;
            } else {
                if (j > l) {
                    push(lbstack, l);
                    push(ubstack, j);
                }
                l = i;
            }
        }
    }
    return 0;
}

//int qs(struct Person *arr, int first, int last, int glub) { //вроде тоже работает
//    if (first < last) {
//        if (glub < 10000) {
//            int left = first, right = last;
//            struct Person middle = arr[(left + right) / 2];
//            while (left <= right) {
//                while (compare(middle, arr[left])) left++;
//                while (compare(arr[right], middle)) right--;
//                if (left <= right) {
//                    struct Person tmp = arr[left];
//                    arr[left] = arr[right];
//                    arr[right] = tmp;
//                    left++;
//                    right--;
//                }
//            }
//            qs(arr, first, right, glub + 1);
//            qs(arr, left, last, glub + 1);
//        } else {                                        //если большая глубина рекурсии спрыгиваем на итеративную реализацию
//            return qsit(arr, last - first + 1);
//        }
//    }
//}

int main(int argc, char **argv) {
    if (argc < 2) {
        printf("\nInput file name not passed\n");
        return 1;
    }
    if (argc < 3) {
        printf("\nOutput file name not passed\n");
        return 2;
    }
    FILE *in;
    in = fopen(argv[1], "r");
    if (!in) {
        printf("\nInput file not found\n");
        return 3;
    }
    struct Person *arr;
    int capacity = 1;
    int size = 0;
    arr = (struct Person *) malloc(capacity * sizeof(struct Person));
    if (arr == NULL) {
        error:
        printf("Failed to allocate memory");
        fclose(in);
        return 4;
    }
    while (1) {
        char temp[21];
        fscanf(in, "%s", temp);
        if (feof(in)) break;
        for (int i = 0; i < 21; i++) {
            arr[size].surname[i] = temp[i];
        }
        fscanf(in, "%s %s %lli", arr[size].name, arr[size].patronymic, &arr[size].number);
        if (size + 1 == capacity) {
            capacity *= 2;
            struct Person *sup = arr;
            arr = (struct Person *) realloc(arr, capacity * sizeof(struct Person));
            if (arr == NULL) {
                free(sup);
                goto error;
            }
        }
        size++;
    }
    fclose(in);
    if (qsit(arr, size) < 0) { //вдруг память выделить не получиться
        free(arr);
        printf("Failed to allocate memory");
        return 4;
    }
    FILE *out;
    out = fopen(argv[2], "w");
    if (!out) {
        printf("Failed to open output file");
        free(arr);
        return 5;
    }
    for (int i = 1; i < size; i++) {
        if(compare(arr[i - 1], arr[i]) == 1) printf("!!!");
    }

    for (int i = 0; i < size; i++)
        fprintf(out, "%s %s %s %lli\n", arr[i].surname, arr[i].name, arr[i].patronymic, arr[i].number);
    fclose(out);
    free(arr);
    return 0;
}
