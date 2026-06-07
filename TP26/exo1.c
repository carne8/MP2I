#include <time.h>
#include <stdlib.h>
#include <stdio.h>
void swap(int* i, int* j) {
    int temp = *i;
    *i = *j;
    *j = temp;
}

int partitionner_lomuto(int* t, int len, int pivot) {
    swap(t + pivot, t + len - 1);

    int i = 0;
    int pivot_v = t[len-1];

    for (int j = 0; j < len; j++) {
        if (t[j] <= pivot_v) {
            swap(t + i, t + j);
            i++;
        }
    }

    return --i;
}

void tri_rapide(int* t, int len) {
    if (len <= 1) return;
    int i = partitionner_lomuto(t, len, rand() % len);
    tri_rapide(t, i);
    tri_rapide(t + i+1, len - i-1);
}

int main() {
    srand(time(NULL));
    int t[] = {0, 1, 2, 5, 7, 8, 4};
    tri_rapide(t, 7);
    for (int i = 0; i < 7; i++) printf("%d ", t[i]);
    printf("\n");
    return 0;
}