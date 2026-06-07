#include <stdio.h>
#include <time.h>
#include <stdlib.h>
#include <string.h>

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
            swap(t, t + j);
            i++;
        }
    }

    return --i;
}


int partitionner_hoare(int* t, int len, int pivot) {
    swap(t + pivot, t);
    int p = t[0];

    int i = 1;
    int j = len;

    while (i != j) {
        if (t[i] <= p) i++;
        else if (t[i] > p) swap(t+i, t + --j);
    }

    swap(t, t + --i);
    return i;
}

void tri_rapide(int* t, int len) {
    if (len <= 1) return;
    int i = partitionner_hoare(t, len, 0);
    tri_rapide(t, i);
    tri_rapide(t + i+1, len - i-1);
}

int* tableau_aleatoire(int n) {
    int* t = malloc(n * sizeof(int));
    srand(time(NULL));
    for (int i = 0; i < n; i++) t[i] = rand() % n;
    return t;
}

void tri_naif(int* t, int n) {
    for (int i = 0; i < n; i++)
    for (int j = i+1; j < n; j++)
    if (t[i] > t[j]) swap(t + i, t + j);
}

void test_temps_execution(int* t, int n) {
    int* t1 = malloc(n * sizeof(int));
    int* t2 = malloc(n * sizeof(int));
    memcpy(t1, t, n);
    memcpy(t2, t, n);

    int c = clock();
    tri_naif(t1, n);
    int time1 = clock() - c;
    
    c = clock();
    tri_rapide(t2, n);
    int time2 = clock() - c;

    printf("Bulles: %ld ms\n", time1 * 1000L / CLOCKS_PER_SEC);
    printf("Rapide: %ld ms\n", time2 * 1000L / CLOCKS_PER_SEC);
    free(t1);
    free(t2);
    printf("\n");
}

void test_temps_execution_croissant(int* t, int n) {
    tri_rapide(t, n);
    printf("Croissant: \n");
    for (int i = 0; i < n; i++) printf("%d ", t[i]);
    printf("\n");
    test_temps_execution(t, n);
}

void test_temps_execution_decroissant(int* t, int n) {
    for (int i = 0; i < n; i++) t[i] *= -1;
    tri_rapide(t, n);
    for (int i = 0; i < n; i++) t[i] *= -1;
    printf("Décroissant: \n");
    for (int i = 0; i < n; i++) printf("%d ", t[i]);
    printf("\n");
    test_temps_execution(t, n);
}

int main() {
    int n = 17;
    int* t = tableau_aleatoire(n);
    test_temps_execution(t, n);
    test_temps_execution_croissant(t, n);
    test_temps_execution_decroissant(t, n);
    
    free(t);
    return 0;
}