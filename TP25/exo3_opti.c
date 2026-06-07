#include <assert.h>
#include <stdbool.h>
#include <stdio.h>  // printf
#include <stdlib.h> // malloc et rand
#include <time.h>   // time, pour la seed du générateur de nombres aléatoires


void affiche_tableau(int arr[], int n) {
  for (int i = 0; i < n; i++) {
      printf("%2d ", arr[i]);
  }
  printf("\n");
}

int* genere_tableau_aleatoire(int n) {
  int* t = malloc(sizeof(int) * n);
  for (int i = 0; i < n; i++) {
      t[i] = rand() % 1000; // On se limite à des nombres entre 0 et 99 pour être facile à lire
  }
  return t;
}

bool est_trie(int* t, int n) {
  for (int i = 0; i < n - 1; i++) {
      if (t[i] > t[i + 1]) {
          return false;
      }
  }
  return true;
}





void fusion(int* t, int len, int* dst) {
    int* left = t;
    int* right = t + len/2;
    int* left_end = right;
    int* right_end = t + len;

    // Don't merge if already sorted
    if (*(right-1) <= *right) {
        for (; t < right_end;) *dst++ = *t++;
        return;
    }

    while (left < left_end && right < right_end) {
        if (*left < *right)
            *dst++ = *left++;
        else
            *dst++ = *right++;
    }

    for (;left < left_end;) *dst++ = *left++;
    for (;right < right_end;) *dst++ = *right++;
}

void tri_fusion_aux(int* t, int len, int* dst) {
    if (len <= 1) return;

    tri_fusion_aux(dst, len/2, t);
    tri_fusion_aux(dst + len/2, len/2 + len%2, t + len/2);
    fusion(dst, len, t);
}

void tri_fusion(int* t1, int len) {
    int* t2 = malloc(len * sizeof(int));
    for (int i = 0; i < len; i++) t2[i] = t1[i];
    tri_fusion_aux(t1, len, t2);
    free(t2);
}

int int_comparator( const void * first, const void * second ) {
    int firstInt = * (const int *) first;
    int secondInt = * (const int *) second;
    return firstInt - secondInt;
}

int main() {
    int n = 1000000;
    srand(time(NULL));
    int* t1 = genere_tableau_aleatoire(n);
    srand(time(NULL));
    int* t2 = genere_tableau_aleatoire(n);

    int time0 = clock();
    tri_fusion(t1, n);
    int time1 = clock();
    qsort(t2, n, sizeof(int), int_comparator);
    int time2 = clock();

    printf("Fusion: %lims\n", (long)((time1 - time0) * 10e3 / CLOCKS_PER_SEC));
    printf("QSort:  %lims\n", (long)((time2 - time1) * 10e3 / CLOCKS_PER_SEC));

    assert(est_trie(t1, n));
    assert(est_trie(t2, n));
    free(t1);
    free(t2);
}
