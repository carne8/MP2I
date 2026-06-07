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



void fusion(int* t, int len) {
    int* temp = malloc(sizeof(int) * len);
    int x = 0;
    int i = 0;
    int j = len/2;

    while (i < len/2 && j < len) {
        if (t[i] < t[j])
            temp[x++] = t[i++];
        else
            temp[x++] = t[j++];
    }

    for (; i < len/2; i++) temp[x++] = t[i];
    for (; j < len; j++) temp[x++] = t[j];
    for (int i = 0; i < len; i++) t[i] = temp[i];
    free(temp);
}

void tri_fusion(int* t, int len) {
    if (len == 0 || len == 1) return;

    tri_fusion(t, len/2);
    tri_fusion(&t[len/2], len/2 + len%2);
    fusion(t, len);
}

int int_comparator( const void * first, const void * second ) {
    int firstInt = * (const int *) first;
    int secondInt = * (const int *) second;
    return firstInt - secondInt;
}

int main() {
    int n = 10000;
    srand(time(NULL));
    int* t1 = genere_tableau_aleatoire(n);
    srand(time(NULL));
    int* t2 = genere_tableau_aleatoire(n);

    int time0 = clock();
    tri_fusion(t1, n);
    int time1 = clock();
    qsort(t2, n, sizeof(int), int_comparator);
    int time2 = clock();

    printf("Fusion: %i\n", time1 - time0);
    printf("QSort:  %i\n", time2 - time1);

    assert(est_trie(t1, n));
    assert(est_trie(t2, n));
    free(t1);
    free(t2);
}
