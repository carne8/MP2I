#include <assert.h>
#include <limits.h>
#include <math.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdlib.h>
#include <stdio.h>
#include <time.h>
#define TAILLE_TABLEAU 25
int tab[TAILLE_TABLEAU];

void initialiser() {
  for (int i = 0; i < TAILLE_TABLEAU; i++) tab[i] = 0;
}

void affiche() {
  for (int i = 0; i < TAILLE_TABLEAU; i++) printf("[%.2i] -> %i\n", i, tab[i]);
}

void affiche_reversed() {
  for (int i = TAILLE_TABLEAU-1; i >= 0; i--) printf("[%.2i] -> %i\n", i, tab[i]);
}

int min_tab(void) {
  int minimum_value = INT_MAX;

  for (int i = 0; i < TAILLE_TABLEAU; i++) {
    if (tab[i] < minimum_value) minimum_value = tab[i];
  }

  return minimum_value;
}

int64_t somme(void) {
  int64_t s = 0;
  for (int i = 0; i < TAILLE_TABLEAU; i++) {
    s += tab[i];
  }
  return s;
}

double moyenne(void) {
  return (double)somme() / TAILLE_TABLEAU;
}

void swap(int x, int y) {
  if (x == y) return;
  tab[x] ^= tab[y];
  tab[y] ^= tab[x];
  tab[x] ^= tab[y];
}

void renverser(void) {
  for (int a = 0; a < TAILLE_TABLEAU / 2; a++) {
    int b = TAILLE_TABLEAU - 1 - a;
    swap(a, b);
  }
}

bool est_trie(void) {
  int i = 1;
  bool est_trie = true;
  while (i < TAILLE_TABLEAU && est_trie) {
    est_trie = tab[i-1] <= tab[i];
    i++;
  }

  return est_trie;
}

void random_fill() {
  for (int i = 0; i < TAILLE_TABLEAU; i++) tab[i] = rand();
}


int indice_min(int debut, int fin) {
  assert(debut <= fin);
  int minimum_value_index = debut;

  for (int i = debut; i <= fin; i++) {
    if (tab[i] < tab[minimum_value_index])
      minimum_value_index = i;
  }

  return minimum_value_index;
}

/// Performs n(n+1)/2 comparisons
void trie() {
  for (int i = 0; i < TAILLE_TABLEAU; i++) {
    int min_index = indice_min(i, TAILLE_TABLEAU - 1);
    swap(i, min_index);
  }
}

int main() {
  srand(time(NULL)); // initialisation de la graine

  initialiser();
  random_fill();
  printf("somme -> %li\n", (long)somme()); // Conversion used to stop clang warning
  printf("min tab -> %i\n", min_tab());
  printf("moyenne -> %f\n", moyenne());
  printf("\n");

  affiche();
  renverser();
  printf("reversed\n");
  affiche_reversed();
  printf("\n");


  trie();
  printf("Triage fini.\n");
  est_trie() ? printf("Tableau trié.\n") : printf("Tableau non trié (celui qui a codé ce programme s'est loupé).\n");
  affiche();
  return 0;
}