#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct {
    int taille;   // Nombre d'éléments dans le tas
    int capacite; // Taille du tableau tab
    int *tab;
} tas;

tas* creer_tas(int capacite) {
    tas* t = malloc(sizeof(tas));
    t->capacite = capacite;
    t->taille = 0;
    t->tab = malloc(capacite * sizeof(int));

    return t;
}

void free_tas(tas* t) { free(t->tab); free(t); }
bool est_vide(tas* t) { return t->taille == 0; }
int pere(int i) {
    assert(i != 0);
    return (i-1)/2;
}
int gauche(int i) { return 2*i + 1; }
int droite(int i) { return 2*i + 2; }

void echanger(tas* t, int i, int j) {
    t->tab[i] ^= t->tab[j];
    t->tab[j] ^= t->tab[i];
    t->tab[i] ^= t->tab[j];
}

void percoler_bas(tas* t, int i) {
    int g = gauche(i);
    int d = droite(i);

    int next;
    if (d < t->taille) {
        if (t->tab[i] >= t->tab[g]) {
            if (t->tab[i] >= t->tab[d]) return;
            else next = d;
        }
        else {
            if (t->tab[g] < t->tab[d]) next = d;
            else next = g;
        }
    }
    else if (g < t->taille && t->tab[i] < t->tab[g]) next = g;
    else return;

    echanger(t, i, next);
    percoler_bas(t, next);
}

void percoler_haut(tas* t, int i) {
    if (i == 0) return;
    if (t->tab[pere(i)] >= t->tab[i]) return;
    echanger(t, pere(i), i);
    percoler_haut(t, pere(i));
}

void inserer(tas* t, int x) {
    assert(t->taille < t->capacite);
    t->tab[t->taille] = x;
    t->taille++;
    percoler_haut(t, t->taille-1);
}

int extraire_max(tas* t) {
    assert(t->taille > 0);
    int max = t->tab[0];
    t->taille--;
    t->tab[0] = t->tab[t->taille];
    percoler_bas(t, 0);
    return max;
}

void modifier(tas* t, int i, int x) {
    int old = t->tab[i];
    t->tab[i] = x;

    if (x > old) percoler_haut(t, i);
    else if (x < old) percoler_bas(t, i);
}

// Exercice 2
void tri_tas(int* t, int n) {
    tas* tas = creer_tas(n);
    for (int i = 0; i < n; i++) inserer(tas, t[i]);
    for (int i = 0; i < n; i++) t[n-1-i] = extraire_max(tas); // Tri dans l'ordre croissant
    free_tas(tas);
}

void tri_tas_opti(int* t, int n) {
    tas tas = { .capacite = n, .tab = t, .taille = n };
    for (int i = 0; i < n; i++) percoler_haut(&tas, i);
    for (int i = 0; i < n; i++) t[tas.taille-1] = extraire_max(&tas);
}


void display_tab(int* t, int n) {
    if (n == 0) {
        printf("[]");
        return;
    }

    printf("[ %d, ", t[0]);
    for (int i = 1; i < n-1; i++) printf("%d, ", t[i]);
    printf("%d ]\n", t[n-1]);
}

int main() {
    int tab[] = {0, -1, 42, 1};
    display_tab(tab, 4);
    tri_tas_opti((int*)&tab, 4);
    display_tab(tab, 4);
    return 0;
}
