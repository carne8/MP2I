#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

bool est_dans(int x, int* T, int n) {
    for (int i = 0; i < n; i++)
        if (x == T[i]) return true;
    return false;
}

bool est_dans_rec(int x, int* T, int n) {
    int mid_index = n/2;
    int mid_value = T[mid_index];
    if (mid_value < x) return est_dans_rec(x, T, mid_value-1);
    if (mid_value > x) return est_dans_rec(x, &T[mid_index+1], mid_index);
    return true;
}

int main() {
    // Q2: Lorsqu'on fait la dichotomie sur un tableau de taille 1 ou 0
    int tab[] = {0,1,2,3,4,5};
    if (est_dans_rec(0, tab, 6)) printf("Est dans tab\n");
    else printf("N'est pas dans tab\n");
    return 0;
}