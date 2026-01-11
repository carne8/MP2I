#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int64_t binom(int n, int k) {
    assert(0 <= k && k <= n);
    if (n < 0 || k < 0) return 0;
    if (k == 0 || n == k) return 1;
    return binom(n - 1, k - 1) + binom(n - 1, k);
}

int nb_appels_binom(int n, int k) {
    assert(0 <= k && k <= n);
    if (n < 0 || k < 0) return 1;
    if (k == 0 || n == k) return 1;
    return 1 + nb_appels_binom(n-1, k-1) + nb_appels_binom(n-1, k);
}

int64_t** creer_matrice_nulle(int m) {
    int64_t** lignes = malloc(sizeof(int64_t*) * m);
    for (int i = 0; i < m; i++) lignes[i] = calloc(m, sizeof(int64_t));
    return lignes;
}

void detruire_matrice(int64_t** mat, int m) {
    for (int i = 0; i < m; i++) free(mat[i]);
    free(mat);
}

int64_t binom_from_mat(int64_t** mat, int n, int k) {
    assert(0 <= k && k <= n);
    if (n < 0 || k < 0) return 0;
    if (k == 0 || n == k) return 1;
    return mat[n - 1][k - 1] + mat[n - 1][k];
}

int64_t binom_memo(int n, int k) {
    int64_t** mat = creer_matrice_nulle(n+1);
    for (int i = 0; i <= n; i++) mat[i][i] = 1;
    for (int i = 0; i <= n; i++) mat[i][0] = 1;

    for (int line = 1; line < n; line++)
        for (int column = 1; column <= k && column <= line; column++)
            mat[line][column] = binom_from_mat(mat, line, column);

    int64_t b = binom_from_mat(mat, n, k);
    detruire_matrice(mat, n+1);
    return b;
}

int main() {
    printf("%ld\n", binom_memo(100, 10));
    // Q3: On calcul plusieurs fois la même chose.
    // Q7: Parce que "j parmi i" a besoin des cases au dessus et ont rempli la matrice de haut en bas.
    return 0;
}