#include "stdbool.h"
#include <stdio.h>
#include <stdlib.h>

bool** create_matrix(int n) {
    bool** lignes = malloc(sizeof(bool*) * n);
    for (int i = 0; i < n; i++)
        lignes[i] = calloc(n, sizeof(bool));

    return lignes;
}

void free_matrix(bool** m, int n) {
    for (int i = 0; i < n; i++) free(m[i]);
    free(m);
}

bool** identity(int n) {
    bool** m = create_matrix(n);
    for (int i = 0; i < n; i++) m[i][i] = true;
    return m;
}

bool** produit(bool** a, bool** b, int n) {
    bool** m = create_matrix(n);
    for (int i = 0; i < n; i++) {
    for (int j = 0; j < n; j++) {
    for (int k = 0; k < n; k++)
        m[i][j] |= a[i][k] & b[k][j];
    }}

    return m;
}

bool** closure(bool** m, int n) {
    bool** r = identity(n);

    for (int k = 0; k <= n; k++) {
        bool** r_plus = produit(r, m, n);

        for (int i = 0; i < n; i++)
            for (int j = 0; j < n; j++)
                r[i][j] = r_plus[i][j];

        free_matrix(r_plus, n);
    }

    return r;
}

bool* accessible(bool** m, int n, int x) {
    // On fait un parcours en largeure
    bool* deja_enfile = calloc(n, sizeof(bool));
    int* queue = calloc(n, sizeof(int));
    int queue_idx = 0;

    queue[queue_idx++] = x;
    deja_enfile[x] = true;

    while (queue_idx != 0)
    {
        int y = queue[--queue_idx];

        for (int i = 0; i < n; i++) {
            if (!m[y][i]) continue;
            if (deja_enfile[i]) continue;
            deja_enfile[i] = true;
            queue[queue_idx++] = m[y][i];
        }
    }

    free(queue);
    return deja_enfile;
}


void print_m(bool** m, int n) {
    for (int i = 0; i < n; i++) {
        for (int j = 0; j < n; j++) printf(m[i][j] ? "@" : "0");
        printf("\n");
    }
    printf("\n");
}

void print_t(bool* m, int n) {
    for (int i = 0; i < n; i++) printf(m[i] ? "@" : "0");
    printf("\n\n");
}

bool** closure2(bool** m, int n) {
    bool** r = malloc(n * sizeof(bool*));

    for (int i = 0; i < n; i++)
        r[i] = accessible(m, n, i);

    return r;
}

int main() {
    bool** m1 = create_matrix(2);
    m1[0][0] = true;
    m1[0][1] = true;
    m1[1][1] = true;

    bool** m2 = create_matrix(2);
    m2[0][1] = true;
    m2[1][0] = true;

    bool** m3 = produit(m1, m2, 2);

    bool* x = accessible(m1, 2, 1);
    print_t(x, 2);

    bool** closure_1 = closure(m1, 2);
    bool** closure_2 = closure2(m1, 2);

    print_m(closure_1, 2);
    print_m(closure_2, 2);

    free_matrix(m1, 2);
    free_matrix(m2, 2);
    free_matrix(m3, 2);
    free(x);
    free_matrix(closure_1, 2);
    free_matrix(closure_2, 2);
    return 0;
}