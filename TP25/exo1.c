#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>


int grille_exemple[9][9] = {
    {2, 5, 0, 0, 3, 0, 9, 0, 1},
    {0, 1, 0, 0, 0, 4, 0, 0, 0},
    {4, 0, 7, 0, 0, 0, 2, 0, 8},
    {0, 0, 5, 2, 0, 0, 0, 0, 0},
    {0, 0, 0, 0, 9, 8, 1, 0, 0},
    {0, 4, 0, 0, 0, 3, 0, 0, 0},
    {0, 0, 0, 3, 6, 0, 0, 7, 2},
    {0, 7, 0, 0, 0, 0, 0, 0, 3},
    {9, 0, 3, 0, 0, 0, 6, 0, 4}
};

void affiche_grille(int grille[9][9]) {
    for (int y = 0; y < 9; y++) {
        if (y != 0 && y % 3 == 0) printf("+-----+-----+-----+\n");
        for (int x = 0; x < 9; x++) 
            if (grille[y][x]) printf("|%d", grille[y][x]);
            else printf("| ");
        printf("|\n");
    }
}

void suivant(int grille[9][9], int i, int j, int* x, int* y) {
    while (i < 9 && j < 9 && grille[j][i] != 0) {
        i++;
        if (i >= 9) { j++; i = 0; }
    }
    // while (j < 9) {
    //     if (i >= 9) { i = 0; j++; }
    //     if (grille[j][i] == 0) {
    //         *x = i;
    //         *y = j;
    //         return;
    //     }
    //     i++;
    // }

    if (i >= 9 || j >= 9) {
        *x = -1; *y = -1;
    } else {
        *x = i; 
        *y = j;
    }
}

bool valide(int grille[9][9], int i, int j) {
    int value = grille[j][i];

    // Ligne
    for (int x = 0; x < 9; x++)
        if (x != i && grille[j][x] == value) return false;
    // Colonne
    for (int y = 0; y < 9; y++)
        if (y != j && grille[y][i] == value) return false;
    // Carré
    // for (int h = 0; h < 9; h++) {
    //     int y = x_init + h / 3;
    //     int x = y_init + h % 3;
    //     if (x != i && y != j && grille[y][x] == value) return false;
    // }
    int x_init = (i / 3) * 3;
    int y_init = (j / 3) * 3;
    for (int y = y_init; y < y_init + 3; y++)
        for (int x = x_init; x < x_init + 3; x++)
            if (x != i && y != j && grille[y][x] == value) return false;

    return true;
}

bool resout_aux(int grille[9][9], int i, int j) {
    for (int c = 1; c <= 9; c++) {
        grille[j][i] = c;
        if (!valide(grille, i, j)) continue;

        int i_suivant = i;
        int j_suivant = j;
        suivant(grille, i, j, &i_suivant, &j_suivant);
        
        if (i_suivant == -1) return true;
        if (resout_aux(grille, i_suivant, j_suivant)) return true;
    }

    grille[j][i] = 0;
    return false;
}

bool resout(int grille[9][9]) {
    int i = 0;
    int j = 0;
    suivant(grille, i, j, &i, &j);
    return resout_aux(grille, i, j);
}

int main() {
    affiche_grille(grille_exemple);

    int x = 0;
    int y = 0;
    suivant(grille_exemple, 0, 0, &x, &y);
    assert(x == 2 && y == 0);
    suivant(grille_exemple, 2, 3, &x, &y);
    assert(x == 4 && y == 3);
    suivant(grille_exemple, 6, 8, &x, &y);
    assert(x == 7 && y == 8);
    suivant(grille_exemple, 8, 8, &x, &y);
    assert(x == -1 && y == -1);

    grille_exemple[0][2] = 1;
    assert(!valide(grille_exemple, 2, 0));
    grille_exemple[0][2] = 7;
    assert(!valide(grille_exemple, 2, 0));
    grille_exemple[0][2] = 4;
    assert(!valide(grille_exemple, 2, 0));
    grille_exemple[0][2] = 6;
    assert(valide(grille_exemple, 2, 0));
    grille_exemple[0][2] = 8;
    assert(valide(grille_exemple, 2, 0));
    grille_exemple[0][2] = 0;

    printf("\n\nSolving...\n");
    assert(resout(grille_exemple));
    affiche_grille(grille_exemple);

    return 0;
}