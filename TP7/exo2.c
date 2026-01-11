#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

int nb_chemins(int m, int n) {
    if (m == 0 && n == 0) return 0;
    if ((m == 1 && n == 0) || (m == 0 && n == 1)) return 1;
    if (m <= 0) return nb_chemins(0, n-1);
    if (n <= 0) return nb_chemins(m-1, 0);
    return nb_chemins(m-1, n) + nb_chemins(m, n-1);
}

int main() {
    // Q1
    // Pour (1,0) et (0,1); il n'y a qu'une seule
    // façon de se déplacer vers la case (0, 0)

    // Q2
    // C(m, n) en fonction de C(m-1, n) et C(m, n-1)
    // C(m, n) = C(m-1, n) + C(m, n-1)

    printf("%d\n", nb_chemins(3, 21));
    printf("%d\n", nb_chemins(2, 1));
    return 0;
}