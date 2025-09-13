#include <stdio.h>

void fonction_mystere(int* x, int* y) {
    *x = *x - *y;
    // x1 == x0-y0;
    *y = *x + *y;
    // y1 == x1+y0 == x0-y0+y0 == x0;
    *x = *y - *x;
    // x2 == y1-x1 == x0-(x0-y0) == y0;
}

int main() {
    int a = 34, b = 34;
    fonction_mystere(&a, &b);
    // Je guess que ça échange les valeurs des deux variables.
    printf("a = %i, b = %i\n", a, b);
    // Ça fonctionne si a == b.

    fonction_mystere(&a, &a);
    printf("a = %i, b = %i\n", a, b);
    // Ça ne fonctionne point si x == y.

    return 0;
}