#include <stdio.h>
void echange_entiers(int x, int y) {
    int temp = x;
    x = y;
    y = temp;
}

void echange_entiers_fixed(int* x, int* y) {
    int temp = *x;
    *x = *y;
    *y = temp;
}

int main() {
    int a = 1, b = 2;
    echange_entiers(a, b);
    echange_entiers_fixed(&a, &b);

    printf("a = %i, b = %i\n", a, b);

    return 0;
}