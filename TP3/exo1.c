#include <stdio.h>
#include <stdlib.h>

int main() {
    int k = -25;
    int* p_k = NULL;
    p_k = &k;

    k = 42;
    *p_k = 69;

    printf("%i and %i\n", k, *p_k);
    printf("%p\n", p_k);

    return 0;
}