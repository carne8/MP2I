#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>

double* creer_tableau(int n, double x) {
    double* p = malloc(n * sizeof(double));
    for (int i = 0; i < n; i++) p[i] = x;
    return p;
}

int main() {
    printf("Double uses %lu bytes\n", sizeof(double));

    double* p_double = malloc(sizeof(double));
    *p_double = 42.;
    printf("Double: %f\n", *p_double);
    free(p_double);

    double* tab = creer_tableau(420000000, 42);
    printf("%f\n", tab[0]);
    sleep(5);
    free(tab); // Segmentation fault if no free

    return 0;
}