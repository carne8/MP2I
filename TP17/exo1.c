#include <math.h>
#include <stdio.h>
#include <stdlib.h>

typedef struct Complexe {
    float re;
    float im;
} complexe;

void print_tab(int* tab, int n) {
    if (n == 0) {
        printf("[]\n");
        return;
    }

    printf("[ ");
    for (int i = 0; i < n-1; i++) {
        printf("%i, ", tab[i]);
    }
    printf("%i ]\n", tab[n-1]);
}
void print_tab2(double** tab, int n) {
    if (n == 0) {
        printf("[]\n");
        return;
    }

    printf("[ ");
    for (int i = 0; i < n-1; i++) {
        printf("%f, ", *tab[i]);
    }
    printf("%f ]\n", *tab[n-1]);
}
void print_tab3(complexe* tab, int n) {
    if (n == 0) {
        printf("[]\n");
        return;
    }

    printf("[ ");
    for (int i = 0; i < n-1; i++) {
        printf("%f + i%f, ", tab[i].re, tab[i].im);
    }
    printf("%f + i%f ]\n", tab[n-1].re, tab[n-1].im);
}

// Tri 4
int compare_tri_4(const void* xp, const void* yp) {
    int x = *(int*)xp;
    int y = *(int*)yp;
    return abs(x) - abs(y);
}

void tri_4(int* tab, int n) {
    qsort(tab, n, sizeof(int), compare_tri_4);
}

// Tri 5
int compare_tri_5(const void* xp, const void* yp) {
    double x = **(double**)xp;
    double y = **(double**)yp;
    return fabs(x) < fabs(y) ? -1 : 1;
}

void tri_5(double** tab, int n) {
    qsort(tab, n, sizeof(double*), compare_tri_5);
}

// Tri 6
float mod_squared(complexe c) { return c.re*c.re + c.im*c.im; }
int compare_tri_6(const void* xp, const void* yp) {
    complexe x = *(complexe*)xp;
    complexe y = *(complexe*)yp;
    return mod_squared(y) < mod_squared(x) ? -1 : 1;
}

void tri_6(complexe* tab, int n) {
    qsort(tab, n, sizeof(complexe*), compare_tri_6);
}

int main() {
    int tab1[] = { -3, 4, 5, 2, 1 };

    // Tri 4
    tri_4(tab1, 5);
    print_tab(tab1, 5);

    // Tri 5
    double d1 = 0.4;
    double d2 = -0.3;
    double d3 = -0.001;
    double* tab2[] = { &d1, &d2, &d3 };
    tri_5(tab2, 3);
    print_tab2(tab2, 3);

    // Tri 6
    complexe tab3[] = { { 1.0, 0.0 }, { 2.0, 0.0 }, { 1.0, 1.0 } };
    tri_6(tab3, 3);
    print_tab3(tab3 , 3);

    return 0;
}
