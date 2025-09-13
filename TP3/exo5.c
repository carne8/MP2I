#include <stdio.h>
#include <unistd.h>
#include <math.h>

struct S {
    int a;
    double b;
    char* c;
};

void afficher_s(struct S elt) {
    printf("{ a = %i\n", elt.a);
    printf("  b = %f\n", elt.b);
    printf("  c = %p }\n", elt.c);
}

typedef struct {
    double re;
    double im;
} complexe_t;

double partie_reelle(complexe_t x) { return x.re; }
double partie_imaginaire(complexe_t x) { return x.im; }
void afficher_complexe(complexe_t elt) {
    double re = partie_reelle(elt);
    double im = partie_imaginaire(elt);
    if (im < 0) printf("(%f - i*%f)\n", re, -im);
    else printf("(%f + i*%f)\n", re, im);
}

complexe_t addition(complexe_t x, complexe_t y) {
    x.re += y.re;
    x.im += y.im;
    return x;
}
complexe_t conjugue(complexe_t x) {
    x.im *= -1;
    return x;
}
double module(complexe_t x) {
    return sqrt(x.im*x.im + x.re*x.re);
}
complexe_t multiplication(complexe_t x, complexe_t y) {
    complexe_t product;
    product.re = x.re * y.re - x.im * y.im;
    product.im = x.re * y.im + x.im * y.re;
    return product;
}

void conjugue_en_place(complexe_t* x) {
    x->im *= -1;
}

int main() {
    struct S elt;
    elt.a = 42;
    elt.b = 42.42;
    elt.c = "Hello world!";
    afficher_s(elt);
    printf("\n");

    complexe_t a;
    a.re = 1;
    a.im = 2;
    complexe_t b;
    b.re = 2;
    b.im = 1;

    afficher_complexe(a);
    printf("+ ");
    afficher_complexe(b);
    printf("= ");
    afficher_complexe(addition(a, b));
    printf("\n");

    printf("Conjugué de \n");
    afficher_complexe(a);
    printf("= ");
    afficher_complexe(conjugue(a));
    printf("\n");

    printf("Module de \n");
    afficher_complexe(a);
    printf("= %f\n", module(a));
    printf("\n");

    afficher_complexe(a);
    printf("* ");
    afficher_complexe(b);
    printf("= ");
    afficher_complexe(multiplication(a, b));
    printf("\n");

    printf("Conjugué en place de \n");
    afficher_complexe(b);
    printf("= ");
    conjugue_en_place(&b);
    afficher_complexe(b);
    printf("\n");

    return 0;
}