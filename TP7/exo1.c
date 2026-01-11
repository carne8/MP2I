#include <assert.h>
#include <stdbool.h>
#include <stdio.h>

int u(int n) {
    if (n == 0) return 1;
    return 3*u(n-1) + 2;
}

int u_iter(int n) {
    int u = 1;
    for (int i = 0; i < n; i++) u = 3*u + 2;
    return u;
}

int PGCD_rec(int a, int b) {
    if (b <= 0) return a;
    return PGCD_rec(b, a % b);
}

int fib_rec(int n) {
    if (n == 0 || n == 1) return n;
    return fib_rec(n - 1) + fib_rec(n - 2);
}

typedef struct {int fst; int snd;} Couple;

int b(int n);
int a(int n) {
    if (n == 0) return 0;
    return b(n-1);
}

int b(int n) {
    if (n == 0) return 1;
    return a(n-1) + b(n-1);
}

Couple fib_couple(int n) {
    Couple c = {a(n), b(n)};
    return c;
}

int main() {
    printf("u0: %d\n", u(0));
    printf("u1: %d\n", u(1));
    printf("u2: %d\n", u(2));
    printf("u3: %d\n", u(3));
    printf("u4: %d\n", u(4));
    printf("u5: %d\n", u(5));

    printf("iter_u0: %d\n", u_iter(0));
    printf("iter_u1: %d\n", u_iter(1));
    printf("iter_u2: %d\n", u_iter(2));
    printf("iter_u3: %d\n", u_iter(3));
    printf("iter_u4: %d\n", u_iter(4));
    printf("iter_u5: %d\n", u_iter(5));


    printf("PGCD(15, 25): %d\n", PGCD_rec(15, 25));
    printf("fib(2): %d\n", fib_rec(2));
    printf("fib(3): %d\n", fib_rec(3));
    printf("fib(4): %d\n", fib_rec(4));
    printf("fib(5): %d\n", fib_rec(5));
    // printf("fib(45): %d\n", fib_rec(45)); // ça prend du temps

    Couple c = fib_couple(100);
    printf("fib-couple(100): %d\n", c.fst); // ça prend du temps
    return 0;
}