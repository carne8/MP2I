#include <assert.h>
#include <stdio.h>
#include <stdbool.h>

int absolue(int x) { return x >= 0 ? x : -x; }
void afficher_k_fois(int n, char c) {
  for (; n > 0; n--)
    printf("%c", c);
  printf("\n");
}

void square(int size) {
  for (int s = size; s > 0; s--) {
    for (int s = size; s > 0; s--)
      printf("*");
    printf("\n");
  }
}

void pyramid(int size) {
  for (int line = 0; line <= size; line++) {
    for (int i = line; i > 0; i--)
      printf("*");
    printf("\n");
  }
}

void upside_down_pyramid(int size) {
  for (int line = size; line > 0; line--) {
    for (int i = line; i > 0; i--)
      printf("*");
    printf("\n");
  }
}

void semi_diamond(int size) {
  int width = size % 2 == 1 ? size : size + 1;
  for (int line = 0; line <= width; line++) {
    for (int i = line; i > 0; i--)
      printf("*");
    printf("\n");
  }
  for (int line = width-1; line > 0; line--) {
    for (int i = line; i > 0; i--)
      printf("*");
    printf("\n");
  }
}

void diamond(int size) {
  int width = size % 2 == 1 ? size : size + 1;

  for (int line = 1; line <= width; line += 2) {
    int space_count = (width - line)/2;
    for (int i = 0; i < space_count; i++) printf(" ");
    for (int i = 0; i < line; i++) printf("*");
    for (int i = 0; i < space_count; i++) printf(" ");
    printf("\n");
  }
  for (int line = width-2; line > 0; line -= 2) {
    int space_count = (width - line)/2;
    for (int i = 0; i < space_count; i++) printf(" ");
    for (int i = 0; i < line; i++) printf("*");
    for (int i = 0; i < space_count; i++) printf(" ");
    printf("\n");
  }
}

void empty_diamond(int size) {
  int width = size % 2 == 1 ? size : size + 1;

  for (int line = 1; line <= width; line += 2) {
    int space_count = (width - line)/2;
    for (int i = 0; i < space_count; i++) printf(" ");

    if (line == 1) printf("*");
    else {
      printf("*");
      for (int i = 0; i < line-2; i++) printf(" ");
      printf("*");
    }

    for (int i = 0; i < space_count; i++) printf(" ");
    printf("\n");
  }
  for (int line = width-2; line > 0; line -= 2) {
    int space_count = (width - line)/2;
    for (int i = 0; i < space_count; i++) printf(" ");

    if (line == 1) printf("*");
    else {
      printf("*");
      for (int i = 0; i < line-2; i++) printf(" ");
      printf("*");
    }

    for (int i = 0; i < space_count; i++) printf(" ");
    printf("\n");
  }
}

double expo_rapide(double x, int n) {
  if (n == 0) return 1;
  if (n % 2 == 0) {
    double squared_x = expo_rapide(x, n / 2);
    return squared_x*squared_x;
  }

  double squared_x = expo_rapide(x, (n-1) / 2);
  return x*squared_x*squared_x;
}

bool est_premier(int n) {
  assert(n >= 0);
  if (n == 1) return false;
  for (int i = 2; i < n; i++) {
    if (n % i == 0) return false;
  }
  return true;
}

void affiche_diviseurs(int n) {
  assert(n >= 0);
  for (int i = 1; i <= n; i++) {
    if (n % i == 0) printf("%i is a divisor of %i\n", i, n);
  }
}

int main() {
  printf("|-5| = %i\n", absolue(-5));
  printf("|5| = %i\n", absolue(5));
  printf("|0| = %i\n", absolue(0));
  afficher_k_fois(5, '*');

  printf("\n");
  square(5);
  printf("\n");
  pyramid(5);
  printf("\n");
  upside_down_pyramid(5);
  printf("\n");
  semi_diamond(5);
  printf("\n");
  diamond(5);
  printf("\n");
  empty_diamond(5);

  printf("\n");
  printf("exp(1, 1) = %f\n", expo_rapide(1, 1));
  printf("exp(1, 5) = %f\n", expo_rapide(1, 5));
  printf("exp(2, 8) = %f\n", expo_rapide(2, 8));
  printf("exp(3, 5) = %f\n", expo_rapide(3, 5));

  printf("\n");
  est_premier(2) ? printf("2 is prime\n") : printf("2 is not prime\n");
  est_premier(5) ? printf("5 is prime\n") : printf("5 is not prime\n");
  est_premier(89) ? printf("89 is prime\n") : printf("89 is not prime\n");
  est_premier(1) ? printf("1 is prime\n") : printf("1 is not prime\n");

  printf("\n");
  affiche_diviseurs(28);

  return 0;
}