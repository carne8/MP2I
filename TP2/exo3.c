#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

void affiche_arguments(int argc, char *argv[]) {
  for (int i = 0; i < argc; i++) printf("%s\n", argv[i]);
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

int main(int argc, char *argv[]) {
  // printf("%s\n", argv[0]);
  // affiche_arguments(argc, argv);
  if (argc < 3) {
    printf("Error: Not enough arguments.\n");
    printf("Usage: ./exo3 X N\n");
    printf("Returns X to the power N.\n");
    return 0;
  }

  double x = atof(argv[1]);
  int n = atoi(argv[2]);
  // atof and atoi returns 0 if strings are not numbers.

  if (n < 0) {
    printf("Error: negative exposants are not supported.\n");
    return 1;
  }

  printf("%f^%i = %f\n", x, n, expo_rapide(x, n));
  return 0;
}