// a = 108; b = 48;
// a = 48; b = 108 % 48 = 12;
// a = 12; b = 48 % 12 = 0;

#include <stdio.h>
int PGCD(int a, int b) {
    printf("%d\n", b);
    // On voit que les valeurs de b sont des entiers 
    // strictments décroissants et minorés par 0.
    // Donc la boucle se terminera. Car la suite (b) est finie.

    // b est un variant de boucle.
    
    while (b > 0) {
        int temp = a;
        a = b;
        b = temp % b;
        printf("%d\n", b); 
    }

    return a;
}

int main() {
    printf("%d\n", PGCD(69, 68));
}