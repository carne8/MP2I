#include <assert.h>
#include <unistd.h>

typedef struct {
    int longueur;
    int* donnees;
} tableau_entiers;

int get(tableau_entiers t, int k) {
    assert(k < t.longueur);
    assert(k >= 0);
    return t.donnees[k];
}

int main() {
    return 0;
}