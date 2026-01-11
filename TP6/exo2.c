#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

struct Noeud {
    int valeur;
    struct Noeud* gauche;
    struct Noeud* droit;
};
typedef struct Noeud arbre;

arbre* construire_arbre(int valeur, arbre* gauche, arbre* droit) {
    arbre* node = malloc(sizeof(struct Noeud));
    node->valeur = valeur;
    node->gauche = gauche;
    node->droit = droit;
    return node;
}

int max(int a, int b) { return a < b ? b : a; }

int hauteur(arbre* a) {
    if (a == NULL) return -1;
    return max(hauteur(a->gauche), hauteur(a->droit)) + 1;
}

int taille(arbre* a) {
    if (a == NULL) return 0;
    return taille(a->gauche) + taille(a->droit) + 1;
}

bool est_dans_arbre(int valeur, arbre* a) {
    if (a == NULL) return false;
    return 
        a->valeur == valeur 
        || est_dans_arbre(valeur, a->gauche) 
        || est_dans_arbre(valeur, a->droit);
}

void ajoute_valeur(int valeur, arbre* a) {
    if (a == NULL) return;
    a->valeur += valeur;
    ajoute_valeur(valeur, a->gauche);
    ajoute_valeur(valeur, a->droit);
}

bool est_strict(arbre* a) {
    return a->gauche != a->droit
        ? false
        : est_strict(a->gauche) && est_strict(a->droit);
}

int main() {
    arbre* a = construire_arbre(
        8,
        construire_arbre(5, NULL, NULL),
        construire_arbre(5, NULL, NULL)
    );

    ajoute_valeur(1, a);

    printf("%s\n", est_dans_arbre(5, a) ? "true" : "false");

    return 0;
}