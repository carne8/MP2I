#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

struct Maillon {
    double tete;
    struct Maillon* queue;
};

typedef struct Maillon liste;

liste* cons(double tete, liste* queue) {
    liste* liste = malloc(sizeof(struct Maillon));
    assert(liste != NULL);
    liste->tete = tete;
    liste->queue = queue;
    return liste;
}

liste* depuis_tableau(double* tab, int n) {
    if (n < 1) return NULL;
    return cons(tab[0], depuis_tableau(&tab[1], n-1));
}

void liberer_liste(liste* liste) {
    if (liste == NULL) return;
    liberer_liste(liste->queue);
    free(liste);
}

double hd(liste* l) {
    assert(l != NULL);
    return l->tete;
}

liste* tl(liste* l) {
    assert(l != NULL);
    return l->queue;
}

void iter_liste(liste* l) {
    if (l == NULL) return;
    printf("%f\n", l->tete);
    iter_liste(l->queue);
}

int longueur(liste* l) {
    if (l == NULL) return 0;
    return longueur(l->queue) + 1;
}

double somme(liste* l) {
    if (l == NULL) return 0;
    return l->tete + somme(l->queue);
}

double* vers_tableau(liste* l) {
    int length = longueur(l);
    assert(length != 0);
    double* tableau = malloc(sizeof(double) * length);

    liste* maillon = l;
    int i = 0;
    while (maillon != NULL) {
        tableau[i] = maillon->tete;
        i++;
        maillon = maillon->queue;
    }

    return tableau;
}

bool sont_egales(liste* l1, liste* l2) {
    liste* m1 = l1;
    liste* m2 = l2;
    bool equal = true;

    while (equal && !(m1 == NULL && m2 == NULL)) {
        equal = m1->tete == m2->tete;
        m1 = m1->queue;
        m2 = m2->queue;
    }

    return equal;
}

bool sont_egales2(liste* l1, liste* l2) {
    if (l1 == NULL && l2 == NULL) return true;
    if (l1 == NULL || l2 == NULL) return false;
    return sont_egales2(l1->queue, l2->queue);
}

bool est_triee(liste* l) {
    if (l == NULL) return true;
    bool triee = true;
    double prev = l->tete;
    liste* m = l->queue;

    while (triee && m != NULL) {
        triee = prev <= m->tete;
        prev = m->tete;
        m = m->queue;
    }

    return triee;
}

liste* insere(double x, liste* l) {
    if (l == NULL || l->tete > x) return cons(x, l);
    l->queue = insere(x, l->queue);
    return l;
}

int main() {
    double tab[] = {1,2,3,4,68};
    liste* l = depuis_tableau(tab, 5);
    iter_liste(l);
    printf("Longueur: %i\n", longueur(l));
    printf("Somme: %f\n", somme(l));

    double* t = vers_tableau(l);

    printf("[");
    for (int i = 0; i < longueur(l); i++) printf("%f, ", t[i]);
    printf("]\n");

    free(t);
    liberer_liste(l);

    liste* l1 = cons(2, cons(3, cons(5, NULL)));
    liste* l2 = cons(2, cons(3.2, cons(3, NULL)));
    liste* l3 = NULL;
    if (sont_egales(l1, l2)) printf("Equal\n");
    else printf("Not equal\n");

    printf("l1: %s\n", est_triee(l1) ? "triée" : "pas triée");
    printf("l2: %s\n", est_triee(l2) ? "triée" : "pas triée");
    printf("l3: %s\n", est_triee(l3) ? "triée" : "pas triée");

    liste* l2prime = insere(1.5, l2);

    liberer_liste(l1);
    liberer_liste(l3);
    liberer_liste(l2prime);
    return 0;
}