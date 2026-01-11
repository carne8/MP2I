#include <stdio.h>
#include <stdbool.h>

int doublon(int tab[], int n) {
    int doublon = -1;
    int i = 0;

    // Supposons tab trié
    int max = -1;

    while (doublon == -1 && i < n) {
        if (tab[i] <= max) doublon = tab[i];
        else max = tab[i];
        i++;
    }

    return doublon;
}


void print_tab(int tab[], int n) {
    printf("[");
    for (int i = 0; i < n; i++) printf(i == n-1 ? "%d" : "%d, ", tab[i]);
    printf("]\n");
}
void fusion_tableaux(int tab1[], int n1, int tab2[], int n2, int result[]) {
    int i1 = 0;
    int i2 = 0;
    int ir = 0;

    while (i1 < n1 || i2 < n2) {
        if (i2 == n2 || (i1 < n1 && tab1[i1] <= tab2[i2])) {
            result[ir] = tab1[i1];
            i1++;
        } else {
            result[ir] = tab2[i2];
            i2++;
        }
        ir++;
    }
}

void swap(int tab[], int i1, int i2) {
    int temp = tab[i1];
    tab[i1] = tab[i2];
    tab[i2] = temp;
}

void rotation_droite(int tab[], int n, int k) {
    for (int x = 0; x < k; x++) {
        int mem = -1;
        for (int i = 0; i < n; i++) {
            int to_move = mem == -1 ? tab[i] : mem;

            int target_idx = (i+1)%n;
            mem = tab[target_idx];
            tab[target_idx] = to_move;
        }
    }
}

int main() {
    int tab[] = { 1, 2, 3, 4, 5 };
    printf("%i\n", doublon(tab, 5));

    // int tab1[3] = {0, 2, 8};
    // int tab2[2] = {1, 5};
    // int tab3[5] = {};

    // print_tab(tab1, 3);
    // print_tab(tab2, 2);
    // fusion_tableaux(tab1, 3, tab2, 2, tab3);
    // print_tab(tab3, 5);

    int tab1[] = {1, 3, 5};
    int tab2[] = {2, 3, 4, 6};
    int tab3[7] = {};
    fusion_tableaux(tab1, 3, tab2, 4, tab3);
    print_tab(tab3, 7);

    // printf("\n");
    // print_tab(tab1, 4);
    // rotation_droite(tab1, 4, 2);
    // print_tab(tab1, 4);

    return 0;
}