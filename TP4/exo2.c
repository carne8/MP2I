#include <stdio.h>
#include <stdbool.h>

void remonter_bulles(int tab[], int n, int i) {
    for (int k = 1; k < n-i; k++) {
        int left_idx = k-1;
        int right_idx = k;

        if (tab[left_idx] > tab[right_idx]) {
            int temp = tab[left_idx];
            tab[left_idx] = tab[right_idx];
            tab[right_idx] = temp;
        }

        printf("[");
        for (int i = 0; i < n; i++) printf("%d, ", tab[i]);
        printf("]\n");
    }
}

bool remonter_bulles_opti(int tab[], int n, int i) {
    bool swap_happened = false;
    
    for (int k = 1; k < n-i; k++) {
        int left_idx = k-1;
        int right_idx = k;

        if (tab[left_idx] > tab[right_idx]) {
            int temp = tab[left_idx];
            tab[left_idx] = tab[right_idx];
            tab[right_idx] = temp;
            swap_happened = true;
        }
    }
    printf("opti[");
    for (int i = 0; i < n; i++) printf("%d, ", tab[i]);
    printf("]\n");
    return swap_happened;
}

void tri_bulle(int tab[], int n) {
    for (int i = 0; i < n; i++) {
        remonter_bulles(tab, n, i);
    }
}

void tri_bulle_opti(int tab[], int n) {
    int i = 0;
    bool sorted = false;
    while (i < n && !sorted) {
        sorted = !remonter_bulles_opti(tab, n, i);
        i++;
    }
}

int main() {
    int tab[4] = {69, 4, 2, 3};
    tri_bulle(tab, 4);
    int tab2[4] = {69, 4, 2, 3};
    tri_bulle_opti(tab2, 4);
}