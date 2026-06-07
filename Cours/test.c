#include <assert.h>

int expo_rapide(int x, int p) {
    if (p == 0) return 1;
    if (p == 1) return x;
    return (p % 2 == 0 ? 1 : x) * expo_rapide(x*x, p/2);
}

int expo_rapide_imp(int x, int p) {
    int res = 1;
    while (p > 0) {
        if (p % 2 == 1) res *= x;
        x *= x;
        p /= 2;
    }
    return res;
}

int main() {
    assert(expo_rapide(2, 0) == expo_rapide_imp(2, 0));
    assert(expo_rapide(2, 2) == expo_rapide_imp(2, 2));
    assert(expo_rapide(2, 3) == expo_rapide_imp(2, 3));
    assert(expo_rapide(2, 4) == expo_rapide_imp(2, 4));
    assert(expo_rapide(2, 5) == expo_rapide_imp(2, 5));
    return 0;
}
