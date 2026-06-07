#include <assert.h>
#include <stdint.h>
#include <stdbool.h>
#include <stdio.h>
#include <stdlib.h>

typedef uint32_t T;

const uint8_t empty = 0;
const uint8_t occupied = 1;

struct bucket {
    uint8_t status;
    T element;
};
typedef struct bucket bucket;

struct set {
    int p;
    bucket *buckets;
    uint64_t nb_empty;
};
typedef struct set set;


set *set_new(void);
bool set_is_member(set *s, T x);
void set_add(set *s, T x);
void set_remove(set *s, T x);
void set_delete(set *s);

inline uint64_t p2(int p) { return (uint64_t)1 << p; }

uint64_t hash(T x, int p) {
    uint64_t p_ = sizeof(T)*8 - p;
    return x << p_ >> p_;
}

set *empty_table(int p) {
    set* s = malloc(sizeof(set));
    s->p = p;
    s->nb_empty = p2(p);
    s->buckets = malloc(s->nb_empty * sizeof(bucket));

    for (uint64_t i = 0; i < s->nb_empty; i++) {
        s->buckets[i].status = empty;
        s->buckets[i].element = 0;
    }

    return s;
}

set *set_new() { return empty_table(1); }
set *set_exemple() {
    set* s = empty_table(2);
    s->nb_empty = 1;

    int h0 = hash(1492, s->p);
    s->buckets[h0].element = 1492;
    s->buckets[h0].status = occupied;
    printf("Put 1492 at %i\n", h0);

    int h1 = hash(1515, s->p);
    s->buckets[h1].element = 1515;
    s->buckets[h1].status = occupied;
    printf("Put 1515 at %i\n", h1);

    int h2 = 1; //hash(1939, s->p);
    s->buckets[h2].element = 1939;
    s->buckets[h2].status = occupied;
    printf("Put 1939 at %i\n", h2);

    return s;
}

void set_delete(set *s) {
    free(s->buckets);
    free(s);
}


uint64_t search_v1(set *s, T x, bool *found) {
    int h = hash(x, s->p);
    int max_h = hash(x-1, s->p);
    *found = false;

    T x_toto = x;

    while (!*found && h != max_h) {
        bucket bucket = s->buckets[h];
        *found = bucket.status == occupied && bucket.element == x;
        h = hash(++x_toto, s->p);
    }

    return hash(--x_toto, s->p);
}

uint64_t search(set *s, T x, bool *found) {
    int h = hash(x, s->p);
    int table_size = p2(s->p);

    for (int i = 0; i < table_size; i++) {
        bucket bucket = s->buckets[h];

        if (bucket.status == occupied && bucket.element == x) {
            *found = true;
            return h;
        }
        if (bucket.status == empty) {
            *found = false;
            return h;
        }

        h = hash(h + 1, s->p);
    }

    // Element not present and table is full
    exit(1);
} // Ça termine car j'ai utilisé une boucle for

bool set_is_member(set* s, T x) {
    bool found;
    search(s, x, &found);
    return found;
}

T set_get(set *s, uint64_t i) { return s->buckets[i].element; }
uint64_t set_next(set* s, uint64_t i) {
    uint64_t size = p2(s->p);
    i++;
    for (; i < size; i++) {
        if (s->buckets[i].status == occupied) return i;
    }
    return size;
}
uint64_t set_begin(set* s) { return set_next(s, -1); }
uint64_t set_end(set* s) { return p2(s->p); }

bool all_even(set *s) {
    uint64_t i = set_begin(s);
    while (i != set_end(s)) {
        printf("i = %ld -> %d\n", i, set_get(s, i));
        if (set_get(s, i) % 2 == 1) return false;
        i = set_next(s, i);
    }
    return true;
}


void add_no_resize(set* s, T x) {
    bool found;
    uint64_t i = search(s, x, &found);
    if (found) return;;
    s->buckets[i].element = x;
    s->buckets[i].status = occupied;
    s->nb_empty -= 1;
}

void resize(set* s, int p) {
    set* new_set = empty_table(p);

    for (uint64_t i = set_begin(s);
         i < set_end(s);
         i = set_next(s, i)
    ) add_no_resize(new_set, set_get(s, i));

    free(s->buckets);
    s->buckets = new_set->buckets;
    s->nb_empty = new_set->nb_empty;
    s->p = p;

    free(new_set);
}

void set_add(set* s, T x) {
    add_no_resize(s, x);
    if (s->nb_empty * 3 < p2(s->p)) resize(s, s->p+1);
}

int main() {
    assert(hash(0, 3) == 0);
    assert(hash(1, 3) == 1);
    assert(hash(2, 3) == 2);
    assert(hash(8, 3) == 0);
    assert(hash(16, 3) == 0);

    set* exemple = set_exemple();

    bool found;
    uint64_t i = search(exemple, 1492, &found);
    printf("1492 is at %ld, found = %d\n", i, found);
    i = search(exemple, 1515, &found);
    printf("1515 is at %ld, found = %d\n", i, found);
    i = search(exemple, 1939, &found);
    printf("1939 is at %ld, found = %d\n", i, found);
    // if (0) printf("0 is true\n");
    // else printf("0 is false\n");

    printf("\n");
    printf(set_is_member(exemple, 1492) ? "1492 exists\n" : "1492 not present\n");
    printf(set_is_member(exemple, 1515) ? "1515 exists\n" : "1515 not present\n");
    printf(set_is_member(exemple, 1939) ? "1939 exists\n" : "1939 not present\n");
    printf(set_is_member(exemple, 12) ? "12 exists\n" : "12 not present\n");

    printf("\n");
    printf(all_even(exemple) ? "all even\n" : "not all even\n");

    // printf("\nResize:\n");
    // printf("%ld available\n", exemple->nb_empty);
    // resize(exemple, 3);
    // printf("%ld now available\n", exemple->nb_empty);
    // for (uint64_t i = set_begin(exemple);
    //      i < set_end(exemple);
    //      i = set_next(exemple, i)
    // ) printf("%d\n", set_get(exemple, i));

    printf("\nAdd and resize:\n");
    printf("%ld available\n", exemple->nb_empty);
    set_add(exemple, 3);
    printf("%ld now available\n", exemple->nb_empty);
    for (uint64_t i = set_begin(exemple);
         i < set_end(exemple);
         i = set_next(exemple, i)
    ) printf("%d\n", set_get(exemple, i));

    set_delete(exemple);
    return 0;
}
