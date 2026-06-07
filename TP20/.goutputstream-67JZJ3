#include <assert.h>
#include <stdio.h>
#include <stdlib.h>

struct node {
    int value;
    struct node* next;
};
typedef struct node node ;

typedef struct {
    node* head;
    node* tail;
} queue;

node* create_node(int value) {
    node* n = malloc(sizeof(node));
    n->value = value;
    n->next = NULL;
    return n;
}

queue* create() {
    queue* q = malloc(sizeof(queue));
    q->head = NULL;
    q->tail = NULL;
    return q;
}

void free_nodes(node* n) {
    if (n == NULL) return;
    node* next = n->next;
    free(n);
    free_nodes(next);
}

void free_queue(queue* q) {
    free_nodes(q->head);
    free(q);
}

void enqueue(queue* q, int e) {
    if (q->tail == NULL) {
        q->tail = create_node(e);
    } else {
        q->tail->next = create_node(e);
        q->tail = q->tail->next;
    }

    if (q->head == NULL) q->head = q->tail;
}

int dequeue(queue* q) {
    assert(q->head != NULL);
    node* head = q->head;

    int e = head->value;
    q->head = head->next;

    if (q->head == NULL) q->tail = NULL;
    free(head);
    return e;
}

int peek(queue* q) {
    assert(q->head != NULL);
    return q->head->value;
}

int min_(int a, int b) { return a < b ? a : b; }

int* hamming(int n) {
    int* tab = malloc(n * sizeof(int));

    queue* f2 = create(); // Capa = n
    queue* f3 = create(); // Capa = n
    queue* f5 = create(); // Capa = n

    enqueue(f2, 1); enqueue(f3, 1); enqueue(f5, 1);
    tab[0] = 1;

    for (int i = 1; i < n; i++) {
        int m = min_(min_(peek(f2), peek(f3)), peek(f5));
        if (peek(f2) == m) dequeue(f2);
        if (peek(f3) == m) dequeue(f3);
        if (peek(f5) == m) dequeue(f5);

        enqueue(f2, m * 2);
        enqueue(f3, m * 3);
        enqueue(f5, m * 5);
        tab[i] = m;
        printf("%d\n", m);
    }

    free_queue(f2);
    free_queue(f3);
    free_queue(f5);
    return tab;
}

int main() {
    int* t = hamming(100);
    free(t);
    return 0;
}
