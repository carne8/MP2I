#include <assert.h>
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
    free(head);
    return e;
}

int main() {
    queue* queue = create();
    for (int i = 0; i < 10; i++) enqueue(queue, i);
    for (int i = 0; i < 10; i++) assert(dequeue(queue) == i);

    free_queue(queue);
}
