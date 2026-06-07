#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#define STACK_MAX_LENGTH 1000

typedef struct {
    int* data;
    int size;
} stack_t;

stack_t* new_stack() {
    stack_t* s = malloc(sizeof(stack_t));
    s->data = malloc(sizeof(int) * STACK_MAX_LENGTH);
    s->size = 0;
    return s;
}

bool is_empty(stack_t* s) { return s->size == 0; }
bool is_full(stack_t* s) { return s->size == STACK_MAX_LENGTH; }

void push(int x, stack_t* s) {
    assert(s->size < STACK_MAX_LENGTH);
    s->data[s->size] = x;
    s->size++;
}

int pop(stack_t* s) {
    assert(s->size > 0);
    s->size--;
    return s->data[s->size];
}

void free_stack(stack_t* s) {
    free(s->data);
    free(s);
}

int peek(stack_t* s) {
    assert(s->size > 0);
    return s->data[s->size - 1];
}

int egal(stack_t* p1, stack_t* p2) {
    if (p1 == NULL && p2 == NULL) return true;
    if (p1 == NULL || p2 == NULL) return false;
    stack_t* temp = new_stack();

    bool equals = true;
    bool both_empty = false;
    while (equals && !both_empty) {
        if (is_empty(p1) && is_empty(p2)) {
            both_empty = true;
            continue;
        }
        if (is_empty(p1) || is_empty(p2)) {
            equals = false;
            continue;
        }

        int a = pop(p1);
        int b = pop(p2);
        equals = a == b;
        push(a, temp);
        push(b, temp);
    }

    while (!is_empty(temp)) {
        push(pop(temp), p2);
        push(pop(temp), p1);
    }

    free_stack(temp);
    return equals;
}

void iter_pile(stack_t* p) {
    if (p == NULL) return;
    stack_t* temp = new_stack();

    while (!is_empty(p)) {
        int e = pop(p);
        push(e, temp);
        printf("%d\n", e);
    }

    while (!is_empty(temp)) push(pop(temp), p); // Refill the stack
    free_stack(temp);
}

int main() {
    stack_t* s1 = new_stack();
    push(0, s1);
    push(2, s1);
    push(3, s1);

    stack_t* s2 = new_stack();
    push(0, s2);
    push(2, s2);
    push(3, s2);

    stack_t* s3 = new_stack();
    push(1, s3);
    push(3, s3);
    push(4, s3);

    // Test iter
    printf("First iter:\n");
    iter_pile(s3);
    printf("Second iter:\n");
    iter_pile(s3);

    // Test assert
    assert(egal(s1, s2));
    assert(!egal(s1, s3));

    // Ensure stacks did not change
    assert(pop(s1) == 3);
    assert(pop(s1) == 2);
    assert(pop(s1) == 0);

    assert(pop(s2) == 3);
    assert(pop(s2) == 2);
    assert(pop(s2) == 0);

    assert(pop(s3) == 4);
    assert(pop(s3) == 3);
    assert(pop(s3) == 1);

    // Free memory
    free_stack(s1);
    free_stack(s2);
    free_stack(s3);
}
