#include <stdio.h>

struct S {
    int* q;
    int* x;
};


int main() {
    struct S s;
    struct S* y = &s;

    int a = 0;
    int* ptr_a = &a;
    y->x = ptr_a;
    return 0;
}

/*struct RT {
    int A;
    int B[10][20];
    int C;
};

struct ST {
    struct RT X;
    int Y;
    struct RT Z;
};
int *foo(struct ST *s) {
    return &s[1].Z.B[5][13];
}*/