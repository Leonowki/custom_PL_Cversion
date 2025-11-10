#include <stdio.h>

int main() {
    int a = 1;
    a++;
    int b = ++a;

    printf("a = %d (0x%X)\n", a, a);
    printf("b = %d (0x%X)\n", b, b);

    return 0;
}
