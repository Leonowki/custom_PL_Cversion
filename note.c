#include <stdio.h>

int main(void)
{
    /* Simple declarations */
    int a = 10, i = 5, x = 1, y = 2, z = 3;
    char ch = 'A', c1 = 65;

    /* Chained assignment */
    int p, q, r;
    p = q = r = 7;

    /* Arithmetic and precedence */
    int sum = a + i;
    int prod = a * (i + x);
    int expr1 = (1 + 2) * 3;
    int expr2 = 1 + 2 * 3;

    /* Unary operations */
    int neg = -5;
    int plus = +10;
    int combined = -(a + i) + +(x - y);
    int double_neg = -(-neg);

    /* Increment/decrement */
    int inc_test = 0;
    inc_test = ++inc_test + 1;
    int post = inc_test++;

    /* Safe divisor example */
    int safe_divisor = 1;
    int complicated = (((1 + 2) * (3 + 4)) - (a * (i + x))) / safe_divisor;

    /* Deep nesting */
    int deep = (((((((1 + 2) * 3) - 4) / 1) + 6) * 7) - 8);

    /* Mixed char and int arithmetic */
    int from_char = ch + 1;
    char ch2 = 'A' + 5;
    int diff = 'z' - 'a';

    /* Character escape and hex */
    char newline = '\n';
    // char hexC = '\x41';

    int spaced = 42;
    char compact = 'Z';

    /* Multiple declarations */
    int m = 1, n = 2, o = 3;

    /* Associativity tests */
    int assoc1 = 1 + 2 + 3;
    int assoc2 = 100 / 10 / 2;

    /* Tricky operator combos */
    int u = 3, v = 4, w = 5;
    int tricky1 = u++ + v;  /* (u++) + v */
    int tricky2 = v-- - -w; /* (v--) - (-w) */

    /* Comma operator with assignments */
    int tmp = 0;
    tmp = (tmp = 1, tmp += 2, tmp * 3);

    /* Inner scope shadowing */
    int shadow = 5;

    /* More arithmetic stress */
    int A = 2, B = 3, C = 4, D = 5;

    /* -------------------- PRINT ALL IN HEX -------------------- */
    printf("a = 0x%X\n", a);
    printf("i = 0x%X\n", i);
    printf("x = 0x%X\n", x);
    printf("y = 0x%X\n", y);
    printf("z = 0x%X\n", z);
    printf("ch = 0x%X\n", ch);
    printf("c1 = 0x%X\n", c1);
    printf("p = 0x%X, q = 0x%X, r = 0x%X\n", p, q, r);
    printf("sum = 0x%X\n", sum);
    printf("prod = 0x%X\n", prod);
    printf("expr1 = 0x%X, expr2 = 0x%X\n", expr1, expr2);
    printf("neg = 0x%X, plus = 0x%X\n", neg, plus);
    printf("combined = 0x%X, double_neg = 0x%X\n", combined, double_neg);
    printf("inc_test = 0x%X, post = 0x%X\n", inc_test, post);
    printf("complicated = 0x%X\n", complicated);
    printf("deep = 0x%X\n", deep);
    printf("from_char = 0x%X, ch2 = 0x%X, diff = 0x%X\n", from_char, ch2, diff);
    printf("newline = 0x%X\n", newline);
    printf("spaced = 0x%X, compact = 0x%X\n", spaced, compact);
    printf("m = 0x%X , n = 0x%X , o = 0x%X\n", m, n, o);
    printf("assoc1 = 0x%X, assoc2 = 0x%X\n", assoc1, assoc2);
    printf("tricky1 = 0x%X, tricky2 = 0x%X\n", tricky1, tricky2);
    printf("u= 0x%X , v = 0x%X , w = 0x%X\n", u, v, w);
    printf("tmp = 0x%X\n", tmp);
    printf("shadow = 0x%X\n", shadow);
    printf("A = 0x%X, B = 0x%X, C = 0x%X, D = 0x%X\n", A, B, C, D);

    return 0;
}
