#include <stdio.h>

void foo() {
    int a = 123;
    int b = 075;
    int sum = a + b;
    printf("Suma: %d\n", sum);
}

void bar() {
    float x = 1.23;
    float y = 3.14e-2;
    float sum = x + y;
    printf("Suma de flotantes: %.2f\n", sum);
}

void printNumbers() {
    for (int i = 0; i < 5; i++) {
        printf("i: %d\n", i);
    }
}

void printWhile() {
    int j = 0;
    while (j < 5) {
        printf("j: %d\n", j);
        j++;
    }
}

void checkSwitch(int d) {
    switch(d) {
        case 123:
            printf("d es 123\n");
            break;
        case 075:
            printf("d es 075\n");
            break;
        default:
            printf("d no es 123 ni 075\n");
            break;
    }
}

int main() {
    foo();
    bar();
    
    int c = 0x1A;
    if (c < 100) {
        printf("c es menor que 100\n");
    } else {
        printf("c es mayor o igual que 100\n");
    }

    printNumbers();
    printWhile();
    checkSwitch(45);

    char ch = 'a';
    printf("Caracter: %c\n", ch);

    const char *str = "Hello, world!";
    printf("Cadena: %s\n", str);

    double d = 3.14e-2;
    printf("Doble: %.2f\n", d);
    
    return 0;
}