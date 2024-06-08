#include <stdio.h>

int factorial(int n) {
   if (n == 0 || n == 1) {
      return 1;
   } else {
      return n * factorial(n - 1);
   }
}

int main() {
   int num;

   printf("Introduce un número para calcular su factorial: ");
   scanf("%d", &num);

   printf("El factorial de %d es %d\n", num, factorial(num));

   return 0;
}
