#include <stdio.h>

int f(long a, long b) {
  return a;
}

int main() {
  long a = 1;
  long b = 2;
  long c = f(a, b);
  printf("c: %ld\n", c);
}

