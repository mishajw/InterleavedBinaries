#include "help.c"

void f(long *x) {
  *x += 1;
}

int main() {
  long x = 0;
  f(&x);
  write((void *)&x, sizeof(long));
}

