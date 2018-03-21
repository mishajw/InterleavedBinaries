#include "help.c"

int f(long a, long b) {
  return a + b;
}

int main() {
  long a = 1;
  long b = 2;
  long c = f(a, b);
  write((void *)&c, sizeof(long));
}

