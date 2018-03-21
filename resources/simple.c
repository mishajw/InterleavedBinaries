#include "help.c"

int main() {
  long a = 1;
  long b = 2;
  long c = a + b;
  write((void *)&c, sizeof(long));
  return 0;
}

