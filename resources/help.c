#include <syscall.h>

long write(const void *s, const long size) {
    long result;
    __asm__ __volatile__(
      "syscall"
      : "=a"(result)
      : "0"(__NR_write), "D"(1), "S"(s), "d"(size)
      : "cc", "rcx", "r11", "memory");
    return result;
}

void exit(int code) {
  __asm__ __volatile__(
    "syscall"
    :
    : "a"(__NR_exit)
    : "cc", "rcx", "r11", "memory");
  __builtin_unreachable();
}

int main();

void _start() {
  int return_code = main();
  exit(return_code);
}

