#!/usr/bin/env sh

name=$1

mkdir -p bin

COMMON_COMMANDS=" \
  -nostdlib \
  -fno-stack-protector \
  -fno-plt \
  -O0 \
  -mpush-args \
  -fno-asynchronous-unwind-tables"

# Create ASM from C
gcc \
  $COMMON_COMMANDS \
  resources/${name}.c \
  -S -o bin/${name}.s && \

# Create object from ASM
gcc \
  $COMMON_COMMANDS \
  -c bin/${name}.s -o bin/${name}.o && \

# Create executable from object
gcc \
  $COMMON_COMMANDS \
  bin/${name}.o -o bin/${name}

