#!/bin/sh
set -e

gcc -O0 -Wall -Wextra -Wvla -fsanitize=address,undefined -lm -ldl squelette.c -o executable.exe
./executable.exe ./images/bird.png ./out.png