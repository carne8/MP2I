#!/bin/sh
set -e

gcc -lm -Wall -Wextra -Werror -Wvla -fsanitize=address,undefined -o exo ./exo$1.c
./exo