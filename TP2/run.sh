#!/bin/sh
set -e

gcc -Wall -Wextra -Werror -Wvla -fsanitize=address,undefined -o exo ./exo$1.c
./exo