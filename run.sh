#/bin/bash

dune exec -- bin/main.exe tests/$1.tig > tests/$1.s

as tests/$1.s -o tests/$1.o

gcc -static -no-pie -o tests/$1 tests/$1.o runtime.c

./tests/$1
