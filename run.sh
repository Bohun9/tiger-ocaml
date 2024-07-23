#/bin/bash

dune exec -- bin/main.exe tests/$1.tig > tests/$1.s

as tests/$1.s -o tests/$1.o

ld tests/$1.o -o tests/$1

./tests/$1
