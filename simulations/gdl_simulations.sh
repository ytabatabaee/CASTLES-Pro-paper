#!/bin/bash

# DISCO model conditions

# low dup rate

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gdl_1e-10_1 -ot 0 -op 1 -lb f:0.0000000001 -ld f:0.0000000001 -lt f:0 > log-gdl_1e-10_1.txt

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gdl_1e-10_0 -ot 0 -op 1 -lb f:0.0000000001 -ld f:0.0 -lt f:0 > log-gdl_1e-10_0.txt

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gdl_1e-10_05 -ot 0 -op 1 -lb f:0.0000000001 -ld f:0.00000000005 -lt f:0 > log-gdl_1e-10_05.txt

exit

# varying ILS
./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o default -ot 0 -op 1 -lb f:0.0000000005 -ld f:0.0000000005 -lt f:0 > log-default.txt

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:10000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o ils_1e4 -ot 0 -op 1 -lb f:0.0000000005 -ld f:0.0000000005 -lt f:0 > log-ils_1e4.txt

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:200000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o ils_2e8 -ot 0 -op 1 -lb f:0.0000000005 -ld f:0.0000000005 -lt f:0 > log-ils_2e8.txt


# ASTRAL-Pro default model condition
./simphy -sl f:25 -rs 50 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:470000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 9644 -v 3 -o default -ot 0 -op 1 -lb f:0.00000000049 -ld f:0.00000000049 -lt f:0 > log-a-pro.txt
