#!/bin/bash

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-12_1 -ot 0 -op 1 -lb f:0.000000000001 -ld f:0.000000000001 -lt f:0 > log-20_gdl_1e-12_1.txt

exit

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:470000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 100_gdl_1e-12_1_hILS -ot 0 -op 1 -lb f:0.000000000001 -ld f:0.000000000001 -lt f:0 > log-100_gdl_1e-12_1_hILS.txt

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 100_gdl_1e-12_1 -ot 0 -op 1 -lb f:0.000000000001 -ld f:0.000000000001 -lt f:0 > log-100_gdl_1e-12_1.txt

exit

./simphy -sl f:50 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:470000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 50_gdl_1e-12_1_hILS -ot 0 -op 1 -lb f:0.000000000001 -ld f:0.000000000001 -lt f:0 > log-50_gdl_1e-12_1_hILS.txt

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-12_1 -ot 0 -op 1 -lb f:0.000000000001 -ld f:0.000000000001 -lt f:0 > log-20_gdl_1e-12_1.txt

exit

./simphy -sl f:100 -rs 10 -rl f:10000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gtrees_10000_l1 -ot 0 -op 1 -lb f:0.0000000005 -ld f:0.0000000005 -lt f:0 > log-gtrees_10000_l1.txt

exit

# high dup rate

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gdl_1e-9_0 -ot 0 -op 1 -lb f:0.000000001 -ld f:0.0 -lt f:0 > log-gdl_1e-9_0.txt

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gdl_1e-9_05 -ot 0 -op 1 -lb f:0.000000001 -ld f:0.0000000005 -lt f:0 > log-gdl_1e-9_05.txt

#./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gdl_1e-9_1 -ot 0 -op 1 -lb f:0.000000001 -ld f:0.000000001 -lt f:0 > log-gdl_1e-9_1.txt

exit

# DISCO+QR dataset model conditions

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:470000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-12_1_hILS -ot 0 -op 1 -lb f:0.000000000001 -ld f:0.000000000001 -lt f:0 > log-20_gdl_1e-12_1_hILS.txt

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:470000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-12_1_hILS -ot 0 -op 1 -lb f:0.000000000001 -ld f:0.000000000001 -lt f:0 > log-20_gdl_1e-12_1_hILS.txt

exit

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:470000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-13_1_hILS -ot 0 -op 1 -lb f:0.0000000000001 -ld f:0.0000000000001 -lt f:0 > log-20_gdl_1e-13_1_hILS.txt

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:470000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-12_1_hILS -ot 0 -op 1 -lb f:0.000000000001 -ld f:0.000000000001 -lt f:0 > log-20_gdl_1e-12_1_hILS.txt

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:470000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-11_1_hILS -ot 0 -op 1 -lb f:0.00000000001 -ld f:0.00000000001 -lt f:0 > log-20_gdl_1e-11_1_hILS.txt

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:470000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-10_1_hILS -ot 0 -op 1 -lb f:0.0000000001 -ld f:0.0000000001 -lt f:0 > log-20_gdl_1e-10_1_hILS.txt

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:470000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_5e-10_1_hILS -ot 0 -op 1 -lb f:0.0000000005 -ld f:0.0000000005 -lt f:0 > log-20_gdl_5e-10_1_hILS.txt

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:470000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-9_1_hILS -ot 0 -op 1 -lb f:0.000000001 -ld f:0.000000001 -lt f:0 > log-20_gdl_1e-9_1_hILS.txt

exit

./simphy -sl f:50 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 50_gdl_1e-12_1 -ot 0 -op 1 -lb f:0.000000000001 -ld f:0.000000000001 -lt f:0 > log-50_gdl_1e-12_1.txt

exit

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-13_1 -ot 0 -op 1 -lb f:0.0000000000001 -ld f:0.0000000000001 -lt f:0 > log-20_gdl_1e-13_1.txt

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-12_1 -ot 0 -op 1 -lb f:0.000000000001 -ld f:0.000000000001 -lt f:0 > log-20_gdl_1e-12_1.txt

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-11_1 -ot 0 -op 1 -lb f:0.00000000001 -ld f:0.00000000001 -lt f:0 > log-20_gdl_1e-11_1.txt

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-10_1 -ot 0 -op 1 -lb f:0.0000000001 -ld f:0.0000000001 -lt f:0 > log-20_gdl_1e-10_1.txt

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_5e-10_1 -ot 0 -op 1 -lb f:0.0000000005 -ld f:0.0000000005 -lt f:0 > log-20_gdl_5e-10_1.txt

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-9_1 -ot 0 -op 1 -lb f:0.000000001 -ld f:0.000000001 -lt f:0 > log-20_gdl_1e-9_1.txt

exit

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:200000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-11_1_hILS -ot 0 -op 1 -lb f:0.00000000001 -ld f:0.00000000001 -lt f:0 > log-20_gdl_1e-11_1_hILS.txt

exit

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-10_1 -ot 0 -op 1 -lb f:0.0000000001 -ld f:0.0000000001 -lt f:0 > log-20_gdl_1e-10_1.txt

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:200000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-10_1_hILS -ot 0 -op 1 -lb f:0.0000000001 -ld f:0.0000000001 -lt f:0 > log-20_gdl_1e-10_1_hILS.txt

exit

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-9_1 -ot 0 -op 1 -lb f:0.000000001 -ld f:0.000000001 -lt f:0 > log-20_gdl_1e-9_1.txt

./simphy -sl f:20 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:200000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o 20_gdl_1e-9_1_hILS -ot 0 -op 1 -lb f:0.000000001 -ld f:0.000000001 -lt f:0 > log-20_gdl_1e-9_1_hILS.txt

# DISCO dataset model conditions

# 1000 species

./simphy -sl f:1000 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o species_1000 -ot 0 -op 1 -lb f:0.0000000005 -ld f:0.0000000005 -lt f:0 > log-species_1000.txt

exit

# high dup rate

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gdl_1e-9_0 -ot 0 -op 1 -lb f:0.000000001 -ld f:0.0 -lt f:0 > log-gdl_1e-9_0.txt

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gdl_1e-9_05 -ot 0 -op 1 -lb f:0.000000001 -ld f:0.0000000005 -lt f:0 > log-gdl_1e-9_05.txt

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gdl_1e-9_1 -ot 0 -op 1 -lb f:0.000000001 -ld f:0.000000001 -lt f:0 > log-gdl_1e-9_1.txt

# default

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o default -ot 0 -op 1 -lb f:0.0000000005 -ld f:0.0000000005 -lt f:0 > log-default.txt

# 10,000 genes

./simphy -sl f:100 -rs 10 -rl f:10000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gtrees_10000_l1 -ot 0 -op 1 -lb f:0.0000000005 -ld f:0.0000000005 -lt f:0 > log-gtrees_10000_l1.txt

./simphy -sl f:100 -rs 10 -rl f:10000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gtrees_10000_l0 -ot 0 -op 1 -lb f:0.0000000005 -ld f:0 -lt f:0 > log-gtrees_10000_l0.txt

./simphy -sl f:100 -rs 10 -rl f:10000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gtrees_10000_l05 -ot 0 -op 1 -lb f:0.0000000005 -ld f:0.00000000025 -lt f:0 > log-gtrees_10000_l05.txt


# mid dup rate

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gdl_5e-10_0 -ot 0 -op 1 -lb f:0.0000000005 -ld f:0 -lt f:0 > log-gdl_5e-10_0.txt

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gdl_5e-10_05 -ot 0 -op 1 -lb f:0.0000000005 -ld f:0.00000000025 -lt f:0 > log-gdl_5e-10_05.txt


# low dup rate

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gdl_1e-10_1 -ot 0 -op 1 -lb f:0.0000000001 -ld f:0.0000000001 -lt f:0 > log-gdl_1e-10_1.txt

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gdl_1e-10_0 -ot 0 -op 1 -lb f:0.0000000001 -ld f:0.0 -lt f:0 > log-gdl_1e-10_0.txt

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:50000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o gdl_1e-10_05 -ot 0 -op 1 -lb f:0.0000000001 -ld f:0.00000000005 -lt f:0 > log-gdl_1e-10_05.txt

# varying ILS

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:10000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o ils_1e4 -ot 0 -op 1 -lb f:0.0000000005 -ld f:0.0000000005 -lt f:0 > log-ils_1e4.txt

./simphy -sl f:100 -rs 10 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:200000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 8472 -v 3 -o ils_2e8 -ot 0 -op 1 -lb f:0.0000000005 -ld f:0.0000000005 -lt f:0 > log-ils_2e8.txt

# ASTRAL-Pro default model condition

./simphy -sl f:25 -rs 50 -rl f:1000 -rg 1 -sb f:0.000000005 -sd f:0 -st ln:21.25,0.2 -so f:1 -si f:1 -sp f:470000000 -su ln:-21.9,0.1 -hh f:1 -hs ln:1.5,1 -hl ln:1.551533,0.6931472 -hg ln:1.5,1 -cs 9644 -v 3 -o default_a_pro -ot 0 -op 1 -lb f:0.00000000049 -ld f:0.00000000049 -lt f:0 > log-a-pro.txt
