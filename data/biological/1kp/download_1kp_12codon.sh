#!/bin/bash 

for r in {10001..10002}
do
   wget -r -np https://datacommons.cyverse.org/browse/iplant/home/shared/onekp_pilot/all_gene_clusters/gene_trees/FNA/12_codon/10001-15000/${r}.tre   
done


