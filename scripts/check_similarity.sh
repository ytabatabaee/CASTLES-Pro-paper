#!/bin/bash

d=20_gdl_1e-12_1

for i in $(seq -f "%02g" 1 10)
do
  python3 compare_trees.py -t1 disco_SU/$d/${i}/s_tree.trees -t2 disco_qr_datasets/$d/${i}/s_tree.trees
done
