#!/bin/bash

d=gdl_1e-10_05
echo $d

for x in disco_SU/$d/??; do cat $x/g_trees*trees|sed -e "s/_0_0//g" >> $x/truegenetrees; rm $x/g_trees*trees;  done;
for x in disco_SU/$d/??; do cat $x/g_trees*ralpha|sed -e "s/_0_0//g" >> $x/g_trees_ralpha.txt; rm $x/g_trees*ralpha;  done;

for i in $(seq -f "%02g" 1 10)
do
   cp disco_original/$d/${i}/g_500.trees disco_SU/$d/${i}/
   cp disco_original/$d/${i}/g_100.trees disco_SU/$d/${i}/
   cp disco_original/$d/${i}/g_50.trees disco_SU/$d/${i}/
   cp disco_original/$d/${i}/g_true.trees disco_SU/$d/${i}/
done
