#!/bin/bash

#conditions=(20_gdl_1e-11_1 20_gdl_1e-12_1 20_gdl_1e-13_1 20_gdl_1e-10_1 20_gdl_5e-10_1 20_gdl_1e-9_1)
#conditions=(20_gdl_1e-11_1_hILS 20_gdl_1e-12_1_hILS 20_gdl_1e-13_1_hILS 20_gdl_1e-10_1_hILS 20_gdl_5e-10_1_hILS 20_gdl_1e-9_1_hILS)
conditions=(20_gdl_1e-12_1)

for d in ${conditions[@]}; do
  echo $d
  for x in $d/??; do cat $x/g_trees*trees|sed -e "s/_0_0//g" >> $x/truegenetrees; rm $x/g_trees*trees;  done;
  for x in $d/??; do cat $x/g_trees*ralpha|sed -e "s/_0_0//g" >> $x/g_trees_ralpha.txt; rm $x/g_trees*ralpha;  done;
  for i in $(seq -f "%02g" 1 10)
  do
     cp disco_qr_datasets/$d/${i}/g_500.trees $d/${i}/
     cp disco_qr_datasets/$d/${i}/g_100.trees $d/${i}/
     cp disco_qr_datasets/$d/${i}/g_50.trees $d/${i}/
     cp disco_qr_datasets/$d/${i}/g_true.trees $d/${i}/
  done
done
