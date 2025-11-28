## CASTLES-Pro Datasets

This repository contains the datasets and scripts used in the following paper:

- Y. Tabatabaee, C. Zhang, S. Arasti, S. Mirarab (2025). Species tree branch length estimation despite incomplete lineage sorting, duplication, and loss. Genome Biology and Evolution. Volume 17, Issue 11. https://academic.oup.com/gbe/article/17/11/evaf200/8343050

For experiments in this study, we analyzed three sets of simulated datasets and nine biological datasets with different sources of gene tree discordance (details below). All datasets can be accessed from [this](https://drive.google.com/drive/folders/1xrDjxkSSnbcvUVW_QcaUS8-mou1sJyww?usp=sharing) Google Drive link. In all simulated datasets, the true species trees have branch lengths in substitution-units.

### Simulated datasets

**ILS-only simulations**

For the ILS simulations, we reused the 100-taxon ILS-only dataset from [Tabatabaee et. al. (2023)](https://academic.oup.com/bioinformatics/article/39/Supplement_1/i185/7210452) available at [https://github.com/ytabatabaee/CASTLES-paper/](https://github.com/ytabatabaee/CASTLES-paper/tree/main). This dataset has four model conditions with varying sequence lengths (1600bp, 800bp, 400bp, 200bp) corresponding to different levels of gene tree estimation error (23%, 31%, 42%, and 55%), each with 50 replicates. Results and intermediate data from the experiments in the paper are available in `ASTRAL_SU.tar.xz`. 

Below is a description of files in each directory.

- `s_tree.trees`: true species tree in substitution units
- `s_tree.ralpha`: mutation rates for species tree branches in pre-order traversal
- `g_trees_ralpha.txt`: mutation rates for gene tree branches in pre-order traversal
- `truegenetrees`: true gene trees in substitution units
- `fasttree_genetrees_[seq-len]_non`: gene trees estimated using FastTree2 from alignments with length [seq-len]bp
- `erable_patristic_all_fasttree_genetrees_[seq-len]_non_s_tree.trees.derooted.length.nwk`: true species tree furnished with ERaBLE SU branch lengths on gene trees estimated from alignments with length [seq-len]bp
- `patristic_[MODE]_fasttree_genetrees_[seq-len]_non.mat`: patristic distance matrix calculated using minimum (`min`), average (`avg`) or all (`all`) pairwise distances for gene trees estimated from alignments with length [seq-len]bp
- `concat_for_fasttree_[seq-len].fasta` or `all-genes_for_fasttree.phylip`: concatenation of all gene alignments with length [seq-len]bp
- `RAxML_result.concat_for_fasttree_[seq-len]_s_tree.trees`: true species tree furnished with RAxML SU branch lengths from concatenation of alignments with length [seq-len]bp
- `castles_fasttree_genetrees_[seq-len]_non_s_tree.trees`: true species tree furnished with CASTLES SU branch lengths run on gene trees estimated from alignments with length [seq-len]bp
- `castles2_fasttree_genetrees_[seq-len]_non_s_tree.trees`: true species tree furnished with CASTLES-Pro (with Taylor approximation) SU branch lengths run on gene trees estimated from alignments with length [seq-len]bp
- `castles2_lambert_fasttree_genetrees_[seq-len]_non_s_tree.trees`: true species tree furnished with CASTLES-Pro (with Lambert approximation) SU branch lengths run on gene trees estimated from alignments with length [seq-len]bp
- `castles2_lamberts1_fasttree_genetrees_[seq-len]_non_s_tree.trees`: true species tree furnished with CASTLES-Pro (with Lambert approximation and psuedocountof 1/s) SU branch lengths run on gene trees estimated from alignments with length [seq-len]bp
- `castles2_lamberts_fasttree_genetrees_[seq-len]_non_s_tree.trees`: true species tree furnished with CASTLES-Pro (with Lambert approximation and psuedocountof 1/2s) SU branch lengths run on gene trees estimated from alignments with length [seq-len]bp
- `castlespro_fasttree_genetrees_[seq-len]_non_s_tree.trees`: true species tree furnished with CASTLES-Pro SU branch lengths run on gene trees estimated from alignments with length [seq-len]bp
- `fastme_BalLS_patristic_[MODE]_fasttree_genetrees_[seq-len]_non_s_tree.trees.derooted`: true species tree furnished with FastME SU branch lengths with minimum or average distances run on gene trees estimated from alignments with length [seq-len]bp
- `TCMM_castlespro_fasttree_genetrees_[seq-len]_non_s_tree_lam_0.trees`: true species tree furnished with TCMM SU branch lengths on gene trees estimated from alignments with length [seq-len]bp
- `TCMM_castlespro_fasttree_genetrees_[seq-len]_non_s_tree_lam_best.trees`: true species tree furnished with TCMM+CASTLES-Pro SU branch lengths on gene trees estimated from alignments with length [seq-len]bp


**HGT+ILS simulations**

We generated a new 50-taxon HGT+ILS dataset based on the parameters from [Davidson et. al. (2015)](https://bmcgenomics.biomedcentral.com/articles/10.1186/1471-2164-16-S10-S1) study. This dataset has six model conditions named as `model.50.2000000.0.000001.[HGT-rate]` with different levels of HGT, each with 50 replicates. The average number of HGT events per gene for the six model conditions are 0, 0.08, 0.2, 0.8, 8 and 20, corresponding to HGT rates of 10^−9x(0, 2, 5, 20, 200, and 500). The original dataset is available at [https://databank.illinois.edu/datasets/IDB-6670066](https://databank.illinois.edu/datasets/IDB-6670066). Our new dataset, as well as the intermediate data and output of methods are available in `HGT_SU.tar.xz`. 

Below is a description of files in each directory.

- `s_tree.trees`: true species tree in substitution units
- `s_tree.ralpha`: mutation rates for species tree branches in pre-order traversal
- `g_trees_ralpha.txt`: mutation rates for gene tree branches in pre-order traversal
- `truegenetrees`: true gene trees in substitution units
- `estimatedgenetre`: estimated gene trees
- `erable_patristic_all_estimatedgenetre_s_tree.trees.derooted.length.nwk`: true species tree furnished with ERaBLE SU branch lengths on estimated gene trees
- `patristic_[MODE]_estimatedgenetre.mat`: patristic distance matrix calculated using minimum (`min`), average (`avg`) or all (`all`) pairwise distances for estimated gene trees
- `all-genes.phylip`: concatenation of all gene alignments
- `RAxML_result.concat_s_tree.trees`: true species tree furnished with RAxML SU branch lengths
- `castles_estimatedgenetre_s_tree.trees`: true species tree furnished with CASTLES SU branch lengths run on estimated gene trees
- `castlespro_estimatedgenetre_s_tree.trees`: true species tree furnished with CASTLES-Pro SU branch lengths run on estimated gene trees
- `fastme_BalLS_patristic_[MODE]_estimatedgenetre_s_tree.trees.derooted`: true species tree furnished with FastME SU branch lengths with minimum or average distances (specified with MODE) run on estimated gene trees
- `TCMM_castlespro_estimatedgenetre_lam_0.trees`: true species tree furnished with TCMM SU branch lengths run on estimated gene trees
- `TCMM_castlespro_estimatedgenetre_lam_best.trees`: true species tree furnished with TCMM+CASTLES-Pro SU branch lengths run on estimated gene trees


**GDL+ILS simulations**

We updated the GDL+ILS dataset from [Willson et. al. (2022, 2023)](https://academic.oup.com/sysbio/article/71/3/610/6358739) to produce species trees with substitution-unit branch lengths. The model conditions of this dataset are represented as `[num-taxa]_gdl_[GDL-rate]_[dup/loss-ratio]_[high/low-ILS]`. The `default` model condition has 100 taxa, GDL rate of 10^-10 with equal loss rate. The number of genes varies between 50-1000, with the exception of the model condition `gtrees_10000_l1` that has up to 10,000 genes. The number of species varies between 20 to 1000 (for the `species_1000` model condition). The original datasets (including the sequence alignments) are available at [https://databank.illinois.edu/datasets/IDB-4050038](https://databank.illinois.edu/datasets/IDB-4050038) and  [https://databank.illinois.edu/datasets/IDB-5748609](https://databank.illinois.edu/datasets/IDB-5748609).  Results and intermediate data from the experiments in the paper are available in `GDL_SU.tar.xz`. 

Below is a description of files in each directory.

- `s_tree.trees`: true species tree in substitution units
- `s_tree.ralpha`: mutation rates for species tree branches in pre-order traversal
- `g_trees_ralpha.txt`: mutation rates for gene tree branches in pre-order traversal
- `truegenetrees`: true gene trees in substitution units
- `g_[seq-len].trees.[num-genes]`: `num-genes` gene family trees estimated from `seq-len` sequence alignments
- `erable_patristic_all_disco_g_[seq-len].trees.[num-genes]_s_tree.trees.derooted.length.nwk`: true species tree furnished with ERaBLE SU branch lengths on DISCO decomposed gene trees
- `patristic_[MODE]_disco_g_[seq-len].trees.[num-genes].mat`: patristic distance matrix calculated using average (`avg`) or all (`all`) pairwise distances for DISCO decomposed estimated gene trees
- `concat_g_[seq-len].[num-genes]`: concatenation of `num-genes` gene sequence alignments with length `seq-len`
- `disco_g_[seq-len].trees.[num-genes]`: single-copy DISCO decomposed gene trees estimated from `g_[seq-len].trees.[num-genes]` gene family trees
- `RAxML_result.concat_g_[seq-len].[num-genes]_s_tree.trees`: true species tree furnished with CA-DISCO (with RAxML) SU branch lengths
- `castles-original_disco_g_[seq-len].trees_s_tree.trees`: true species tree furnished with CASTLES-DISCO SU branch lengths run on gene trees estimated from `seq-len` sequence alignments
- `castlespro_g_[seq-len].trees.[num-genes]_s_tree.trees`: true species tree furnished with CASTLES-Pro SU branch lengths run on gene trees estimated from `seq-len` sequence alignments
- `fastme_BalLS_patristic_avg_disco_g_[seq-len].trees.[num-genes]_s_tree.trees.derooted`: true species tree furnished with FastME SU branch lengths with average distances run on DISCO decomposed estimated gene trees

### Biological datasets
We analyzed three biological datasets where ILS is the primary source of gene tree discordance, three datasets with GDL, and three bacterial datasets with high rates of HGT. The results of all experiments are provided in `biological.tar.xz`.

Below is a description of files in each directory.

#### ILS
- **Brids**: 363-taxon dataset from [Stiller et al. (2024)](https://www.nature.com/articles/s41586-024-07323-1) with 63,430 single-copy genes.
  - Original dataset: https://erda.ku.dk/archives/341f72708302f1d0c461ad616e783b86/published-archive.html
  - `stiller_concat.rooted.tre`: ASTRAL species tree with SU branch lengths estimated using concatenation.
  - `stiller_castles_pro.rooted.tre`: ASTRAL species tree with SU branch lengths estimated using CASTLES-Pro.
  - `stiller.nexus`: Comparison between CASTLES-Pro and CAML branch lengths.
  - `astral_63K.tre`: ASTRAL tree estimated from the 63K gene trees.
- **Bees**: 32-taxon dataset from [Bossert et al. (2021)](https://academic.oup.com/sysbio/article/70/4/803/6050959) with 853 single-copy genes.
  - Original dataset: https://datadryad.org/stash/dataset/doi:10.5061/dryad.z08kprrb6
  - `genetrees.tre`: Gene trees with SU branch lengths.
  - `genetrees_remove_305.tre`: SU gene trees with gene 305 removed.
  - `treeshrink_bees.tre`: TreeShrink gene trees with SU branch lengths.
  - `bees.nexus` and `bee_caml_astral.pdf`: Comparison between CASTLES-Pro and CAML branch lengths.
  - `caml_no_outgroup.tre`: ASTRAL species tree with SU branch lengths estimated using concatenation.
  - `castles_pro_no_outgroup.tre`: ASTRAL species tree with SU branch lengths estimated using CASTLES-Pro.
  - `castles_pro_remove_305_no_outgroup.tre`: ASTRAL species tree with SU branch lengths estimated using CASTLES-Pro on gene trees with gene 305 removed.
  - `castles_pro_treeshrink_no_outgroup.tre`: ASTRAL species tree with SU branch lengths estimated using CASTLES-Pro on TreeShrink gene trees.
- **Mammals**: 37-taxon dataset from [Song et al. (2012)](https://www.pnas.org/doi/10.1073/pnas.1211733109) with 424 single-copy genes.
  - Original dataset: https://erda.ku.dk/archives/341f72708302f1d0c461ad616e783b86/published-archive.html
  - `genetrees.tre`: Gene trees with SU branch lengths.
  - `treeshrink_mammals.tre`: TreeShrink gene trees.
  - `astral4_no_outgroup.tre`: Species tree estimated using ASTRAL.
  - `mammals.nexus`: Comparison between CASTLES-Pro and CAML branch lengths.
  - `caml_no_outgroup.tre`: ASTRAL species tree with SU branch lengths estimated using concatenation.
  - `castles_pro_no_outgroup.tre`: ASTRAL species tree with SU branch lengths estimated using CASTLES-Pro.
  - `castles_pro_treeshrink_no_outgroup.tre`: CASTLES-Pro tree estimated from TreeShrink gene trees.
  
#### GDL
- **Fungi**: 16-taxon dataset from [Butler et al. (2009)](https://www.nature.com/articles/nature08064) with 706 single-copy genes and 7,180 multi-copy genes.
  - Original dataset: https://compbio.mit.edu/candida/
  - `mrbayes.tre`: MrBayes species tree with SU branch lengths.
  - `castles_pro_pep.ml.renamed.trees`: ASTRAL-Pro species tree with SU branch lengths estimated using CASTLES-Pro.
  - `apro_pep.ml.renamed.trees`: ASTRAL-Pro species tree.
  - `pep.ml.renamed.trees`: Multi-copy gene trees with SU branch lengths.
  - `fungi.nexus`: Comparison between CASTLES-Pro and MrBayes branch lengths.
- **Plants (`1KP`)**: 80-taxon dataset from [Wickett et al. (2014)](https://www.pnas.org/doi/10.1073/pnas.1323926111) with 424 single-copy genes and 9,610 multi-copy genes.
  - Original dataset: https://datacommons.cyverse.org/browse/iplant/home/shared/onekp_pilot
  - `caml.tre.induced`: Concatenation species tree with SU branch lengths.
  - `1kp.nexus`: Comparison between CASTLES-Pro and concatenation branch lengths.
  - `castlespro_mul.tre.induced`: ASTRAL species tree with SU branch lengths estimated using CASTLES-Pro.
  - `mul.trees.renamed`: Multi-copy gene trees with SU branch lengths.
- **Eudicots (`buxus`)**: 40-taxon dataset from [Chanderbali et al. (2022)](https://www.nature.com/articles/s41467-022-28312-w) with 345 single-copy genes and 2,573 multi-copy genes.
  - Original dataset: https://datadryad.org/stash/dataset/doi:10.5061/dryad.cjsxksn6d
  - `caml_353_no_outgroup.tre`: Main concatenation species tree with SU branch lengths.
  - `castles_pro_mul_2573_no_outgroup.tre`: ASTRAL-Pro species tree with SU branch lengths estimated using CASTLES-Pro.
  - `castles_pro_353_no_outgroup.tre`: ASTRAL-Pro species tree with SU branch lengths estimated using CASTLES-Pro using 353 single-copy gene trees.
  - `castles_pro_busco_no_outgroup.tre`: ASTRAL-Pro species tree with SU branch lengths estimated using CASTLES-Pro using BUSCO gene trees.
  - `orthogroups_2573_clean.trees`: Multi-copy gene trees with SU branch lengths.
  - `sc_353.trees`: Single-copy gene trees with SU branch lengths.
  - `Busco_1246.trees`: BUSCO gene trees.
  - `buxus.nexus`: Comparison between CASTLES-Pro and concatenation branch lengths. 
    
#### HGT
- **Bacterial (core genes)**: 72-taxon dataset from [Williams et al. (2020)](https://www.nature.com/articles/s41559-019-1040-x) with 49 single-copy genes.
  - Original dataset: https://doi.org/10.6084/m9.figshare.13395470
  - `astral_core_genes.tre`: ASTRAL species tree estimated using the core gene trees.
  - `caml_core_genes.tre`: ASTRAL species tree with SU branch lengths estimated using concatenation.
  - `castles_pro_core_genes.tre`: ASTRAL species tree with SU branch lengths estimated using CASTLES-Pro.
  - `core_genes.tre`: Gene trees with SU branch lengths.
  - `core.nexus`: Comparison between CASTLES-Pro and concatenation branch lengths. 
- **Bacterial (non-ribosomal genes)**: 108-taxon dataset from [Petitjean et al. (2015)](https://pubmed.ncbi.nlm.nih.gov/25527841/) with 38 single-copy genes.
  - Original dataset: https://doi.org/10.6084/m9.figshare.13395470
  - `astral_non_ribosomal_genes.tre`: ASTRAL species tree estimated on non-ribosomal gene trees.
  - `caml_non_ribosomal_genes.tre`: ASTRAL species tree with SU branch lengths estimated using concatenation.
  - `castles_pro_non_ribosomal.tre`: ASTRAL species tree with SU branch lengths estimated using CASTLES-Pro.
  - `non_ribosomal_genes.tre`: Gene trees with SU branch lengths.
  - `non_ribosomal.nexus`: Comparison between CASTLES-Pro and concatenation branch lengths.
- **Bacterial (WoL)**: 10,575-taxon dataset from [Zhu et al. (2019)](https://www.nature.com/articles/s41467-019-13443-4) with 381 single-copy genes.
  - Original dataset: https://biocore.github.io/wol/
  - `astral.rand.lpp.nwk`: ASTRAL species trees with SU branch lengths estimated using concatenation.
  - `castles_pro_wol.tre`: ASTRAL species tree with SU branch lengths estimated using CASTLES-Pro.
  - `wol_genestrees.tre.zip`: WoL gene trees.
  - `wol.nexus`: Comparison between CASTLES-Pro and concatenation branch lengths.
  - `aster_v1.13.2.4_wol.annotated.nwk`: ASTER annotation of ASTRAL species tree for branch length estimation.
  - `castles_pro_wol_treeshrink.tre`: ASTRAL species tree with SU branch lengths estimated using CASTLES-Pro using TreeShrink gene trees.
