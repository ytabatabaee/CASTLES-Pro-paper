## CASTLES-Pro Datasets

This repository contains the datasets and scripts used in the following paper:

- Y. Tabatabaee, C. Zhang, S. Mirarab, Species tree branch length estimation despite incomplete lineage sorting, duplication, and loss

For experiments in this study, we analyzed three sets of simulated datasets and nine biological dataset with different sources of gene tree discordance (details below). All datasets can be accessed from [this](https://drive.google.com/drive/folders/1xrDjxkSSnbcvUVW_QcaUS8-mou1sJyww?usp=sharing) Google Drive link. In all simulated datasets, the true species trees have branch lengths in substitution-units. 

### Simulated datasets

**ILS-only simulations**

- We reused the 101-taxon ILS-only dataset from [Tabatabaee et. al. (2023)](https://academic.oup.com/bioinformatics/article/39/Supplement_1/i185/7210452) available at [https://github.com/ytabatabaee/CASTLES-paper/](https://github.com/ytabatabaee/CASTLES-paper/tree/main). Results and intermediate data from the experiments in the paper are available [here](https://drive.google.com/file/d/1UhF5IlTgZ1_SJ2UMlX0gUtexAMfHMObs/view?usp=sharing).

**HGT+ILS simulations**
- We generated a new 51-taxon HGT+ILS dataset based on the parameters from [Davidson et. al. (2015)](https://bmcgenomics.biomedcentral.com/articles/10.1186/1471-2164-16-S10-S1) study. The original dataset is available at [https://databank.illinois.edu/datasets/IDB-6670066](https://databank.illinois.edu/datasets/IDB-6670066). Our new dataset, as well as the intermediate data and output of methods are available [here](https://drive.google.com/file/d/1MstkPWFCHtoDbJpCnCaSMPE8LGqzX9Kg/view?usp=sharing).

**GDL+ILS simulations**
- We updated the GDL+ILS dataset from [Willson et. al. (2022, 2023)](https://academic.oup.com/sysbio/article/71/3/610/6358739) with species trees with substitution-unit branch lengths. The original datasets are available at [https://databank.illinois.edu/datasets/IDB-4050038](https://databank.illinois.edu/datasets/IDB-4050038) and  [https://databank.illinois.edu/datasets/IDB-5748609](https://databank.illinois.edu/datasets/IDB-5748609).  Results and intermediate data from the experiments in the paper are available [here](https://drive.google.com/drive/folders/1xrDjxkSSnbcvUVW_QcaUS8-mou1sJyww?usp=sharing).

### Biological dataset
#### ILS
- **Brids**: 363-taxon dataset from [Stiller et al. (2024)](https://www.nature.com/articles/s41586-024-07323-1) with 63,430 single-copy genes.
- **Bees**: 32-taxon dataset from [Bossert et al. (2021)](https://academic.oup.com/sysbio/article/70/4/803/6050959) with 853 single-copy genes.
- **Mammals**: 37-taxon dataset from [Song et al. (2012)](https://www.pnas.org/doi/10.1073/pnas.1211733109) with 424 single-copy genes.
#### GDL
- **Fungi**: 16-taxon dataset from [Butler et al. (2009)](https://www.nature.com/articles/nature08064) with 706 single-copy genes and 7,180 multi-copy genes.
- **Plants**: 83-taxon dataset from [Wickett et al. (2014)](https://www.pnas.org/doi/10.1073/pnas.1323926111) with 424 single-copy genes and 9,610 multi-copy genes.
- **Eudicots**: 40-taxon dataset from [Chanderbali et al. (2022)](https://www.nature.com/articles/s41467-022-28312-w) with 345 single-copy genes and 2,573 multi-copy genes.
#### HGT
- **Bacterial (core genes)**: 72-taxon dataset from [Williams et al. (2020)](https://www.nature.com/articles/s41559-019-1040-x) with 49 single-copy genes.
- **Bacterial (non-ribosomal genes)**: 108-taxon dataset from [Petitjean et al. (2015)](https://pubmed.ncbi.nlm.nih.gov/25527841/) with 38 single-copy genes.
- **Bacterial (WoL)**: 10,575-taxon dataset from [Zhu et al. (2019)](https://www.nature.com/articles/s41467-019-13443-4) with 381 single-copy genes.
