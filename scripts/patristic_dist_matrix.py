import dendropy
import numpy as np
import argparse


def main(args):
    tns = dendropy.TaxonNamespace()
    gts = dendropy.TreeList.get(path=args.genetrees, schema='newick', taxon_namespace=tns)

    dist_mat = np.zeros((len(tns), len(tns), len(gts)))
    idx2 = 0
    for idx in range(len(gts)):
        gts[idx].deroot()
        pdc = gts[idx].phylogenetic_distance_matrix()
        exclude = True
        for i in range(len(tns)):
            for j in range(i + 1, len(tns)):
                try:
                   dist_mat[i][j][idx2] = dist_mat[j][i][idx2] = pdc(tns[i], tns[j])
                   if dist_mat[j][i][idx2] != 0: # excluding gene trees with all zero branches
                       exclude = False
                except:
                   dist_mat[i][j][idx2] = dist_mat[j][i][idx2] = None # missing taxa pair in a gene tree
        if exclude:
            continue
        else:
            idx2 += 1

    # missing data imputation with mean
    dist_mat = dist_mat[:, :, :idx2]
    for i in range(len(tns)):
        for j in range(i + 1, len(tns)):
            mean_dist_pair = np.nanmean(dist_mat[i][j][:])
            assert np.nanmean(dist_mat[i][j][:]) == np.nanmean(dist_mat[j][i][:])
            dist_mat[i][j][:][np.isnan(dist_mat[i][j][:])] = dist_mat[j][i][:][np.isnan(dist_mat[j][i][:])] = mean_dist_pair

    if args.mode == 'all':
        with open(args.outputmatrix, 'w') as f:
            f.write(str(idx2) + '\n\n')
            for idx in range(idx2):
                f.write(str(len(tns)) + ' 1 ' + '\n')
                for i in range(len(tns)):
                    f.write(tns[i].label + '     ')
                    for j in range(len(tns)):
                        f.write(str('{:.9f}'.format(dist_mat[i][j][idx])) + ' ')
                    f.write('\n')
                f.write('\n')
    else:
        if args.mode == 'avg':
            summary_dist_mat = np.mean(dist_mat, axis=2)
        elif args.mode == 'min':
            summary_dist_mat = np.min(dist_mat, axis=2)
        elif args.mode == 'med':
            summary_dist_mat = np.median(dist_mat, axis=2)
        else:
            return
        with open(args.outputmatrix, 'w') as f:
            f.write(str(len(tns))+'\n')
            for i in range(len(tns)):
                f.write(tns[i].label + '     ')
                for j in range(len(tns)):
                    f.write(str('{:.9f}'.format(summary_dist_mat[i][j]))+' ')
                f.write('\n')


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="CASTLES")
    parser.add_argument("-g", "--genetrees", type=str, required=True,
                        help="Gene trees file in newick format")
    parser.add_argument("-m", "--mode", type=str, required=False,
                        help="options: min, avg, med, all", default='avg')
    parser.add_argument("-o", "--outputmatrix", type=str, required=False,
                        help="Output distance matrix in Phylip format")
    main(parser.parse_args())
