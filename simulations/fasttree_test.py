import multiprocessing as mp
import subprocess
import sys
import os


r = sys.argv[1].zfill(2) # replicate
g = '' if sys.argv[2] == '1000' else '_'+sys.argv[2] # number of genes
dataset_path = '/scratch/users/syt3/data/HGT_SU'
species_tree_name = 's_tree.trees'


def run_aster(condition):
    output_path = dataset_path  + '/' + condition + '/' + r + '/'
    cmd = 'FastTree -nt -gtr -quiet -nopr -gamma -n 1000 ' + output_path + 'all-genes.phylip' + ' > ' + output_path + 'estimatedgenetre'
    #full_cmd = '/usr/bin/time -v -o ' + output_path + '.stat' + ' -f "QR*\t%e\t%M" ' + cmd
    p = subprocess.Popen(cmd, stdout=subprocess.PIPE, shell=True)
    _, _ = p.communicate()


if __name__ == '__main__':
    model_conditions = ['model.50.2000000.0.000001.0', 'model.50.2000000.0.000001.0.000000005', 'model.50.2000000.0.000001.0.0000002', 'model.50.2000000.0.000001.0.000000002', 'model.50.2000000.0.000001.0.00000002', 'model.50.2000000.0.000001.0.0000005']
    #for condition in model_conditions:
    #    run_aster(condition)
    with mp.Pool(mp.cpu_count()) as p:
        p.map(run_aster, model_conditions)
