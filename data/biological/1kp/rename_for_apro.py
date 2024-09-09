import argparse
import os
import sys

def main(args):
    with open(args.tree, 'r') as file:
        tree_str = file.read()
    tree_str = tree_str.replace('Marchantia_polymorpha', 'Marchantiapolymorpha')
    tree_str = tree_str.replace('Marchantia_emarginata', 'Marchantiaemarginata')
    tree_str = tree_str.replace('Nothoceros_aenigmaticus', 'Nothocerosaenigmaticus')
    tree_str = tree_str.replace('Nothoceros_vincentianus', 'Nothocerosvincentianus')
    tree_str = tree_str.replace('Cycas_rumphii', 'Cycas')
    tree_str = tree_str.replace('Cycasrumphii', 'Cycas')
    tree_str = tree_str.replace('Cycas_micholitzii', 'Cycasmicholitzii')
    tree_str = tree_str.replace('Coleochaete_irregularis', 'Coleochaete')
    tree_str = tree_str.replace('Coleochaeteirregularis', 'Coleochaete')
    tree_str = tree_str.replace('Coleochaete_scutata', 'Coleochaetescutata')
    tree_str = tree_str.replace('Cylindrocystis_brebissonii', 'Cylindrocystisbrebissonii')
    tree_str = tree_str.replace('Cylindrocystisbrebissonii', 'Cylindrocystis')
    tree_str = tree_str.replace('Cylindrocystis_cushleckae', 'Cylindrocystiscushleckae')
    tree_str = tree_str.replace('Selaginella_moellendorffii_genome', 'Selaginellamoellendorffiigenome')
    tree_str = tree_str.replace('Selaginella_moellendorffii_1kp', 'Selaginellamoellendorffii1kp')
    tree_str = tree_str.replace('Zamia', 'Zami')
    tree_str = tree_str.replace('Cyathea', 'Alsophila')
    with open(args.tree, 'w') as f:
        f.write(tree_str)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Unify species names in multi-copy gene family trees")
    parser.add_argument("-t", "--tree", type=str,  required=True,
                        help="File containing tree in newick format")
    args = parser.parse_args()
    main(parser.parse_args())
    #os.system('sed -i "s@Marchantia_polymorpha@Marchantiapolymorpha@g" '  + args.tree + ' > ' + args.tree + '.renamed')
    #os.system('sed -i "s@Marchantia_emarginata@Marchantiaemarginata@g" ' + args.tree + '.renamed' + ' > ' + args.tree + '.renamed')
    #os.system('sed -i "s@Nothoceros_aenigmaticus@Nothocerosaenigmaticus@g" ' + args.tree + '.renamed' + ' > ' + args.tree + '.renamed')
    #os.system('sed -i "s@Nothoceros_vincentianus@Nothocerosvincentianus@g" ' + args.tree + '.renamed' + ' > ' + args.tree + '.renamed')
    os.system('sed "s@\_[^),;:]*@@g" ' + args.tree + ' > ' + args.tree + '.renamed')
    #os.system('sed "s@\.[^.),;:]*@@g" ' + args.tree + ' > ' + args.tree + '.renamed')
