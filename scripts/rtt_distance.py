import dendropy
import argparse
import re
import math
import numpy as np
from itertools import combinations


def convert_to_unit_tree(args):
    tns = dendropy.TaxonNamespace()
    t1 = dendropy.Tree.get(path=args.tree, schema='newick', taxon_namespace=tns, rooting='force-rooted')
    tree_height = 0
    for node in t1.preorder_node_iter():
        if not node.parent_node:
            node.value = 0
        else:
            node.value = node.parent_node.value + node.edge.length
            tree_height = max(tree_height, node.value)

    cnt = 0
    sum_rtt = 0
    with open(args.output, 'w') as f:
        f.write('method,taxa,rtt\n')
        for node in t1.postorder_node_iter():
            if node.is_leaf():
                f.write(args.method +',' + node.taxon.label + ',' + str(node.value) + '\n')
                sum_rtt += node.value
                cnt += 1


    print('max rtt dist:', tree_height)
    print('avg rtt dist:', sum_rtt / cnt)


if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Compute avgerage and maximum Root-to-Tip distance")
    parser.add_argument("-t", "--tree", type=str,  required=True,
                        help="tree in newick format")
    parser.add_argument("-o", "--output", type=str,  required=True,
                        help="output rtt distances")
    parser.add_argument("-m", "--method", type=str,  required=True,
                        help="method name")
    args = parser.parse_args()
    convert_to_unit_tree(args)
