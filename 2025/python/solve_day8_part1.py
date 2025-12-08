from math import dist, prod
import networkx as nx

def solve_day8_part1(text, n_edges = 1000):
    coords = [line.split(",") for line in text]
    coords = [[int(x) for x in box] for box in coords]

    edges = []
    for i in range(len(coords)):
        for j in range(len(coords)):
            if i < j:
                box_1, box_2 = coords[i], coords[j]
                edge_len = dist(box_1, box_2)
                edges += [[i, j, edge_len]]

    edges.sort(key = lambda x : x[2])
    edges = edges[:n_edges]
    edges = [edge[:2] for edge in edges] # remove distances

    uf = nx.utils.UnionFind()
    for box_1, box_2 in edges:
        uf.union(box_1, box_2)

    groups = list(uf.to_sets())
    sizes = [len(g) for g in groups]
    sizes.sort(reverse=True)

    return(prod(sizes[:3]))
