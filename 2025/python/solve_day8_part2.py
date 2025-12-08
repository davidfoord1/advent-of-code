from math import dist
import networkx as nx

def solve_day8_part2(text):
    coords = [line.split(",") for line in text]
    coords = [[int(x) for x in box] for box in coords]
    n_boxes = len(coords)

    edges = []
    for i in range(len(coords)):
        for j in range(len(coords)):
            if i < j:
                box_1, box_2 = coords[i], coords[j]
                edge_len = dist(box_1, box_2)
                edges += [[i, j, edge_len]]

    edges.sort(key = lambda x : x[2])
    edges = [edge[:2] for edge in edges] # remove distances

    uf = nx.utils.UnionFind()
    n_circuits = n_boxes
    for box_1, box_2 in edges:
        # union if boxes not already in same circuit
        if uf[box_1] != uf[box_2]:
            uf.union(box_1, box_2)
            n_circuits -= 1

        if n_circuits == 1:
            return(coords[box_1][0] * coords[box_2][0])
