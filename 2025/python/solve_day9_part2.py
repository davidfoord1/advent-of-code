from shapely import Polygon, box

def solve_day9_part2(text):
    coords = [line.split(",") for line in text]
    coords = [[int(num) for num in coord] for coord in coords]
    n_corners = len(coords)

    rects = []
    areas = []
    for i in range(n_corners):
        for j in range(n_corners):
            if i < j:
                r1, c1 = coords[i]
                r2, c2 = coords[j]
                rmin, rmax = sorted([r1, r2])
                cmin, cmax = sorted([c1, c2])

                rect = box(rmin, cmin, rmax, cmax)
                rects += [[rect, rect.area]]

    rects.sort(key = lambda x: x[1], reverse = True)
    rects, areas = zip(*rects)

    polygon = Polygon(coords)

    for i in range(len(areas)):
        if not polygon.covers(rects[i]):
            return(int(areas[i]))
