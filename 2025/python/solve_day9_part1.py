from shapely import box

def solve_day9_part1(text):
    coords = [line.split(",") for line in text]
    coords = [[int(num) for num in coord] for coord in coords]
    n_corners = len(coords)
    max_area = 0

    for i in range(n_corners):
        for j in range(n_corners):
            if i < j:
                r1, c1 = coords[i]
                r2, c2 = coords[j]
                rmin, rmax = sorted([r1, r2])
                cmin, cmax = sorted([c1, c2])

                rect = box(rmin, cmin, rmax, cmax)
                area = rect.area

                if area > max_area:
                    max_area = area

    return(int(max_area))
