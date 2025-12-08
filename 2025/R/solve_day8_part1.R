solve_day8_part1 <- function(input, n_edges = 1000) {
  xyz <- read.table(text = input, sep =",", col.names = c("x", "y", "z"))
  n_coords <- NROW(xyz)
  xyz$graph_id <- xyz$box_id <- seq_len(n_coords)

  pairs <- merge(xyz, xyz, by = NULL, suffixes = c("_1", "_2"))

  pairs <- pairs |>
    subset(box_id_1 < box_id_2) |>
    transform(dist = sqrt((x_2 - x_1)^2 + (y_2 - y_1)^2 + (z_2 - z_1)^2))


  pairs <- sort_by(pairs, ~dist)
  pairs <- pairs[1:n_edges, ]

  for (edge in seq_len(n_edges)) {
    box_id_1 <- pairs[edge, "box_id_1"]
    box_id_2 <- pairs[edge, "box_id_2"]

    graph_id_1 <- xyz$graph_id[box_id_1]
    graph_id_2 <- xyz$graph_id[box_id_2]

    xyz$graph_id[xyz$graph_id == graph_id_2] <- graph_id_1
  }

  circuit_sizes <- as.numeric(table(xyz$graph_id))
  largest_circuits <- sort(circuit_sizes, decreasing = TRUE)[1:3]
  prod(largest_circuits)
}
