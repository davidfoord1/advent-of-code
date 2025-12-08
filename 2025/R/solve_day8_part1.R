solve_day8_part1 <- function(input, n_edges = 1000) {
  xyz <- read.table(text = input, sep =",", col.names = c("x", "y", "z"))
  n_coords <- NROW(xyz)
  xyz$box_id <- seq_len(n_coords)

  pairs <- merge(xyz, xyz, by = NULL, suffixes = c("_1", "_2"))
  pairs <- pairs |>
    transform(dist = sqrt((x_2 - x_1)^2 + (y_2 - y_1)^2 + (z_2 - z_1)^2)) |>
    subset(dist != 0) |>
    subset(!duplicated(dist))

  pairs <- sort_by(pairs, ~dist)
  pairs <- pairs[1:n_edges, ]

  xyz$graph_id <- xyz$box_id

  for (edge in seq_len(n_edges)) {
    box_id_1 <- pairs[edge, "box_id_1"]
    box_id_2 <- pairs[edge, "box_id_2"]

    graph_id_1 <- xyz[box_id_1, "graph_id"]
    graph_id_2 <- xyz[box_id_2, "graph_id"]

    graph_ids <- sort(c(graph_id_1, graph_id_2))
    new_graph_id <- graph_ids[[1]]
    old_graph_id <- graph_ids[[2]]

    xyz$graph_id[xyz$graph_id == old_graph_id] <- new_graph_id
  }

  circuit_sizes <- as.numeric(table(xyz$graph_id))
  largest_circuits <- sort(circuit_sizes, decreasing = TRUE)[1:3]
  prod(largest_circuits)
}
