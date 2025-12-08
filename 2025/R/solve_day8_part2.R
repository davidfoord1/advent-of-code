solve_day8_part2 <- function(input) {
  xyz <- read.table(text = input, sep =",", col.names = c("x", "y", "z"))
  n_coords <- NROW(xyz)
  xyz$box_id <- seq_len(n_coords)

  pairs <- merge(xyz, xyz, by = NULL, suffixes = c("_1", "_2"))
  pairs <- pairs |>
    transform(dist = sqrt((x_2 - x_1)^2 + (y_2 - y_1)^2 + (z_2 - z_1)^2)) |>
    subset(dist != 0) |>
    subset(!duplicated(dist))

  pairs <- sort_by(pairs, ~dist)
  n_edges <- NROW(pairs)

  xyz$graph_id <- xyz$box_id

  for (edge in seq_len(n_edges)) {
    box_id_1 <- pairs[edge, "box_id_1"]
    box_id_2 <- pairs[edge, "box_id_2"]
    box_ids <- c(box_id_1, box_id_2)

    graph_id_1 <- xyz[box_id_1, "graph_id"]
    graph_id_2 <- xyz[box_id_2, "graph_id"]
    graph_ids <- c(graph_id_1, graph_id_2)

    graph_id_order <- order(graph_ids)

    graph_ids <- graph_ids[graph_id_order]
    box_ids <- box_ids[graph_id_order]

    new_graph_id <- graph_ids[[1]]
    old_graph_id <- graph_ids[[2]]

    xyz$graph_id[xyz$graph_id == old_graph_id] <- new_graph_id

    if (all(xyz$graph_id == 1)) {
      return(prod(xyz[xyz$box_id %in% box_ids, "x"]))
    }
  }
}
