solve_day8_part2 <- function(input) {
  xyz <- read.table(text = input, sep =",", col.names = c("x", "y", "z"))
  n_boxes <- NROW(xyz)
  xyz$graph_id <- xyz$box_id <- seq_len(n_boxes)

  pairs <- merge(xyz, xyz, by = NULL, suffixes = c("_1", "_2"))

  pairs <- pairs |>
    subset(box_id_1 < box_id_2) |>
    transform(dist = sqrt((x_2 - x_1)^2 + (y_2 - y_1)^2 + (z_2 - z_1)^2))

  pairs <- sort_by(pairs, ~dist)
  n_edges <- NROW(pairs)
  n_circuits <- n_boxes

  for (edge in seq_len(n_edges)) {
    box_id_1 <- pairs[edge, "box_id_1"]
    box_id_2 <- pairs[edge, "box_id_2"]

    graph_id_1 <- xyz$graph_id[box_id_1]
    graph_id_2 <- xyz$graph_id[box_id_2]

    if (graph_id_1 != graph_id_2) {
      n_circuits <- n_circuits - 1L
      xyz$graph_id[xyz$graph_id == graph_id_2] <- graph_id_1
    }

    if (n_circuits == 1L) {
      return(prod(xyz[c(box_id_1, box_id_2), "x"]))
    }
  }
}
