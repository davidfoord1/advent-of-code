solve_day8_part2 <- function(input) {
 instructions <- unlist(strsplit(input[[1]], ""))

 nodes <- input[-c(1, 2)]
 nodes <- stringi::stri_extract_all(nodes, regex = "[A-Z]{3}")

 names(nodes) <- vapply(nodes, \(x) x[[1]], character(1))

 nodes <- lapply(nodes, function(x) {
   x <- x[-1]
   names(x) <- c("L", "R")
   x
 })

 # starting state
 current_nodes <- names(nodes)[endsWith(names(nodes), "A")]
 steps <- rep(0, length(current_nodes))
 instruction <- 1

 # end state
 while (!all(endsWith(current_nodes, "Z"))) {
   # ASSUMING start-end nodes come in 1:1 pairs
   # loop through each node's sequence until it first encounters its end node
   # storing the number of steps
   for (node_i in seq_along(current_nodes)) {
     if (!endsWith(current_nodes[[node_i]], "Z")) {
       # get nodes current nodes point to
       full_node <- nodes[[current_nodes[[node_i]]]]
       # follow step instruction to next node
       current_nodes[[node_i]] <- full_node[[instructions[[instruction]]]]

       steps[[node_i]] <- steps[[node_i]] + 1
     }
   }

   # next instruction or repeat instructions after going through them all
   if (instruction < length(instructions)) {
     instruction <- instruction + 1
   } else {
     instruction <- 1
   }
 }

 # ASSUMING each start-end pair leads to an end-end cycle
 # of the same number of steps
 # find the smallest common multiple of the steps
 # i.e. all cycles align
 cheapr::scm(steps)
}