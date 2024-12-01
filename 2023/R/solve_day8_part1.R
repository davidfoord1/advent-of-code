solve_day8_part1 <- function(input) {
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
 current_node <- "AAA"
 steps <- 0
 instruction <- 1

 while (current_node != "ZZZ") {
   # get nodes current nodes point to
   current_node <- nodes[[current_node]]
   # follow step instruction to next node
   current_node <- current_node[[instructions[[instruction]]]]

   steps <- steps + 1

   # next instruction or repeat instructions after going through them all
   if (instruction < length(instructions)) {
     instruction <- instruction + 1
   } else {
     instruction <- 1
   }
 }

 steps
}
