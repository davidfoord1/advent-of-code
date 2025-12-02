solve_day16_part1 <- function(input) {
  nrows <- length(input)
  maze <<- matrix(unlist(strsplit(input, "")),
                  nrow = nrows,
                  byrow = TRUE)

  ncols <- NCOL(maze)

  start <- which(maze == "S", arr.ind = TRUE)
  end <- which(maze == "E", arr.ind = TRUE)
  # N, E, S, W as 1, 2, 3, 4
  dirs <<- list(c(-1L, 0L), c(0L, 1L), c(1L, 0L), c(0L, -1L))

  cost_queue <- create_priority_queue(nrows * ncols)

  # store each position as pos[x, y], facing, cost
  # starting with 2 for East and 0 cost:
  cost_queue <- add_item(cost_queue,
                         list(pos = start, facing = 2L, cost = 0L))
  free_ptr <- 2L
  min_cost_ptr <- 1L

  # visited 3D array with facing as 3rd dimension?
  visited <<- array(Inf, dim = c(nrows, ncols, 4L))

  start <- read_min(cost_queue)

  pos <- start[["pos"]]
  facing <- start[["facing"]]
  cost <- start[["cost"]]

  while(any(pos != end)) {
    # identify next possible positions and there costs
    # +1L cost for moving in the current dir
    # +1000L cost for rotating
    increase <- 1L

    # add to cost_queue and visited?
    cost_queue <- add_pos(cost_queue, pos, facing, cost, increase)

    # turns cost more
    increase <- 1000L
    # 90 degrees clockwise
    facing <- facing %% 4L + 1L
    cost_queue <- add_pos(cost_queue, pos, facing, cost, increase)

    # 90 degrees anticlockwise (270 anticlockwise)
    facing <- facing %% 4L + 1L
    facing <- facing %% 4L + 1L
    cost_queue <- add_pos(cost_queue, pos, facing, cost, increase)

    # remove current item from queue
    cost_queue <- remove_min(cost_queue)
    next_min <- read_min(cost_queue)

    pos <- next_min[["pos"]]
    facing <- next_min[["facing"]]
    cost <- next_min[["cost"]]
  }

  visited[pos[[1L]], pos[[2L]], facing]
}

#' Find the first free space in the cost queue
which_free <- function(cost_queue) {
  empty <- vapply(cost_queue, is.null, logical(1))

  if (any(empty)) {
    which.max(empty) # first empty slot
  } else {
    length(cost_queue) + 25L # expand list if no empty slos
  }
}

# assuming checking the whole vector is quicker than growing and
# shrinking by popping and inserting elements
which_min_cost <- function(cost_queue) {
  costs <- vapply(
    cost_queue,
    \(x) if (!is.null(x)) x[[3L]] else Inf,
    numeric(1L)
  )

  which.min(costs)
}

add_pos <- function(cost_queue, pos, facing, cost, increase) {
  if (increase == 1L) {
    new_pos <- pos + dirs[[facing]]
  } else {
    new_pos <- pos # when we rotate without changing coordinates
  }

  # if it's a dead end, discard this path
  if (maze[new_pos] == "#") {
    return(cost_queue)
  }

  new_cost <- cost + increase

  # if we've already go to the same pos and facing with less cost,
  # let's discard this path
  if (visited[new_pos[[1L]], new_pos[[2L]], facing] < new_cost) {
    return(cost_queue)
  }

  visited[new_pos[[1L]], new_pos[[2L]], facing] <<- new_cost
  cost_queue <- add_item(cost_queue,
                         list(pos = new_pos, facing = facing, cost = new_cost))

  cost_queue
}



# binary heap implementation of cost queue
create_priority_queue <- function(capacity = 100L) {
  list(heap = vector("list", capacity),
       size = 0L, # actual number of elements
       capacity = capacity) # maximum number of elements
}

add_item <- function(queue, element) {
  size <- queue[["size"]]
  capacity <- queue[["capacity"]]
  # double queue capacity if at full capacity
  if (size == capacity) {
    queue[["capacity"]] <- capacity * 2L
    new_heap <- vector("list", queue[["capacity"]])
    new_heap[1:size] <- queue[["heap"]]
    queue[["heap"]] <- new_heap
  }

  new_size <- size + 1
  queue[["size"]] <- new_size
  queue[["heap"]][[new_size]] <- element

  i <- new_size
  while(i > 1 &&
        queue[["heap"]][[i]][["cost"]] <
        queue[["heap"]][[floor(i / 2)]][["cost"]]) {
    parent <- floor(i / 2)
    queue[["heap"]][c(i, parent)] <- queue[["heap"]][c(parent, i)]
    i <- parent
  }

  queue
}

read_min <- function(queue) {
  size <- queue[["size"]]

  queue[["heap"]][[1L]]
}

remove_min <- function(queue) {
  size <- queue[["size"]]
  if (size == 0L) stop("Heap is empty")

  # store element with smallest cost
  # min_element <- queue[["heap"]][[1L]]
  # replace root with last element
  queue[["heap"]][[1L]] <- queue[["heap"]][[size]]
  # clear last element
  # we use list(NULL) instead of just NULL to preserve the heap reserved length
  queue[["heap"]][[size]] <- list(NULL)
  new_size <- size -1L
  queue[["size"]] <- new_size

  i <- 1L
  # bubble sort to maintain cost order
  repeat {
    left <- 2L * i
    right <- 2L * i + 1L
    smallest <- i

    if (left <= new_size &&
        queue$heap[[left]]$cost < queue$heap[[smallest]]$cost) {
      smallest <- left
    }

    if (right <= new_size &&
        queue$heap[[right]]$cost < queue$heap[[smallest]]$cost) {
      smallest <- right
    }

    if (smallest == i ) break

    # swap
    queue$heap[c(i, smallest)] <- queue$heap[c(smallest, i)]
    i <- smallest
  }

  queue
}