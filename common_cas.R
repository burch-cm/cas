# set up the initial grid

make_grid <- function(n = 5, k = 3){
  # n x n matrix
  # k target values
  A <- matrix(0, nrow = n, ncol = n)
  t_i <- sample(1:n, k)
  t_j <- sample(1:n, k)
  for(i in 1:n){
    A[t_i[i], t_j[i]] <- 1
  }
  return(A)
}

# create a list of all possible states
library(gtools)
get_permutations <- function(n = 5, g = c(0, 1)){
  # n dimensions
  # g is a vector of generators
  p_matrix <- permutations(length(g), n, g, repeats.allowed = TRUE)
  return(p_matrix)
}

# get a view of the neighborhood
look_around <- function(t_matrix, pos = c(1, 1)){
  # look at immediate "neighborhood" as vector
  # t_matrix is result of make_grid() fn
  # n x n vector with included targets
  # initialize viewpoint vector
  view <- vector("numeric", length = 5)
  # check N
  view[1] <- ifelse(pos[1] <= 1,
                    t_matrix[nrow(t_matrix), pos[2]],
                    t_matrix[pos[1] - 1, pos[2]])
  # check S
  view[2] <- ifelse(pos[1] >= nrow(t_matrix),
                    t_matrix[1, pos[2]],
                    t_matrix[pos[1] + 1, pos[2]])
  # check W
  view[3] <- ifelse(pos[2] <= 1,
                    t_matrix[pos[1], 1],
                    t_matrix[pos[1], pos[2] - 1])
  # check E
  view[4] <- ifelse(pos[2] >= ncol(t_matrix),
                    t_matrix[pos[1], ncol(t_matrix)],
                    t_matrix[pos[1], pos[2] + 1])
  # check underfoot
  view[5] <- t_matrix[pos[1], pos[2]]
  # return a vector of values to the NSWE<p>
  return(view)
}


get_strat_random <- function(n = 32, g = c(1:6)){
  # n = length of strategy vector
  # g = vector of generators
  return(sample(g, n, replace = TRUE))
}

find_state <- function(p_matrix, view){
  # which row of the permutation matric matches the view from the current position?
  t_vec <- apply(p_matrix, 1, identical, view)
  return(which(t_vec))
}

find_strat <- function(state_no, strat){
  # what strategy is at the same position in the given strategy vector?
  return(strat[state_no])
}

