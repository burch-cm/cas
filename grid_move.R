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
  outs <- permutations(2, n, g, repeats.allowed = TRUE)
  return(outs)
}

get_strat_random <- function(n = 32, g = c(1:6)){
  # n = length of strategy vector
  # g = vector of generators
  return(sample(g, n, replace = TRUE))
}

