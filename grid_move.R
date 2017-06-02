# set up the initial grid
A <- matrix(0, nrow = 5, ncol= 5)

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
