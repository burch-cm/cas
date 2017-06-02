# get a view of the neighborhood
look_around <- function(t_matrix, pos = c(1, 1)){
  # look at immediate "neighborhood" as vector
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


# get a fitness score for each strategy
calc_fitness <- function(t_matrix, strat, n_steps = 200){

}