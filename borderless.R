# get a fitness score for each strategy
calc_fitness <- function(t_matrix, p_matrix, strat_vec, n_steps = 200, start_pos = c(1, 1), start_score = 0, v = FALSE){
  # t_matrix = target matrix, n x n with targets
  # p_matrix = n x k matrix with all permutaions of situation bits
  # strat_vec = k length vector of responses to situations
  # n_steps = how many iterations to complete
  # start_pos = initial current position
  cstep = 1 # step counter
  cpos = start_pos # current position tracker
  if(v) print(cpos)
  while(cstep <= n_steps){
    # see what's around the current position
    view <- look_around(t_matrix, pos = cpos)
    # check against strategy vector for response to view
    strat <- as.character(find_strat(find_state(p_matrix, view), strat_vec))
    # initialize a 'score' of 'fitness'
    score <- start_score
    # carry out strategy - move, do nothing, pick up target, etc.
    # positions: 1 = N, 2 = S, 3 = W, 4 = E, 5 = random, 6 = pick up target
    if(v){
      print(sprintf("Local view: %d", view))
      print(sprintf("Strategy selected: %s", strat))
    }
    
    switch(strat,
           "1" = ifelse(cpos[1] <= 1,
                      cpos <- c(ncol(t_matrix), cpos[2]),
                      cpos <- c(cpos[1] - 1, cpos[2])),
           "2" = ifelse(cpos[1] >= nrow(t_matrix),
                      cpos <- c(1, cpos[2]),
                      cpos <- c(cpos[1] + 1, cpos[2])),
           "3" = ifelse(cpos[2] <= 1,
                      cpos <- c(cpos[1], ncol(t_matrix)),
                      cpos <- c(cpos[1], cpos[1] - 1)),
           "4" = ifelse(cpos[2] >= ncol(t_matrix),
                      cpos <- c(cpos[1], 1),
                      cpos <- c(cpos[1], cpos[2] + 1)),
           "5" = cpos <- c(cpos[1] + sample(c(-1, 0, 1), 1),
                           cpos[2] + sample(c(-1, 0, 1), 1)),
           "6" = ifelse(t_matrix[cpos[1], cpos[2]] == 1,
                      score <- score + 5,
                      score <- score - 3)
          )
    if(v){
      sprintf("current fitness: %d", score)
    }
    # iterate step counter
    cstep <- cstep + 1
  }
  return(score)
}
