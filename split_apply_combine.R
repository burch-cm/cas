# change letters in strat randomly based on probability of change
mutate <- function(strat, p = 0.05, g = c(1:6)){
  m_fun <- function(letter, p = p){
    if(runif(1, 0, 1) <= p){
      letter <- sample(g, 1)
    }
    return(letter)
  }
  return(sapply(strat, FUN = m_fun, p = p))
}

# get offspring for one pair by splitting, mutating, combining the pair
offspring <- function(s1, s2, mutate = TRUE, p = 0.05, split = NA, names = c('c1', 'c2')){
  # if unequal length strings, this won't work
  if(length(s1) != length(s2)) warning("Parent string lengths are unequal!")
  # if split not provided, choose randomly (default behavior)
  if(is.na(split)) split <- sample(c(1:length(s1)-1), 1)
  # randomly change some bits on parent strings?
  if(mutate){
    s1 <- sapply(s1, FUN = mutate, p = p)
    s2 <- sapply(s2, FUN = mutate, p = p)
  }
  c1 <- c(s1[1:split], s2[(split+1):length(s2)])
  c2 <- c(s2[1:split], s1[(split+1):length(s1)])
  return(cbind(c1, c2))
}

evolution <- function(generation, fitness_fun, ...){
  
}