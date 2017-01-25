library(data.table)
library(magrittr)

labels = data.frame(label = c(1:3), name = c('empty', 'wall', 'can'))
moves = data.frame(label = c(1:7), name = c('move_N', 'move_S', 'move_E', 'move_W', 'move_R', 'stay_put', 'pick_up_can'))

# build decision matrix
grid <- labels$label %>% expand.grid(., ., ., ., .)
mult <- c(10000, 1000, 100, 10, 1)
grid <- t(t(grid) * mult)
states <- rowSums(grid)
state.dt <- data.table(state = states) %>% .[order(states)]
setkey(state.dt, state)

# look around and determine the state of neighbors
lookAround <- function(can_grid, position){
    sit_code = c(NA, NA, NA, NA, NA)
    if(position[1] == 1){
        sit_code[1] = 2
    } else {
        sit_code[1] = can_grid[position[1]-1, position[2]]
    }
    if(position[1] == 10){
        sit_code[2] = 2
    } else {
        sit_code[2] = can_grid[position[1]+1, position[2]]
    }
    if(position[2] == 10){
        sit_code[3] = 2
    } else {
        sit_code[3] = can_grid[position[1], position[2]+1]
    }
    if(position[2] == 1){
        sit_code[4] = 2
    } else {
        sit_code[4] = can_grid[position[1], position[2]-1]
    }
    sit_code[5] = can_grid[position[1], position[2]]
    return(as.numeric(paste(sit_code, collapse = '')))
}

# check fitness
checkFit <- function(strategy, nmoves = 200, times = 100){
    ## evaluates the strategy for <nmoves> moves over <times> iterations
    # initalize score
    score <- 0
    for(i in 1:times){
        # make a grid of 'cans', 10 x 10
        can_grid <- sample(c(1, 3), 100, replace = TRUE)
        dim(can_grid) <- c(10, 10) # in matrix format
        pos = c(1, 1) # start positon for each trial
        for(move in 1:nmoves){
            cstate <- lookAround(can_grid, pos)
            mv <- strategy[state == cstate, strategy]
            switch(mv,
                   if(pos[1] == 1){ # move North
                       score = score - 5
                   } else {
                       pos[1] = pos[1]-1
                   },
                   if(pos[1] == 10){ # move South
                       score = score - 5
                   } else {
                       pos[1] = pos[1]+1
                   },
                   if(pos[2] == 10){ # move East
                       score = score - 5
                   } else {
                       pos[2] = pos[2]+1
                   },
                   if(pos[2] == 1){ # move West
                       score = score - 5
                   } else {
                       pos[2] = pos[2]-1
                   },
                   pos = pos, # random
                   pos = pos, # stay
                   if(can_grid[pos[1], pos[2]] == 3){ # pick up can
                       score = score + 10
                       can_grid[pos[1], pos[2]] <- 1 # set to empty
                   } else {
                       score = score - 1
                   }
            ) # end switch
        } # end for loop
    }
    return(score/times) # average over all iterations
}

# make an entire generation of strategies
gen <- list()
for(i in 1:200){
    # for the 200 individuals in the generation
    gen[[i]] <- copy(state.dt[, strategy := sample(moves, nrow(state.dt), replace = TRUE)])
    # have to copy or will change by reference
}


green_ooze <- function(incoming, p = 0.01){
    # because it mutates things
    sample(c(incoming, 1, 2, 3, 4, 5, 6, 7),
           1, prob = c(1-p, p/7, p/7, p/7, p/7, p/7, p/7, p/7))
}

snowstate <- 1
snowflake <- function(x){
    cat(sprintf("[%s]...", snowstate))
    y <- checkFit(x)
    snowstate <<- snowstate + 1
    return(y)
}
fitsums <- c()

newGen <- function(generation = gen, pmutate = 0.01){
    new.gen <- list()
    print("Checking fitness for current generation. ")
    gen.fit <- lapply(gen, snowflake)
    gen.fit.dt <- data.table(stratnum = 1:length(gen.fit), fitness = unlist(gen.fit))
    gen.fit.dt[fitness < 0, fitness := 0.01] # set negatives to 0.01
    gen.fit.dt[, fitness := fitness^2] # square the fitness values
    sum.fit <- gen.fit.dt[, sum(fitness)]
    fitsums <<- c(fitsums, sum.fit) # store generation fitness sum
    gen.fit.dt[, iprob := fitness/sum.fit] # as probabilities for selection
    print("Selecting and combining strategies for new generation. ")
    indices <- seq(1, 200, 2)
    for(i in 1:100){
        p <- gen.fit.dt[, sample(stratnum, 2, prob = iprob)]
        locus <- sample(2:242, 1)
        p1 <- generation[[p[1]]]$strategy
        p2 <- generation[[p[2]]]$strategy
        c1 <- c(p1[1:locus], p2[(locus+1):243]) %>% sapply(green_ooze)
        c2 <- c(p2[1:locus], p1[(locus+1):243]) %>% sapply(green_ooze)
        new.gen[[indices[i]]] <- data.table(state = gen.dt$state, strategy = c1)
        new.gen[[indices[i]+1]] <- data.table(state = gen.dt$state, strategy = c2)
    }
    return(new.gen)
}
