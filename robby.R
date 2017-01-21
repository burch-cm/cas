library(magrittr)
library(data.table)

## calculate fitness of population
checkFit <- function(strategy, fitness = 0, nactions = 200, nsessions = 100){
    # ----- Helper Functions -----
    ## get info on neightboring cells
    lookAround <- function(pos){
        ## to the North
        if(pos[1] == 1){
            N <- 'wall'
        } else if(cangrid[pos[1]-1, pos[2]] == 1){
            N <- 'can'
        } else {
            N <- 'empty'
        }
        ## to the South
        if(pos[1] == dim(cangrid)[1]){
            S <- 'wall'
        } else if(cangrid[pos[1]+1, pos[2]] == 1){
            S <- 'can'
        } else {
            S <- 'empty'
        }
        ## to the East
        if(pos[2] == dim(cangrid)[1]){
            E <- 'wall'
        } else if(cangrid[pos[1], pos[2]+1] == 1){
            E <- 'can'
        } else {
            E <- 'empty'
        }
        ## to the West
        if(pos[2] == 1){
            W <- 'wall'
        } else if(cangrid[pos[1], pos[2]-1] == 1){
            W <- 'can'
        } else {
            W <- 'empty'
        }
        ## the current square
        if(cangrid[pos[1], pos[2]] == 1){
            C <- 'can'
        } else {
            C <- 'empty'
        }
        return(c(N, S, E, W, C))
    }
    ## calculate the consequences of the action
    doAction <- function(action, lstate = state, lscore = score, lpos = pos){
        # 1 = MvN, 2 = MvS, 3 = MvE, 4 = MvW, 5 = MvR, 6 = stay, 7 = get_can
        switch(action,
               "1" = if(lstate[1] == 'wall'){
                   lscore <- lscore - 5
               } else {
                   lpos <- c(lpos[1]-1, lpos[2])
               },
               "2" = if(lstate[2] == 'wall'){
                   lscore <- lscore - 5
               } else {
                   lpos <- c(lpos[1]+1, lpos[2])
               },
               "3" = if(lstate[3] == 'wall'){
                   lscore <- lscore - 5
               } else {
                   lpos <- c(lpos[1], lpos[2]+1)
               },
               "4" = if(lstate[4] == 'wall'){
                   lscore <- lscore - 5
               },
               "5" = sample(1:4, 1) %>%
                   switch(
                       if(lstate[1] == 'wall'){
                           lscore <- lscore - 5
                       } else {
                           lpos <- c(lpos[1]-1, lpos[2])
                       },
                       if(lstate[2] == 'wall'){
                           lscore <- lscore - 5
                       } else {
                           lpos <- c(lpos[1]+1, lpos[2])
                       },
                       if(lstate[3] == 'wall'){
                           lscore <- lscore - 5
                       } else {
                           lpos <- c(lpos[1], lpos[2]+1)
                       },
                       if(lstate[4] == 'wall'){
                           lscore <- lscore - 5
                       }
                   ),
               "6" = lpos <- lpos,
               "7" = if(lstate[5] == 'can'){
                   lscore <- lscore + 10
                   cangrid[lpos[1], lpos[2]] <- 0
               } else {
                   lscore <- lscore - 1
               })
        return(c(lpos, lscore))
    }
    # ----- Load data -----
    ## make a data table of the current strategy
    stratdf$cs <- strategy %>% strsplit('')
    totalscores <- c() # store the scores for each iteration
    ## for each of 100 sessions, do this -----
    session_id <- 1
    while(session_id <= nsessions){
        ## make the grid of cans
        canlocs <- sample(0:1, 100, replace = TRUE, prob = c(0.5, 0.5))
        cangrid <- matrix(canlocs, 10, 10)
        ## set starting position
        pos = c(1,1) # y then x -> row then column
        ## initialize local fitness score
        score <- fitness
        ### for each session, for 200 moves per session, do this -----
        action_iterator = 1
        while(action_iterator <= nactions){
            ## determine the state by looking around at neighbor cells
            state <- lookAround(pos)
            ## lookup state in strategy table -----
            action <- stratdf[North == state[1] &
                                  South == state[2] &
                                  East == state[3] &
                                  West == state[4] &
                                  Current == state[5],
                              cs]
            ## carry out the action
            res <- doAction(action)
            ## update the local state with the consequences
            pos <- c(res[1], res[2])
            score <- res[3] # this is the running score total
            action_iterator = action_iterator + 1
        }
        ### After each session but before the next iteration -----
        totalscores <- append(totalscores, score)
        session_id = session_id + 1
    }
    ### After all iterations -----
    return(sum(totalscores))
}
## split parents into children
makeChildren <- function(parent1, parent2){
    # splits 2 parents into two children
    # determine where to split
    locus <- sample(2:nchar(parent1)-1, 1)
    c1.a <- parent1 %>% substr(1, locus)
    c1.b <- parent2 %>% substr(locus+1, nchar(parent2))
    c2.a <- parent2 %>% substr(1, locus)
    c2.b <- parent1 %>% substr(locus+1, nchar(parent1))
    c1 <- paste(c1.a, c1.b, sep ='')
    c2 <- paste(c2.a, c2.b, sep ='')
    return(c(c1, c2))
}
##### mutate #####
mutateStrat <- function(strat, p=0.005){
    cvec <- unlist(strsplit(strat, ''))
    probs <- c(1-p, p/7, p/7, p/7, p/7, p/7, p/7, p/7)
    for(i in 1:length(cvec)){
        cvec[i] <- sample(c(cvec[i], '1', '2', '3', '4', '5', '6', '7'),
                          1, prob = probs)
    }
    return(paste(cvec, collapse=''))
}

##### params #####
ngens = 4 # how many generations to run the model for
labels = c('empty', 'can', 'wall')
stratdf = expand.grid(labels, labels, labels, labels, labels)
names(stratdf) <- c('North', 'South', 'East', 'West', 'Current')
stratdf <- as.data.table(stratdf)


# store the resulting fitness values for analysis
gen.fit <- data.table(gen = c(1:10), avg = 0, top = 0)

## generate the initial population #####

genmat <- matrix()
for(i in 1:200){
    strategy <- sample(1:7, 243, replace = TRUE) %>% paste(collapse = "")
    genmat[i] <- strategy
}
gendf <- data.frame(strategy = genmat, fitness = 0, stringsAsFactors = FALSE)
gen.dt <- as.data.table(gendf)
rm(genmat, gendf)

##### repeat this whole mess for n generations #####

current.gen <- 1 # keep track of which generation we're in

# set up a progress bar
pb <- txtProgressBar(min = 0, max = ngens)
while(current.gen <= ngens){
    # get fitness values for the individuals in the current population
    print(paste("Beginning fitness evaluation for generation", current.gen))
    pb <- txtProgressBar(min = 0, max = 200, style = 3)
    for(i in 1:200){
        gen.dt$fitness[i] <- checkFit(gen.dt$strategy[i])
        setTxtProgressBar(pb, value = i)
    }
    close(pb)
    print(paste("Beginning evolution for generation", current.gen))
    # do the evolution
    # fitness below 0 becomes 0
    gen.dt[fitness < 0, fitness := 0]
    # push to gen.fit
    gen.fit[gen == current.gen, `:=` (avg = gen.dt[, mean(fitness)], top = gen.dt[, max(fitness)])]
    # square distances from zero
    gen.dt[, fitness := fitness^2]
    # convert to probabilities
    fitsum <- gen.dt[, sum(fitness)]
    gen.dt[, prob := fitness/fitsum] # prob sums to 1
    ## populate next generation
    # placeholder
    V <- data.frame(p1 = NA, p2 = NA)
    # next generation
    gen.next <- c()
    # build the next generation
    step = 1
    while(step <= 100){
        #select two parents and get two offspring
        V[, c("p1", "p2")] <- gen.dt[, sample(fitness, 2, prob = prob)]
        s1 <- gen.dt[fitness == V$p1, strategy]
        s2 <- gen.dt[fitness == V$p2, strategy]
        kids <- makeChildren(s1, s2)
        #turn them into mutant ninja turtles
        c1 <- mutateStrat(kids[1])
        c2 <- mutateStrat(kids[2])
        #add them to next generation
        gen.next <- c(gen.next, c1, c2)
        step = step+1
    }
    # convert next generation into current generation and reset fitness and prob.
    gen.dt[, `:=` (strategy = gen.next, fitness = 0, prob = NULL)]
    # iterate generation counter
    print(paste("Evaluation and evolution of generation", current.gen, "complete"))
    setTxtProgressBar(pb, value = current.gen)
    current.gen = current.gen+1
}
close(pb)

print("Saving to file...")
saveRDS(gen.dt, "final_generation.rds")
saveRDS(gen.fit, "fitness_by_generation.rds")