#copying some of the code to an R file so i can run it outside of Rstudio.


#generate full strategy matrix

generate_strategies <- function(n) {
  #give all strategies of length n
  
  #base case
  if (n ==1) {
    return (matrix(c(FALSE, TRUE)))
  }
  
  #recursive definition
  tail <- generate_strategies(n - 1)
  count <- dim(tail)[1]
  
  start_0 <- cbind(matrix(rep(FALSE, count)), tail)
  start_1 <- cbind(matrix(rep(TRUE, count)), tail)
  
  return(rbind(start_0, start_1))
}

all_strategies <- generate_strategies(10)

#filter to less than 5 breakpoints

#boolean vector with which strategies qualify
less_than_5_strats <- c()
for (i in 1:nrow(all_strategies)) {
  less_than_5_strats[i] <- sum(all_strategies[i, ]) <= 5
}

#filter strategies matrix
strategies_less_than_5 <- all_strategies[less_than_5_strats, ]

source("drivingModelDriftNewIKIOld.R")

resultsSimpleDriftNewIKIOldSim10 <- runAllComplexStrategies(nrSimulations = 10)
save(resultsSimpleDriftNewIKIOldSim10, file = "resultsSimpleDriftNewIKIOldSim10.RData")

resultsSimpleDriftNewIKIOldSim50 <- runAllComplexStrategies(nrSimulations = 50)
save(resultsSimpleDriftNewIKIOldSim50, file = "resultsSimpleDriftNewIKIOldSim50.RData")

source("drivingModelDriftNewIKINew.R")

resultsSimpleDriftNewIKINewSim10 <- runAllComplexStrategies(nrSimulations = 10)
save(resultsSimpleDriftNewIKINewSim10, file = "resultsSimpleDriftNewIKINewSim10.RData")

resultsSimpleDriftNewIKINewSim50 <- runAllComplexStrategies(nrSimulations = 50)
save(resultsSimpleDriftNewIKINewSim50, file = "resultsSimpleDriftNewIKINewSim50.RData")
