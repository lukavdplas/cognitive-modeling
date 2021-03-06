
library(ggplot2)

# we will work with points 1 to 250 (cm)
scale.points <- c(1:250) 

# we create a dataframe for plotting
example.height <- data.frame(x=scale.points) 

# we use sapply, which is a vectorized function application; see help if you don't understand it

# we add y, which is just the probability density function described above (normal distribution)
example.height$y <- sapply(example.height$x, function(x) {dnorm(x, mean=180, sd=10)}) 

# this starts the plot creation
g1 <- ggplot(example.height, aes(x=x, y=y)) 

# we make the plot more pretty: we specify it should fill in area and add labels
g1 <- g1 + geom_area(fill="green", alpha=.4)  + xlab("height") + ylab("P") + theme_gray(20) 

g1

literal.listener <- function(x, threshold, densityf, cumulativef) {
  
  ifelse(
    x>=threshold,
    densityf(x)/(1-cumulativef(threshold)),
    0
  )
  
}

threshold <- 170

example.height$updated <- sapply(example.height$x, function(x) {literal.listener(x=x, threshold=threshold, densityf=function(x) {dnorm(x, 180, 10)}, cumulativef=function(x) {pnorm(x, 180, 10)} )}) 

# this starts the plot creation
g1 <- ggplot(example.height, aes(x=x, y=y)) 
g1 <- g1 + geom_area(fill="green", alpha=.4) 

# we add the result of updated belief
g1 <- g1 + geom_area(aes(y=updated),fill="steelblue", alpha=.4) 
g1 <- g1 + xlab("height") + ylab("P") + theme_gray(20) 


g1


expected.success <- function(threshold, scale.points, densityf, cumulativef) {
  
  ifelse(threshold>min(scale.points), sum(sapply(scale.points[scale.points<threshold], function(x) {densityf(x) * densityf(x)})), 0) + 
    sum(sapply(scale.points[scale.points>=threshold], function(x) {densityf(x) * literal.listener(x, threshold, densityf, cumulativef)}))
  
}

#Task 1

utility <- function(threshold, scale.points, coverage.parameter, densityf, cumulativef) {
  ES <- expected.success(threshold, scale.points, densityf, cumulativef)
  cumulative <- coverage.parameter * cumulativef((threshold))
  ES + cumulative
}


probability.threshold <- function(threshold, scale.points, lambda, coverage.parameter, densityf, cumulativef) {
  threshold_value <- function(t) { exp(lambda * utility(t, scale.points, coverage.parameter, densityf, cumulativef)) }
  
  numerator <- threshold_value(threshold)
  denominator <- sum(sapply(scale.points, threshold_value ))
  
  numerator / denominator
}

use.adjective <- function(degree, scale.points, lambda, coverage.parameter, densityf, cumulativef) {
  
  scale.points.leq <- scale.points[scale.points <= degree]
  probability <- sapply(scale.points.leq, 
                          function(d) probability.threshold(d, scale.points, lambda, coverage.parameter, densityf, cumulativef))
  
  sigma <- sum(probability)
  
  sigma
}

use.adjective.fast <- function(degree, scale.points, probability.of.thresholds) {
  
  probability <- probability.of.thresholds[scale.points <= degree]
  
  sigma <- sum(probability)
  
  sigma
}

# Help - tests you should pass

#probability.threshold is a probability, so if you sum up all values it generates, the result should be 1
round(sum(sapply(1:10, function(x) {probability.threshold(x, 1:10, 50, 0, function(x) {dnorm(x, 5, 1)}, function(x) {pnorm(x, 5, 1)})}))) == 1

#for narrow normal distribution, prob. threshold should be max just one value above the average
which(sapply(1:10, function(x) {probability.threshold(x, 1:10, 50, 0, function(x) {dnorm(x, 5, 1)}, function(x) {pnorm(x, 5, 1)})})==max(sapply(1:10, function(x) {probability.threshold(x, 1:10, 50, 0, function(x) {dnorm(x, 5, 1)}, function(x) {pnorm(x, 5, 1)})}))) == 6

#use.adjective should be very unlikely on values 5 and smaller and very likely afterwards
round(sapply(1:10, function(x) {use.adjective(x, 1:10, 50, 0, function(x) {dnorm(x, 5, 1)}, function(x) {pnorm(x, 5, 1)})})[5], 3) == 0.005
round(sapply(1:10, function(x) {use.adjective(x, 1:10, 50, 0, function(x) {dnorm(x, 5, 1)}, function(x) {pnorm(x, 5, 1)})})[6], 3) == 1


#probabilities for different threshold values
lambda <- 50
coverage <- 0
densityf <- function(x) {dnorm(x, mean=180, sd=10)}
cumulativef <- function(x) {pnorm(x, 180, 10)}
probability.of.thresholds <- sapply(scale.points, 
                                    function(d) probability.threshold(d, scale.points, lambda, coverage, densityf, cumulativef))

#sigma for different threshold values
sigma.of.thresholds <- sapply(scale.points, 
                              function(d) use.adjective.fast(d, scale.points, probability.of.thresholds))


plotdata <- data.frame("height" = scale.points, "P" = probability.of.thresholds, "sigma" = sigma.of.thresholds)

#plot probabilites
g1 <- ggplot(plotdata) + 
  geom_area(aes(x=height, y=P), fill="green", alpha=.4)


g2 <- ggplot(plotdata) + 
  geom_area(aes(x=height, y=sigma), fill="steelblue", alpha=.4)



# Task 2:
# Explore expectded.success and use.adjective for various prior distribution functions.
# For this, assume that coverage.parameter $c$ is at 0 and lambda is at 50.

#apply to IQ

scale.points <- c(0:150) 
densityf <- function(x) {dnorm(x, mean=100, sd=15)}
cumulativef <- function(x) {pnorm(x, 100, 15)}
probability.of.thresholds <- sapply(scale.points, 
                                    function(d) probability.threshold(d, scale.points, lambda, coverage, densityf, cumulativef))
sigma.of.thresholds <- sapply(scale.points, 
                              function(d) use.adjective(d, scale.points, probability.of.thresholds))


expected.success.values <- sapply(scale.points, function(d) expected.success(d,  scale.points, densityf, cumulativef))


plotdata <- data.frame("IQ" = scale.points, "P" = probability.of.thresholds, "sigma" = sigma.of.thresholds, "Success" = expected.success.values)


#plot probabilites
g1 <- ggplot(plotdata) + 
  geom_area(aes(x=IQ, y=P), fill="green", alpha=.4)


g2 <- ggplot(plotdata) + 
  geom_area(aes(x=IQ, y=sigma), fill="steelblue", alpha=.4)

g3 <- ggplot(plotdata) + 
  geom_area(aes(x=IQ, y=Success), fill="steelblue", alpha=.4)


#For waiting times

scale.points <- c(0:30) 
densityf <- function(x) {dgamma(x, shape = 2, scale = 1)}
cumulativef <- function(x) {pgamma(x, shape = 2, scale = 1)}

probability.of.thresholds <- sapply(scale.points, 
                                    function(d) probability.threshold(d, scale.points, lambda, coverage, densityf, cumulativef))
sigma.of.thresholds <- sapply(scale.points, 
                              function(d) use.adjective(d, scale.points, probability.of.thresholds))


expected.success.values <- sapply(scale.points, function(d) expected.success(d,  scale.points, densityf, cumulativef))


plotdata <- data.frame("Time" = scale.points, "P" = probability.of.thresholds, "sigma" = sigma.of.thresholds, "Success" = expected.success.values)

#plot probabilites
g1 <- ggplot(plotdata) + 
  geom_area(aes(x=Time, y=P), fill="green", alpha=.4)

g2 <- ggplot(plotdata) + 
  geom_area(aes(x=Time, y=sigma), fill="steelblue", alpha=.4)

g3 <- ggplot(plotdata) + 
  geom_area(aes(x=Time, y=Success), fill="steelblue", alpha=.4)


# Experimental data

data.adjective <- read.csv(file="adjective-data.csv", header=TRUE)

gaussian.dist <-   c(1,2,3,4,5,6,5,4,3,2,1,0,0,0)
left.skew.dist <-  c(2,5,6,6,5,4,3,2,1,1,1,0,0,0)
moved.dist <-      c(0,0,0,1,2,3,4,5,6,5,4,3,2,1)
right.skew.dist <- c(1,1,1,2,3,4,5,6,6,5,2,0,0,0)

sapply(1:14, function(x) {round(length(rgamma(360, shape=1, scale=100)[which(round(rgamma(360, shape=4, scale=1.5)) == x)])/10)})


data.gaus <- data.adjective[data.adjective$Distribution=="gaussian",]
data.left <- data.adjective[data.adjective$Distribution=="left",]
data.moved <- data.adjective[data.adjective$Distribution=="moved",]

library(ggplot2)
library(gridExtra)
p.g <- ggplot(data.gaus,aes(x=Stimulus,y=100*percentage,colour=Adjective))+geom_line()+ ylab("P") +ggtitle("gaussian")
p.l <- ggplot(data.left,aes(x=Stimulus,y=100*percentage,colour=Adjective))+geom_line()+ggtitle("left skewed")
p.m <- ggplot(data.moved,aes(x=Stimulus,y=100*percentage,colour=Adjective))+geom_line()+ggtitle("moved")

grid.arrange(p.g,p.l,p.m,ncol=2)

# Task 3:
# check cor between predicted and observed data
scale.points <- c(1:14)
densityf.gaus <- function(x) {dnorm(x, mean=6, sd=2)}
cumulativef.gaus <- function(x) {pnorm(x, mean=6, sd=2)}

densityf.left <- function(x) {dgamma(x, shape = 4, scale = 1.5)}
cumulativef.left <- function(x) {pgamma(x, shape = 4, scale = 1.5)}

densityf.moved <- function(x) {dnorm(x, mean=9, sd=2)}
cumulativef.moved <- function(x) {pnorm(x, mean=9, sd=2)}

lambda = 40
coverage.options <- c(0.1, 0, -0.1)

try.parameters <- function(densityf, cumulativef, coverage.parameter) {
  probability.of.thresholds <- sapply(scale.points, 
                                      function(d) probability.threshold(d, scale.points, lambda, coverage.parameter, densityf, cumulativef))
  sigma.of.thresholds <- sapply(scale.points, 
                                function(d) use.adjective.fast(d, scale.points, probability.of.thresholds))
  
  sigma.of.thresholds
}

df <- data.frame(stimulus = c(0), P = c(0), distribution = c(""), coverage = c(0), stringsAsFactors = FALSE)
for (c in coverage.options) {
  gaus.sigmas <- try.parameters(densityf.gaus, cumulativef.gaus, c)
  left.sigmas <- try.parameters(densityf.left, cumulativef.left, c)
  moved.sigmas <- try.parameters(densityf.moved, cumulativef.moved, c)
  
  for (point in scale.points) {
    df <- rbind(df, c(point, gaus.sigmas[point], "gaussian", c))
    df <- rbind(df, c(point, left.sigmas[point], "left", c))
    df <- rbind(df, c(point, moved.sigmas[point], "moved", c))
  }
}

df$distribution <- as.factor(df$distribution)

get.correlation <- function(dist1, dist2, coverage.parameter, model.results, experiment.results) {
  model.selection <- model.results[model.results$distribution == dist1,]
  model.selection <- model.selection[model.selection$coverage == coverage.parameter,]
  model.vec <- as.numeric(model.selection$P)
  
  experiment.selection <- experiment.results[experiment.results$Adjective == "big",]
  experiment.selection <- experiment.selection[experiment.selection$Distribution == dist2,]
  

  cor(model.vec, experiment.selection$percentage, method = "pearson")
}

distributions <- c("gaussian", "gaussian", "left", "moved")

for (dist in distributions) {
  for (coverage in coverage.options) {
    correlation <- get.correlation(dist, dist, coverage, df, data.adjective )
    print(c(dist, coverage, correlation))
  }
}

# effect of flipping prior distribution

best.coverage <- 0.1
dist.options <- c("gaussian", "left")

for (dist.a in dist.options) {
  for (dist.b in dist.options) {
    correlation <- get.correlation(dist.a, dist.b, best.coverage, df, data.adjective )
    print(c(dist.a, dist.b, correlation))    
  }
}
  
#install.packages("BayesianTools")
library(BayesianTools)

prior <- createUniformPrior(lower=c(0,0.1), upper=c(0.1,1), best=NULL)

data.gaus.big <- subset(data.gaus, Adjective == "big")

likelihood <- function(param1) {
  
  collect <- 0
  
  for (i in 1:14) {
    collect  <- collect + dnorm(data.gaus.big$percentage[i], mean=param1[1]+param1[2], sd=0.1, log=TRUE)
    
  }
  
  return(collect)
} 

bayesianSetup <- createBayesianSetup(likelihood = likelihood, prior = prior)

iter = 10000
settings = list(iterations = iter, message = FALSE)
out <- runMCMC(bayesianSetup = bayesianSetup, settings = settings)

summary(out)

# Task 4

model.prediction <- function(coverage.parameter, lambda) {
  scale.points <- c(1:14)
  densityf <- function(x) {dnorm(x, mean=6, sd=2)}
  cumulativef <- function(x) {pnorm(x, mean=6, sd=2)}
  
  probability.of.thresholds <- sapply(scale.points, 
                                      function(d) probability.threshold(d, scale.points, lambda, coverage.parameter, densityf, cumulativef))
  sigma.of.thresholds <- sapply(scale.points, 
                                function(d) use.adjective.fast(d, scale.points, probability.of.thresholds))
  
  sigma.of.thresholds
  
}

prior <- createUniformPrior(lower=c(-1,1), upper=c(1,50), best=NULL)

likelihood <- function(param1) {
  
  collect <- 0
  
  for (i in 1:14) {
    collect  <- collect + dnorm(data.gaus.big$percentage[i], mean=model.prediction(param1[1], param1[2])[i], sd=0.1, log=TRUE)
    
  }
  
  return(collect)
} 

bayesianSetup <- createBayesianSetup(likelihood = likelihood, prior = prior)

iter = 10000

settings = list(iterations = iter, message = FALSE)

out <- runMCMC(bayesianSetup = bayesianSetup, settings = settings)


# Task 5

define.likelihood <- function(adjective, distribution) {
  #retrieve relevant experiment data
  experiment.data <- data.adjective[data.adjective$Adjective == adjective]
  experiment.data <- experiment.data[experiment.data$Distribution == distribution]
  
  #make relevant model predict function
  model.predict <- function(coverage.parameter, lambda) {
    scale.points <- c(1:14)
    
    #define density and cumulative functions
    if (distribution == "gaussian") {
      densityf <- function(x) {dnorm(x, mean=6, sd=2)}
      cumulativef <- function(x) {pnorm(x, mean=6, sd=2)}      
    }
    if (distribution == "left") {
      densityf <- function(x) {dgamma(x, shape = 4, scale = 1.5)}
      cumulativef <- function(x) {pgamma(x, shape = 4, scale = 1.5)}     
    }
    if (distribution == "moved") {
      densityf <- function(x) {dnorm(x, mean=9, sd=2)}
      cumulativef <- function(x) {pnorm(x, mean=9, sd=2)}
    }
    
    #compute model predictions
    probability.of.thresholds <- sapply(scale.points, 
                                        function(d) probability.threshold(d, scale.points, lambda, coverage.parameter, densityf, cumulativef))
    sigma.of.thresholds <- sapply(scale.points, 
                                  function(d) use.adjective.fast(d, scale.points, probability.of.thresholds))
    
    sigma.of.thresholds
  }
  
  #define likelihood
  collect <- 0
  
  for (i in 1:14) {
    collect  <- collect + dnorm(experiment.data$percentage[i], mean=model.predict(param1[1], param1[2])[i], sd=0.1, log=TRUE)
    
  }
  
  return(collect)
}

for (adjective in c("big", "pointy", "tall")) {
  for (distribution in distributions) {
    bayesianSetup <- createBayesianSetup(likelihood = define.likelihood(adjective, distribution), prior = prior)
    
    iter = 10000
    
    settings = list(iterations = iter, message = FALSE)
    
    out <- runMCMC(bayesianSetup = bayesianSetup, settings = settings)
    
    objectname <- paste("output_", adjective, "_", distribution, sep="")
    filename <- paste("output_", adjective, "_", distribution, ".RData", sep="")
    
    save(out, list = c(objectname), file = filename)
  }
}

#import simulations

