# Real time updating


library(tidyverse)
library(patchwork)


prior_mean <- 100
prior_sd <- 50
ikis <- rnorm(100, mean = 150, 10)

# Could be done on single incremental or updating on particular chunks:
# - all 30 secs
# - after 10 events

# To estimate the probability of particular observations

# Get probability of first data point given the prior
p_data <- dnorm(ikis[1], mean = prior_mean, sd = prior_sd)

# To estimate participant behaviour:
p_data <- dnorm(ikis[1:10], mean = prior_mean, sd = prior_sd)
sum(p_data)

# Use first data point to update prior mean and prior sd

lambda <- 1.4
N <- 90
p <- lambda / N
sims <- 10000
sample <- replicate(sims, sum(rbinom(N, 1, p)))
#sample <- rpois(n = sims, lambda = lambda)

mean(sample)
pmf_goals <- table(sample) / sum(table(sample))

# Prior
alpha <- 1.4 # priors rate
qs <- sample(0:10, 101,  replace = TRUE)
ps <- rgamma(qs, shape = alpha)
prior <- table(ps) / sum(table(ps))
mean(prior)
prior_index <- as.numeric(names(prior))
sum(prior_index * prior)

# Update
# if lambda 1.4, whats the prob of scoring 4 goals
k <- 4
#dpois(k, lambda)
likelihood <- dpois(x = k, lambda = prior_index)

# update prior with likelihood
pmf <- prior * likelihood
update <- pmf / sum(pmf)
sum(pmf)

lams <- rgamma(1, shape = lambda)
goals <- rpois(18, lambda = lams/18)
alpha <- 1.4
beta <- 1
num_goals <- 0
num_5_mins <- 0
x <- seq(0,10)
for(i in seq_along(goals)){
  num_goals <- num_goals + goals[i]
  num_5_mins <- num_5_mins + 1
  y <- dgamma(x, shape = 1.4 + num_goals, scale = 1/(1 + num_5_mins/18))
  if(i == 1){
    plot <- ggplot(data = NULL, aes(x = x, y = exp(y))) +
      geom_line() 
#      labs(subtitle = paste("Goals scored so far: ", num_goals))
  }
  if(i > 1)
    plot <- plot + geom_line(aes(x = x, y = exp(y)))
}

plot
  