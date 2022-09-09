library(tidyverse)
library(janitor)
theme_set(theme_bw() +
            theme(legend.position = "top",
                  legend.justification = "right"))

# Simulate lookback data
# avg. number of lookbacks per 50 keys
lambda <- 4 # this is the parameter to recover
n_increments <- 50
(lookback_data <- rpois(n = n_increments, lambda = lambda))

data <- tibble(increment = 1:n_increments,
               sum_lookbacks = lookback_data) %>% 
  mutate(lambda_posterior = NA,
         lambda_prior = NA,
         p_obs = NA)

# Incremental modelling
lambda_prior <- 10 # <- this one is obviously a very extreme priors, just as example

# Theoretical lambda space
max_lambda <- round(max(data$sum_lookbacks), -1)
lambdas <- seq(.1, max_lambda, length = 100)
likelihood <- tibble(.rows = length(lambdas))

for(i in 1:n_increments){
  # data
  new_obs <- data$sum_lookbacks[i]

  # prior probability distribution over lambda
  prior <- dgamma(lambdas, lambda_prior, 1, log = TRUE)

  # Likelihood of current and previous increments
  # Sum of the log liklihoods is equivalent to the product of the likelihood
  likelihood <- rowSums(
    bind_cols(likelihood, 
              dpois(new_obs, lambdas, log = TRUE)))

  # We add prior to likelihood cause both are on the log scale
  posterior <- likelihood + prior
  # Normalising (so it sums to unity) requires exponential
  posterior <- exp(posterior) / sum(exp(posterior))

  # Find lambda with highest prior / posterior probability
  lambda_posterior <- lambdas[which.max(posterior)]

  # Get conditional probabilities    
  p_obs_given_posterior <- 1 - pgamma(new_obs, lambda_posterior)

  # Save to data  
  data$lambda_posterior[i] <- lambda_posterior
  data$lambda_prior[i] <- lambda_prior  
  data$p_obs[i] <- p_obs_given_posterior

  # New prior
  lambda_prior <- lambda_posterior
  last_posterior <- posterior
}

glimpse(data);data

(mean <- data$lambda_posterior[n_increments])

grid_data <- tibble(lambdas = lambdas,
                    posterior = last_posterior)

sample_n(grid_data, 
         size = 10000, 
         weight = posterior,
         replace = TRUE) %>% 
  tabyl(lambdas) %>% 
  mutate(cumsum(percent))

ggplot(grid_data, aes(x = lambdas, y = posterior)) + 
  geom_point() + 
  geom_segment(aes(x = lambdas, xend = lambdas, y = 0, yend = posterior)) +
  coord_cartesian(xlim = c(2, 6)) +
#  geom_vline(xintercept = mean, colour = "red") +
  geom_vline(xintercept = lambda, 
             colour = "red", 
             linetype = "dashed",
             size = 1) +
  labs(caption = "True parameter value shown as vertical red line")

data %>% 
  pivot_longer(c(sum_lookbacks, starts_with("p_"))) %>% 
  ggplot(aes(x = increment, y = value, colour = name)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = .05) +
  facet_wrap(~name, scales = "free")

data %>% 
    mutate(across(starts_with("p_obs"), ~.>.05)) %>% 
    ggplot(aes(x = increment, 
               y = sum_lookbacks, 
               colour = p_obs, 
               group = 1)) +
    geom_point(size = 4) +
    geom_line(colour = "black")  

  
data %>% 
  pivot_longer(c(sum_lookbacks, lambda_posterior, lambda_prior)) %>% 
  ggplot(aes(x = increment, y = value, colour = name)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = lambda,             
             colour = "red", 
             linetype = "dashed",
             size = 1) +
  labs(caption = "True parameter value shown as horizontal red line",
       colour = "")

  
#  geom_abline(slope = coef(m)[2]*keys_in_increment, 
#              intercept = 0)

# not actually a residual. A residual your be the predicted data compared
# to the observed data
data <- data %>% 
  mutate(residual = sum_lookbacks - lambda_posterior)

write_csv(data, "data/incremental_simulated_lookbacks.csv")


