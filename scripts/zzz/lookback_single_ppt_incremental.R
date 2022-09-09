library(tidyverse)
theme_set(theme_bw() +
            theme(legend.position = "top",
                  legend.justification = "right"))

# Load data
data <- read_csv("data/all_data_incremental.csv") %>% 
  filter(task == "A") %>% 
  select(ppt, is_lookback) %>% 
  group_by(ppt) %>%
  mutate(cumsum = cumsum(is_lookback),
         idx = 1:n(),
         idx_ctr = scale(idx, scale = FALSE)[,1]) %>% 
  ungroup() %>% 
  filter(ppt == "M19-0036") 

# Model for prior
m <- lm(cumsum ~ idx_ctr, data = data)
coef(m)

# Bin data into key transitions of 100
keys_in_increment <- 50
n_bins <- floor(nrow(data)/keys_in_increment) * keys_in_increment
data <- data[1:n_bins,]

data_increm <- data %>% 
  mutate(increment = rep(1:(bins/keys_in_increment), each = keys_in_increment)) %>% 
  group_by(increment) %>% 
  summarise(sum_lookbacks = sum(is_lookback)) %>% 
  mutate(cumsum_lookbacks = cumsum(sum_lookbacks),
         lambda_posterior = NA,
         lambda_prior = NA,
         p_obs_given_prior = NA,
  #       p_obs_given_posterior = NA,
         p_lambda_given_prior = NA)

# Incremental modelling
lambda_prior <- lambda_prior_init <- coef(m)[2] * keys_in_increment

# Theoretical lambda space
max_lambda <- round(max(data_increm$cumsum_lookbacks), -2)
lambdas <- seq(.1, max_lambda, .5)

for(i in 1:nrow(data_increm)){
  # ,rate = 1 / ( 1 + i / n_bins )

  # data
#  i <- 1
  new_obs <- data_increm$sum_lookbacks[i]
#  new_obs <- data_increm$cumsum_lookbacks[i]
  
  # prior probability distribution over lambda
  prior <- dgamma(lambdas, shape = lambda_prior, log = T)
  likelihood <- dgamma(new_obs, shape = lambdas, log = T)
  posterior <- likelihood + prior

  lambda_posterior <- lambdas[which.max(posterior)]
  lambda_prior <- lambdas[which.max(prior)]
  
  p_obs_given_prior <- 1 - pgamma(new_obs, shape =  lambda_prior)
  p_obs_given_posterior <- 1 - pgamma(new_obs, shape = lambda_posterior)
  p_lambda_given_prior <- 1 - pgamma(lambda_posterior, shape = lambda_prior)
  
  # Save to data  
  data_increm$lambda_posterior[i] <- lambda_posterior
  data_increm$lambda_prior[i] <- lambda_prior  
  data_increm$p_obs_given_prior[i] <- p_obs_given_prior
#  data_increm$p_obs_given_posterior[i] <- p_obs_given_posterior
  data_increm$p_lambda_given_prior[i] <- p_lambda_given_prior  

  # New prior
#  lambda_prior <- lambda_prior_init# * i#lambda_posterior
  lambda_prior <- lambda_posterior
  
  }

glimpse(data_increm);data_increm

data_increm$lambda_posterior[nrow(data_increm)]
#data_increm$lambda_prior[nrow(data_increm)]

data_increm %>% 
  mutate(prev_lookbacks = lag(sum_lookbacks),
         diff_lookbacks = sum_lookbacks - prev_lookbacks) %>% 
  pivot_longer(c(diff_lookbacks, starts_with("p_"))) %>% 
  ggplot(aes(x = increment, y = value, colour = name)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = .05) +
  facet_wrap(~name, scales = "free")

data_increm %>% 
  pivot_longer(c(sum_lookbacks, lambda_posterior, lambda_prior)) %>% 
  ggplot(aes(x = increment, y = value, colour = name)) +
  geom_point() +
  geom_line() 
#  geom_abline(slope = coef(m)[2]*keys_in_increment, 
#              intercept = 0)

dens <- density(data_increm$lambda_posterior)
mean <- dens$x[which.max(dens$y)]
ggplot(data_increm, aes(x = sum_lookbacks)) +
  geom_density(aes(x = lambda_posterior)) +
  geom_histogram(aes(y = ..density..)) +
  geom_vline(xintercept = mean) +
  geom_label(x = mean, y = .05, label = paste("lambda","==", round(mean,2)), parse = TRUE)

# for the brms model, get residuals per index to detect large deviations. 

tibble(lambda = dens$x, density = dens$y) %>%
  #mutate(lam_mean = mean(lambda)) %>% 
  mutate(var = (lambda - mean)^2 * density) %>% 
  summarise(rate = mean/sqrt(sum(var)),
            scale = sqrt(sum(var)) / mean,
            shape = mean^2/sqrt(sum(var))) 

data_increm <- data_increm %>% 
  mutate(residual = sum_lookbacks - lambda_posterior)

write_csv(data_increm, "data/incremental_lookbacks.csv")
