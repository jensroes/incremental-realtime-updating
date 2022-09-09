library(tidyverse)
library(brms)

# Simulate lookback data
n_increments <- 50

# avg. number of lookbacks per 50 keys
lambda <- 4 # this is the parameter to recover

# draw a lambda from prior
set.seed(365)
data <- tibble(increment = 1:n_increments,
               sum_lookbacks = rpois(n = n_increments, lambda = lambda))

# Sampling parameters
n_cores = 3
n_chains = 3
iterations = 20000

# Specify model 
model <- bf(sum_lookbacks ~ 1, family = poisson())

# Run model
fit <- brm(model, 
           data = data,
           chains = n_chains, 
           cores = n_cores,
           iter = iterations, 
           warmup = iterations/2,
           seed = 365)

posterior_summary(fit) %>% 
  as.data.frame() %>% 
  rownames_to_column() %>% 
  as_tibble() %>% 
  select(1,2,4,5) %>% 
  filter(str_detect(rowname, "b_Intercept")) %>% 
  mutate(across(-rowname, exp))

# Convergence and model check
rhat(fit)
pp_check(fit, nsamples = 500)
plot(fit)
#plot(conditional_effects(fit), points = T)

# Save model
saveRDS(fit, 
        file = "stanout/lookback_simulated_offline.rda", 
        compress = "xz")
