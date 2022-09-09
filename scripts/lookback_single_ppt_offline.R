library(tidyverse)
library(brms)

# Data of participant with most lookbacks
data <- read_csv("data/all_data_incremental.csv") %>% 
  filter(task == "A", ppt == "M19-0036") %>% 
  select(is_lookback) 
  
keys_in_increment <- 50
n_bins <- floor(nrow(data)/keys_in_increment) * keys_in_increment
data <- data[1:n_bins,]

data <- data %>% 
  mutate(increment = rep(1:(bins/keys_in_increment), each = keys_in_increment)) %>% 
  group_by(increment) %>% 
  summarise(sum_lookbacks = sum(is_lookback)) 

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
        file = "stanout/lookback_single_ppt_offline.rda", 
        compress = "xz")
