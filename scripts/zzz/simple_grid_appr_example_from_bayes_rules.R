lambda <- 4 # this is the parameter to recover
(fake_data <- rpois(n = 30, lambda = lambda))

# Step 1: Define a grid of 501 lambda values
grid_data <- tibble(lambda_grid = seq(from = 0.1, to = 10, length = 500)) 

# Step 2: Evaluate the prior & likelihood at each lambda
grid_data <- map(fake_data, ~dpois(., grid_data$lambda_grid, log = T)) %>% 
  bind_cols() %>% 
  rowSums() %>% 
  bind_cols(grid_data) %>% 
  rename(likelihood = `...1`) %>% 
  mutate(prior = dgamma(lambda_grid, 3, 1, log = T))

# Step 3: Approximate the posterior
grid_data <- grid_data %>% 
  mutate(unnormalized = likelihood + prior,
         posterior = exp(unnormalized) / sum(exp(unnormalized)))

(map <- grid_data$lambda_grid[which.max(grid_data$posterior)])

# Set the seed
set.seed(84735)

# Step 4: sample from the discretized posterior
post_sample <- sample_n(grid_data, size = 10000, 
                        weight = posterior, replace = TRUE)

# Histogram of the grid simulation with posterior pdf 
ggplot(post_sample, aes(x = lambda_grid)) + 
  geom_histogram(aes(y = ..density..), color = "white") + 
  lims(x = c(0, 10)) +
  geom_vline(xintercept = map)
