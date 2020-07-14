library(tidyverse)
# total number of locations
N <- seq(30, 130, by = 20)
# type of spatial relations
spatial_relations <- c("random", "two_regions", "central_periphery")
# proportion of variation in the explored variable
p <- seq(0.05, 0.5, by = 0.05)
# amount of clusters
k_for_calculation <- seq(0.1, 0.9, by = 0.1)
# percantage of observations taken from each cluster
r <- seq(0.1, 0.9, by = 0.1)

df <- expand.grid(N, p, k_for_calculation, r)
names(df) <- c("N", "p", "k", "r")
rm(N, p, r, spatial_relations, k_for_calculation)

df %>% 
  mutate(k = k*N,
         id = 1:n()) ->
  df

set.seed(42)
map_dfr(seq_along(df$N), function(i){
  # create a samle of places
  tibble(id = i,
         x = rnorm(df$N[i]*df$p[i], mean = -1),
         y = rnorm(df$N[i]*df$p[i], mean = 1),
         value = "a") ->
    a
  tibble(id = i,
         x = rnorm(df$N[i]*(1 - df$p[i])),
         y = rnorm(df$N[i]*(1 - df$p[i])),
         value = "b")  %>% 
    bind_rows(a) %>% 
    mutate(spatial_relations = "two_regions")
}) %>% 
  left_join(df) ->
  results_regions

set.seed(42)
map_dfr(seq_along(df$N), function(i){
  # create a samle of places
  tibble(id = i,
         x = rnorm(df$N[i]*df$p[i]),
         y = rnorm(df$N[i]*df$p[i]),
         value = "a") ->
    a
  tibble(id = i,
         x = rnorm(df$N[i]*(1 - df$p[i]), sd = 3),
         y = rnorm(df$N[i]*(1 - df$p[i]), sd = 3),
         value = "b")  %>% 
    bind_rows(a) %>% 
    mutate(spatial_relations = "central_periphery")
}) %>% 
  left_join(df) ->
  results_central

set.seed(42)
map_dfr(seq_along(df$N), function(i){
  # create a samle of places
  tibble(id = i,
         x = rnorm(df$N[i]*df$p[i]),
         y = rnorm(df$N[i]*df$p[i]),
         value = "a") ->
    a
  tibble(id = i,
         x = rnorm(df$N[i]*(1 - df$p[i])),
         y = rnorm(df$N[i]*(1 - df$p[i])),
         value = "b")  %>% 
    bind_rows(a) %>% 
    mutate(spatial_relations = "random")
}) %>% 
  left_join(df) ->
  results_random

results_random %>% 
  bind_rows(results_central, 
            results_regions) %>% 
  mutate(id = 1:n()) ->
  results_all
rm(results_random, results_central, results_regions, df)

