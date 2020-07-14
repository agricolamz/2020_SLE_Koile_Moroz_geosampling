# total number of locations
N <- seq(20, 120, by = 20)
# type of spatial relations
spatial_relations <- c("random", "two_regions", "central_periphery")
# proportion of variation in the explored variable
p <- seq(0.1, 0.9, by = 0.1)
# amount of clusters
k_for_calculation <- seq(0.1, 0.9, by = 0.1)
# percantage of observations taken from each cluster
r <- seq(0.1, 0.9, by = 0.1)

df <- expand.grid(N, spatial_relations, p, k_for_calculation, r)
names(df) <- c("N", "spatial_relations", "p", "k", "r")

df %>% 
  mutate(k = k*N,
         n = N*r/k,
         id = 1:n()) ->
  df

set.seed(42)
map_dfr(seq_along(df$N), function(i){
  # create a samle of places
  tibble(id = i,
         x = rnorm(df$N[i]),
         y = rnorm(df$N[i]))
}) %>% 
  left_join(df) ->
  results

library(furrr)
future::plan(multiprocess)
map_dfr(seq_along(results$id), function(i){
  tibble(value = sample(c("a", "b"), 
                        size = results$N[i], 
                        prob = c(results$p[i], 1 - results$p[i]), 
                        replace = TRUE))
}) %>% 
  right_join(results) ->
  results_2

  filter(id == 1000) %>% 
  ggplot(aes(x, y))+
  geom_point()
         




         value = sample(x = c("a", "b"), 
                        size = 1,
                        prob = c(p, 1-p), 
                        replace = TRUE),
         Var1 = Var1 + rnorm(N*length(probs)*length(types)),
         Var2 = Var2 + rnorm(N*length(probs)*length(types)),
         value = case_when(
           spatial_relations == "random" ~ value,
           spatial_relations == "two_regions" ~ ifelse(Var1 > Var2, 
                                       sample(c(value, "b")), 
                                       sample(c(value, "a"))),
           spatial_relations == "central_periphery" ~ ifelse(sqrt((Var1 - N/2)^2 + (Var2 - N/2)^2) > sqrt(2*(N/4)^2), 
                                     sample(c(value, "b")), 
                                     sample(c(value, "a"))))) ->
  results
