all_categories_values <- function(n_villages = 30, n_categories = 5){
  
  # from https://stackoverflow.com/a/32618446/6056442
  w <- 1:n_villages
  t <- n_villages
  D <- list()
  for (j in 0:t) D[[paste(0, j)]] <- list(c())
  for (i in 1:t) D[[paste(i, 0)]] <- list()
  for (j in 1:t) {
    for (i in 1:t) {
      D[[paste(i, j)]] <- do.call(c, lapply(0:floor(i/w[j]), 
                                            function(r) {
                                              lapply(D[[paste(i-r*w[j], j-1)]], 
                                                     function(x) c(x, rep(w[j], r)))
                                            }))
    }
  }
  r <- D[[paste(t, t)]]
  
  library(tidyverse)
  r[which(unlist(lapply(r, length)) %in% n_categories)] %>% 
    as.data.frame() %>%
    t() %>% 
    unname() %>% 
    as_tibble() %>% 
    mutate(set = 1:n()) %>% 
    pivot_longer(values_to = "value", names_to = "column", -set) %>% 
    group_by(set) %>% 
    mutate(ratio = value/n_villages,
           H = -sum(ratio*log2(ratio))) %>% 
    select(-ratio) %>% 
    mutate(n_villages,
           n_categories) %>% 
    pivot_wider(values_from = value, names_from = column) %>% 
    ungroup()
}


n_villages <- 50
n_categories <- 3

df <- all_categories_values(n_villages, n_categories)

map_dfr(seq_along(df$set), function(j){
  generate_equadistant(N = n_categories, n = df[j,5:ncol(df)]) %>% 
    bind_cols(df[j,1:4])  %>% 
    mutate(type = "equadistant")  
}) ->
    equadistant_dataset


# number of villages 30-40-50-60-70-80
# number of categories 3-4-5-6-7-8-9-10

# 4Garik
# rewrite wih Ezi's code
# generate all data and run all clustering in order to produce entropy vs proportion of variation discovered
