setwd("/home/agricolamz/work/materials/2020_SLE_Koile_Moroz_geosampling")
library(tidyverse)
df <- data.table::fread("generated_data/equadistant_dataset.csv",nrows = 1000000)

colnames(df) <-
  c(
    'x',
    'y',
    'id',
    'set',
    'H',
    'n_villages',
    'n_categories',
    'V1',
    'V2',
    'V3',
    'V4',
    'V5',
    'V6',
    'V7',
    'V8',
    'V9',
    'V10'
  )

df2 %>% 
  ggplot(aes(H))+
  geom_density()+
  facet_grid(n_villages~n_categories, margins = TRUE, scales = "free_x")+
  theme_bw()


df %>% 
  slice(1:300) %>% 
  group_by(set) %>% 
  mutate(cluster = kmeans(as.matrix(tibble(x, y)), centers = unique(n_categories))$cluster)


test <- sample(unique(df$set), 6)

df %>% 
  filter(set %in% test) %>% 
  mutate(label = str_c(set, "\nN = ", n_categories, " n = ", n_villages, " H = ", H)) %>% 
  ggplot(aes(x, y, color = factor(id)))+
  geom_point()+
  facet_wrap(~label)+
  stat_ellipse()+
  theme_minimal()



map_dfr(trials$id, function(x){
  circ %>% 
    mutate(id = 1:n(),
           cluster = kmeans(as.matrix(tibble(latitude, longitude)), centers = trials$n_centers[x])$cluster) %>% 
    group_by(cluster) %>% 
    sample_n(1) %>% 
    ungroup() %>% 
    count(dialect) %>% 
    mutate(trial = x,
           proportion = trials$proportion[x],
           type = "k-means")
}) ->
  sample_results_k_means




