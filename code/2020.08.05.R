library(tidyverse)
theme_set(theme_bw())

# df_cl_5_v_30 ------------------------------------------------------------
n_villages <- 30
n_category <- 5
X <- as.matrix(expand.grid(1:n_villages, 
                           1:n_villages, 
                           1:n_villages, 
                           1:n_villages)) # repeat n_clusters - 1 times
X %>% 
  as_tibble() %>% 
  mutate(rs = rowSums(.)) %>% 
  filter(rs <= n_villages) %>% 
  mutate(Var5 = n_villages-rs) %>% 
  filter(Var5 > 0) %>% 
  filter(Var1 >= Var2,
         Var1 >= Var3,
         Var1 >= Var4,
         Var1 >= Var5,
         Var2 >= Var3,
         Var2 >= Var4,
         Var2 >= Var5,
         Var3 >= Var4,
         Var3 >= Var5,
         Var4 >= Var5) %>% 
  select(-rs) %>% 
  rowwise() %>% 
  mutate(H = -sum(Var1/n_villages*log2(Var1/n_villages),
                  Var2/n_villages*log2(Var2/n_villages),
                  Var3/n_villages*log2(Var3/n_villages),
                  Var4/n_villages*log2(Var4/n_villages),
                  Var5/n_villages*log2(Var5/n_villages))) %>% 
  arrange(H) %>% 
  ungroup() %>% 
  mutate(id = 1:n(),
         n_villages = n_villages,
         n_category = n_category) %>% 
  select(id, n_category, n_villages, H, Var1, Var2, Var3, Var4, Var5) %>% 
  slice(round(seq(nrow(.), 1, length.out = 30))) ->
  cl_5_v_30_sample

scale <- 1
eq_x5 <- cos(2*pi/5*0:4)*scale
eq_y5 <- sin(2*pi/5*0:4)*scale
cp_x5 <- c(0, 1, 0, -1, 0) * scale
cp_y5 <- c(0, 0, 1, 0, -1) * scale


map_dfr(1:30, function(i){
  tibble(x = c(rnorm(cl_5_v_30_sample$Var1[i], mean = cp_x5[1]),
               rnorm(cl_5_v_30_sample$Var2[i], mean = cp_x5[2]),
               rnorm(cl_5_v_30_sample$Var3[i], mean = cp_x5[3]),
               rnorm(cl_5_v_30_sample$Var4[i], mean = cp_x5[4]),
               rnorm(cl_5_v_30_sample$Var5[i], mean = cp_x5[5])),
         y = c(rnorm(cl_5_v_30_sample$Var1[i], mean = cp_y5[1]),
               rnorm(cl_5_v_30_sample$Var2[i], mean = cp_y5[2]),
               rnorm(cl_5_v_30_sample$Var3[i], mean = cp_y5[3]),
               rnorm(cl_5_v_30_sample$Var4[i], mean = cp_y5[4]),
               rnorm(cl_5_v_30_sample$Var5[i], mean = cp_y5[5])),
         value = c(rep("Var1", cl_5_v_30_sample$Var1[i]),
                   rep("Var2", cl_5_v_30_sample$Var2[i]),
                   rep("Var3", cl_5_v_30_sample$Var3[i]),
                   rep("Var4", cl_5_v_30_sample$Var4[i]),
                   rep("Var5", cl_5_v_30_sample$Var5[i])),
         type = "central-periphery") ->
    cp
  
  tibble(x = c(rnorm(cl_5_v_30_sample$Var1[i], mean = eq_x5[1]),
               rnorm(cl_5_v_30_sample$Var2[i], mean = eq_x5[2]),
               rnorm(cl_5_v_30_sample$Var3[i], mean = eq_x5[3]),
               rnorm(cl_5_v_30_sample$Var4[i], mean = eq_x5[4]),
               rnorm(cl_5_v_30_sample$Var5[i], mean = eq_x5[5])),
         y = c(rnorm(cl_5_v_30_sample$Var1[i], mean = eq_y5[1]),
               rnorm(cl_5_v_30_sample$Var2[i], mean = eq_y5[2]),
               rnorm(cl_5_v_30_sample$Var3[i], mean = eq_y5[3]),
               rnorm(cl_5_v_30_sample$Var4[i], mean = eq_y5[4]),
               rnorm(cl_5_v_30_sample$Var5[i], mean = eq_y5[5])),
         value = c(rep("Var1", cl_5_v_30_sample$Var1[i]),
                   rep("Var2", cl_5_v_30_sample$Var2[i]),
                   rep("Var3", cl_5_v_30_sample$Var3[i]),
                   rep("Var4", cl_5_v_30_sample$Var4[i]),
                   rep("Var5", cl_5_v_30_sample$Var5[i])),
         type = "equadistant") ->
    eq
  
  tibble(x = c(rnorm(cl_5_v_30_sample$Var1[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var2[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var3[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var4[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var5[i], sd = scale)),
         y = c(rnorm(cl_5_v_30_sample$Var1[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var2[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var3[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var4[i], sd = scale),
               rnorm(cl_5_v_30_sample$Var5[i], sd = scale)),
         value = c(rep("Var1", cl_5_v_30_sample$Var1[i]),
                   rep("Var2", cl_5_v_30_sample$Var2[i]),
                   rep("Var3", cl_5_v_30_sample$Var3[i]),
                   rep("Var4", cl_5_v_30_sample$Var4[i]),
                   rep("Var5", cl_5_v_30_sample$Var5[i])),
         type = "random") ->
    rd
  
  eq %>% 
    bind_rows(cp, rd) %>% 
    mutate(id = cl_5_v_30_sample$id[i],
           vars = str_c(cl_5_v_30_sample$Var1[i], 
                        cl_5_v_30_sample$Var2[i], 
                        cl_5_v_30_sample$Var3[i], 
                        cl_5_v_30_sample$Var4[i], 
                        cl_5_v_30_sample$Var5[i], 
                        sep = "-"),
           n_category = cl_5_v_30_sample$n_category[i],
           n_villages = cl_5_v_30_sample$n_villages[i],
           H = cl_5_v_30_sample$H[i])
}) ->
  df_cl_5_v_30


# df_cl_5_v_50 ------------------------------------------------------------
n_villages <- 50
n_category <- 5
X <- as.matrix(expand.grid(1:n_villages, 
                           1:n_villages, 
                           1:n_villages, 
                           1:n_villages)) # repeat n_clusters - 1 times
X %>% 
  as_tibble() %>% 
  mutate(rs = rowSums(.)) %>% 
  filter(rs <= n_villages) %>% 
  mutate(Var5 = n_villages-rs) %>% 
  filter(Var5 > 0) %>% 
  filter(Var1 >= Var2,
         Var1 >= Var3,
         Var1 >= Var4,
         Var1 >= Var5,
         Var2 >= Var3,
         Var2 >= Var4,
         Var2 >= Var5,
         Var3 >= Var4,
         Var3 >= Var5,
         Var4 >= Var5) %>% 
  select(-rs) %>% 
  rowwise() %>% 
  mutate(H = -sum(Var1/n_villages*log2(Var1/n_villages),
                  Var2/n_villages*log2(Var2/n_villages),
                  Var3/n_villages*log2(Var3/n_villages),
                  Var4/n_villages*log2(Var4/n_villages),
                  Var5/n_villages*log2(Var5/n_villages))) %>% 
  arrange(H) %>% 
  ungroup() %>% 
  mutate(id = 1:n(),
         n_villages = n_villages,
         n_category = n_category) %>% 
  select(id, n_category, n_villages, H, Var1, Var2, Var3, Var4, Var5) %>% 
  slice(round(seq(nrow(.), 1, length.out = 30))) ->
  cl_5_v_50_sample

map_dfr(1:30, function(i){
  tibble(x = c(rnorm(cl_5_v_50_sample$Var1[i], mean = cp_x5[1]),
               rnorm(cl_5_v_50_sample$Var2[i], mean = cp_x5[2]),
               rnorm(cl_5_v_50_sample$Var3[i], mean = cp_x5[3]),
               rnorm(cl_5_v_50_sample$Var4[i], mean = cp_x5[4]),
               rnorm(cl_5_v_50_sample$Var5[i], mean = cp_x5[5])),
         y = c(rnorm(cl_5_v_50_sample$Var1[i], mean = cp_y5[1]),
               rnorm(cl_5_v_50_sample$Var2[i], mean = cp_y5[2]),
               rnorm(cl_5_v_50_sample$Var3[i], mean = cp_y5[3]),
               rnorm(cl_5_v_50_sample$Var4[i], mean = cp_y5[4]),
               rnorm(cl_5_v_50_sample$Var5[i], mean = cp_y5[5])),
         value = c(rep("Var1", cl_5_v_50_sample$Var1[i]),
                  rep("Var2", cl_5_v_50_sample$Var2[i]),
                  rep("Var3", cl_5_v_50_sample$Var3[i]),
                  rep("Var4", cl_5_v_50_sample$Var4[i]),
                  rep("Var5", cl_5_v_50_sample$Var5[i])),
         type = "central-periphery") ->
    cp
  
  tibble(x = c(rnorm(cl_5_v_50_sample$Var1[i], mean = eq_x5[1]),
               rnorm(cl_5_v_50_sample$Var2[i], mean = eq_x5[2]),
               rnorm(cl_5_v_50_sample$Var3[i], mean = eq_x5[3]),
               rnorm(cl_5_v_50_sample$Var4[i], mean = eq_x5[4]),
               rnorm(cl_5_v_50_sample$Var5[i], mean = eq_x5[5])),
         y = c(rnorm(cl_5_v_50_sample$Var1[i], mean = eq_y5[1]),
               rnorm(cl_5_v_50_sample$Var2[i], mean = eq_y5[2]),
               rnorm(cl_5_v_50_sample$Var3[i], mean = eq_y5[3]),
               rnorm(cl_5_v_50_sample$Var4[i], mean = eq_y5[4]),
               rnorm(cl_5_v_50_sample$Var5[i], mean = eq_y5[5])),
         value = c(rep("Var1", cl_5_v_50_sample$Var1[i]),
                  rep("Var2", cl_5_v_50_sample$Var2[i]),
                  rep("Var3", cl_5_v_50_sample$Var3[i]),
                  rep("Var4", cl_5_v_50_sample$Var4[i]),
                  rep("Var5", cl_5_v_50_sample$Var5[i])),
         type = "equadistant") ->
    eq

  tibble(x = c(rnorm(cl_5_v_50_sample$Var1[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var2[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var3[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var4[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var5[i], sd = scale)),
         y = c(rnorm(cl_5_v_50_sample$Var1[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var2[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var3[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var4[i], sd = scale),
               rnorm(cl_5_v_50_sample$Var5[i], sd = scale)),
         value = c(rep("Var1", cl_5_v_50_sample$Var1[i]),
                  rep("Var2", cl_5_v_50_sample$Var2[i]),
                  rep("Var3", cl_5_v_50_sample$Var3[i]),
                  rep("Var4", cl_5_v_50_sample$Var4[i]),
                  rep("Var5", cl_5_v_50_sample$Var5[i])),
         type = "random") ->
    rd
  
  eq %>% 
    bind_rows(cp, rd) %>% 
    mutate(id = cl_5_v_50_sample$id[i],
           vars = str_c(cl_5_v_50_sample$Var1[i], 
                        cl_5_v_50_sample$Var2[i], 
                        cl_5_v_50_sample$Var3[i], 
                        cl_5_v_50_sample$Var4[i], 
                        cl_5_v_50_sample$Var5[i], 
                        sep = "-"),
           n_category = cl_5_v_50_sample$n_category[i],
           n_villages = cl_5_v_50_sample$n_villages[i],
           H = cl_5_v_50_sample$H[i])
}) ->
  df_cl_5_v_50

# df_cl_5_v_70 ------------------------------------------------------------
n_villages <- 70
n_category <- 5
X <- as.matrix(expand.grid(1:n_villages, 
                           1:n_villages, 
                           1:n_villages, 
                           1:n_villages)) # repeat n_clusters - 1 times
X %>% 
  as_tibble() %>% 
  mutate(rs = rowSums(.)) %>% 
  filter(rs <= n_villages) %>% 
  mutate(Var5 = n_villages-rs) %>% 
  filter(Var5 > 0) %>% 
  filter(Var1 >= Var2,
         Var1 >= Var3,
         Var1 >= Var4,
         Var1 >= Var5,
         Var2 >= Var3,
         Var2 >= Var4,
         Var2 >= Var5,
         Var3 >= Var4,
         Var3 >= Var5,
         Var4 >= Var5) %>% 
  select(-rs) %>% 
  rowwise() %>% 
  mutate(H = -sum(Var1/n_villages*log2(Var1/n_villages),
                  Var2/n_villages*log2(Var2/n_villages),
                  Var3/n_villages*log2(Var3/n_villages),
                  Var4/n_villages*log2(Var4/n_villages),
                  Var5/n_villages*log2(Var5/n_villages))) %>% 
  arrange(H) %>% 
  ungroup() %>% 
  mutate(id = 1:n(),
         n_villages = n_villages,
         n_category = n_category) %>% 
  select(id, n_category, n_villages, H, Var1, Var2, Var3, Var4, Var5) %>% 
  slice(round(seq(nrow(.), 1, length.out = 70))) ->
  cl_5_v_70_sample

map_dfr(1:70, function(i){
  tibble(x = c(rnorm(cl_5_v_70_sample$Var1[i], mean = cp_x5[1]),
               rnorm(cl_5_v_70_sample$Var2[i], mean = cp_x5[2]),
               rnorm(cl_5_v_70_sample$Var3[i], mean = cp_x5[3]),
               rnorm(cl_5_v_70_sample$Var4[i], mean = cp_x5[4]),
               rnorm(cl_5_v_70_sample$Var5[i], mean = cp_x5[5])),
         y = c(rnorm(cl_5_v_70_sample$Var1[i], mean = cp_y5[1]),
               rnorm(cl_5_v_70_sample$Var2[i], mean = cp_y5[2]),
               rnorm(cl_5_v_70_sample$Var3[i], mean = cp_y5[3]),
               rnorm(cl_5_v_70_sample$Var4[i], mean = cp_y5[4]),
               rnorm(cl_5_v_70_sample$Var5[i], mean = cp_y5[5])),
         value = c(rep("Var1", cl_5_v_70_sample$Var1[i]),
                   rep("Var2", cl_5_v_70_sample$Var2[i]),
                   rep("Var3", cl_5_v_70_sample$Var3[i]),
                   rep("Var4", cl_5_v_70_sample$Var4[i]),
                   rep("Var5", cl_5_v_70_sample$Var5[i])),
         type = "central-periphery") ->
    cp
  
  tibble(x = c(rnorm(cl_5_v_70_sample$Var1[i], mean = eq_x5[1]),
               rnorm(cl_5_v_70_sample$Var2[i], mean = eq_x5[2]),
               rnorm(cl_5_v_70_sample$Var3[i], mean = eq_x5[3]),
               rnorm(cl_5_v_70_sample$Var4[i], mean = eq_x5[4]),
               rnorm(cl_5_v_70_sample$Var5[i], mean = eq_x5[5])),
         y = c(rnorm(cl_5_v_70_sample$Var1[i], mean = eq_y5[1]),
               rnorm(cl_5_v_70_sample$Var2[i], mean = eq_y5[2]),
               rnorm(cl_5_v_70_sample$Var3[i], mean = eq_y5[3]),
               rnorm(cl_5_v_70_sample$Var4[i], mean = eq_y5[4]),
               rnorm(cl_5_v_70_sample$Var5[i], mean = eq_y5[5])),
         value = c(rep("Var1", cl_5_v_70_sample$Var1[i]),
                   rep("Var2", cl_5_v_70_sample$Var2[i]),
                   rep("Var3", cl_5_v_70_sample$Var3[i]),
                   rep("Var4", cl_5_v_70_sample$Var4[i]),
                   rep("Var5", cl_5_v_70_sample$Var5[i])),
         type = "equadistant") ->
    eq
  
  tibble(x = c(rnorm(cl_5_v_70_sample$Var1[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var2[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var3[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var4[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var5[i], sd = scale)),
         y = c(rnorm(cl_5_v_70_sample$Var1[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var2[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var3[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var4[i], sd = scale),
               rnorm(cl_5_v_70_sample$Var5[i], sd = scale)),
         value = c(rep("Var1", cl_5_v_70_sample$Var1[i]),
                   rep("Var2", cl_5_v_70_sample$Var2[i]),
                   rep("Var3", cl_5_v_70_sample$Var3[i]),
                   rep("Var4", cl_5_v_70_sample$Var4[i]),
                   rep("Var5", cl_5_v_70_sample$Var5[i])),
         type = "random") ->
    rd
  
  eq %>% 
    bind_rows(cp, rd) %>% 
    mutate(id = cl_5_v_70_sample$id[i],
           vars = str_c(cl_5_v_70_sample$Var1[i], 
                        cl_5_v_70_sample$Var2[i], 
                        cl_5_v_70_sample$Var3[i], 
                        cl_5_v_70_sample$Var4[i], 
                        cl_5_v_70_sample$Var5[i], 
                        sep = "-"),
           n_category = cl_5_v_70_sample$n_category[i],
           n_villages = cl_5_v_70_sample$n_villages[i],
           H = cl_5_v_70_sample$H[i])
}) ->
  df_cl_5_v_70

# df_cl_4_v_30 ------------------------------------------------------------
n_villages <- 30
n_category <- 4
X <- as.matrix(expand.grid(1:n_villages, 
                           1:n_villages, 
                           1:n_villages)) # repeat n_clusters - 1 times
X %>% 
  as_tibble() %>% 
  mutate(rs = rowSums(.)) %>% 
  filter(rs <= n_villages) %>% 
  mutate(Var4 = n_villages-rs) %>% 
  filter(Var4 > 0) %>% 
  filter(Var1 >= Var2,
         Var1 >= Var3,
         Var1 >= Var4,
         Var2 >= Var3,
         Var2 >= Var4,
         Var3 >= Var4) %>% 
  select(-rs) %>% 
  rowwise() %>% 
  mutate(H = -sum(Var1/n_villages*log2(Var1/n_villages),
                  Var2/n_villages*log2(Var2/n_villages),
                  Var3/n_villages*log2(Var3/n_villages),
                  Var4/n_villages*log2(Var4/n_villages))) %>% 
  arrange(H) %>% 
  ungroup() %>% 
  mutate(id = 1:n(),
         n_villages = n_villages,
         n_category = n_category) %>% 
  select(id, n_category, n_villages, H, Var1, Var2, Var3, Var4) %>% 
  slice(round(seq(nrow(.), 1, length.out = 30))) ->
  cl_4_v_30_sample

scale <- 1
eq_x4 = c(1, 0, -1, 0)* scale
eq_y4 = c(0, 1, 0, -1)* scale
cp_x4 <- c(0, (2*cos(2*pi/3*0:2))) * scale
cp_y4 <- c(0, (2*sin(2*pi/3*0:3))) * scale

map_dfr(1:30, function(i){
  tibble(x = c(rnorm(cl_4_v_30_sample$Var1[i], mean = eq_x4[1]),
               rnorm(cl_4_v_30_sample$Var2[i], mean = eq_x4[2]),
               rnorm(cl_4_v_30_sample$Var3[i], mean = eq_x4[3]),
               rnorm(cl_4_v_30_sample$Var4[i], mean = eq_x4[4])),
         y = c(rnorm(cl_4_v_30_sample$Var1[i], mean = eq_y4[1]),
               rnorm(cl_4_v_30_sample$Var2[i], mean = eq_y4[2]),
               rnorm(cl_4_v_30_sample$Var3[i], mean = eq_y4[3]),
               rnorm(cl_4_v_30_sample$Var4[i], mean = eq_y4[4])),
         value = c(rep("Var1", cl_4_v_30_sample$Var1[i]),
                   rep("Var2", cl_4_v_30_sample$Var2[i]),
                   rep("Var3", cl_4_v_30_sample$Var3[i]),
                   rep("Var4", cl_4_v_30_sample$Var4[i])),
         type = "equadistant") ->
    eq
  
  tibble(x = c(rnorm(cl_4_v_30_sample$Var1[i], mean = cp_x4[1]),
               rnorm(cl_4_v_30_sample$Var2[i], mean = cp_x4[2]),
               rnorm(cl_4_v_30_sample$Var3[i], mean = cp_x4[3]),
               rnorm(cl_4_v_30_sample$Var4[i], mean = cp_x4[4])),
         y = c(rnorm(cl_4_v_30_sample$Var1[i], mean = cp_y4[1]),
               rnorm(cl_4_v_30_sample$Var2[i], mean = cp_y4[2]),
               rnorm(cl_4_v_30_sample$Var3[i], mean = cp_y4[3]),
               rnorm(cl_4_v_30_sample$Var4[i], mean = cp_y4[4])),
         value = c(rep("Var1", cl_4_v_30_sample$Var1[i]),
                   rep("Var2", cl_4_v_30_sample$Var2[i]),
                   rep("Var3", cl_4_v_30_sample$Var3[i]),
                   rep("Var4", cl_4_v_30_sample$Var4[i])),
         type = "central-periphery") ->
    cp
  
  tibble(x = c(rnorm(cl_4_v_30_sample$Var1[i], sd = scale),
               rnorm(cl_4_v_30_sample$Var2[i], sd = scale),
               rnorm(cl_4_v_30_sample$Var3[i], sd = scale),
               rnorm(cl_4_v_30_sample$Var4[i], sd = scale)),
         y = c(rnorm(cl_4_v_30_sample$Var1[i], sd = scale),
               rnorm(cl_4_v_30_sample$Var2[i], sd = scale),
               rnorm(cl_4_v_30_sample$Var3[i], sd = scale),
               rnorm(cl_4_v_30_sample$Var4[i], sd = scale)),
         value = c(rep("Var1", cl_4_v_30_sample$Var1[i]),
                   rep("Var2", cl_4_v_30_sample$Var2[i]),
                   rep("Var3", cl_4_v_30_sample$Var3[i]),
                   rep("Var4", cl_4_v_30_sample$Var4[i])),
         type = "random") ->
    rd
  
  eq %>% 
    bind_rows(cp, rd) %>% 
    mutate(id = cl_4_v_30_sample$id[i],
           vars = str_c(cl_4_v_30_sample$Var1[i], 
                        cl_4_v_30_sample$Var2[i], 
                        cl_4_v_30_sample$Var3[i], 
                        cl_4_v_30_sample$Var4[i], 
                        sep = "-"),
           n_category = cl_4_v_30_sample$n_category[i],
           n_villages = cl_4_v_30_sample$n_villages[i],
           H = cl_4_v_30_sample$H[i])
}) ->
  df_cl_4_v_30


# df_cl_4_v_50 ------------------------------------------------------------
n_villages <- 50
n_category <- 4
X <- as.matrix(expand.grid(1:n_villages, 
                           1:n_villages, 
                           1:n_villages)) # repeat n_clusters - 1 times
X %>% 
  as_tibble() %>% 
  mutate(rs = rowSums(.)) %>% 
  filter(rs <= n_villages) %>% 
  mutate(Var4 = n_villages-rs) %>% 
  filter(Var4 > 0) %>% 
  filter(Var1 >= Var2,
         Var1 >= Var3,
         Var1 >= Var4,
         Var2 >= Var3,
         Var2 >= Var4,
         Var3 >= Var4) %>% 
  select(-rs) %>% 
  rowwise() %>% 
  mutate(H = -sum(Var1/n_villages*log2(Var1/n_villages),
                  Var2/n_villages*log2(Var2/n_villages),
                  Var3/n_villages*log2(Var3/n_villages),
                  Var4/n_villages*log2(Var4/n_villages))) %>% 
  arrange(H) %>% 
  ungroup() %>% 
  mutate(id = 1:n(),
         n_villages = n_villages,
         n_category = n_category) %>% 
  select(id, n_category, n_villages, H, Var1, Var2, Var3, Var4) %>% 
  slice(round(seq(nrow(.), 1, length.out = 30))) ->
  cl_4_v_50_sample

map_dfr(1:30, function(i){
  tibble(x = c(rnorm(cl_4_v_50_sample$Var1[i], mean = eq_x4[1]),
               rnorm(cl_4_v_50_sample$Var2[i], mean = eq_x4[2]),
               rnorm(cl_4_v_50_sample$Var3[i], mean = eq_x4[3]),
               rnorm(cl_4_v_50_sample$Var4[i], mean = eq_x4[4])),
         y = c(rnorm(cl_4_v_50_sample$Var1[i], mean = eq_y4[1]),
               rnorm(cl_4_v_50_sample$Var2[i], mean = eq_y4[2]),
               rnorm(cl_4_v_50_sample$Var3[i], mean = eq_y4[3]),
               rnorm(cl_4_v_50_sample$Var4[i], mean = eq_y4[4])),
         value = c(rep("Var1", cl_4_v_50_sample$Var1[i]),
                   rep("Var2", cl_4_v_50_sample$Var2[i]),
                   rep("Var3", cl_4_v_50_sample$Var3[i]),
                   rep("Var4", cl_4_v_50_sample$Var4[i])),
         type = "equadistant") ->
    eq
  
  tibble(x = c(rnorm(cl_4_v_50_sample$Var1[i], mean = cp_x4[1]),
               rnorm(cl_4_v_50_sample$Var2[i], mean = cp_x4[2]),
               rnorm(cl_4_v_50_sample$Var3[i], mean = cp_x4[3]),
               rnorm(cl_4_v_50_sample$Var4[i], mean = cp_x4[4])),
         y = c(rnorm(cl_4_v_50_sample$Var1[i], mean = cp_y4[1]),
               rnorm(cl_4_v_50_sample$Var2[i], mean = cp_y4[2]),
               rnorm(cl_4_v_50_sample$Var3[i], mean = cp_y4[3]),
               rnorm(cl_4_v_50_sample$Var4[i], mean = cp_y4[4])),
         value = c(rep("Var1", cl_4_v_50_sample$Var1[i]),
                   rep("Var2", cl_4_v_50_sample$Var2[i]),
                   rep("Var3", cl_4_v_50_sample$Var3[i]),
                   rep("Var4", cl_4_v_50_sample$Var4[i])),
         type = "central-periphery") ->
    cp
  
  tibble(x = c(rnorm(cl_4_v_50_sample$Var1[i], sd = scale),
               rnorm(cl_4_v_50_sample$Var2[i], sd = scale),
               rnorm(cl_4_v_50_sample$Var3[i], sd = scale),
               rnorm(cl_4_v_50_sample$Var4[i], sd = scale)),
         y = c(rnorm(cl_4_v_50_sample$Var1[i], sd = scale),
               rnorm(cl_4_v_50_sample$Var2[i], sd = scale),
               rnorm(cl_4_v_50_sample$Var3[i], sd = scale),
               rnorm(cl_4_v_50_sample$Var4[i], sd = scale)),
         value = c(rep("Var1", cl_4_v_50_sample$Var1[i]),
                   rep("Var2", cl_4_v_50_sample$Var2[i]),
                   rep("Var3", cl_4_v_50_sample$Var3[i]),
                   rep("Var4", cl_4_v_50_sample$Var4[i])),
         type = "random") ->
    rd
  
  eq %>% 
    bind_rows(cp, rd) %>% 
    mutate(id = cl_4_v_50_sample$id[i],
           vars = str_c(cl_4_v_50_sample$Var1[i], 
                        cl_4_v_50_sample$Var2[i], 
                        cl_4_v_50_sample$Var3[i], 
                        cl_4_v_50_sample$Var4[i], 
                        sep = "-"),
           n_category = cl_4_v_50_sample$n_category[i],
           n_villages = cl_4_v_50_sample$n_villages[i],
           H = cl_4_v_50_sample$H[i])
}) ->
  df_cl_4_v_50

theme_set(theme_bw())

# df_cl_4_v_70 ------------------------------------------------------------
n_villages <- 70
n_category <- 4
X <- as.matrix(expand.grid(1:n_villages, 
                           1:n_villages, 
                           1:n_villages)) # repeat n_clusters - 1 times
X %>% 
  as_tibble() %>% 
  mutate(rs = rowSums(.)) %>% 
  filter(rs <= n_villages) %>% 
  mutate(Var4 = n_villages-rs) %>% 
  filter(Var4 > 0) %>% 
  filter(Var1 >= Var2,
         Var1 >= Var3,
         Var1 >= Var4,
         Var2 >= Var3,
         Var2 >= Var4,
         Var3 >= Var4) %>% 
  select(-rs) %>% 
  rowwise() %>% 
  mutate(H = -sum(Var1/n_villages*log2(Var1/n_villages),
                  Var2/n_villages*log2(Var2/n_villages),
                  Var3/n_villages*log2(Var3/n_villages),
                  Var4/n_villages*log2(Var4/n_villages))) %>% 
  arrange(H) %>% 
  ungroup() %>% 
  mutate(id = 1:n(),
         n_villages = n_villages,
         n_category = n_category) %>% 
  select(id, n_category, n_villages, H, Var1, Var2, Var3, Var4) %>% 
  slice(round(seq(nrow(.), 1, length.out = 70))) ->
  cl_4_v_70_sample

map_dfr(1:70, function(i){
  tibble(x = c(rnorm(cl_4_v_70_sample$Var1[i], mean = eq_x4[1]),
               rnorm(cl_4_v_70_sample$Var2[i], mean = eq_x4[2]),
               rnorm(cl_4_v_70_sample$Var3[i], mean = eq_x4[3]),
               rnorm(cl_4_v_70_sample$Var4[i], mean = eq_x4[4])),
         y = c(rnorm(cl_4_v_70_sample$Var1[i], mean = eq_y4[1]),
               rnorm(cl_4_v_70_sample$Var2[i], mean = eq_y4[2]),
               rnorm(cl_4_v_70_sample$Var3[i], mean = eq_y4[3]),
               rnorm(cl_4_v_70_sample$Var4[i], mean = eq_y4[4])),
         value = c(rep("Var1", cl_4_v_70_sample$Var1[i]),
                   rep("Var2", cl_4_v_70_sample$Var2[i]),
                   rep("Var3", cl_4_v_70_sample$Var3[i]),
                   rep("Var4", cl_4_v_70_sample$Var4[i])),
         type = "equadistant") ->
    eq
  
  tibble(x = c(rnorm(cl_4_v_70_sample$Var1[i], mean = cp_x4[1]),
               rnorm(cl_4_v_70_sample$Var2[i], mean = cp_x4[2]),
               rnorm(cl_4_v_70_sample$Var3[i], mean = cp_x4[3]),
               rnorm(cl_4_v_70_sample$Var4[i], mean = cp_x4[4])),
         y = c(rnorm(cl_4_v_70_sample$Var1[i], mean = cp_y4[1]),
               rnorm(cl_4_v_70_sample$Var2[i], mean = cp_y4[2]),
               rnorm(cl_4_v_70_sample$Var3[i], mean = cp_y4[3]),
               rnorm(cl_4_v_70_sample$Var4[i], mean = cp_y4[4])),
         value = c(rep("Var1", cl_4_v_70_sample$Var1[i]),
                   rep("Var2", cl_4_v_70_sample$Var2[i]),
                   rep("Var3", cl_4_v_70_sample$Var3[i]),
                   rep("Var4", cl_4_v_70_sample$Var4[i])),
         type = "central-periphery") ->
    cp
  
  tibble(x = c(rnorm(cl_4_v_70_sample$Var1[i], sd = scale),
               rnorm(cl_4_v_70_sample$Var2[i], sd = scale),
               rnorm(cl_4_v_70_sample$Var3[i], sd = scale),
               rnorm(cl_4_v_70_sample$Var4[i], sd = scale)),
         y = c(rnorm(cl_4_v_70_sample$Var1[i], sd = scale),
               rnorm(cl_4_v_70_sample$Var2[i], sd = scale),
               rnorm(cl_4_v_70_sample$Var3[i], sd = scale),
               rnorm(cl_4_v_70_sample$Var4[i], sd = scale)),
         value = c(rep("Var1", cl_4_v_70_sample$Var1[i]),
                   rep("Var2", cl_4_v_70_sample$Var2[i]),
                   rep("Var3", cl_4_v_70_sample$Var3[i]),
                   rep("Var4", cl_4_v_70_sample$Var4[i])),
         type = "random") ->
    rd
  
  eq %>% 
    bind_rows(cp, rd) %>% 
    mutate(id = cl_4_v_70_sample$id[i],
           vars = str_c(cl_4_v_70_sample$Var1[i], 
                        cl_4_v_70_sample$Var2[i], 
                        cl_4_v_70_sample$Var3[i], 
                        cl_4_v_70_sample$Var4[i], 
                        sep = "-"),
           n_category = cl_4_v_70_sample$n_category[i],
           n_villages = cl_4_v_70_sample$n_villages[i],
           H = cl_4_v_70_sample$H[i])
}) ->
  df_cl_4_v_70

# merge all  --------------------------------------------------------------
df_cl_5_v_70 %>% 
  bind_rows(df_cl_5_v_50, df_cl_5_v_30, 
            df_cl_4_v_70, df_cl_4_v_50, df_cl_4_v_30) ->
  df_cl_5_4

df_cl_5_4 %>% 
  count(n_villages, type, n_category)


df_cl_5_4 %>% 
  filter(id > 9000, n_category == 5) %>% 
  ggplot(aes(x, y, color = value, shape = value))+
  geom_point(size = 3)+
  stat_ellipse()+
  facet_grid(type~round(H, 2))
