library(tidyverse)

generate_equadistant <- function(N = 5, 
                                 n = 1000, 
                                 r,
                                 central_distance = function(r){log(r)},
                                 neighbour_distance = function(r, N){2*r*sin(pi/N)},
                                 center = FALSE){
  
  if(center){
    N <- N-1
  }
  angle <- 2*pi/N
  lapply(1:N, function(k){
    V1 <- (2-rlnorm(n = n, meanlog = 0, sdlog = central_distance(r)))*r
    V2 <- rnorm(n = n, mean = 0, sd = neighbour_distance(r, N))
    while(sum(V1 > r*1.5 | V1 < -r*1.5) > 0){
      V1 <- ifelse(V1 > r*1.5 | V1 < -r*1.5, 
                   (2-rlnorm(n = n, meanlog = 0, sdlog = 1))*r, 
                   V1)  
    }
    df <- data.frame(V1, V2)
    df$V1 <- df$V1+r
    df$x <- df$V1*cos(angle*k)-df$V2*sin(angle*k)
    df$y <- df$V1*sin(angle*k)+df$V2*cos(angle*k)
    return(df[,3:4])
  }) %>% 
    do.call(rbind, .) %>% 
    as.data.frame() %>% 
    mutate(id = rep(1:N, each = n)) ->
    results
  
  if(center){
    data.frame(x = rnorm(n = n, mean = 0, sd = r*0.75),
               y = rnorm(n = n, mean = 0, sd = r*0.75),
               id = N + 1) %>% 
      bind_rows(results) ->
      results
  }
  results %>% 
    mutate(id = factor(id)) %>% 
    return()
}

set.seed(42)
generate_equadistant(N = 24, n = 100, r = 26, 
                     central_distance = function(x){log(x)/5},
                     neighbour_distance = function(r, N){0.6*r*sin(pi/N)},                    
                     center = TRUE) %>% 
  ggplot(aes(x, y, color = id))+
  stat_ellipse()+
  theme_bw()
