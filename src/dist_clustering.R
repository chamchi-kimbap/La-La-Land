rm(list = ls())
gc(reset = T)

library(statip)
load('db_seoul_not_scale.RData')

vT = db_seoul_not_scale$avg_temp

vF = vT*9/5+32

vV = db_seoul_not_scale$avg_wind

db_seoul_not_scale$sens_temp = 13.12 + 0.6215*vT - 11.37*(vV^0.16) + 
  0.3965*(vV^0.16)*vT

Kmeans_Thi_Dist_Summer = function(k = 3){
  
  gun = sort(unique(db_seoul_not_scale$code_gun))
  
  #### Set initial centers of clusters
  init_center_gun = unique(db_seoul_not_scale$code_gun)[sample(25, k)]
  
  thi_summer = db_seoul_not_scale %>% 
    filter(month %in% c('06', '07', '08')) %>% 
    split(.$code_gun) %>% 
    map(function(x) round(x$thi, 2))
  
  dist_thi = tibble() 
  
  #### Compute hellinger distance between center and others
  for(i in 1:k) {
    dist_thi = lapply(thi_summer, function(x){
      tryCatch(hellinger(na.omit(x), na.omit(thi_summer[[init_center_gun[i]]]), lower=0, upper=100), error = function(e) 1)}) %>% 
      unlist() %>% 
      as_tibble() %>% 
      bind_cols(dist_thi)
  }
  
  #### Include data in nearsest cluster
  cluster = apply(dist_thi, 1, function(x) which.min(x))
  
  center = rep(1, k)
  
  #### Compute new centers of clusters
  iter = 1
  
  while(iter <= 100){
    
    for(j in 1:k){
      
      dist_between_center = tibble() 
      
      for(l in 1:table(cluster)[j]){
        dist_between_center = lapply(thi_summer[gun[cluster == j]], function(x){
          tryCatch(hellinger(na.omit(x), na.omit(thi_summer[gun[cluster == j]][[l]]), lower=0, upper=100), error = function(e) 1)}) %>% 
          unlist() %>% 
          as_tibble() %>% 
          bind_cols(dist_between_center)
      }
      
      dist_center = rowSums(dist_between_center)
      
      center[j] = gun[cluster == j][which.min(dist_center)]
    }
    
    dist_thi_new = tibble()
    
    for(n in 1:k) {
      
      dist_thi_new = lapply(thi_summer, function(x){
        tryCatch(hellinger(na.omit(x), na.omit(thi_summer[[center[n]]]), lower=0, upper=100), error = function(e) 1)}) %>% 
        unlist() %>% 
        as_tibble() %>% 
        bind_cols(dist_thi_new)
    }
    
    cluster = apply(dist_thi_new, 1, function(x) which.min(x))
    
    iter = iter + 1
  }
  return(list(center = center, cluster = cluster))
}

Kmeans_ST_Dist_Winter = function(k = 3){
  
  gun = sort(unique(db_seoul_not_scale$code_gun))
  
  #### Set initial centers of clusters
  init_center_gun = unique(db_seoul_not_scale$code_gun)[sample(25, k)]
  
  st_winter = db_seoul_not_scale %>% 
    filter(month %in% c('12', '01', '02')) %>% 
    split(.$code_gun) %>% 
    map(function(x) round(x$sens_temp, 2))
  
  dist_thi = tibble() 
  
  #### Compute hellinger distance between center and others
  for(i in 1:k) {
    dist_thi = lapply(st_winter, function(x){
      tryCatch(hellinger(na.omit(x), na.omit(st_winter[[init_center_gun[i]]]), lower=0, upper=100), error = function(e) 1)}) %>% 
      unlist() %>% 
      as_tibble() %>% 
      bind_cols(dist_thi)
  }
  
  #### Include data in nearsest cluster
  cluster = apply(dist_thi, 1, function(x) which.min(x))
  
  center = rep(1, k)
  
  #### Compute new centers of clusters
  iter = 1
  
  while(iter <= 100){
    
    for(j in 1:k){
      
      dist_between_center = tibble() 
      
      for(l in 1:table(cluster)[j]){
        dist_between_center = lapply(st_winter[gun[cluster == j]], function(x){
          tryCatch(hellinger(na.omit(x), na.omit(st_winter[gun[cluster == j]][[l]]), lower=0, upper=100), error = function(e) 1)}) %>% 
          unlist() %>% 
          as_tibble() %>% 
          bind_cols(dist_between_center)
      }
      
      dist_center = rowSums(dist_between_center)
      
      center[j] = gun[cluster == j][which.min(dist_center)]
    }
    
    dist_thi_new = tibble()
    
    for(n in 1:k) {
      
      dist_thi_new = lapply(st_winter, function(x){
        tryCatch(hellinger(na.omit(x), na.omit(st_winter[[center[n]]]), lower=0, upper=100), error = function(e) 1)}) %>% 
        unlist() %>% 
        as_tibble() %>% 
        bind_cols(dist_thi_new)
    }
    
    cluster = apply(dist_thi_new, 1, function(x) which.min(x))
    
    iter = iter + 1
  }
  return(list(center = center, cluster = cluster))
}

km_winter = Kmeans_ST_Dist_Winter()
