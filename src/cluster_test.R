rm(list= ls())
gc(reset = TRUE)

# setwd('C:\\Users\\UOS\\Desktop\\weather\\data')

library(tidyverse)
library(data.table)

load('lala_store.RData')

load('cluster_thi_summer.RData')
load('cluster_st_winter.RData')

db_lala = fread('db_lala.csv', encoding = 'UTF-8') %>% as_tibble()

db_lala$lala_sale_date = as.character(db_lala$lala_sale_date)

db_lala = db_lala %>% mutate(month = substr(lala_sale_date, 5, 6))

db_lala_sum_summer = db_lala %>% filter(month %in% c('06', '07', '08')) %>% group_by(lala_gun, lala_category) %>% summarise(sum_qty = sum(lala_qty)) %>% ungroup()

db_lala_sum_summer = db_lala_sum_summer %>% left_join(lala_store, by = c('lala_gun' = 'code_gun'))

db_lala_sum_summer = db_lala_sum_summer %>% mutate(qty_store_count = sum_qty/store_count)

db_lala_sum_summer = db_lala_sum_summer %>% left_join(cluster_thi_summer, by = c('lala_gun' = 'code_gun'))

db_lala_summer_category_thi_cluster = db_lala_sum_summer %>% 
        filter(!is.na(cluster)) %>% 
        group_by(cluster, lala_category) %>% 
        summarise(sum_qsc = mean(qty_store_count, na.rm = T)) %>%
        ungroup()

db_lala_for_aov = db_lala %>% left_join(lala_store, by = c('lala_gun' = 'code_gun')) %>% left_join(cluster_thi_summer, by = c('lala_gun' = 'code_gun'))

db_lala_for_aov = db_lala_for_aov %>% mutate(lala_qty_sc = lala_qty/store_count)

db_lala_for_aov_summer = db_lala_for_aov %>% filter(month %in% c('06', '07', '08')) %>% group_by(lala_gun, lala_sale_date, lala_category, cluster) %>% summarise(lala_sum_qty_sc = sum(lala_qty_sc)) %>% ungroup()

#### 구별 판매량
db_lala_aov_spread = db_lala_for_aov_summer %>% spread(lala_category, lala_sum_qty_sc)

oneway.anova = function(x, data) {oneway.test(x ~ cluster, data)$p.value}

db_lala_aov_spread %>% summarise_at(colnames(.)[4:13], funs(oneway.anova(., data = db_lala_aov_spread)))

#### 클러스터별 판매량 검정
db_lala_cluster_aov = db_lala_for_aov_summer %>% group_by(cluster, lala_sale_date, lala_category) %>% summarise(cluster_qty = mean(lala_sum_qty_sc)) %>% spread(lala_category, cluster_qty)

#### 클러스터별 판매량
oneway.test(네일 ~ cluster, data = db_lala_cluster_aov)$p.value # 기각 못함
oneway.test(립컬러 ~ cluster, data = db_lala_cluster_aov)$p.value # 기각
oneway.test(마스크팩 ~ cluster, data = db_lala_cluster_aov)$p.value # 기각 못함
oneway.test(바디로션 ~ cluster, data = db_lala_cluster_aov)$p.value # 기각
oneway.test(선케어 ~ cluster, data = db_lala_cluster_aov)$p.value # 기각 or 기각 못함
oneway.test(제모제 ~ cluster, data = db_lala_cluster_aov)$p.value # 기각 못함
oneway.test(체중조절 ~ cluster, data = db_lala_cluster_aov)$p.value # 기각 못함
oneway.test(크림로션 ~ cluster, data = db_lala_cluster_aov)$p.value # 기각
oneway.test(훼이셜클렌저 ~ cluster, data = db_lala_cluster_aov)$p.value # 기각

# TukeyHSD 

lip = aov(립컬러 ~ cluster, data = db_lala_cluster_aov)
plot(TukeyHSD(lip))

body = aov(바디로션 ~ cluster, data = db_lala_cluster_aov)
plot(TukeyHSD(body))

sun = aov(선케어 ~ cluster, data = db_lala_cluster_aov)
plot(TukeyHSD(sun))

cream = aov(크림로션 ~ cluster, data = db_lala_cluster_aov)
plot(TukeyHSD(cream))

facial = aov(훼이셜클렌저 ~ cluster, data = db_lala_cluster_aov)
plot(TukeyHSD(facial))

db_lala_cluster_aov %>% summarise_at(colnames(.)[3:12], funs(oneway.test(. ~ cluster, data = db_lala_cluster_aov)))

db_category_cluster %>% spread(lala_category, mean_qty_cluster) %>% apply(., 2, function(x) which.max(x))

db_lala_sum_summer_thi_cluster %>% group_by(lala_category, cluster) %>% summarise(mean(qty_over_pd, na.rm = T)) %>% View()

db_lala_sum_summer_thi_cluster %>% group_by(lala_gun, lala_category) %>% summarise(mean(qty_over_pd, na.rm = T)) %>% View()

# winter

db_lala_sum_winter = db_lala %>% filter(month %in% c('12', '01', '02')) %>% group_by(lala_gun, lala_category) %>% summarise(sum_qty = sum(lala_qty)) %>% ungroup()

db_lala_sum_winter = db_lala_sum_winter %>% left_join(lala_store, by = c('lala_gun' = 'code_gun'))

db_lala_sum_winter = db_lala_sum_winter %>% mutate(qty_store_count = sum_qty/store_count)

db_lala_sum_winter = db_lala_sum_winter %>% left_join(cluster_st_winter, by = c('lala_gun' = 'code_gun'))

db_lala_winter_category_st_cluster = db_lala_sum_winter %>% 
        filter(!is.na(cluster)) %>% 
        group_by(cluster, lala_category) %>% 
        summarise(sum_qsc = mean(qty_store_count, na.rm = T)) %>%
        ungroup()

db_lala_for_aov = db_lala %>% left_join(lala_store, by = c('lala_gun' = 'code_gun')) %>% left_join(cluster_st_winter, by = c('lala_gun' = 'code_gun'))

db_lala_for_aov = db_lala_for_aov %>% mutate(lala_qty_sc = lala_qty/store_count)

db_lala_for_aov_winter = db_lala_for_aov %>% filter(month %in% c('12', '01', '02')) %>% group_by(lala_gun, lala_sale_date, lala_category, cluster) %>% summarise(lala_sum_qty_sc = sum(lala_qty_sc)) %>% ungroup()

# 구별 판매량

db_lala_aov_spread = db_lala_for_aov_winter %>% spread(lala_category, lala_sum_qty_sc)

oneway.anova = function(x, data) {oneway.test(x ~ cluster, data)$p.value}

db_lala_aov_spread %>% summarise_at(colnames(.)[4:13], funs(oneway.anova(., data = db_lala_aov_spread)))

# 클러스터별 판매량

db_lala_cluster_aov = db_lala_for_aov_winter %>% group_by(cluster, lala_sale_date, lala_category) %>% summarise(cluster_qty = mean(lala_sum_qty_sc)) %>% spread(lala_category, cluster_qty)


####클러스터별 판매량

oneway.test(네일 ~ cluster, data = db_lala_cluster_aov)$p.value 
oneway.test(립컬러 ~ cluster, data = db_lala_cluster_aov)$p.value 
oneway.test(마스크팩 ~ cluster, data = db_lala_cluster_aov)$p.value 
oneway.test(바디로션 ~ cluster, data = db_lala_cluster_aov)$p.value 
oneway.test(선케어 ~ cluster, data = db_lala_cluster_aov)$p.value 
oneway.test(제모제 ~ cluster, data = db_lala_cluster_aov)$p.value 
oneway.test(체중조절 ~ cluster, data = db_lala_cluster_aov)$p.value 
oneway.test(크림로션 ~ cluster, data = db_lala_cluster_aov)$p.value 
oneway.test(훼이셜클렌저 ~ cluster, data = db_lala_cluster_aov)$p.value 