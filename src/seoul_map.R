if(!require(devtools)){install.packages("devtools")}; require(devtools)
devtools::install_github("dkahle/ggmap", ref="tidyup");require(ggmap)   # 2.7ver.
if(!require(googleway)){install.packages("googleway")}; require(googleway)
if(!require(ggplot2)){install.packages("ggplot2")}; require(ggplot2)
if(!require(raster)){install.packages("raster")}; require(raster)   # map_update
if(!require(viridis)){install.packages("viridis")};require(viridis)   # map_update
if(!require(dplyr)){install.packages("dplyr")}; require(dplyr)
if(!require(rgdal)){install.packages("rgdal")}; require(rgdal)

api_key <-""
register_google(key = api_key); has_google_key()   # check api_key

# 시군 구분 행정지도 가져오기
korea_new <- shapefile('shp/SIG_201804/TL_SCCO_SIG.shp')
seoul_map <- subset(korea_new, as.integer(korea_new$SIG_CD)%/%1000 == 11)

colnames(cluster_thi_summer) = c("SIG_KOR_NM","count")
colnames(cluster_st_winter) = c("SIG_KOR_NM","count")

cluster_thi_summer$SIG_KOR_NM = as.character(cluster_thi_summer$SIG_KOR_NM)
cluster_thi_summer$count =as.numeric(cluster_thi_summer$count)

cluster_st_winter$SIG_KOR_NM = as.character(cluster_st_winter$SIG_KOR_NM)
cluster_st_winter$count = as.numeric(cluster_st_winter$count)


seoul_admin <- merge(seoul_map, cluster_st_winter, by="SIG_KOR_NM")
x <- seoul_admin$count

par(mfrow=c(1,1), mar=c(0.05,0.15,0.05,0.15))
plot(seoul_map, col=c("maroon2","navyblue","powderblue")[x])
text(coordinates(seoul_map), seoul_admin$SIG_KOR_NM, cex=1, col = "white")
