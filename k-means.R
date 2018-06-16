library(dplyr)
library(plyr)
library(sp)
library(TSP)

setwd("C:/Users/aless/Desktop/Data Science/Decision model/Progetto Santa Travelling/Progetto")
cities <- read.csv("santa_cities.csv")
rapp <- c()
for(i in 50:250){
  print(paste0(i,"/",200))
  set.seed(123)
  clu <- kmeans(x = as.matrix(cities[,-1]), centers = i, iter.max = 500)
  rapp1 <- (clu$betweenss)/(clu$totss)
  rapp <- rbind(rapp, rapp1)
}

rap <- data.frame(rapp)
rap$ncluster <- 50:250

tot_cluster <- rap[rap$rapp == max(rap$rapp),"ncluster"]
tot_cluster <- 50
set.seed(123)
clu <- kmeans(x = as.matrix(cities[,-1]), centers = tot_cluster, iter.max = 500)
max_size <- max(clu$size)
cluster <- cities
cluster$ncluster <- clu$cluster
#esporta
write.csv(cluster, "10cluster_kmeans.csv")
