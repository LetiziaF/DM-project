library(dplyr)
library(plyr)
library(sp)
library(TSP)

setwd("C:/Users/aless/Desktop/Data Science/Decision model/Progetto Santa Travelling/Progetto")

#dataset
cities <- read.csv("santa_cities.csv")

#Scelta del numero di cluster 
tot_cluster <- 50

#K-means per la divisione dei nodi in cluster
set.seed(123)
clu <- kmeans(x = as.matrix(cities[,-1]), centers = tot_cluster, iter.max = 500)
max_size <- max(clu$size)
cluster <- cities
cluster$ncluster <- clu$cluster

write.csv(cluster, "10cluster_kmeans.csv")