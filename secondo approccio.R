library(GA)
library(dplyr)
library(ggmap)
library(ggplot2)
library(TSP)

setwd("C:/Users/aless/Desktop/Data Science/Decision model/Progetto Santa Travelling/Progetto")
cities <- read.csv("santa_cities.csv")

set.seed(123)
cluster <- kmeans(x = as.matrix(cities[,-1]), centers = 50, iter.max = 100)

cities$ncluster <- cluster$cluster

centroidi <- data.frame(cluster$centers)

centroidi$ncluster <- row.names(centroidi)
centroidi2 <- centroidi

tourLength <- function(tour, distMatrix) {
  route <- embed(tour, 2)[, 2:1]
  sum(distMatrix[route])
}
tpsFitness <- function(tour, ...) {
  1/tourLength(tour, ...)
}

distanza <- as.matrix(dist(select(centroidi, x, y), diag = TRUE))
set.seed(123)
GAfit <- ga(type = "permutation", 
            fitness = tpsFitness, 
            distMatrix = distanza, 
            lower = 1, 
            upper = nrow(centroidi),
            maxiter = 10000,
            popSize = 1000,
            keepBest = TRUE)
solution <- GAfit@solution[1,]
solution <- data.frame(solution)
colnames(solution) <- "ncluster"
solution$pos <- 1:nrow(solution)

solution <- merge(solution,centroidi, by = "ncluster")
solution <- arrange(solution,pos)
sol <- solution[,-c(3,4)]

#1.cerco primo centroide e punto di inizio nel primo cluster
centroidi$flag <- ifelse(centroidi$ncluster == "36","red","black")
plot(centroidi$x, centroidi$y, col = centroidi$flag)

trova_distanza <- data.frame(cities[cities$ncluster == 42,"id"])
colnames(trova_distanza) <- "id"
trova_distanza$cenx <- centroidi[centroidi$ncluster == 42,"x"]
trova_distanza$ceny <- centroidi[centroidi$ncluster == 42,"y"]
trova_distanza <- merge(trova_distanza, cities, by = "id")
trova_distanza$dist <- sqrt((trova_distanza$cenx - trova_distanza$x)^2 + 
                              (trova_distanza$ceny - trova_distanza$y)^2)
start <- trova_distanza[trova_distanza$dist == max(trova_distanza$dist),"id"]

#1.percorso
risultato <- data.frame()
conta <- 1
for(i in sol$ncluster){
  print(paste0(conta, "/", nrow(sol)))
  clui <- cities[cities$ncluster == i,c("id","x","y")]
  clui$nrow <- 1:nrow(clui)
  row.names(clui) <- clui$nrow
  if(i == 42){
    start <- clui[clui$id == start,"nrow"]
  }
  if(i != 42){
    trova_distanza <- data.frame(cities[cities$ncluster == i,c("id","x","y")])
    trova_distanza$pfinale <- pfinale$id
    trova_distanza$px <- pfinale$x
    trova_distanza$py <- pfinale$y
    trova_distanza$dist <- sqrt((trova_distanza$px - trova_distanza$x)^2 + 
                                  (trova_distanza$py - trova_distanza$y)^2)
    start <- trova_distanza[trova_distanza$dist == min(trova_distanza$dist),"id"]
    start <- clui[clui$id == start, "nrow"]
  }
  dis <- as.matrix(dist(select(clui, x, y), diag = TRUE))
  tsp <- TSP(as.dist(dis))
  set.seed(123)
  tour <- solve_TSP(tsp, method = "nn", start = start)
  tour <- solve_TSP(tsp, method = "2-opt", control = list(tour = tour))
  tour <- data.frame(tour)
  colnames(tour) <- "nrow"
  tour$pos <- 1:nrow(tour)
  tour <- merge(tour,clui, by = "nrow")
  tour <- arrange(tour,pos)
  risultato <- rbind(risultato, data.frame(tour$id))
  pfinale <- tour[tour$pos == max(tour$pos),c("id","x","y")]
  conta <- conta + 1
}

#2.cerco primo centroide e punto di inizio primo cluster
centroidi2$flag <- ifelse(centroidi2$ncluster == "36","red","black")
plot(centroidi2$x, centroidi2$y, col = centroidi2$flag)

trova_distanza <- data.frame(cities[cities$ncluster == 8,"id"])
colnames(trova_distanza) <- "id"
trova_distanza$cenx <- centroidi[centroidi$ncluster == 8,"x"]
trova_distanza$ceny <- centroidi[centroidi$ncluster == 8,"y"]
trova_distanza <- merge(trova_distanza, cities, by = "id")
trova_distanza$dist <- sqrt((trova_distanza$cenx - trova_distanza$x)^2 + 
                              (trova_distanza$ceny - trova_distanza$y)^2)
start <- trova_distanza[trova_distanza$dist == max(trova_distanza$dist),"id"]

#2.percorso
risultato2 <- data.frame()
sol_inv <- rev(sol$ncluster)
conta <- 1
for(i in sol_inv){
  print(paste0(conta, "/", nrow(sol)))
  clui <- cities[cities$ncluster == i,c("id","x","y")]
  clui$nrow <- 1:nrow(clui)
  row.names(clui) <- clui$nrow
  if(i == 8){
    start <- clui[clui$id == start,"nrow"]
  }
  if(i != 8){
    trova_distanza <- data.frame(cities[cities$ncluster == i,c("id","x","y")])
    trova_distanza$pfinale <- pfinale$id
    trova_distanza$px <- pfinale$x
    trova_distanza$py <- pfinale$y
    trova_distanza$dist <- sqrt((trova_distanza$px - trova_distanza$x)^2 + 
                                  (trova_distanza$py - trova_distanza$y)^2)
    start <- trova_distanza[trova_distanza$dist == min(trova_distanza$dist),"id"]
    start <- clui[clui$id == start, "nrow"]
  }
  dis <- as.matrix(dist(select(clui, x, y), diag = TRUE))
  
  ###############################
  #setta distanza
  controllo <- risultato
  controllo$pos <- 1:nrow(controllo)
  controllo <- controllo[which(controllo$id %in% clui$id),]
  row.names(controllo) <- 1:nrow(controllo)
  colnames(controllo) <- c("id","pos")
  for(n in as.numeric(row.names(controllo))){
    if(controllo[n,"pos"] != max(controllo$pos)){
      cuno <- controllo[n, "id"]
      cdue <- controllo[(n+1), "id"]
      if((controllo[controllo$id == cuno,"pos"] - controllo[controllo$id == cdue,"pos"]) == 1 |
         (controllo[controllo$id == cuno,"pos"] - controllo[controllo$id == cdue,"pos"]) == - 1){
        nrow1 <- clui[clui$id == cuno,"nrow"]
        nrow2 <- clui[clui$id == cdue,"nrow"]
        dis[nrow1,nrow2] <- dis[nrow1,nrow2] * 2
        dis[nrow2,nrow1] <- dis[nrow1,nrow2] * 2
      }
    }
  }
  #####################################
  tsp <- TSP(as.dist(dis))
  set.seed(123)
  tour <- solve_TSP(tsp, method = "nn", start = start)
  tour <- solve_TSP(tsp, method = "2-opt", control = list(tour = tour))
  tour <- data.frame(tour)
  colnames(tour) <- "nrow"
  tour$pos <- 1:nrow(tour)
  tour <- merge(tour,clui, by = "nrow")
  tour <- arrange(tour,pos)
  risultato2 <- rbind(risultato2, data.frame(tour$id))
  pfinale <- tour[tour$pos == max(tour$pos),c("id","x","y")]
  conta <- conta + 1
}

colnames(risultato) <- "id1"
colnames(risultato2) <- "id2"

risultato$pos <- 1:nrow(risultato)
risultato2$pos <- 1:nrow(risultato2)
totale <- merge(risultato, risultato2, by = "pos")
write.csv(totale, file = "percorso_nngenetic_pulito.csv")
