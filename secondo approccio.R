library(sp)
library(TSP)
library(dplyr)
library(plyr)

#dataset
setwd("C:/Users/Francesco/Desktop/Progetto decision model")
cluster <- read.csv("cluster_kmeans.csv")
cluster <- cluster[,-1]
row.names(cluster) <- cluster$id
tot_cluster <- length(table(cluster$ncluster))
max_size <- max(table(cluster$ncluster))
solution <- data.frame()
solution2 <- data.frame()

#trova centroidi
punti_medi <- data.frame(matrix(ncol = 3, nrow = tot_cluster))
colnames(punti_medi) <- c("ncluster","x","y")
punti_medi$ncluster <- 1:nrow(punti_medi)
for(i in punti_medi$ncluster){
  c <- cluster[cluster$ncluster == i, c("x","y")]
  punti_medi[i,"x"] <- mean(c$x)
  punti_medi[i,"y"] <- mean(c$y)
}
punti_medi2 <- punti_medi

#scegli cluster 1 iniziale (quello più in basso a sinistra)
trova_inizio <- punti_medi
trova_inizio$sum <- trova_inizio$x * 100 / max(trova_inizio$x) + trova_inizio$y * 100 / max(trova_inizio$y)
gruppo <- trova_inizio[trova_inizio$sum == min(trova_inizio$sum),"ncluster"]
gruppo <- gruppo[1]

#scegli cluster 2 iniziale (quello più in alto a destra)
gruppo2 <- trova_inizio[trova_inizio$sum == max(trova_inizio$sum),"ncluster"]
gruppo <- gruppo[1]

#cluster iniziale 1
c <- cluster[cluster$ncluster == gruppo,]
c$nrow <- 1:nrow(c)
#punto iniziale
c$sum <- c$x * 100 / max(c$x) + c$y * 100 / max(c$y)
start <- c[c$sum == min(c$sum),"nrow"]
start <- start[1]
solution <- rbind(solution, c[c$nrow == start,c("id","x","y","ncluster")])
solution$pos <- 1

#percorso 1
for(i in 1:tot_cluster){
  conta_n <- paste0("numero",i, "/", tot_cluster)
  print("####################################################################")
  print(conta_n)
  print("####################################################################")
  
  disc <- as.matrix(dist(select(c, x, y), diag = TRUE))
  tsp <- TSP(as.dist(disc))
  set.seed(123)
  tour <- solve_TSP(tsp, method = "nn", start = start)
  tour <- solve_TSP(tsp, method = "2-opt", control = list(tour = tour))
  tour <- data.frame(tour)
  colnames(tour) <- "nrow"
  tour$pos <- 1:nrow(tour)
  tour <- merge(tour, c, by = "nrow")
  tour <- arrange(tour, pos)
  #trovo la distanza tra il punto finale e i punti medi degli altri cluster
  if(i != tot_cluster){
    pfinale <- tour[tour$nrow == nrow(tour),c("id","x","y","ncluster")]
    x <- pfinale[,"x"][1]
    y <- pfinale[,"y"][1]
    punti_medi <- punti_medi[punti_medi$ncluster != gruppo,]
    punti_medi$dist <- sqrt((x - punti_medi$x)^2 + (y - punti_medi$y)^2)
    gruppo <- punti_medi[punti_medi$dist == min(punti_medi$dist),"ncluster"]
    gruppo <- gruppo[1]
    punti_medi <- punti_medi[,-4]
  }
  #setto la posizione considerando anche i cluster già eseguiti
  elimina <- row.names(tour[tour$pos == 1,])
  tour <- tour[which(row.names(tour) != elimina),]
  tour$pos <- tour$pos + max(solution$pos) - 1
  solution <- rbind(solution,tour[,c("pos","id","x","y","ncluster")])
  solution <- arrange(solution,pos)
  c <- cluster[cluster$ncluster == gruppo,]
  c <- rbind(c,pfinale)
  c$nrow <- 1:nrow(c)
  start <- c[c$id == pfinale$id,"nrow"]
  start <- start[1]
}

#cluster iniziale 2
c2 <- cluster[cluster$ncluster == gruppo,]
c2$nrow <- 1:nrow(c2)
#punto iniziale
c2$sum <- c2$x*100 / max(c2$x) + c2$y * 100 / max(c2$y)
start2 <- c2[c2$sum == max(c2$sum), "nrow"]
start2 <- start2[1]
solution2 <- rbind(solution2, c2[c2$nrow == start2, c("id","x","y","ncluster")])
solution2$pos <- 1

#percorso 2
for(i in 1:tot_cluster){
  conta_n <- paste0("numero",i,"/", tot_cluster)
  print("####################################################################")
  print(conta_n)
  print("####################################################################")
  
  disc2 <- as.matrix(dist(select(c2, x, y), diag = TRUE))
  #assegno peso a combinazioni già avvenute all'interno del cluster
  combinazioni <- solution[solution$ncluster == gruppo2,]
  for(n in 1:(nrow(combinazioni)-1)){
    fname <- as.character(combinazioni[n,"id"])
    sname <- as.character(combinazioni[n+1,"id"])
    disc2[fname,sname] <- max(disc2) * 1000000000
    disc2[sname,fname] <- max(disc2) * 1000000000
  }
  #assegno peso a combinazioni già avvenute tra il punto finale e i punti del cluster successivo
  finale <- c2[c2$nrow == start2, "id"]
  pos_finale <- solution[solution$id == finale,"pos"]
  pos_finale
  for(n in c(-1,1)){
    if((pos_finale + n) <= nrow(solution)){
      fname <- as.character(solution[solution$pos == (pos_finale + n),"id"])
      sname <- as.character(finale)
      if(fname %in% row.names(disc2)){
        disc2[fname,sname] <- max(disc2) * 1000000000
        disc2[sname,fname] <- max(disc2) * 1000000000
      }
    }
  }
  
  #percorso
  tsp2 <- TSP(as.dist(disc2))
  set.seed(123)
  tour2 <- solve_TSP(tsp2, method = "nn", start = start2)
  tour2 <- solve_TSP(tsp2, method = "2-opt", control = list(tour = tour2))
  tour2 <- data.frame(tour2)
  colnames(tour2) <- "nrow"
  tour2$pos <- 1:nrow(tour2)
  tour2 <- merge(tour2, c2, by = "nrow")
  tour2 <- arrange(tour2, pos)
  #trovo la distanza tra il punto finale e i punti medi degli altri cluster
  if(i != tot_cluster){
    pfinale <- tour2[tour2$nrow == nrow(tour2),c("id","x","y","ncluster")]
    x <- pfinale[,"x"][1]
    y <- pfinale[,"y"][1]
    punti_medi2 <- punti_medi2[punti_medi2$ncluster != gruppo2,]
    punti_medi2$dist <- sqrt((x - punti_medi2$x)^2 + (y - punti_medi2$y)^2)
    gruppo2 <- punti_medi2[punti_medi2$dist == min(punti_medi2$dist),"ncluster"]
    gruppo2 <- gruppo2[1]
    punti_medi2 <- punti_medi2[,-4]
  }
  #setto la posizione considerando anche i cluster già eseguiti
  elimina2 <- row.names(tour2[tour2$pos == 1,])
  tour2 <- tour2[which(row.names(tour2) != elimina2),]
  tour2$pos <- tour2$pos + max(solution2$pos) - 1
  solution2 <- rbind(solution2,tour2[,c("pos","id","x","y","ncluster")])
  solution2 <- arrange(solution2,pos)
  c2 <- cluster[cluster$ncluster == gruppo2,]
  c2 <- rbind(c2,pfinale)
  c2$nrow <- 1:nrow(c2)
  start2 <- c2[c2$id == pfinale$id,"nrow"]
  start2 <- start2[1]
}

ris <- data.frame(solution[,"id"])
ris1 <- data.frame(solution2[,"id"])

#scarica dataframe
colnames(ris) <- "id1"
colnames(ris1) <- "id2"
ris$pos <- 1:nrow(ris)
ris1$pos <- 1:nrow(ris1)
risultati <- merge(ris,ris1, by = "pos")
write.csv(risultati, "50percorso_nnotp_kmeans_progressivo.csv")
