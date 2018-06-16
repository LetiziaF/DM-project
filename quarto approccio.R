library(sp)
library(TSP)
library(dplyr)
library(plyr)

#dataset
setwd("C:/Users/aless/Desktop/Data Science/Decision model/Progetto Santa Travelling/Progetto")
cities <- read.csv("santa_cities.csv")
d <- cities
d1 <- cities
row.names(d) <- d$id
row.names(d1) <- d1$id

#città più in basso a sinistra
d$sum <- (d$x*100)/max(d$x) + (d$y*100/max(d$y))
city <- d[d$sum == min(d$sum),"id"]

#mostra la città
#color <- d
#color$flag <- ifelse(d$sum == min(d$sum), "red","black")
#plot(color$x, color$y, col = color$flag)



risultato <- data.frame()
df <- d[-which(d$id == city),]

for(i in 1:nrow(d)){
  print(paste0(1,"    ",i,"/",nrow(d)))
  risultato <- rbind(risultato, city)
  colnames(risultato) <- "id"
  if((i %% 10000) == 0){
    modifica <- data.frame(risultato[(i-9999):i,])
    colnames(modifica) <- "id"
    modifica$nrow <- 1:nrow(modifica)
    modifica <- merge(modifica, cities, by.x = "id", by.y = "id")
    modifica <- arrange(modifica, nrow)
    matrice <- as.matrix(dist(select(modifica, x, y), diag = TRUE))
    tsp <- TSP(as.dist(matrice))
    tour <- as.TOUR(modifica$nrow)
    set.seed(123)
    tour <- solve_TSP(tsp, method = "2-opt", control = list(tour = tour))
    tour <- data.frame(tour)
    colnames(tour) <- "nrow"
    tour$pos <- 1:nrow(tour)
    tour <- merge(tour,modifica, by = "nrow")
    tour <- arrange(tour, pos)
    tour <- data.frame(tour[,"id"])
    colnames(tour) <- "id"
    risultato <- data.frame(risultato[-((i-9999):i),])
    colnames(risultato) <- "id"
    risultato <- rbind(risultato, tour)
    colnames(risultato) <- "id"
    city <- risultato[i,"id"]
  }
  if(i != nrow(d)){
    distanza <- data.frame(df[,c("id","x","y")])
    distanza$city <- city
    distanza$xc <- d[d$id == city,"x"]
    distanza$yc <- d[d$id == city,"y"]
    distanza$distanza <- sqrt((distanza$xc - distanza$x)^2 + 
                                (distanza$yc - distanza$y)^2)
    city <- distanza[distanza$distanza == min(distanza$distanza),"id"]
    city <- city[1]
    df <- df[-which(df$id == city),]
  }
}

#città più in alto a destra
d1$sum <- (d1$x*100)/max(d1$x) + (d1$y*100/max(d1$y))
city1 <- d1[d1$sum == max(d1$sum),"id"]

risultato1 <- data.frame()
df1 <- d1[-which(d1$id == city1),]

for(i in 1:nrow(d1)){
  print(paste0(2,"    ",i,"/",nrow(d1)))
  risultato1 <- rbind(risultato1, city1)
  colnames(risultato1) <- "id"
  if((i %% 10000) == 0){
    modifica <- data.frame(risultato1[(i-9999):i,])
    colnames(modifica) <- "id"
    modifica$nrow <- 1:nrow(modifica)
    modifica <- merge(modifica, cities, by.x = "id", by.y = "id")
    modifica <- arrange(modifica, nrow)
    matrice <- as.matrix(dist(select(modifica, x, y), diag = TRUE))
    
    #setta distanza
    controllo <- risultato
    controllo$pos <- 1:nrow(controllo)
    controllo <- controllo[which(controllo$id %in% modifica$id),]
    row.names(controllo) <- 1:nrow(controllo)
    for(n in as.numeric(row.names(controllo))){
      if(controllo[n,"pos"] != max(controllo$pos)){
        cuno <- controllo[n, "id"]
        cdue <- controllo[(n+1), "id"]
        if((controllo[controllo$id == cuno,"pos"] - controllo[controllo$id == cdue,"pos"]) == 1 |
           (controllo[controllo$id == cuno,"pos"] - controllo[controllo$id == cdue,"pos"]) == - 1){
          nrow1 <- modifica[modifica$id == cuno,"nrow"]
          nrow2 <- modifica[modifica$id == cdue,"nrow"]
          matrice[nrow1,nrow2] <- matrice[nrow1,nrow2] * 10000
          matrice[nrow2,nrow1] <- matrice[nrow1,nrow2] * 10000
        }
      }
    }
    tsp <- TSP(as.dist(matrice))
    tour <- as.TOUR(modifica$nrow)
    set.seed(123)
    tour <- solve_TSP(tsp, method = "2-opt", control = list(tour = tour))
    tour <- data.frame(tour)
    colnames(tour) <- "nrow"
    tour$pos <- 1:nrow(tour)
    tour <- merge(tour,modifica, by = "nrow")
    tour <- arrange(tour, pos)
    tour <- data.frame(tour[,"id"])
    colnames(tour) <- "id"
    risultato1 <- data.frame(risultato1[-((i-9999):i),])
    colnames(risultato1) <- "id"
    risultato1 <- rbind(risultato1, tour)
    colnames(risultato1) <- "id"
    city1 <- risultato1[i,"id"]
  }
  if(i != nrow(d1)){
    distanza1 <- data.frame(df1[,c("id","x","y")])
    distanza1$city <- city1
    distanza1$xc <- d1[d1$id == city1,"x"]
    distanza1$yc <- d1[d1$id == city1,"y"]
    distanza1$distanza <- sqrt((distanza1$xc - distanza1$x)^2 + 
                                (distanza1$yc - distanza1$y)^2)
    city1 <- distanza1[distanza1$distanza == min(distanza1$distanza),"id"]
    city1 <- city1[1]
    df1 <- df1[-which(df1$id == city1),]
  }
}

colnames(risultato) <- "id"
colnames(risultato1) <- "id1"

risultato$pos <- 1:nrow(risultato)
risultato1$pos <- 1:nrow(risultato1)
r <- merge(risultato, risultato1, by = "pos")
colnames(r) <- c("pos","id1","id2")
write.csv(r, file = "percorso_nn_opt_nocluster.csv")
