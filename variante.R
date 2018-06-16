library(GA)
library(stats4)
library(shiny)
library(dplyr)

#Funzioni che verranno utilizzate per l'applicazione del SA
distance <- function(sq){
  sq2 <- embed(sq,2)
  sum(distmat[cbind(sq2[,2],sq2[,1])])
}

genseq <- function(sq){
  idx <- seq(2, nrow(distmat)-1)
  changepoints <- sample(idx, size = 2, replace = FALSE)
  tmp <- sq[changepoints[1]]
  sq[changepoints[1]] <- sq[changepoints[2]]
  sq[changepoints[2]] <- tmp
  sq
}

#Importazione dei dataset
setwd("C:/Users/Francesco/Desktop/Progetto decision model")
cities <- read.csv("santa_cities.csv")
#Percorso ottenuto in precedenza attraverso nel codice "quarto approccio"
percorsi <- read.csv("percorso_nn_opt_nocluster.csv")
percorsi <- percorsi[,-1]

#Preparazione del percorso 1
percorso1 <- percorsi[,c(1,2)]
colnames(percorso1) <- c("pos","id")
percorso1 <- merge(percorso1, cities, by = "id")
percorso1 <- arrange(percorso1, pos)

#Preparazione del percorso 2
percorso2 <- percorsi[,c(1,3)]
colnames(percorso2) <- c("pos","id")
percorso2 <- merge(percorso2, cities, by = "id")
percorso2 <- arrange(percorso2, pos)


#Applicazione del SA sul percorso 1 in blocchi di 10.000 nodi e 
#salvataggio dei risultati nel dataframe "risultato"
minimo <- 1
risultato <- data.frame()
p <- data.frame()
percorso1$pos <- 1:nrow(percorso1)
numero_div <- 15

for(i in 1:numero_div){
  print(paste0(i,"/",numero_div))
  massimo <- i * (150000/numero_div)
  if(i != 1){
    colnames(ris1) <- "id"
    p <- ris[-which(ris$id %in% ris1$id),]
    p <- p[,c(3,4,5,6)]
    colnames(p) <- c("id","pos","x","y")
    p1 <- percorso1[(minimo+1):massimo,]
    p <- rbind(p, p1)
  }
  if(i == 1){
    p <- percorso1[1:massimo,]
  }
  colnames(p) <- c("id","pos","x","y")
  p$nrow <- 1:nrow(p)
  dist <- (dist(select(p,x,y)))
  distmat <- as.matrix(dist)
  sq <- p$nrow
  
  loc <- as.matrix(cbind(p$x, p$y))
  x <- loc[,1] 
  y <- loc[,2]
  s <- seq_len(nrow(distmat))
  tsp_init <- loc[sq,]
  
  set.seed(123)
  res <- optim(sq,distance,genseq,method = "SANN", 
               control = list(maxit = 300000, trace = TRUE, REPORT = 500))
  
  ris <- data.frame(res$par)
  colnames(ris) <- "nrow"
  ris$pos <- 1:nrow(ris)
  ris <- merge(ris, p, by = "nrow")
  ris <- arrange(ris, pos.x)
  if(i != numero_div){
    if(i == 1){
      ris1 <- data.frame(ris[1:9500,"id"])
      colnames(ris1) <- "id"
    }
    if(i != 1){
      ris1 <- data.frame(ris[1:10000,"id"])
      colnames(ris1) <- "id"
    }
  }
  if(i == numero_div){
    ris1 <- data.frame(ris[,"id"])
    colnames(ris1) <- "id"
  }
  risultato <- rbind(risultato, ris1)
  colnames(risultato) <- "id"
  minimo <- massimo
}

#Applicazione del SA sul percorso 2 in blocchi di 10.000 nodi con i pesi posti
#alle combinazioni già avvenute nel percorso 1 e 
#salvataggio dei risultati nel dataframe "risultato2" 
minimo <- 1
numero_div <- 15
risultato2 <- data.frame()
p <- data.frame()
percorso2$pos <- 1:nrow(percorso2)

for(i in 1:numero_div){
  print(paste0(i,"/",numero_div))
  massimo <- i * (150000/numero_div)
  if(i != 1){
    colnames(ris1) <- "id"
    p <- ris[-which(ris$id %in% ris1$id),]
    p <- p[,c(3,4,5,6)]
    colnames(p) <- c("id","pos","x","y")
    p1 <- percorso2[(minimo+1):massimo,]
    p <- rbind(p, p1)
  }
  if(i == 1){
    p <- percorso2[1:massimo,]
  }
  colnames(p) <- c("id","pos","x","y")
  p$nrow <- 1:nrow(p)
  distmat <- as.matrix(dist(select(p,x,y)))
  
#Venogno posti i pesi alle combinazioni di nodi già avvenute nel primo percorso per
#evitare che vengano ripetute anche nel secondo 
  controllo <- risultato
  controllo$pos <- 1:nrow(controllo)
  controllo <- controllo[which(controllo$id %in% p$id),]
  row.names(controllo) <- 1:nrow(controllo)
  for(n in as.numeric(row.names(controllo))){
    if(controllo[n,"pos"] != max(controllo$pos)){
      cuno <- controllo[n, "id"]
      cdue <- controllo[(n+1), "id"]
      if((controllo[controllo$id == cuno,"pos"] - controllo[controllo$id == cdue,"pos"]) == 1 |
         (controllo[controllo$id == cuno,"pos"] - controllo[controllo$id == cdue,"pos"]) == - 1){
        nrow1 <- p[p$id == cuno, "nrow"]
        nrow2 <- p[p$id == cdue, "nrow"]
        distmat[nrow1, nrow2] <- max(distmat)  
        distmat[nrow2, nrow1] <- max(distmat)
      }
    }
  }
  dist <- as.dist(distmat)
  sq <- p$nrow
  
  loc <- as.matrix(cbind(p$x, p$y))
  x <- loc[,1] 
  y <- loc[,2]
  s <- seq_len(nrow(distmat))
  tsp_init <- loc[sq,]
  
  set.seed(123)
  res <- optim(sq,distance,genseq,method = "SANN", 
               control = list(maxit = 300000, trace = TRUE, REPORT = 500))
  
  ris <- data.frame(res$par)
  colnames(ris) <- "nrow"
  ris$pos <- 1:nrow(ris)
  ris <- merge(ris, p, by = "nrow")
  ris <- arrange(ris, pos.x)
  if(i != numero_div){
    if(i == 1){
      ris1 <- data.frame(ris[1:9500,"id"])
      colnames(ris1) <- "id"
    }
    if(i != 1){
      ris1 <- data.frame(ris[1:10000,"id"])
      colnames(ris1) <- "id"
    }
  }
  if(i == numero_div){
    ris1 <- data.frame(ris[,"id"])
    colnames(ris1) <- "id"
  }
  risultato2 <- rbind(risultato2, ris1)
  colnames(risultato2) <- "id"
  minimo <- massimo
}

#Unione dei due dataset "risultato" e "risultato2" 
risultato$pos <- 1:nrow(risultato)
risultato2$pos <- 1:nrow(risultato2)
percorso <- merge(risultato, risultato2, by = "pos")
colnames(percorso) <- c("pos", "id1", "id2")

write.csv(percorso, file = "percorso_nn_opt_sa_nocluster.csv")