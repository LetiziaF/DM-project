library(dplyr)
library(GA)
library(plyr)

tourLength <- function(tour, distMatrix) {
  route <- embed(tour, 2)[, 2:1]
  sum(distMatrix[route])
}
tpsFitness <- function(tour, ...) {
  1/tourLength(tour, ...)
}

setwd("C:/Users/aless/Desktop/Data Science/Decision model/Progetto Santa Travelling")
cities <- read.csv("santa_cities.csv")

#cluster
tot_cluster <- 300
set.seed(123)
clu <- kmeans(x = as.matrix(cities[,-1]), centers = tot_cluster, iter.max = 200)
max_size <- max(clu$size)
cluster <- cities
cluster$ncluster <- clu$cluster



#dataframe su cui salvare risultati
estremi <- data.frame()
estremi2 <- data.frame()
percorsi <- data.frame(matrix(nrow = max_size, ncol = 0))
percorsi2 <- data.frame(matrix(nrow = max_size, ncol = 0))

#distanza nei cluster e dataframe estremi 
for(i in 1:tot_cluster){
  conta_n <- paste0("numero",i)
  print("####################################################################")
  print(conta_n)
  print("####################################################################")
  c <- cluster[cluster$ncluster == i,]
  row.names(c) <- c[,"id"]
  colnames(c) <- c(paste0("id",i),"x","y",paste0("ncluster",i))
  c$nrow <- 1:nrow(c)
  disc <- as.matrix(dist(select(c, x, y), diag = TRUE))
  set.seed(123)
  GAfit <- ga(type = "permutation", 
              fitness = tpsFitness, 
              distMatrix = disc, 
              lower = 1, 
              upper = nrow(c),
              maxiter = 5000,
              popSize = 1000,
              pmutation = 0.1,
              pcrossover = 0.8,
              keepBest = TRUE)
  solution <- GAfit@solution
  row.names(solution) <- paste0("nrow",i)
  solution <- data.frame(t(solution))
  solution[,paste0("position",i)] <- row.names(solution)
  row.names(solution) <- 1:nrow(solution)
  solution <- merge(solution, c[,c(paste0("id",i),"nrow",paste0("ncluster",i))], 
                    by.x = paste0("nrow",i), by.y = "nrow")
  sol <- solution
  est <- rbind(solution[solution[,paste0("position",i)] == "x1", paste0("id",i)],
               solution[solution[,paste0("position",i)] == paste0("x",nrow(solution)), 
                        paste0("id",i)])
  est <- data.frame(est)
  colnames(est) <- "id"
  estremi <- rbind(estremi,est)
  n_aggiungi <- max_size - nrow(solution)
  aggiungi <- data.frame(matrix(nrow = n_aggiungi, ncol = 4))
  colnames(aggiungi) <- colnames(solution)
  solution <- rbind(solution,aggiungi)
  percorsi <- cbind(percorsi, solution)
  
  #secondo percorso
  #ordino posizione primo percorso
  sol <- sol[complete.cases(sol),]
  for(n in row.names(sol)){
    sol[n,"pos"] <- as.numeric(unlist(strsplit(sol[n,paste0("position",i)],split='x', 
                                               fixed=TRUE))[2])
  }
  sol <- arrange(sol,pos)
  disc2 <- disc
  #assegno peso a combinazioni già avvenute
  for(n in 1:(nrow(sol)-1)){
    fname <- as.character(sol[n,paste0("id",i)])
    sname <- as.character(sol[n+1,paste0("id",i)])
    disc2[fname,sname] <- max(disc2) * 1000000000
    disc2[sname,fname] <- max(disc2) * 1000000000
  }
  #calcolo nuova soluzione
  set.seed(123)
  GAfit2 <- ga(type = "permutation",
               fitness = tpsFitness,
               distMatrix = disc2,
               lower = 1,
               upper = nrow(c),
               maxiter = 5000,
               popSize = 1000,
               pmutation = 0.1,
               pcrossover = 0.8,
               keepBest = TRUE)
  sol <- GAfit2@solution
  row.names(sol) <- paste0("nrow",i)
  sol <- data.frame(t(sol))
  sol[,paste0("position",i)] <- row.names(sol)
  row.names(sol) <- 1:nrow(sol)
  sol <- merge(sol, c[,c(paste0("id",i),"nrow",paste0("ncluster",i))],
               by.x = paste0("nrow",i), by.y = "nrow")
  est2 <- rbind(sol[sol[,paste0("position",i)] == "x1", paste0("id",i)],
                sol[sol[,paste0("position",i)] == paste0("x",nrow(sol)),
                    paste0("id",i)])
  est2 <- data.frame(est2)
  colnames(est2) <- "id"
  estremi2 <- rbind(estremi2,est2)
  n_aggiungi2 <- max_size - nrow(sol)
  aggiungi2 <- data.frame(matrix(nrow = n_aggiungi2, ncol = 4))
  colnames(aggiungi2) <- colnames(sol)
  sol <- rbind(sol,aggiungi2)
  percorsi2 <- cbind(percorsi2, sol)
}

#primo cluster
estremi <- merge(estremi, cluster[,c("ncluster","id","x","y")], by = "id")
row.names(estremi) <- estremi$id
dise <- as.matrix(dist(select(estremi, x, y), diag = TRUE))
colnames(dise) <- row.names(estremi)
#setta 0 la distanza tra le città all'interno dello stesso cluster
for(i in row.names(estremi)){
  for(l in row.names(estremi)){
    if(estremi[i,"id"] != estremi[l,"id"]){
      if(estremi[i,"ncluster"] == estremi[l,"ncluster"]){
        dise[i,l] <- 0
      }
    }
  }
}
#soluzione 1 estremi
estremi$nrow <- 1:nrow(estremi)
set.seed(123)
GAfit1 <- ga(type = "permutation", 
             fitness = tpsFitness, 
             distMatrix = dise, 
             lower = 1, 
             upper = nrow(estremi),
             maxiter = 5000,
             popSize = 1000,
             pmutation = 0.1,
             pcrossover = 0.8,
             keepBest = TRUE)
solution1 <- GAfit1@solution
solution1 <- data.frame(t(solution1))
colnames(solution1) <- "nrow"
solution1$pos <- row.names(solution1)
solution1 <- merge(solution1, estremi, by = "nrow")
for(i in row.names(solution1)){
  solution1[i, "position"] <- as.numeric(unlist(strsplit(solution1[i,"pos"],split='x', 
                                                    fixed=TRUE))[2])
}
solution1 <- arrange(solution1,position)
ris <- data.frame(matrix(nrow = 0, ncol = 1))
for(i in unique(solution1$ncluster)){
  tempo <- percorsi[,c(paste0("nrow",i),paste0("position",i),paste0("id",i),
                       paste0("ncluster",i))]
  tempo <- tempo[complete.cases(tempo),]
  for(n in row.names(tempo)){
    tempo[n, paste0("pos",i)] <- as.numeric(unlist(strsplit(tempo[n,
                                    paste0("position",i)],split='x', fixed=TRUE))[2])
  }
  colnames(tempo) <- c("nrow","position","id","ncluster","pos")
  tempo <- arrange(tempo,pos)
  minimo <- min(solution1[solution1$ncluster == i,]["position"])
  if(solution1[solution1$position == minimo,"id"] != tempo[1,"id"]){
    tempo <- tempo[seq(dim(tempo)[1],1),]
  }
  ris <- rbind(ris, data.frame(tempo$id))
}

#secondo cluster
estremi2 <- merge(estremi2, cluster[,c("ncluster","id","x","y")], by = "id")
row.names(estremi2) <- estremi2$id
dise2 <- as.matrix(dist(select(estremi2, x, y), diag = TRUE))
colnames(dise2) <- row.names(estremi2)
#setta 0 la distanza tra le città all'interno dello stesso cluster
for(i in row.names(estremi2)){
  for(l in row.names(estremi2)){
    if(estremi2[i,"id"] != estremi2[l,"id"]){
      if(estremi2[i,"ncluster"] == estremi2[l,"ncluster"]){
        dise2[i,l] <- 0
      }
    }
  }
}
#setta infinito la distanza tra combinazioni di città già usate
for(i in 1:(nrow(solution1)-1)){
  if((i %% 2) == 0){
    n <- solution1[solution1$position == i,"id"]
    m <- solution1[solution1$position == (i+1), "id"]
    if(n %in% estremi2$id){
      if(m %in% estremi2$id){
        dise2[n,m] <- max(dise2)*1000000000
        dise2[m,n] <- max(dise2)*1000000000
      }
    }
  }
}

#soluzione 2 estremi
estremi2$nrow <- 1:nrow(estremi2)
set.seed(123)
GAfit3 <- ga(type = "permutation", 
             fitness = tpsFitness, 
             distMatrix = dise2, 
             lower = 1, 
             upper = nrow(estremi),
             maxiter = 5000,
             popSize = 1000,
             pmutation = 0.1,
             pcrossover = 0.8,
             keepBest = TRUE)
sol1 <- GAfit3@solution
sol1 <- data.frame(t(sol1))
colnames(sol1) <- "nrow"
sol1$pos <- row.names(sol1)
sol1 <- merge(sol1, estremi, by = "nrow")
for(i in row.names(sol1)){
  sol1[i, "position"] <- as.numeric(unlist(strsplit(sol1[i,"pos"],split='x', 
                                                         fixed=TRUE))[2])
}
sol1 <- arrange(sol1,position)
ris1 <- data.frame(matrix(nrow = 0, ncol = 1))
for(i in unique(sol1$ncluster)){
  tempo1 <- percorsi2[,c(paste0("nrow",i),paste0("position",i),paste0("id",i),
                       paste0("ncluster",i))]
  tempo1 <- tempo1[complete.cases(tempo1),]
  for(n in row.names(tempo1)){
    tempo1[n, paste0("pos",i)] <- as.numeric(unlist(strsplit(tempo1[n,
                              paste0("position",i)],split='x', fixed=TRUE))[2])
  }
  colnames(tempo1) <- c("nrow","position","id","ncluster","pos")
  tempo1 <- arrange(tempo1,pos)
  minimo1 <- min(sol1[sol1$ncluster == i,]["position"])
  if(sol1[sol1$position == minimo1,"id"] != tempo1[1,"id"]){
    tempo1 <- tempo1[seq(dim(tempo1)[1],1),]
  }
  ris1 <- rbind(ris1, data.frame(tempo1$id))
}

write.csv(ris, file = "percorso1_genetico_base(leti).csv")
write.csv(ris1, file = "percorso2_genetico_base(leti).csv")