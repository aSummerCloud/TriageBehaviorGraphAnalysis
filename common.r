#find common subgraph

#common subgraph from different graphs


filepath_id= function(definition, id1){
  dir = paste("SubGraph", definition, sep="//")
  pair1 = paste(dir,id1, sep="//")
  file1 = paste(pair1, "csv", sep=".")
  return(file1)
}

filepath_pair = function(definition, id1, id2){
  dir = paste("SubGraph", definition, sep="//")
  id1_id2 = paste(id1, id2, sep="_")
  pair1 = paste(dir,id1_id2, sep="//")
  file1 = paste(pair1, "csv", sep=".")
  return(file1)
}

filepath_output = function(id1, id2, id3, id4){
  id1_id2 = paste(id1, id2, sep="_")
  id3_id4 = paste(id3, id4, sep="_")
  id1_id2_id3_id4 = paste(id1_id2, id3_id4, sep="_")
  
  pair = paste("SubGraph",id1_id2_id3_id4, sep="//")
  file = paste(pair, "csv", sep=".")
  return(file)
}

common4_3_25 = function(id1, id2, id3, id4){
  file1 <- filepath_pair(id1, id2)
  file2 <- filepath_pair(id3, id4)
  output <- filepath_output(id1, id2, id3, id4)
  
  common1 <- read.csv(file=file1, head=TRUE, sep=",")
  common2 <- read.csv(file=file2, head=TRUE, sep=",")
  
  unique1 <- unique(common1)
  unique2 <- unique(common2)

  
  n1 <- nrow(unique1)
  n2 <- nrow(unique2)
  
  
  result <- list()
  index <- 1
  
  
  for(i in 1: n1){
    for(j in 1:n2){
      vector1<-unique1[i, 3:25]
      vector2 <-unique2[j,3:25]
      if(equalFeatures(vector1, vector2)){
         result[[index]] <- unique1[i,] 
         index <- index +1
      }
    }
  }
  
  resultMatrix <- do.call(rbind, result)
  write.table(resultMatrix, file=output, sep=",")
  return(index)
}




common2pair_3_25 = function(pair1, pair2){
  file1 <- filepath_id(pair1)
  file2 <- filepath_id(pair2)
  output <- filepath_pair(pair1, pair2)
  
  common1 <- read.csv(file=file1, head=TRUE, sep=",")
  common2 <- read.csv(file=file2, head=TRUE, sep=",")
  
  unique1 <- unique(common1)
  unique2 <- unique(common2)
  
  
  n1 <- nrow(unique1)
  n2 <- nrow(unique2)
  
  
  result <- list()
  index <- 1
  
  
  for(i in 1: n1){
    for(j in 1:n2){
      vector1<-unique1[i, 3:25]
      vector2 <-unique2[j,3:25]
      if(equalFeatures(vector1, vector2)){
        result[[index]] <- unique1[i,] 
        index <- index +1
      }
    }
  }
  
  resultMatrix <- do.call(rbind, result)
  write.table(resultMatrix, file=output, sep=",")
  return(index)
}


common2pair_sameVs = function(definition,pair1, pair2){
  file1 <- filepath_id(definition, pair1)
  file2 <- filepath_id(definition, pair2)
  output <- filepath_pair(definition,pair1, pair2)
  

  common1 <- read.csv(file=file1, head=TRUE, sep=",")
  common2 <- read.csv(file=file2, head=TRUE, sep=",")
  
  id<-as.character(common1[1,1])
  unique1 <- as.matrix(unique(common1[,33]))
  unique2 <- as.matrix(unique(common2[,33]))
  
  
  n1 <- nrow(unique1)
  n2 <- nrow(unique2)
  
  
  result <- list()
  index <- 1
  
  
  for(i in 1: n1){
    for(j in 1:n2){
      vector1<-unique1[i]
      vector2 <-unique2[j]
      if(identical(vector1, vector2)){
        result[[index]] <- c(id,unique1[i])
        index <- index +1
      }
    }
  }
  
  resultMatrix <- do.call(rbind, result)
  a <- unique(resultMatrix[,2])
  write.table(a, file=output, sep=",")
  return(index)
}


