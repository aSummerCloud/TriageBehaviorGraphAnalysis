writeIso_Definition0<- function(id1, id2){
  
  
  
  gid1 <- createGraph_Definition0(id1)
  gid2 <- createGraph_Definition0(id2)
  
  
  pair <- paste(id1, id2, sep="_")
  output <- paste("SubGraph//Definition0", pair, sep="//")
  output <- paste(output, "csv", sep=".")
  if(!file.exists(output)){
    file.create(output)
  }
  
  
  index <- writeIso_g_Definition0(id1, id2, gid1, gid2, output)
  
  return(index)
}

writeIso_g_Definition0 <- function(id_small, id_large, g_small, g_large, output){
  
  
  if(!file.exists(output)){
    file.create(output)
  }
  
  
  sub_list <-getSubgraph_all_comb(g_small)
  
  k <- 1
  index <- 1
  result <- list()
  
  sub_num <- length(sub_list)
  
  while(k<=sub_num){
    sub_k <- sub_list[[k]]
    
    vcolor1 = sapply(V(g_large)$Field, getVInt_Field)
    vcolor2 = sapply(V(sub_k)$Field, getVInt_Field)
    
    iso <- graph.subisomorphic.vf2(g_large, sub_k, 
                                   vertex.color1=vcolor1, 
                                   vertex.color2=vcolor2)
    
    if(!iso$iso){
      k <- k+1
      next
    }
    
    #if the subgraphs are isomorphic, then store this subgraph
    
    #a<-iso$map21
    #suba<-induced.subgraph(graph=g_large, vids=a)
    #sub_k
    new_features <- numeric(0)
    new_features <- c(new_features, id_small, id_large)
    graph_features <- getFeatures(sub_k)
    new_features <- c(new_features, graph_features)
    
    
    # set of vetices in sub_k
    v_set <- paste(V(sub_k)$name, collapse=";")
    new_features <- c(new_features, v_set)
    
    result[[index]]<-new_features
    index <- index+1
    
    k<-k+1
  }
  
  
  #write to file
  resultMatrix <- do.call(rbind, result)
  write.table(resultMatrix, file=output, sep=",")
  
  return(index)
}


writeIso_Definition1<- function(id1, id2){ 
  gid1 <- createGraph_Definition1(id1)
  gid2 <- createGraph_Definition1(id2)
  
  
  pair <- paste(id1, id2, sep="_")
  output <- paste("SubGraph//Definition1", pair, sep="//")
  output <- paste(output, "csv", sep=".")
  if(!file.exists(output)){
    file.create(output)
  }
  
  
  index <- writeIso_g_Definition1(id1, id2, gid1, gid2, output)
  
  return(index)
}

writeIso_g_Definition1 <- function(id_small, id_large, g_small, g_large, output){
  
  
  if(!file.exists(output)){
    file.create(output)
  }
  
  
  sub_list <-getSubgraph_all_comb(g_small)
  
  k <- 1
  index <- 1
  result <- list()
  
  sub_num <- length(sub_list)
  
  while(k<=sub_num){
    sub_k <- sub_list[[k]]
    
    #vcolor1 = sapply(V(g_large)$Field, getVInt_Field)
    #vcolor2 = sapply(V(sub_k)$Field, getVInt_Field)
    
    iso <- graph.subisomorphic.vf2(g_large, sub_k, 
                                   vertex.color1=as.numeric(V(g_large)$Hypo), 
                                   vertex.color2=as.numeric(V(sub_k)$Hypo))
    
    if(!iso$iso){
      k <- k+1
      next
    }
    
    #if the subgraphs are isomorphic, then store this subgraph
   
    new_features <- numeric(0)
    new_features <- c(new_features, id_small, id_large)
    graph_features <- getFeatures(sub_k)
    new_features <- c(new_features, graph_features)
    
    
    # set of vetices in sub_k
    v_set <- paste(V(sub_k)$name, collapse=";")
    new_features <- c(new_features, v_set)
    
    result[[index]]<-new_features
    index <- index+1
    
    k<-k+1
  }
  
  
  #write to file
  resultMatrix <- do.call(rbind, result)
  write.table(resultMatrix, file=output, sep=",")
  
  return(index)
}



writeIso_Definition2<- function(id1, id2){ 
  
  gid1 <- createGraph_Definition2(id1)
  gid2 <- createGraph_Definition2(id2)
  
  pair <- paste(id1, id2, sep="_")
  output <- paste("SubGraph//Definition2", pair, sep="//")
  output <- paste(output, "csv", sep=".")
  if(!file.exists(output)){
    file.create(output)
  }

  index <- writeIso_g_Definition2(id1, id2, gid1, gid2, output)
  
  return(index)
}

writeIso_g_Definition2 <- function(id_small, id_large, g_small, g_large, output){

  if(!file.exists(output)){
    file.create(output)
  }

  sub_list <-getSubgraph_all_comb(g_small)
  
  k <- 1
  index <- 1
  result <- list()
  
  sub_num <- length(sub_list)
  
  while(k<=sub_num){
    sub_k <- sub_list[[k]]
    
    iso <- graph.subisomorphic.vf2(g_large, sub_k, 
                                   vertex.color1=as.numeric(V(g_large)$Hypo), 
                                   vertex.color2=as.numeric(V(sub_k)$Hypo),
                                   edge.color1 = E(g_large)$type,
                                   edge.color2 = E(sub_k)$type
                                  )
    if(!iso$iso){
      k <- k+1
      next
    }
    
    #if the subgraphs are isomorphic, then store this subgraph   
    new_features <- numeric(0)
    new_features <- c(new_features, id_small, id_large)
    graph_features <- getFeatures(sub_k)
    new_features <- c(new_features, graph_features)
  
    # set of vetices in sub_k
    v_set <- paste(V(sub_k)$name, collapse=";")
    new_features <- c(new_features, v_set)
    
    result[[index]]<-new_features
    index <- index+1
    
    k<-k+1
  }

  #write to file
  resultMatrix <- do.call(rbind, result)
  write.table(resultMatrix, file=output, sep=",")
  
  return(index)
}

