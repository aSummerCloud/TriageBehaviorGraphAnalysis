library("igraph")




getISO <- function(id1, id2){
  g1 <- createGraph(id1)
  g2 <- createGraph(id2)
  
  allsubg <-getSubgraph(g1)
  k <- 1
  index <- 1
  result<-list()
  while(k<=length(allsubg)){
    sub_k <- induced.subgraph(graph=g1, vids=allsubg[[k]])
    iso <- graph.subisomorphic.vf2(g2, sub_k, edge.color1 = E(g2)$color, edge.color2=E(sub_k)$color)
    
    if(!iso$iso){
      k < k+1
        next
    }
      
      title1 <- paste(id2, index, sep=" ")
      title2 <- paste(id1, index, sep=" ")
      
     
      a<-iso$map12
      b<-iso$map21
    
    
      suba<-induced.subgraph(graph=g2, vids=V(g2)[a])
    plotGraph_Title(suba, title1)
      
     # 
     # subb<-induced.subgraph(graph=sub_k, vids=b)
    plotGraph_Title(sub_k, title2)
      
      result[[index]]<-iso
      index <- index+1
  
    k<-k+1
  }
  return(result)
}





getISO_Undirected_Unweighted <- function(id1, id2){
  
  
  pairname <- paste(id1, id2, sep="-")
  
  path <- getPlotPath(pairname)
  
  if(!file.exists(path))
    dir.create(path)

  gid1 <- createGraph_Undirected_Unweighted(id1)
  gid2 <- createGraph_Undirected_Unweighted(id2)
  
  lenid1 <- length(V(gid1))
  lenid2 <- length(V(gid2))
  
  g_large <- graph.empty(directed=FALSE)
  g_small <- graph.empty(directed=FALSE)
  id_large <- ""
  id_small <-""
  
  if(lenid1 > lenid2){
    g_small <- gid2
    g_large <- gid1
    
    id_small <- id2
    id_large <- id1
    
  }else{
    g_small <- gid1
    g_large <- gid2
    
    id_small <- id1
    id_large <- id2
  }
  
  sub_list <-getSubgraph_all_comb(g_small)
  
  k <- 1
  index <- 1
  result<-list()
  
  sub_num <- length(sub_list)
  
  while(k<=sub_num){
    sub_k <- sub_list[[k]]
    
    color1 = sapply(V(g_large)$Field, getVInt_Field)
    color2 = sapply(V(sub_k)$Field, getVInt_Field)
    
    iso <- graph.subisomorphic.vf2(g_large, sub_k, vertex.color1=color1, vertex.color2=color2)
    
    if(!iso$iso){
      k <- k+1
      next
    }

    title_large <- paste(index, id_large, sep="-")
    title_small <- paste(index, id_small, sep="-")
    
    
    a<-iso$map21
    
    suba<-induced.subgraph(graph=g_large, vids=a)
    
    mypath1 <- getMyPath("plot", pairname, title_large)
    jpeg(file=mypath1)  
    plotGraph_Undirected_Unweight_Title(suba, title_large)
    dev.off()
    
    mypath2 <- getMyPath("plot", pairname, title_small)
    jpeg(file=mypath2) 
    plotGraph_Undirected_Unweight_Title(sub_k, title_small)
    dev.off()
    
    result[[index]]<-iso
    index <- index+1
    
    k<-k+1
  }
  return(result)
}



subIso_FeatureVector_Undirected_Unweighted <- function(id1, id2, featureVectors, startindex){
  
  
  pairname <- paste(id1, id2, sep="-")
  
  path <- getPlotPath(pairname)
  
  if(!file.exists(path))
    dir.create(path)
  
  gid1 <- createGraph_Undirected_Unweighted(id1)
  gid2 <- createGraph_Undirected_Unweighted(id2)
  
  lenid1 <- length(V(gid1))
  lenid2 <- length(V(gid2))
  
  g_large <- graph.empty(directed=FALSE)
  g_small <- graph.empty(directed=FALSE)
  id_large <- ""
  id_small <-""
  
  if(lenid1 > lenid2){
    g_small <- gid2
    g_large <- gid1
    
    id_small <- id2
    id_large <- id1
    
  }else{
    g_small <- gid1
    g_large <- gid2
    
    id_small <- id1
    id_large <- id2
  }
  
  sub_list <-getSubgraph_all_comb(g_small)
  
  k <- 1
  index <- startindex
  result <- list()
  result <- featureVectors
  
  sub_num <- length(sub_list)
  
  while(k<=sub_num){
    sub_k <- sub_list[[k]]
    
    color1 = sapply(V(g_large)$Field, getVInt_Field)
    color2 = sapply(V(sub_k)$Field, getVInt_Field)
    
    iso <- graph.subisomorphic.vf2(g_large, sub_k, vertex.color1=color1, vertex.color2=color2)
    
    if(!iso$iso){
      k <- k+1
      next
    }
    
    #if the subgraphs are isomorphic, then store this subgraph
    a<-iso$map21
  
    #suba<-induced.subgraph(graph=g_large, vids=a)
    #sub_k
    new_features <- numeric(0)
    new_features <- c(new_features, id_small, id_large)
    graph_features <- getFeatures(sub_k)
    new_features <- c(new_features, graph_features)
    
    
    # set of vetices in sub_k
    v_set <- paste(V(sub_k), collapse=";")
    new_features <- c(new_features, v_set)
    
    result[[index]]<-new_features
    index <- index+1
    
    k<-k+1
  }
  return(result)
}





writeIso_FeatureVector_Undirected_Unweighted <- function(id1, id2){

  
  
  gid1 <- createGraph_Undirected_Unweighted(id1)
  gid2 <- createGraph_Undirected_Unweighted(id2)
  
  lenid1 <- length(V(gid1))
  lenid2 <- length(V(gid2))
  
  g_large <- graph.empty(directed=FALSE)
  g_small <- graph.empty(directed=FALSE)
  id_large <- ""
  id_small <-""
  
  if(lenid1 > lenid2){
    g_small <- gid2
    g_large <- gid1
    
    id_small <- id2
    id_large <- id1
    
  }else{
    g_small <- gid1
    g_large <- gid2
    
    id_small <- id1
    id_large <- id2
  }
  
  pair <- paste(id_small, id_large, sep="_")
  output <- paste("SubGraph", pair, sep="//")
  output <- paste(output, "csv", sep=".")
  if(!file.exists(output)){
    file.create(output)
  }
  
  index <- writeIso_g_FeatureVector_Undirected_Unweighted(id_small, id_large, g_small, g_large, output)
 return (index)
}



writeIso_Ordered_FeatureVector_Undirected_Unweighted <- function(id1, id2){
  

  
  gid1 <- createGraph_Undirected_Unweighted(id1)
  gid2 <- createGraph_Undirected_Unweighted(id2)
  
  lenid1 <- length(V(gid1))
  lenid2 <- length(V(gid2))


  pair <- paste(id1, id2, sep="_")
  output <- paste("SubGraph", pair, sep="//")
  output <- paste(output, "csv", sep=".")
  if(!file.exists(output)){
    file.create(output)
  }
  
  
  index <- writeIso_g_FeatureVector_Undirected_Unweighted(id1, id2, gid1, gid2, output)
  
  return(index)
}



writeIso_g_FeatureVector_Undirected_Unweighted <- function(id_small, id_large, g_small, g_large, output){
  
 
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
    
    color1 = sapply(V(g_large)$Field, getVInt_Field)
    color2 = sapply(V(sub_k)$Field, getVInt_Field)
    
    iso <- graph.subisomorphic.vf2(g_large, sub_k, vertex.color1=color1, vertex.color2=color2)
    
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
    v_set <- paste(V(sub_k), collapse=";")
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






writeIso_Ordered_FeatureVector_directed_weighted <- function(id1, id2){
  

  
  gid1 <- createGraph_Directed_Weighted(id1)
  gid2 <- createGraph_Directed_Weighted(id2)
  
  
  
  
  pair <- paste(id1, id2, sep="_")
  output <- paste("SubGraph", pair, sep="//")
  output <- paste(output, "csv", sep=".")
  if(!file.exists(output)){
    file.create(output)
  }
  
  
  index <- writeIso_g_FeatureVector_directed_weighted(id1, id2, gid1, gid2, output)
  
  return(index)
}

writeIso_g_FeatureVector_directed_weighted <- function(id_small, id_large, g_small, g_large, output){
  
  
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
                                   vertex.color2=as.numeric(V(sub_k)$Hypo),
                                   edge.color1 = E(g_large)$weight,
                                   edge.color2=E(sub_k)$weight)
    
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
    v_set <- paste(V(sub_k), collapse=";")
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

