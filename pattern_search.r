writePattern_Definition2=function(pattern_id,id_list){
  
  pattern<-getPattern_Definition2(pattern_id)
  
  index <- 1
  result <- list()
  
  n <- length(id_list)
  for(i in 1:n){
    
    current_id <- id_list[[i]]
    current_g <- createGraph_Definition2(current_id)
    
    plotGraph_Definition2(current_g, current_id)
    
    new_features <- c()
    new_features <- c(new_features, current_id)
    
    #definition 2
    mapping <- graph.get.subisomorphisms.vf2(current_g, pattern,
                                             vertex.color1=as.numeric(V(current_g)$Hypo), 
                                             vertex.color2=as.numeric(V(pattern)$Hypo),
                                             edge.color1 = E(current_g)$type,
                                             edge.color2=E(pattern)$type)    
    #mapping <- graph.get.subisomorphisms.vf2(current_g, pattern)
    
    if(length(mapping)>0){
      
      for(mapi in 1: length(mapping)){
        current_map = mapping[[mapi]]
        
        #if(any(is.na(names(current_map))))
        #next
        current_features = c(new_features)
        
        vset = as.vector(current_map)
        
        # suba<-induced.subgraph(graph=current_g, vids=V(current_g)[vset])
        
        
        v_set <- paste(vset, collapse=";")
        current_features <- c(current_features, v_set)
        
        result[[index]]<-current_features
        index <- index+1
        
      }
    }
    
  }
  #write to file
  output = "SubGraph//pattern_definition2.csv"
  if(!file.exists(output)){
    file.create(output)
  }
  
  resultMatrix <- do.call(rbind, result)
  write.table(resultMatrix, file=output, sep=",")
  
  return(index)
  
}

writePattern_Definition3=function(pattern_id,id_list){
  
  pattern<-getPattern_Definition3(pattern_id)
  
  index <- 1
  result <- list()
  
  n <- length(id_list)
  for(i in 1:n){
    
    current_id <- id_list[[i]]
    #definition3
    current_g <- createGraph_Definition3(current_id)
    plotGraph_Definition3(current_g, current_id)
    
    new_features <- c()
    new_features <- c(new_features, current_id)
    
    #definition 3
    mapping <- graph.get.subisomorphisms.vf2(current_g, pattern,
                                             vertex.color1=as.numeric(V(current_g)$NFound), 
                                             vertex.color2=as.numeric(V(pattern)$NFound),
                                             edge.color1 = E(current_g)$type,
                                             edge.color2=E(pattern)$type)    
    #mapping <- graph.get.subisomorphisms.vf2(current_g, pattern)
    
    if(length(mapping)>0){
      
      for(mapi in 1: length(mapping)){
        current_map = mapping[[mapi]]
        
        #if(any(is.na(names(current_map))))
        #next
        current_features = c(new_features)
        
        vset = as.vector(current_map)
        
        # suba<-induced.subgraph(graph=current_g, vids=V(current_g)[vset])
        
        
        v_set <- paste(vset, collapse=";")
        current_features <- c(current_features, v_set)
        
        result[[index]]<-current_features
        index <- index+1
        
      }
    }
    
  }
  #write to file
  output = "SubGraph//pattern_definition3.csv"
  if(!file.exists(output)){
    file.create(output)
  }
  
  resultMatrix <- do.call(rbind, result)
  write.table(resultMatrix, file=output, sep=",")
  
  return(index)
  
}



writePattern_Definition4=function(pattern_id,id_list){
  #definition 4
  pattern<-getPattern_Definition4(pattern_id)
  
  index <- 1
  result <- list()
  
  n <- length(id_list)
  for(i in 1:n){
    
    current_id <- id_list[[i]]
    #definition 4
    current_g <- createGraph_Definition4(current_id)
    plotGraph_Definition4(current_g, current_id)
    
    new_features <- c()
    new_features <- c(new_features, current_id)
    
    #definition 4
    color1 = as.numeric(paste(V(current_g)$Hypo, V(current_g)$NFound, sep=""))
    color2 = as.numeric(paste(V(pattern)$Hypo, V(pattern)$NFound, sep=""))
    
    mapping <- graph.get.subisomorphisms.vf2(current_g, pattern,
                                             vertex.color1=color1, 
                                             vertex.color2=color2,
                                             edge.color1 = E(current_g)$type,
                                             edge.color2=E(pattern)$type)    
    #mapping <- graph.get.subisomorphisms.vf2(current_g, pattern)
    
    if(length(mapping)>0){
      
      for(mapi in 1: length(mapping)){
        current_map = mapping[[mapi]]
        
        current_features = c(new_features)
        
        vset = as.vector(current_map)
          
        v_set <- paste(vset, collapse=";")
        current_features <- c(current_features, v_set)
        
        result[[index]]<-current_features
        index <- index+1
        
      }
    }
    
  }
  #write to file
  output = "SubGraph//pattern_definition4.csv"
  if(!file.exists(output)){
    file.create(output)
  }
  
  resultMatrix <- do.call(rbind, result)
  write.table(resultMatrix, file=output, sep=",")
  
  return(index)
  
}


writePattern_Definition01=function(pattern_id,id_list){
  
  pattern<-getPattern_Definition01(pattern_id)
  
  index <- 1
  result <- list()
  
  n <- length(id_list)
  for(i in 1:n){
    
    current_id <- id_list[[i]]
    #definition01
    current_g <- createGraph_Definition01(current_id)
    plotGraph_Definition3(current_g, current_id)
    
    new_features <- c()
    new_features <- c(new_features, current_id)
    
    #definition 01
    mapping <- graph.get.subisomorphisms.vf2(current_g, pattern,
                                             edge.color1 = E(current_g)$type,
                                             edge.color2=E(pattern)$type)    
    #mapping <- graph.get.subisomorphisms.vf2(current_g, pattern)
    
    if(length(mapping)>0){
      
      for(mapi in 1: length(mapping)){
        current_map = mapping[[mapi]]
        

        current_features = c(new_features)
        
        vset = as.vector(current_map)
        v_set <- paste(vset, collapse=";")
        current_features <- c(current_features, v_set)
        
        result[[index]]<-current_features
        index <- index+1
        
      }
    }
    
  }
  #write to file
  output = "SubGraph//pattern_definition01.csv"
  if(!file.exists(output)){
    file.create(output)
  }
  
  resultMatrix <- do.call(rbind, result)
  write.table(resultMatrix, file=output, sep=",")
  
  return(index)
  
}


