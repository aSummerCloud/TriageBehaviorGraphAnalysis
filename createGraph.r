library("igraph")
#todo: 
#(1) draw subgraph
#(2) simplify graph by "sub-sub"--> sub  com-sub-->som
#(3) exact com (4+3)

#create a graph from a file
createGraph_Directed_Weighted <- function(subID){
  
  nd <- read.csv(getNodeFilename(subID), header = TRUE, sep = ",")
  nd <- na.omit(nd) #remove the rows that contain NA
  g <- graph.empty(directed=TRUE) + vertices(nd$Index)
  
  
  V(g)$Index = as.character(nd$Index)
  V(g)$NFound = as.character(nd$NotFound)
  V(g)$Hypo = as.character(nd$HypoGenerated)
  
  V(g)$color = V(g)$Hypo
  V(g)$color = gsub("1", "red", V(g)$color)
  V(g)$color = gsub("0", "blue", V(g)$color)
  
  V(g)$shape = V(g)$NFound
  V(g)$shape = gsub("1", "square", V(g)$shape)
  V(g)$shape = gsub("0", "circle", V(g)$shape)
  
  V(g)$name=V(g)$Index
  
  #V(g)$NFound = as.character(nd$NotFound[match(V(g)$name, nd$Index)])
  #V(g)$Hypo = as.character(nd$HypoGenerated[match(V(g)$name, nd$Index)])
  
  el <- read.csv(getFilename(subID), header = TRUE, sep = ",");
  #el[,1] <- as.character(el[,1])
  #el[,2] <- as.character(el[,2])
  #el <- as.matrix(el) #igraph needs the edgelist to be in matrix format
  #g <- graph.edgelist(el[,1:2]) #We first greate a network from the first two columns, which has the list of vertices
  #E(g)$weight=as.numeric(el[,3][el[,1] %--% el[,2]]) #We then add the edge weights to this network by assigning an edge attribute called 'weight'
  
  for(i in 1:nrow(el)){
    g <- g + edges(c(as.character(el[i,1]), as.character(el[i,2])), weight = as.numeric(el[i,3]))
  }
  

  #
  #if(edge_type == 1){
  #  color = C_IS_EQUAL;
  #}else if(edge_type == 2){
  #  color = C_IS_SUB;
  #}else if(edge_type == 3){
  #  color = C_IS_COM;
  #}else if(edge_type == 4){
  #  color = C_LINK;
  
  E(g)$color = sapply(E(g)$weight, function(x) x%%4)
  E(g)$color = gsub("1", "green", E(g)$color)
  E(g)$color = gsub("2", "blue", E(g)$color)
  E(g)$color = gsub("3", "orange", E(g)$color)
  E(g)$color = gsub("0", "red", E(g)$color)
  
  E(g)$width = sapply(E(g)$weight, function(x) (floor((x-1)/4)*3+1))
  
  #nd <- read.csv(getNodeFilename(subID), header = TRUE, sep = ",");
  #V(g)$NFound = as.character(nd$NotFound[match(V(g)$name, nd$Index)])
  #V(g)$Hypo = as.character(nd$HypoGenerated[match(V(g)$name, nd$Index)])
  
  V(g)$name=V(g)$color
  
  return(g)
}








getVColor_Field <- function(field){
  cols <- rainbow(11)
  
  color <- "grey"
  
  if (identical(field, "datatime"))
    color <- cols[1]
  else if (identical(field, "description"))
    color <- cols[2]
  else if (identical(field, "direction"))
    color <- cols[3]
  else if (identical(field, "category"))
    color <- cols[4]
  else if (identical(field, "message"))
    color <- cols[5] 
  else if (identical(field, "operation"))
    color <- cols[6]
  else if (identical(field, "port"))
    color <- cols[7]
  else if (identical(field, "priority"))
    color <- cols[8]
  else if (identical(field, "protocol"))
    color <- cols[9]
  else if (identical(field, "ip"))
    color <- cols[10]
  else if (identical(field, "service"))
    color <- cols[11]
  
  return (color)
  
}



getVInt_Field <- function(field){
  cols <- c(1,2,3,4,5,6,7,8,9,10,11,12)
  
  color <- "grey"
  
  if (identical(field, "datatime"))
    color <- cols[1]
  else if (identical(field, "description"))
    color <- cols[2]
  else if (identical(field, "direction"))
    color <- cols[3]
  else if (identical(field, "category"))
    color <- cols[4]
  else if (identical(field, "message"))
    color <- cols[5] 
  else if (identical(field, "operation"))
    color <- cols[6]
  else if (identical(field, "port"))
    color <- cols[7]
  else if (identical(field, "priority"))
    color <- cols[8]
  else if (identical(field, "protocol"))
    color <- cols[9]
  else if (identical(field, "ip"))
    color <- cols[10]
  else if (identical(field, "service"))
    color <- cols[11]
  else
    color <- cols[12]
  
  return (color)
  
}





#create an undirected graph from a file
createGraph_Undirected_Unweighted <- function(subID){
  
  filename <- getNodeFilename(subID)
  
  nd <- read.csv(filename, header = TRUE, sep = ",")
  nd <- na.omit(nd) #remove the rows that contain NA
  g <- graph.empty(directed=FALSE) + vertices(nd$Index)
  
  #V(g)$name = as.character(nd$Index)
  #V(g)$NFound = as.character(nd$NotFound)
  #V(g)$Hypo = as.character(nd$HypoGenerated)
  V(g)$Field = as.character(nd$Field)
  
  
  #V(g)$label=paste(V(g)$name, V(g)$Field,sep=':')
  
  V(g)$color = sapply(V(g)$Field, getVColor_Field)
  
  
  el <- read.csv(getEdgeFilename_Undirected_Unweighted(subID), header = TRUE, sep = ",");

  for(i in 1:nrow(el)){
    g <- g + edges(c(as.character(el[i,1]), as.character(el[i,2])), weight = as.numeric(el[i,3]))
  }

  
  return(g)
}


#definition0
createGraph_Definition0<- function(subID){
  
  nd <- read.csv(getNodeFilename(subID), header = TRUE, sep = ",")
  nd <- na.omit(nd) #remove the rows that contain NA
  g <- graph.empty(directed=TRUE) + vertices(nd$Index)
  
  V(g)$Index = as.character(nd$Index)
  V(g)$name=V(g)$Index
  V(g)$Field = as.character(nd$Field)  
  V(g)$color = sapply(V(g)$Field, getVColor_Field)
  
  el <- read.csv(getEdgeFilename_Definition0(subID), header = TRUE, sep = ",");
  
  for(i in 1:nrow(el)){
    g <- g + edges(c(as.character(el[i,1]), as.character(el[i,2])), weight = as.numeric(el[i,3]))
  }
  
# no edge color
  
  
  return(g)
}

#definition1
createGraph_Definition1<- function(subID){
  
  nd <- read.csv(getNodeFilename(subID), header = TRUE, sep = ",")
  nd <- na.omit(nd) #remove the rows that contain NA
  g <- graph.empty(directed=TRUE) + vertices(nd$Index)
  
  V(g)$Index = as.character(nd$Index)
  V(g)$name=V(g)$Index
  
  V(g)$Hypo = as.character(nd$HypoGenerated)
  
  V(g)$color = V(g)$Hypo
  V(g)$color = gsub("1", "red", V(g)$color)
  V(g)$color = gsub("0", "blue", V(g)$color)
  
  el <- read.csv(getEdgeFilename_Definition1(subID), header = TRUE, sep = ",");
  
  for(i in 1:nrow(el)){
    g <- g + edges(c(as.character(el[i,1]), as.character(el[i,2])), weight = as.numeric(el[i,3]))
  }
  
  # no edge color
  
  
  return(g)
}

#definition01
createGraph_Definition01<- function(subID){
  
  g=createGraph_Condition0(subID, FALSE, FALSE, TRUE)
  return (g)
  
}

#definition2
createGraph_Definition2<- function(subID){
  
  g=createGraph_Condition0(subID, TRUE, FALSE, TRUE)
  return (g)

}


#definition3
createGraph_Definition3<- function(subID){
  
  g=createGraph_Condition0(subID, FALSE, TRUE, TRUE)
  return (g)
}


#definition4
createGraph_Definition4<- function(subID){
  
  g=createGraph_Condition0(subID, TRUE, TRUE, TRUE)
  return (g)
}

#logic edges
createGraph_Condition0<- function(subID, hypo, nfound, edgetype){
  
  nd <- read.csv(getNodeFilename(subID), header = TRUE, sep = ",")
  nd <- na.omit(nd) #remove the rows that contain NA
  g <- graph.empty(directed=TRUE) + vertices(nd$Index)
  
  
  V(g)$Index = as.character(nd$Index)
  V(g)$Field = as.character(nd$Field)
  
  if(nfound){
    V(g)$NFound = as.character(nd$NotFound)
    V(g)$shape = V(g)$NFound
    V(g)$shape = gsub("1", "square", V(g)$shape)
    V(g)$shape = gsub("0", "circle", V(g)$shape)
  }
  if(hypo){
    V(g)$Hypo = as.character(nd$HypoGenerated)
    V(g)$color = V(g)$Hypo
    V(g)$color = gsub("1", "red", V(g)$color)
    V(g)$color = gsub("0", "blue", V(g)$color)
  }
  
  
  
  #V(g)$name=V(g)$Index
  
  el <- read.csv(getEdgeFilename_Definition0(subID), header = TRUE, sep = ",");
  
  for(i in 1:nrow(el)){
    g <- g + edges(c(as.character(el[i,1]), as.character(el[i,2])), weight = as.numeric(el[i,3]))
  }
  if(edgetype){
    E(g)$type = sapply(E(g)$weight, function(x) x%%4)
    E(g)$color = sapply(E(g)$weight, function(x) x%%4)
    E(g)$color = gsub("1", "green", E(g)$color)
    E(g)$color = gsub("2", "blue", E(g)$color)
    E(g)$color = gsub("3", "orange", E(g)$color)
    E(g)$color = gsub("0", "red", E(g)$color)
  }
  
  
  
  return(g)
}


#simpified the result iso subgraphs of pattern1
createGraph_SubSeq<- function(id,filepath){
  
  nd <- read.csv(filepath, header = TRUE, sep = ",")
  nd <- as.matrix(na.omit(nd))#remove the rows that contain NA
  g <- graph.empty(directed=TRUE)
  
  n=ncol(nd)
  id_list = nd[,n-1]
  e_list = nd[,n] 
  n = length(e_list)

  for(i in 1:n){
    current_id = id_list[[i]]
    current_vset = e_list[[i]]
    
    if(identical(current_id,id)){
      arraylist <- strsplit(current_vset, "[;]")
      vset <- as.numeric(arraylist[[1]])
      if(!is.element(vset[1],V(g)$name))
        
        g = g+vertices(vset[1])
      if(!is.element(vset[2],V(g)$name))
        g = g+vertices(vset[2])
      
      vid1 = which(V(g)$name == vset[1])
      vid2 = which (V(g)$name == vset[2])
      g = g+edges(vid1,vid2, weight=2)
    }
  }
  
  #simg = removeDup_SingleType(g)
  
  
  
  return(g)
}


#simpified the result iso subgraphs of pattern1
createSubGraph<- function(id,filepath){
  
  nd <- read.csv(filepath, header = TRUE, sep = ",")
  nd <- as.matrix(na.omit(nd))#remove the rows that contain NA
  g <- graph.empty(directed=TRUE)
  
  n=ncol(nd)
  id = "p1"
  v_set_list = nd[,n] 
  n = length(v_set_list)
  
  g = createGraph_Definition2(id)
  
  for(i in 1:n){
    current_id = id
    current_vset = v_set_list[[i]]
    

      arraylist <- strsplit(current_vset, "[;]")
      vset <- as.numeric(arraylist[[1]])
    #subj <- induced.subgraph(graph=sim, vids=subvj)
    allsubg <- c(allsubg, paths[j])
    
      vid_st = v(g)[identical(V(g)$name, vid_st)]
      
      vid1 = which(V(g)$name == vset[1])
      vid2 = which (V(g)$name == vset[2])
      g = g+edges(vid1,vid2, weight=2)
    }
  
  
  #simg = removeDup_SingleType(g)
  
  
  
  return(g)
}

