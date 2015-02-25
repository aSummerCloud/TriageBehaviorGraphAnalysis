



#generate all interesting subgraphs of a given graph
getSubgraph_shortestPath <- function(g1){
  
  #(1) Gain all the subg
  allsubg <- c() #all the subgraphs of g1
  #decompose the graph
  comps <- decompose.graph(g1, min.vertices=2)
  len_comps <- length(comps)
  if(len_comps>0){
    for(n in 1:len_comps){
      subg<-comps[[n]]
      
      sortedname<-sort(as.numeric(V(subg)$name), decreasing=TRUE)
      
      leafnodes <- sapply(V(subg), function(x) length(neighbors(subg,x))==0)
      morethanone <-function(x) length(x)>1
      
      len <- length(sortedname)
      
      if(len > 1){
        for(i in 1:len){
          vi<-toString(sortedname[i]);
          root<-vi
          paths<-get.all.shortest.paths(subg, V(subg)[root], leafnodes)$res
          
          len_paths <- length(paths)
          
          j<-1;
          while(j<=len_paths){
            subvj <- paths[[j]]
            if(length(subvj) >2){
              #subj <- induced.subgraph(graph=sim, vids=subvj)
              allsubg <- c(allsubg, paths[j])
            }
            j <- j+1
          }
        }
      }
    }
  }#comps
  
  return(allsubg)
}


#All possible subgraph (more than 3 nodes, connected)
getSubgraph_all_comb <- function(g1){
  
  allsubg <- list() #all the subgraphs of g1
  #decompose the graph
  comps <- decompose.graph(g1, min.vertices=2)
  len_comps <- length(comps)
  if(len_comps>0){
    for(n in 1:len_comps){
      
      subg<-comps[[n]]
      
      len_sub <- length(V(subg))
      
      if(len_sub <= 3)
        next
      
      for(c in 3:len_sub){
        
        #if(c > len_sub)
        # next
        
        # choose c from total len_sub
        com_num <- choose(len_sub, c)
        
        cc <- combn(V(subg), c)
        
        # add each combination to the result set
        if(ncol(cc) > 0){
          for(ncol_i in 1:ncol(cc)){
            v_set <- cc[, ncol_i]
            induced_graph <- induced.subgraph(subg, v_set)
            if(length(V(induced_graph)[degree(induced_graph)==0]) == 0)
              allsubg <- c(allsubg, list(induced_graph))
          }
        }
      }
      
    }
  }#comps
  
  return(allsubg)
}



getSubgraph_by_Vs_undirected_unweighted=function(id, vset_list){

  
  current_g <- createGraph_Undirected_Unweighted (id)
  vset_list <- unique(vset_list)
  
  result <- list()
  for(i in 1: length(vset_list)){
    current_vset <- vset_list[[i]]
    arraylist <- strsplit(current_vset, "[;]")
    vset <- as.numeric(arraylist[[1]])
    induced_graph <- induced.subgraph(current_g, vset)
    
    title <- paste(id, i, sep="-")
    mypath1 <- getPlotPath("SubGraph_Plot", title)
    jpeg(file=mypath1)  
    plotGraph_Undirected_Unweight_Title(induced_graph, title)
    dev.off()
    
  }
  
  return(TRUE)
}


getSubgraph_by_Vs_directed_weighted=function(id, vset_list){
  
  
  current_g <- createGraph_Directed_Weighted (id)
  vset_list <- unique(vset_list)
  
  result <- list()
  for(i in 1: length(vset_list)){
    current_vset <- vset_list[[i]]
    arraylist <- strsplit(current_vset, "[;]")
    vset <- as.numeric(arraylist[[1]])
    induced_graph <- induced.subgraph(current_g, vset)
    
    title <- paste(id, i, sep="-")
    mypath1 <- getPlotPath("SubGraph_Plot", title)
    jpeg(file=mypath1)  
    plotGraph_Directed_Weighted_Title(induced_graph, title)
    dev.off()
    
  }
  
  return(TRUE)
}


filterSubgraph_Definition2=function(id, vset_list){
   
  result <- list()
  for(i in 1: length(vset_list)){
    
    id <- id_list[[i]]
    current_vset <- vset_list[[i]]
    arraylist <- strsplit(current_vset, "[;]")
    vset <- as.numeric(arraylist[[1]])
    
    
    current_g <- createGraph_Definition2(id)
    induced_graph <- induced.subgraph(current_g, vset)
    
    title <- paste(id, current_vset, sep="-")
    mypath1 <- getPlotPath("SubGraph_Plot", title)
    jpeg(file=mypath1)
    plotGraph_Directed_Weighted_Title(induced_graph, title)
    dev.off()
    
  }
  
  return(TRUE)
}



getLongestPaths <- function(g){
  path_list = list()
  
  sortedname<-sort(as.numeric(V(g)$name), decreasing=TRUE)
  len <- length(sortedname)
  if(len > 1){
    for(i in len:2){
      #root
      vi_name<-toString(sortedname[i])
      vi = which(V(new_g)$name == vi_name)
      vector=c(vi_name)
      path_list = getPath(vi,g,vector,path_list) 
    }
  }
  return(path_list)
}


getPath=function(root,g,vector,list){
  
  neis <- neighbors(g,root,mode="out")
  
  if(length(neis)>0){
    for(k in 1:length(neis)){
      vk <- neis[k]
      temp <- c(vector)
      temp <- c(temp,V(g)[vk]$name)
      list = getPath(vk,g,temp, list)     
    }
  }else{
    if(length(vector)==0 || vector_exist_in_list(vector,list)){
      return (list)
    }else{
      list_len = length(list)
      index = list_len+1
      temp = c(vector)
      list[[index]] = temp
      return (list)
    }
  }
}


getallPath=function(g){
  
  plist = list()
  
  sortedname<-sort(as.numeric(V(g)$name), decreasing=TRUE)
  index = 1
  
  len <- length(sortedname)
  if(len > 1){
    for(i in len:1){
      vi_name<-toString(sortedname[i])
      vi = which(V(g)$name == vi_name)
     
      for(j in 1:len){
        if(i != j){
          vj_name<-toString(sortedname[j])
          vj = which(V(g)$name == vj_name)
          paths = get.all.shortest.paths(g, vi, vj, mode="out")$res
          if(length(paths) != 0 ){
            plist[[index]] = paths[[1]]
            index =index+1
          }
          
        }
      }
      
    }
  }
  return(plist)
}

getCliqueNames=function(g, cliques){
  len = length(cliques)
  names = list()
  index = 1
  if(len > 0){
    for(i in 1:len){
      names[[index]] = V(g)[cliques[[i]]]$name
      index = index+1
    }
  }
  return(names)
}
