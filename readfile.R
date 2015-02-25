library("igraph")


#create a graph from a file
creatGraph <- function(subID){
  el <- read.csv(getFilename(subID), header = TRUE, sep = ",");
  el[,1] <- as.character(el[,1])
  el[,2] <- as.character(el[,2])
  el <- as.matrix(el) #igraph needs the edgelist to be in matrix format
  g <- graph.edgelist(el[,1:2]) #We first greate a network from the first two columns, which has the list of vertices
  E(g)$weight=as.numeric(el[,3]) #We then add the edge weights to this network by assigning an edge attribute called 'weight'
  
  #
  #if(edge_type == 1){
  #  color = C_IS_EQUAL;
  #}else if(edge_type == 2){
  #  color = C_IS_SUB;
  #}else if(edge_type == 3){
  #  color = C_IS_COM;
  #}else if(edge_type == 4){
  #  color = C_LINK;
  
  E(g)$color = E(g)$weight
  E(g)$color = gsub("1", "green", E(g)$color)
  E(g)$color = gsub("2", "blue", E(g)$color)
  E(g)$color = gsub("3", "orange", E(g)$color)
  E(g)$color = gsub("4", "red", E(g)$color)
  
  nd <- read.csv(getNodeFilename(subID), header = TRUE, sep = ",");
  V(g)$NFound = as.character(nd$NotFound[match(V(g)$name, nd$Index)])
  V(g)$Hypo = as.character(nd$HypoGenerated[match(V(g)$name, nd$Index)])
  
  V(g)$color = V(g)$Hypo
  V(g)$color = gsub("1", "red", V(g)$color)
  V(g)$color = gsub("0", "blue", V(g)$color)
  
  V(g)$shape = V(g)$NFound
  V(g)$shape = gsub("1", "square", V(g)$shape)
  V(g)$shape = gsub("0", "circle", V(g)$shape)
  
  return(g)
}

#get the whole path of a subject
getFilename <-function(subID){
  filename <- "C:\\Users\\ASummer\\Google Drive\\Project\\Work\\Filter Graph Analysis\\output\\"
  result <- paste(filename,subID,sep="")
  result <- paste(result,".csv",sep="")
  return(result)
}

getNodeFilename <-function(SubID){
  filename <- "C:\\Users\\ASummer\\Google Drive\\Project\\Work\\Filter Graph Analysis\\data\\"
  result <- paste(filename,subID,sep="")
  result <- paste(result,".csv",sep="")
  return(result)
}


plotGraph<-function(gg){
  plot(gg, layout=layout.circle, vertex.size=8, vertex.label=V(g)$name,
       vertex.label.dist=0.8, edge.arrow.size=0.2)
}

plotIsoGraph <-function(graph1, graph2, iso, index){
  a<-iso[[index]]$map12
  suba<-induced.subgraph(graph=graph1, vids=a)
  plotGraph(suba)
  
  b<-iso[[index]]$map21
  subb<-induced.subgraph(graph=graph2, vids=b)
  plotGraph(subb)
}

getVcount <- function(subID){
  f <- getFilename(subID)
  g<-creatGraph (f)
  E(g)$color <- E(g)$weight
  return(vcount(g))
}

getEcount <- function(subID){
  f <- getFilename(subID)
  g<-creatGraph (f)
  E(g)$color <- E(g)$weight
  return(ecount(g))
}

deleteIsoNodes <- function(g){
  #delete the isolated vertices
  new_g <- delete.vertices(g, V(g)[degree(g)==0])
  return(new_g)
}

removeDupSub <- function(g){
  new_g <- g
  #aggregate issub edges
  sortedname<-sort(as.numeric(V(new_g)$name), decreasing=FALSE)
  len <- length(sortedname)
  if(len > 1){
    for(i in 2:len){
      vi<-toString(sortedname[i]);
      neis <- neighbors(new_g, vi, mode="out")
      if(length(neis) > 0){
        for(j in 1:length(neis)){
          vj <- neis[j]
          weight_ij <- E(new_g)[(vi%--%vj)]$weight;
          if(weight_ij == 2){
            jneis <- neighbors(new_g,vj,mode="out")
            if(length(jneis)>0){
              for(k in 1:length(jneis)){
                vk <- jneis[k]
                weight_jk <-E(new_g)[(vj%--%vk)]$weight;
                if(weight_jk == 2){
                  new_g <- delete.edges(new_g,E(new_g)[(vi%--%vk)])
                }
              }
            }
          }
        }
      }
    }
  }
  return(new_g)
}

#delete the isolated vertices && remove all teh redundent edges
simplifyGraph <- function(g){
  #trim the graph
  #(1) delete the isolated vertices
  new_g <- delete.vertices(g, V(g)[degree(g)==0])
  #(2) remove all the redundent edges
  sortedname<-sort(as.numeric(V(new_g)$name), decreasing=FALSE)
  len <- length(sortedname)
  if(len > 1){
    for(i in 2:len){
      vi<-toString(sortedname[i]);
      neis <- neighbors(new_g, vi, mode="out")
      if(length(neis) > 0){
        for(j in 1:length(neis)){
          vj <- neis[j]
          weight_ij <- E(new_g)[(vi%--%vj)]$weight;
          jneis <- neighbors(new_g,vj,mode="out")
          if(length(jneis)>0){
            for(k in 1:length(jneis)){
              vk <- jneis[k]
              weight_jk <-E(new_g)[(vj%--%vk)]$weight;
              if(weight_jk == weight_ij){
                new_g <- delete.edges(new_g,E(new_g)[(vi%--%vk)])
              }
            }
          }
        }
      }
    }
  }
  return(new_g)
}

#whether a [] exist in a [][]
verctorExist <- function(verctor, array){
  for(i in 1:length(array)){
    item <- array[[i]]
    if(length(vector)==length(item)){
      exist <- TRUE
      for(j in 1:length(vector)){
        if(vector[j] != item[j]){
          exist <-FALSE
          break
        }
      }
      if(exist)
        return(TRUE)
    }
  }
  return(FALSE)
}

#generate all interesting subgraphs of a given graph
getSubgraph <- function(g1){
  
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

#assume the # of nodes in g1 < # of nodes in g2
subgraphISO <- function(g1, g2){
  allsubg <-getSubgraph(g1)
  k <- 1
  index <- 1
  result<-list()
  while(k<=length(allsubg)){
    sub_k <- induced.subgraph(graph=g1, vids=allsubg[[k]])
    iso <- graph.subisomorphic.vf2(g2, sub_k, edge.color1=E(g2)$weight, edge.color2=E(sub_k)$weight)
    if(iso$iso){
      result[[index]]<-iso
      index <- index+1
    }
    k<-k+1
  }
  return(result)
}


#get interesting patterns by ID
getPattern <- function(id){
  if(id == 1){ # sub->sub->sub
    graph.formula( A--B, C--D, E--F, G--H, I, J, K )
  }
}
# test
fp1 <- getFilename("test")
g2<-creatGraph (fp1)
E(g2)$color <- E(g2)$weight

fp2 <- getFilename("test2")
g1<-creatGraph (fp2)
E(g1)$color <- E(g1)$weight


fp3 <- getFilename("test3")
g3<-creatGraph (fp3)
E(g3)$color <- E(g3)$weight





        
        
  
#Test graph decompose
testg<-graph.empty(n=0, directed=TRUE)
testg <-add.vertices(testg,4)
testg <- add.edges(testg, c(1,4))
testg <- add.edges(testg, c(2,3))
plot(testg)
testg<-delete.vertices(testg, V(testg)[degree(testg)==0])
plot(testg)
comps <- decompose.graph(testg, min.vertices=2)
com1 <-comps[[1]];
com2 <-comps[[2]];
sim<-delete.edges(testg,E(testg)[(3%--%1)])
> plot(sim)
> are.connected(1,3)

edgeColor<-function(w){
  if(w==1){
    
  }
}


#P1
sim_p1 <- readGraph("P1")

#P2
sim_p2 <- readGraph("P2")

iso <- subgraphISO(sim_p1, sim_p2);
subj <- induced.subgraph(graph=sim, vids=subvj)




graph.isomorphic.vf2(g1, g2, edge.color1=E(g1)$color,
                     edge.color2=E(g2)$color)

graph.isomorphic.vf2(g1, g2, edge.color1=E(g1)$weight,
                     edge.color2=E(g2)$weight)


graph.get.isomorphisms.vf2(g1, g3, edge.color1=E(g1)$weight,
                     edge.color2=E(g3)$weight)

graph.count.subisomorphisms.vf2(g1, g3, edge.color1=E(g1)$weight,
                           edge.color2=E(g3)$weight)





