#graph feature

getFeatures = function(g){
  features <- c()
  #1  number of nodes
  features <- c(features, vcount(g))
  #2 numbers of edges
  features <- c(features, ecount(g))
  #3 degree of nodes
  features <- c(features, max(degree(g, mode="all")))
  #4 density
  density <- graph.density(g)
  features <- c(features, density)
  #5 number of islands
  features<-c(features, clusters(g)$no)
  #6 global cluster coefficient (close triplets / all triplets)
  features<-c(features, transitivity(g, type="global", isolates="zero"))
  #7 edge connectivity
  features<-c(features, edge.connectivity(g)) 
  #8 diameter of the graph
  features <- c(features, diameter(g))
  #9 number of cliques (number of vertice >=3)
  features <- c(features, length(cliques(g, min="3")))
  #10 the size of the largest clique(s)
  features <- c(features, clique.number(g))
  #11 maximum closeness 
  features <- c(features, max(closeness(g, v=V(g), mode="all")))
  #12 maximum betweeness
  features <- c(features, max(betweenness(g, v=V(g))))
  # number of vertex of field "datatime"
  datatime <- length(V(g)[grepl("datatime", V(g)$Field)])
  # number of vertex of field "description"
  description <- length(V(g)[grepl("description", V(g)$Field)])
  # number of vertex of field "direction"
  direction <- length(V(g)[grepl("direction", V(g)$Field)])
  # number of vertex of field "category"
  category <- length(V(g)[grepl("category", V(g)$Field)])
  # number of vertex of field "message"
  message <- length(V(g)[grepl("message", V(g)$Field)])
  # number of vertex of field "operation"
  operation <- length(V(g)[grepl("operation", V(g)$Field)])
  # number of vertex of field "port"
  port <- length(V(g)[grepl("port", V(g)$Field)])
  # number of vertex of field "priority"
  priority <- length(V(g)[grepl("priority", V(g)$Field)])
  # number of vertex of field "protocol"
  protocol <- length(V(g)[grepl("protocol", V(g)$Field)])
  # number of vertex of field "ip"
  ip <- length(V(g)[grepl("ip", V(g)$Field)])
  # number of vertex of field "service"
  service <- length(V(g)[grepl("service", V(g)$Field)])

  
  # number of vertex of hypo
  nodehypo = length(V(g)[grepl("1", V(g)$Hypo)])
  # number of vertex of Nfound
  nodenfound = length(V(g)[grepl("1", V(g)$NFound)])
  # number of type1_edge
  type1 = length(E(g)[grepl("1",E(g)$weight)])
  # number of type2_edge
  type2 = length(E(g)[grepl("2",E(g)$weight)])  
  # number of type3_edge
  type3 = length(E(g)[grepl("3",E(g)$weight)])
  # number of type4_edge
  type4 = length(E(g)[grepl("4",E(g)$weight)])
  # number of type7_edge
  type7 = length(E(g)[grepl("7",E(g)$weight)])
  
  
  
features <- c(features, 
              datatime, description, direction, category,
              message, operation, port, priority,
              protocol, ip, service,
              nodehypo, nodenfound,
              type1, type2,type3, type4, type7)

return(features)
}


equalFeatures = function(features1, features2){
  return(identical(features1, features2))
}

writeFeatures=function(filepath, id_list){
  if(!file.exists(filepath)){
    file.create(filepath)
  }
  
  result = list()
  len = length(id_list)
  index = 1
  if(len > 0){
    for(i in 1:len){
      current_id = id_list[[i]]
      current_g = createGraph_Definition4(current_id)
      features_vector = getFeatures(current_g)
      result[[index]]=features_vector
      index = index + 1
      
    }
  }


  resultMatrix <- do.call(rbind, result)
  write.table(resultMatrix, file=filepath, sep=",")
  
  return(index)

}

    