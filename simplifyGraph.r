library("igraph")


simGraph<-function(g){
  sim1<-removeDupSub(g)
  sim2<-removeDupSubCom(g, sim1)
  return(sim2)
}

removeDupSub <- function(g){
  # A isSub B; B isSub C; --> A isSub C
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
          if(weight_ij%%4 == 2){
            jneis <- neighbors(new_g,vj,mode="out")
            if(length(jneis)>0){
              for(k in 1:length(jneis)){
                vk <- jneis[k]
                weight_jk <-E(new_g)[(vj%--%vk)]$weight;
                if(weight_jk%%4 == 2){
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



removeDupSubCom <- function(ori, sim){
  # A isCom C; B isSub A; --> B isCom C (remove B-->C)
  new_g <- ori
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
          if(weight_ij%%4 == 2){
            jneis <- neighbors(new_g,vj,mode="out")
            if(length(jneis)>0){
              for(k in 1:length(jneis)){
                vk <- jneis[k]
                if(are.connected(new_g, vi, vk)){
                  weight_jk <-E(new_g)[(vj%--%vk)]$weight
                  if(weight_jk%%4 == 3){
                        weight_ik <- E(new_g)[(vi%--%vk)]$weight
                        if(weight_ik%%4 == 3){
                          if(are.connected(sim, vi, vk)){
                            sim <- delete.edges(sim,E(sim)[(vi%--%vk)])
                          }
                      }
                  }
                }
              }
            }
          }
        }
      }
    }
  }
  return(sim)
}



removeDup_SingleType <- function(g){
  # A isSub B; B isSub C; --> A isSub C
  new_g <- g
  #aggregate issub edges
  sortedname<-sort(as.numeric(V(new_g)$name), decreasing=FALSE)
  len <- length(sortedname)
  if(len > 1){
    for(i in 2:len){
      vi_name<-toString(sortedname[i])
      vi = which(V(new_g)$name == vi_name)

      neis <- neighbors(new_g, vi, mode="out")
      
      if(length(neis) > 0){
        for(j in 1:length(neis)){
          vj <- neis[j]
        
            jneis <- neighbors(new_g,vj,mode="out")
            if(length(jneis)>0){
              for(k in 1:length(jneis)){
                vk <- jneis[k]

                  new_g <- delete.edges(new_g,E(new_g)[(vi%--%vk)])
                
              }
            }
          }
        }
      }
    }
  return(new_g)
}



