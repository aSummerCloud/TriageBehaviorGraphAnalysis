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

#whether a vector exists a list of vector
vector_exist_in_list=function(vector,list){
  len = length(list)
  if(len > 0){
    for(i in 1:len){
      current_v = list[[i]]
      logic_vector = vector %in% current_v
      w = which(logic_vector==FALSE)
      if(identical(w,integer(0))){
        return (TRUE)
      }
    }
  }
  return (FALSE)
}


