#define patterns

#get.subisomorphism is not related to $label $name




getPattern_Condition=function(pattern_id, hypo, nfound, edgetype){
  #definition2
  if(identical(pattern_id,"1")){
    #pattern1: issub issub 

    pattern <-graph.formula( "B"--+"A")
    
    if(hypo)
      V(pattern)$Hypo=c("1","0")
    
    if(nfound)
      V(pattern)$NFound=c("0","0")
    
    if(edgetype)
      E(pattern)$type=c(2)

    return(pattern)
  }
# 
  else if(identical(pattern_id, "2")){
    #node with hypo followed with exact exclude
    #based on V$Hypo E$weight
    patterns= list()
    
    pattern1 <-graph.formula( "B"--+"A")
    pattern2 <-graph.formula( "B"--+"A")
    
    if(hypo){
      V(pattern1)$Hypo=c("1","0")
      V(pattern2)$Hypo= c("1","1")
    }
  
    
    if(edgetype)
      E(pattern)$weight = c(7)
    
    patterns[[1]] = pattern1
    patterns[[2]] = pattern2
    
    return(patterns)
  }
  else if(identical(pattern_id, "3")){
 
    #(nhypo,---link---, hypo )
    patterns= list()
    
    pattern1 <-graph.formula( "B"--+"A")
    pattern2 <-graph.formula( "B"--+"A")
    
    if(hypo){
      V(pattern1)$Hypo=c("1","0")
      V(pattern1)$color=c("red","blue")
      V(pattern1)$shape = c("circle","circle")
      V(pattern2)$Hypo= c("1","1")
    }
    
    
    if(edgetype)
      E(pattern1)$weight = c(4)
  E(pattern1)$color=c("red")
    patterns[[1]] = pattern1
    patterns[[2]] = pattern2
    
    return(patterns)
    
  }
  
}

#Based on definition01
getPattern_Definition01=function(pattern_id){
  p = getPattern_Condition(pattern_id, FALSE, FALSE, TRUE)
  return (p)
}

#Based on definition2
getPattern_Definition2=function(pattern_id){
     p = getPattern_Condition(pattern_id, TRUE, FALSE, TRUE)
     return (p)
}


#Based on definition3
getPattern_Definition3=function(pattern_id){

    p = getPattern_Condition(pattern_id, FALSE, TRUE, TRUE)
      return (p)
    
  
}

#Based on definition4
getPattern_Definition4=function(pattern_id){

    p = getPattern_Condition(pattern_id, TRUE, TRUE, TRUE)
      return (p)
  
}




