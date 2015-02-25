#get the whole path of a subject
getFilename <-function(subID){
  filename <- "output//"
  result <- paste(filename,subID,sep="")
  result <- paste(result,".csv",sep="")
  return(result)
}

getNodeFilename <-function(subID){
  filename <- "data//"
  result <- paste(filename,subID,sep="")
  result <- paste(result,".csv",sep="")
  return(result)
}



getEdgeFilename_Undirected_Unweighted <-function(subID){
  filename <- "output1//"
  result <- paste(filename,subID,sep="")
  result <- paste(result,".csv",sep="")
  return(result)
}

#full directed edge with edgeweight
# A is equal to B (A <----> B)
# A is comp with B (A<---->B)
# A is subed by B (A --> B)
# A corresponds to B ( A <--> B)
getEdgeFilename_Definition0<-function(subID){
  filename <- "output_definition0//"
  result <- paste(filename,subID,sep="")
  result <- paste(result,".csv",sep="")
  return(result)
}

#Definition1 is the same as definition0
getEdgeFilename_Definition1<-function(subID){
  filename <- "output_definition0//"
  result <- paste(filename,subID,sep="")
  result <- paste(result,".csv",sep="")
  return(result)
}


#Definition2 is the same as definition0
getEdgeFilename_Definition2<-function(subID){
  filename <- "output_definition0//"
  result <- paste(filename,subID,sep="")
  result <- paste(result,".csv",sep="")
  return(result)
}



getPlotPath<-function(dir, title){
  
  result <- paste(dir, title, sep="//")
  result <-paste(result, ".jpg", sep="")
  return (result)
}


getMyPath<-function(dir, subdir, title){

  result <- paste(dir, subdir, sep="//")
  result <- paste(result, title, sep="//")
  result <-paste(result, ".jpg", sep="")
  return (result)
}

getPlotPath_plot<-function(id){
  result <- paste("plot", id, sep ="//")
  return (result)
  
}