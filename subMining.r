processInputSubdue = function(graph1, output){
  library(igraph)
  outputFile = file(output, "w")
  write("XP", outputFile)
  for(i in 1:graph1[[1]])
  {
    write(paste("v", i, "a"), outputFile)
  }
  if(graph1[[2]] == TRUE)
  {
    for(i in 1:length(E(graph1)))
    {
      write(paste("d", graph1[[3]][[i]]+1, graph1[[4]][[i]]+1, "t"), outputFile)
    }
  }
  else
  {
    for(i in 1:length(E(graph1)))
    {
      write(paste("u", graph1[[3]][[i]]+1, graph1[[4]][[i]]+1, "t"), outputFile)
    }
  }
  close(outputFile)
}
processOutputSubdue = function(output){
  library(igraph)
  test = readLines(output)
  graph1 = graph.empty()
  
  #generate the vertex list
  vertexList = array()
  for(i in 1:length(test))
  {
    splitString = strsplit(test[i], " ")
    if(length(splitString[[1]]) != 0 && identical(splitString[[1]][[1]], "v"))
    {
      vertexList = append(vertexList,splitString[[1]][[2]])
    }
  }
  
  #Add vertices to the graph
  graph1 = add.vertices(graph1, length(vertexList), attribute = "name")
  
  #Add in vertex attributes
  for(i in 0:length(vertexList))
  {
    graph1 = set.vertex.attribute(graph1, "name", i, i+1)
  }
  
  #Add edges and edge attributes to the graph
  edgeNum = 0
  for(i in 1:length(test))
  {
    splitString = strsplit(test[i], " ")
    if(length(splitString[[1]]) != 0 && (identical(splitString[[1]][[1]], "d") || identical(splitString[[1]][[1]], "e")))
    {
      graph1 = add.edges(graph1, c(as.integer(splitString[[1]][[2]])-1, as.integer(splitString[[1]][[3]])-1), attribute = "name")
      graph1 = set.edge.attribute(graph1, "name", edgeNum, splitString[[1]][[4]])
      edgeNum = edgeNum + 1
    }
    if(length(splitString[[1]]) != 0 && identical(splitString[[1]][[1]], "u"))
    {
      graph1 = add.edges(graph1, c(as.integer(splitString[[1]][[2]])-1, as.integer(splitString[[1]][[3]])-1), attribute = "name")
      graph1 = set.edge.attribute(graph1, "name", edgeNum, splitString[[1]][[4]])
      edgeNum = edgeNum + 1
      graph1 = add.edges(graph1, c(as.integer(splitString[[1]][[3]])-1, as.integer(splitString[[1]][[2]])-1), attribute = "name")
      graph1 = set.edge.attribute(graph1, "name", edgeNum, splitString[[1]][[4]])
      edgeNum = edgeNum + 1
    }
  }
  return(graph1)
}


subdue = function(graph1)
{
  processInputSubdue(graph1, "input.g")
  setwd(paste(Sys.getenv("R_HOME"), "library", "subgraphMining", "subdue-5.2.1", "bin", sep="\\"))
  system("subdue -out output.g -overlap graphs/input.g")
  graph2 = processOutputSubdue("output.g")
  return(graph2)
  unlink("input.g")
  unlink("output.g")
}

processInputGspan = function(graphArray, output)
{
  library(igraph)
  outputFile = file(output, "w")
  for(i in 1:length(graphArray))
  {
    write("XP", outputFile)
    #write vertices
    for(j in 1:graphArray[[i]][[1]])
    {
      write(paste("v", j, "a"), outputFile)
    }
    #write edges
    if(graphArray[[i]][[2]] == TRUE)
    {
      for(j in 1:length(graphArray[[i]][[3]]))
      {
        write(paste("d", graphArray[[i]][[3]][[j]]+1, graphArray[[i]][[4]][[j]]+1, "t"), outputFile)
      }
    }
    else
    {
      for(j in 1:length(graphArray[[i]][[3]]))
      {
        write(paste("u", graphArray[[i]][[3]][[j]]+1, graphArray[[i]][[4]][[j]]+1, "t"), outputFile)
      }
    }
  }
  close(outputFile)
}


processOutputGspan = function(output)
{
  library(igraph)
  test = readLines(output)
  numGraphs = 0
  resultArray = array()
  for(i in 1:length(test))
  {
    if(test[i] == "XP")
    {
      numGraphs = numGraphs + 1
    }
  }
  index = 2
  for(i in 1:numGraphs)
  {
    graph1 = graph.empty()
    vertexNum = 0
    edgeNum = 0
    
    while(index < length(test) && test[index] != "XP")
    {
      splitString = strsplit(test[index], " ")
      if(length(splitString[[1]]) != 0 && identical(splitString[[1]][[1]], "v"))
      {
        graph1 = add.vertices(graph1, 1, attribute = "name")
        vertexNum = vertexNum + 1
      }
      
      #Add edges and edge attributes to the graph
      if(length(splitString[[1]]) != 0 && splitString[[1]][[1]] != "%" && (identical(splitString[[1]][[1]], "d") || identical(splitString[[1]][[1]], "e")))
      {
        graph1 = add.edges(graph1, c(as.integer(splitString[[1]][[2]])-1, as.integer(splitString[[1]][[3]])-1), attribute = "name")
        graph1 = set.edge.attribute(graph1, "name", edgeNum, splitString[[1]][[4]])
        edgeNum = edgeNum + 1
      }
      if(length(splitString[[1]]) != 0 && splitString[[1]][[1]] != "%" && identical(splitString[[1]][[1]], "u"))
      {
        graph1 = add.edges(graph1, c(as.integer(splitString[[1]][[2]])-1, as.integer(splitString[[1]][[3]])-1), attribute = "name")
        graph1 = set.edge.attribute(graph1, "name", edgeNum, splitString[[1]][[4]])
        edgeNum = edgeNum + 1
        graph1 = add.edges(graph1, c(as.integer(splitString[[1]][[3]])-1, as.integer(splitString[[1]][[2]])-1), attribute = "name")
        graph1 = set.edge.attribute(graph1, "name", edgeNum, splitString[[1]][[4]])
        edgeNum = edgeNum + 1
      }
      index = index + 1
    }
    index = index+1
    #Add in vertex attributes
    for(j in 1:vertexNum)
    {
      graph1 = set.vertex.attribute(graph1, "name", j-1, j)
    }
    resultArray = append(resultArray, list(graph1))
  }
  return(resultArray[-1])
}


gspan = function(graphArray, support)
{
  processInputGspan(graphArray, "input.g")
  setwd(paste(Sys.getenv("R_HOME"), "library", "subgraphMining", "parsemis", sep="\\"))
  system(paste("java -jar parsemis-2008-12-01.jar --graphFile=input.g --minimumFrequency=",support," --outputFile=output.g",sep=""))
  graphArray2 = processOutputGspan("output.g")
  return(graphArray2)
  unlink("input.g")
  unlink("output.g")
}



processInputSleuth = function(array1, output){
  outputFile = file(output, "w")
  for(i in 1:length(array1))
  {
    tempC = c(i, i, length(array1[[i]]), array1[[i]])
    write(tempC, outputFile, ncolumns=length(tempC))
  }
  close(outputFile)
}


processOutputSleuth = function(output){
  test = readLines(output)
  resultArray = array()
  for(i in 1:length(test))
  {
    splitString = strsplit(test[i], " ")
    if(length(grep("[a-zA-Z'[']+", splitString[[1]])) == 0) 
    {
      index = 1	
      e = ""
      while(splitString[[1]][[index]] != "-" && splitString[[1]][[index]] != "")
      {
        e = c(e, as.integer(splitString[[1]][[index]]))
        index = index+1
      }
      resultArray = append(resultArray, list(e[-1]))
    }
  }
  return(resultArray)
}

sleuth = function(array1, support){
  processInputSleuth(array1, "input.txt")
  setwd(paste(Sys.getenv("R_HOME"), "library", "subgraphMining", "sleuth", sep="\\"))
  system(paste("vtreeminer.exe -i input.txt -s", support,"-o", sep = " "))
  #finalResult = processOutputSleuth("output.g")
  #return(finalResult)
}