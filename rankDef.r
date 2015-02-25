source("igraph-R-Code//utility.r")
source("igraph-R-Code//getSubgraph.r")
source("igraph-R-Code//createGraph.r")
source("igraph-R-Code//simplifyGraph.r")
source("igraph-R-Code//plotGraph.r")




#id="p1"
#id="193"
id="239"
definition = "definition2"
filepath=paste("SubGraph//pattern_",definition,sep="")
filepath=paste(filepath, ".csv", sep="")

g=createGraph_SubSeq(id,filepath)
cliques = maximal.cliques(g)
plotGraph_Name(g,paste(definition,id,sep=":"))
clique_names = getCliqueNames(g, cliques)

#edges
length(E(g))
