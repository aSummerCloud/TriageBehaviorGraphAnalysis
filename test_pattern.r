library("igraph")

#linux
setwd("//home//amyamyamy//Dropbox//Work//Filter Graph Analysis//")

source("igraph-R-Code//createGraph.r")
source("igraph-R-Code//filepath.r")
source("igraph-R-Code//getSubgraph.r")
source("igraph-R-Code//simplifyGraph.r")
source("igraph-R-Code//plotGraph.r")
source("igraph-R-Code//features.r")
source("igraph-R-Code//subgraphISM.r")
source("igraph-R-Code//common.r")
source("igraph-R-Code//pattern.r")
source("igraph-R-Code//iso.r")
source("igraph-R-Code//pattern_search.r")

#write to pattern_definition2.csv
id_list<-c("p1","101","193","239")
writePattern_Definition2("1",id_list)

#subgraph <- read.csv(file="SubGraph//pattern_definition2.csv", head=TRUE, sep=",")
#d <- as.matrix(subgraph)
#id_list <- d[,1]
#vset_list <- d[,2]
#writeSubgraph_Definition2(id_list, vset_list)


#write to pattern_definition3.csv
id_list<-c("p1","101","193","239")
writePattern_Definition3("1",id_list)


#write to pattern_definition4.csv
id_list<-c("p1","101","193","239")
writePattern_Definition4("1",id_list)

#write to pattern_definition01.csv
id_list<-c("p1","101","193","239")
writePattern_Definition01("1",id_list)



#Vcolor: hypo; Ecolor:type
id_list<-c("p1","101","193","239")
writePattern_Definition2("1",id_list)


