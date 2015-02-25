#definition 0
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
source("igraph-R-Code//iso.r")


#subgraphISM
writeIso_Definition0("p1","101")
writeIso_Definition0("p1", "193")
writeIso_Definition0("p1", "239")




common2pair_3_25("p1_193","p1_101")

#common4_3_25("101","p1","239","193")


common_p1_193_101 <- read.csv(file="SubGraph//p1_193_p1_101.csv", head=TRUE, sep=",")
d <- as.matrix(common_p1_193_101)
id <- d[1,1]
vset_list <- d[,26]
getSubgraph_by_Vs_undirected_unweighted(id, vset_list)
