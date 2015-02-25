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
writeIso_Definition2("p1","101")
writeIso_Definition2("p1", "193")
writeIso_Definition2("p1", "239")


common2pair_sameVs("Definition2","p1_101", "p1_193")
