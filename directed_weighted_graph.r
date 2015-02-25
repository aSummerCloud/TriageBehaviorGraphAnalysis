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



writeIso_Ordered_FeatureVector_directed_weighted("p1","101")
writeIso_Ordered_FeatureVector_directed_weighted("p1", "193")
writeIso_Ordered_FeatureVector_directed_weighted("p1", "239")


#common2pair_3_25("p1_193","p1_101")

#common4_3_25("101","p1","239","193")
common2pair_sameVs("p1_193","p1_101")

common_p1_193_101 <- read.csv(file="SubGraph//p1_193_p1_101.csv", head=TRUE, sep=",")
d <- as.matrix(common_p1_193_101)
id <- d[1,1]
vset_list <- d[,2]
getSubgraph_by_Vs_directed_weighted(id, vset_list)


common2pair_sameVs("p1_193","p1_239")
common_p1_193_239 <- read.csv(file="SubGraph//p1_193_p1_239.csv", head=TRUE, sep=",")
d <- as.matrix(common_p1_193_101)
id <- d[1,1]
vset_list <- d[,2]
getSubgraph_by_Vs_directed_weighted(id, vset_list)


id_list<-c("p1","101","193","239")
writePattern1("1",id_list)

