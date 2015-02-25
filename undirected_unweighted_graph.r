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



#output = "SubGraph//subgraph_features.csv"
#if(!file.exists(output)){

#file.create(output)
#}


#id1 = "p1"
#id2 ="101"

#index = 1
#featureVectors <- list()

#featureVectors <- subIso_FeatureVector_Undirected_Unweighted(id1, id2, featureVectors, index)

#id1="p1"
#id2="193"
#index = length(featureVectors)+1
#featureVectors <- subIso_FeatureVector_Undirected_Unweighted(id1, id2, featureVectors, index)

#resultMatrix <- do.call(rbind, featureVectors)
#colnames(resultMatrix) <- c("id","id1", "id2")
#write.table(resultMatrix, file=output, sep=",")





writeIso_Ordered_FeatureVector_Undirected_Unweighted("p1","101")
writeIso_Ordered_FeatureVector_Undirected_Unweighted("p1", "193")
writeIso_Ordered_FeatureVector_Undirected_Unweighted("p1", "239")


common2pair_3_25("p1_193","p1_101")

#common4_3_25("101","p1","239","193")


common_p1_193_101 <- read.csv(file="SubGraph//p1_193_p1_101.csv", head=TRUE, sep=",")
d <- as.matrix(common_p1_193_101)
id <- d[1,1]
vset_list <- d[,26]
getSubgraph_by_Vs_undirected_unweighted(id, vset_list)
