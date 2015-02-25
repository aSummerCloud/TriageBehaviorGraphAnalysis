library("subgraphMining")
database = array(dim =3)
gp1<-createGraph_Undirected_Unweighted("p1")
g101 <- createGraph_Undirected_Unweighted("101")
g193 <- createGraph_Undirected_Unweighted("193")

database[1] = list(gp1)
database[2] = list(g101)
database[3] = list(g193)

results = gspan(database, "80%")