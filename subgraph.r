library("igraph")

subGraph_Sub <- function(g){
  el <- E(g)[E(g)$weight==2]
  sub <- subgraph.edges(g, el)
  return (sub)
}


subGraph_Nodes <- function(g, vnames){
  sub <- induced.subgraph(g, vnames)
  return (sub)
}


vnames <-c("27", "32","33","34","36","41","42","46","47","58")
subsub <- induced.subgraph(sim2, vnames)


vnames2 <-c("27", "32","33","34","36","41","42","46","47")
subsub2 <- induced.subgraph(sim2, vnames2)