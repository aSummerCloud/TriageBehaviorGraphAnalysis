
library("igraph")

plotGraph_Directed_Weighted<-function(g){
  
  plot(g, layout=layout.circle, vertex.size=8, vertex.label=V(g)$name,
       vertex.label.dist=0.8, edge.arrow.size=0.2, edge.width = E(g)$width)
}

plotGraph_Directed_Weighted_Title<-function(g,title){

  plot(g, layout=layout.circle, vertex.size=8, 
       vertex.color=V(g)$color,
       vertex.label=V(g)$name,
       vertex.label.dist=0.8, 
       edge.color=E(g)$color,
       edge.arrow.size=0.2, 
       edge.width = E(g)$width,
       main=title)
}

plotGraph_Undirected_Unweight<-function(g){
  
  plot(g, layout=layout.circle, vertex.size=8,  
       vertex.label = paste(V(g)$name, V(g)$Field,sep=':'),
       vertex.label.dist=0.8,
        edge.arrow.size=0.2)
}


plotGraph_Undirected_Unweight_Title<-function(g, title){
  
  plot(g, layout=layout.circle, vertex.size=8,  vertex.label=V(g)$label,vertex.label.dist=0.8,
       edge.arrow.size=0.2, main=title)
  
}


plotGraph_Index<-function(g, title){
  
  n <- length(V(g))
  s <- seq(n)
  plot(g, layout=layout.circle, vertex.size=8,  
       vertex.label=paste(s, V(g)$Index, sep=":"),
       vertex.label.dist=0.8,
       edge.arrow.size=0.2, 
       main=title)
  
}

plotGraph_Name<-function(g, title){
  
  n <- length(V(g))
  s <- seq(n)
  plot(g, layout=layout.circle, vertex.size=8,  
       vertex.label=paste("", V(g)$name, sep=""),
       vertex.label.dist=0.8,
       edge.arrow.size=0.2, 
       main=title)
  
}





plotGraph_Definition0<-function(g, title){
  
  plot(g, layout=layout.circle, vertex.size=8,  
       vertex.label=paste(V(g)$Index, V(g)$Field, sep=":"),
       vertex.label.dist=0.8,
       edge.arrow.size=0.2, 
       main=title)
  
}


plotGraph_Definition2<-function(g, title){
  plotGraph_Definition3(g, title)
  
}

plotGraph_Definition3<-function(g, title){
  
  n <- length(V(g))
  s <- seq(n)
  plot(g, layout=layout.circle, vertex.size=8,  
       vertex.label=paste(s, V(g)$Index, sep=":"),
       vertex.label.dist=0.8,
       edge.arrow.size=0.2, 
       main=title)
  
}

plotGraph_Pattern<-function(g, title){
  
  n <- length(V(g))
  s <- seq(n)
  plot(g, layout=layout.circle, vertex.size=8,  
       vertex.label=paste(s, V(g)$Index, sep=":"),
       vertex.label.dist=0.8,
       vertex.color=V(g)$color,
       vertex.shape=V(g)$shape,
       edge.arrow.size=0.2, 
       edge.color=E(g)$color,
       main=title)
  
}



plotGraph_Definition4<-function(g, title){
  plotGraph_Definition3(g, title)
  
}

plotGraph_Legend<-function(g){
  
  plot(g, layout=layout.circle, vertex.size=8, vertex.label=V(g)$name,
       vertex.label.dist=0.8, edge.arrow.size=0.2, edge.width = E(g)$width)
  
  colors = unique(V(g)$color)
  labels = unique(V(g)$Field)
  
  legend("topright",legend=labels, col=colors, pch=19)
  
}



plotGraph_Title<-function(g, title){
  
  plot(g, layout=layout.circle, vertex.size=8, vertex.label=V(g)$name,
       vertex.label.dist=0.8, edge.arrow.size=0.2, edge.width = E(g)$width, main=title)
}

plotGraph_Tree<-function(g,root){
  
  #lay = layout.reingold.tilford(g,root)
  
  lay = layout.fruchterman.reingold.grid(g)
  
  plot(g, layout=lay, vertex.size=8, vertex.label=V(g)$name,
       vertex.label.dist=0.8, edge.arrow.size=0.2, edge.width = E(g)$width)
}

plotGraph_Tree2<-function(g,root){
  
  #lay = layout.reingold.tilford(g,root)
  
  lay = layout.svd(g)
  
  plot(g, layout=lay, vertex.size=8, vertex.label=V(g)$name,
       vertex.label.dist=0.8, edge.arrow.size=0.2, edge.width = E(g)$width)
}


plotGraph_Tree3<-function(g){
  
  lay = layout.reingold.tilford(g)
  
  
  plot(g, layout=lay, vertex.size=8, vertex.label=V(g)$name,
       vertex.label.dist=0.8, edge.arrow.size=0.2, edge.width = E(g)$width)
}


plotGraph_Auto<-function(g){
  
  lay = layout.auto(g)
  
  plot(g, layout=lay, vertex.size=8, vertex.label=V(g)$name,
       vertex.label.dist=0.8, edge.arrow.size=0.2, edge.width = E(g)$width)
}
