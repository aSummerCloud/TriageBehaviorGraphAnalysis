setwd("//Users//Chen//Dropbox//Work//Filter Graph Analysis//igraph-R-Code")


 
    p1 <-graph.formula( "1"--+"2")
    
    V(p1)$color=c("blue","red")
    V(p1)$Hypo=c("0","1")
    V(p1)$NFound=c("0","0")
    V(p1)$shape=c("circle","circle")
    E(p1)$weight=c(2)
    E(p1)$color=c("blue")
 

p2 <-graph.formula( "a"--+"b"--+"c")

V(p2)$color=c("blue","blue","red")
V(p2)$Hypo=c("0","0","1")
V(p2)$NFound=c("0","0","0")
V(p2)$shape=c("circle","circle","circle")
E(p2)$weight=c(2, 2)
E(p2)$color=c("blue","blue")