p1 <-graph.formula( "1"--+"2")

V(p1)$name=c("1","2")
V(p1)$color=c(1,2)
V(p1)$Hypo=c("0","1")
V(p1)$NFound=c("0","0")
V(p1)$shape=c("circle","circle")
E(p1)$weight=c(2)
E(p1)$color=c(2)


p11 <-graph.formula( "1"--+"2", "3"--+"2")
V(p11)$name=c("1","2","3")

V(p11)$color=c(1,2,1)
V(p11)$Hypo=c("0","1","0")
V(p11)$NFound=c("0","0","0")
V(p11)$shape=c("circle","circle","circle")
E(p11)$weight=c(2,2)
E(p11)$color=c(2,2)


p2 <-graph.formula( "a"--+"b"--+"c")

V(p2)$color=c(1,1,2)
V(p2)$Hypo=c("0","0","1")
V(p2)$NFound=c("0","0","0")
V(p2)$shape=c("circle","circle","circle")
E(p2)$weight=c(2, 2)
E(p2)$color=c(2,2)

p3 <-graph.formula( "a"--+"b"--+"c")

V(p3)$color=c(1,1,2)
V(p3)$Hypo=c("0","0","1")
V(p3)$NFound=c("0","0","0")
V(p3)$shape=c("circle","circle","circle")
E(p3)$weight=c(2, 2)
E(p3)$color=c(2,2)


current_g = p2
pattern=p1

iso00 <-graph.get.subisomorphisms.vf2(p3,p2)

isms21 <-graph.get.subisomorphisms.vf2(p2,p1)
ism21 <- graph.subisomorphic.vf2(p2,p1)


isms211 <-graph.get.subisomorphisms.vf2(p2,p11)
ism211 <- graph.subisomorphic.vf2(p2,p11)

iso0<-graph.subisomorphic.vf2(current_g, pattern,
                             vertex.color1=as.numeric(V(current_g)$Hypo), 
                             vertex.color2=as.numeric(V(pattern)$Hypo),
                             edge.color1 = E(current_g)$color,
                             edge.color2=E(pattern)$color
  )

iso1 <- graph.get.subisomorphisms.vf2(current_g, pattern,
                                         vertex.color1=as.numeric(V(current_g)$Hypo), 
                                   vertex.color2=as.numeric(V(pattern)$Hypo),
                                  edge.color1 = E(current_g)$color,
                                 edge.color2=E(pattern)$color)

iso2 <- graph.get.subisomorphisms.vf2(current_g, pattern)