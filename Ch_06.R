
###
# Ch 6.
###

library(sand)
data("lazega")
lazega
A <- get.adjacency(lazega)
v.attrs <- get.data.frame(lazega, what = "vertices")

library(ergm)
lazega.s <- network::as.network(as.matrix(A), directed = FALSE)
network::set.vertex.attribute(lazega.s, "Office", v.attrs$Office)
network::set.vertex.attribute(lazega.s, "Practice", v.attrs$Practice)
network::set.vertex.attribute(lazega.s, "Gender", v.attrs$Gender)
network::set.vertex.attribute(lazega.s, "Seniority", v.attrs$Seniority)

my.ergm.bern <- formula(lazega.s ~ edges)
summary(my.ergm.bern)

my.ergm <- formula(lazega.s ~ edges + kstar(2) + kstar(3) + triangle)
summary(my.ergm)

