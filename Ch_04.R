library(tidyverse)
library(sand)

data(karate)

hist(degree(karate))

hist(graph.strength(karate))

library(igraphdata)
data(yeast)

ecount(yeast)
vcount(yeast)
hist(degree(yeast))
hist(graph.strength(yeast)) # The graph strength is the same as degree for an unweighted graph.

hist(degree(yeast))
d <- 1:max(degree(yeast))-1
ind <- (degree.distribution(yeast) != 0)
plot(log(d[ind]), log(degree.distribution(yeast)[ind]))

a.nn.deg.yeast <- graph.knn(yeast, V(yeast))$knn # average degree of neighbors of a given vertex
plot(log(degree(yeast)), log(a.nn.deg.yeast)) # Vs of lower degree are neighbors with Vs with both high and low degree. But, high-degree Vs are only neighbors with other high-degree Vs.


A <- get.adjacency(karate, sparse = FALSE)
library(network)
g <- network::as.network.matrix(A)
library(sna)
sna::gplot.target(g, degree(g), usearrows = FALSE, circ.lab = FALSE, circ.col= "skyblue", vertex.col=c("blue", rep("red", 32), "yellow"), edge.col = "darkgrey")
sna::gplot.target(g, closeness(g), usearrows = FALSE, circ.lab = FALSE, circ.col= "skyblue", vertex.col=c("blue", rep("red", 32), "yellow"), edge.col = "darkgrey")
sna::gplot.target(g, betweenness(g), usearrows = FALSE, circ.lab = FALSE, circ.col= "skyblue", vertex.col=c("blue", rep("red", 32), "yellow"), edge.col = "darkgrey")
sna::gplot.target(g, evcent(g), usearrows = FALSE, circ.lab = FALSE, circ.col= "skyblue", vertex.col=c("blue", rep("red", 32), "yellow"), edge.col = "darkgrey")

l <- layout.kamada.kawai(aidsblog)
plot(aidsblog, layout=l, vertex.size=10*sqrt(hub.score(aidsblog)$vector)) # Hubs
plot(aidsblog, layout=l, vertex.size=10*sqrt(authority.score(aidsblog)$vector)) # Authority


eb <- edge.betweenness(karate)
E(karate)[order(eb, decreasing= TRUE)[1:3]]


table(sapply(cliques(karate), length))
table(sapply(maximal.cliques(karate), length))

# table(sapply(cliques(yeast), length)) # NOTE: This takes forever since this graph is 2 order of mag larger than the karate graph
# How would one speed this up?
# table(sapply(maximal.cliques(yeast), length))
clique.number(yeast)
cores <- graph.coreness(karate)
sna::gplot.target(g, cores, circ.lab = FALSE,
                  circ.col="skyblue", usearrows = FALSE,
                  vertex.col=cores, edge.col="darkgray")
detach("package:network")
detach("package:sna")

# Dyads arepairs of vertices and, in directed graphs, may take on three possible states: null (nodirected edges), asymmetric (one directededge), or mutual (two directed edges).Similarly, triads are triples of vertices and may take on 16 possible states, rang-ing from the null subgraph to the subgraph in which all three dyads formed by thevertices in the triad have mutual directed edges.
aidsblog <- simplify(aidsblog)
dyad.census(aidsblog)

dyad.census(simplify(yeast)) # This is an undirected graph, so there can be no assymentric dyads
triad.census(simplify(yeast))

graph.motifs(karate, size = 4)
graph.motifs(aidsblog)
graph.motifs(yeast) # This is really fast, even for large networks
# Motifs are based on isomorphic classes
g1 <- graph_from_isomorphism_class(3, 15)
g2 <- graph_from_isomorphism_class(3, 11)
plot(g1)
plot(g2)
isomorphism_class(g1)
isomorphism_class(g2)
isomorphic(g1, g2)



ego.instr <- induced.subgraph(karate, neighborhood(karate, 1, 1)[[1]])
ego.admin <- induced.subgraph(karate, neighborhood(karate, 1, 34)[[1]])
graph.density(karate)
graph.density(ego.instr)
graph.density(ego.admin)

transitivity(karate) # What proportion of the triples close to form triangles?
transitivity(karate, "local", vids=c(1,34))

reciprocity(aidsblog, mode="default")
reciprocity(aidsblog, mode="ratio") # A  concept  unique  to  directed  graphs  is  that  of  reciprocity, i.e.,  the  extent  towhich there is reciprocation among ties in a directed network


is.connected(yeast)
table(sapply(decompose.graph(yeast), vcount)) # There is one 'giant component' that is made up of most of the verticies of the unconnected graph
yeast.gc <- decompose.graph(yeast)[[1]]
average.path.length(yeast.gc)
diameter(yeast.gc)
log(vcount(yeast.gc)) # The shortest path distance scales with log(Nv) rather than Nv and so is considered 'small'
transitivity(yeast.gc) # At the same time, the clustering is relatively large. Close to 50 % of connected triples close to form triangles

vertex.connectivity(yeast.gc)
edge.connectivity(yeast.gc) # In the case of the giant component of the yeast network, the vertex and edgeconnectivity are both equal to one. Thus it requires the removal of only a single well-chosen vertex or edge in order tobreak this subgraph into additional components.
articulation.points(yeast.gc) %>% length()
shortest.paths(yeast.gc) %>% length()
graph.maxflow(yeast.gc)
graph.mincut(yeast.gc)


# Graph partitioning
fastgreedy.community(karate) %>% sizes()
plot(fastgreedy.community(karate), karate)
dendPlot(fastgreedy.community(karate), mode = "phylo")

# Validating graph partitioning
fastgreedy.community(yeast.gc) %>% sizes()
plot(fastgreedy.community(yeast.gc), yeast.gc)
