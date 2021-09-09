
library(igraph)

g <- graph.formula(1-2, 1-3, 2-3, 2-4, 3-5, 4-5, 4-6, 4-7, 5-6, 6-7)

str(g)

V(g)
E(g)

plot(g)
plot(g) # Plot is different each time.

# g is a matrix
g[,]

dg <- graph.formula(1-+2,
                    1-+3,
                    2++3)

str(dg) # The logical variable is binomial answer to question: "Is the graph directed?"
plot(dg)

# Using names of vertices ====
dg <- graph.formula(Sam-+Mary,
                    Sam-+Tom,
                    Mary++Tom)
str(dg)
dg[,]
plot(dg)

V(dg) %>% str()
E(dg) %>% str()

V(dg)$name <- c("Sam", "Mary", "Andis")
plot(dg)

get.edgelist(dg)
get.adjacency(dg) #Directed, so not symmetric
get.adjacency(g) # Undirected = symmetric

# Inducing a subgraph ====
h <- induced.subgraph(g, 1:5)
plot(h)

h <- h + vertices(c(6,7))
plot(h)

H <- h + edges(c(4,6), c(4,7), c(5,6), c(6,7))
plot(H)

h1 <- h
plot(h1)
h2 <- graph.formula(4-6, 4-7, 5-6, 6-7)
plot(h2)

G <- graph.union(h1, h2)
plot(G)

# Decorating graphs ====
V(dg)$gender <- c("M", "F", "M")
plot(dg)

V(g)$color <- "red"
plot(g)

is.weighted(g)
wg <- g
E(wg)$weight <- runif(ecount(wg))

g$name <- "Toy Graph"

# Dataframes ====
library(sand)
g.lazega <- graph.data.frame(elist.lazega,
                             directed = FALSE,
                             vertices = v.attr.lazega)
is.directed(g.lazega)
g.lazega$name <- "Lazega Lawyers"

vcount(g.lazega)
ecount(g.lazega)

list.vertex.attributes(g.lazega)

str(g.lazega)
plot(g.lazega)

neighbors(g.lazega, "V21")
neighbors(g.lazega, "V32")

degree(g.lazega)

# Degree for digraphs
degree(dg, mode = "in")
degree(dg, mode = "out")

is.connected(g.lazega)
clusters(g.lazega) # Notes that V23 and V8 are unconnected from the graph and represent two singleton clusters.

diameter(g.lazega) # The diameter is the longest distance in a graph. The distance or geogesic distance or geodesic is the length of the shortest paths between vertices.
diameter(g)

# Types of graphs
plot(graph.full(7))
plot(graph.ring(7))
plot(graph.tree(7, children = 2, mode = "undirected"))
plot(graph.star(7, mode = "undirected"))
