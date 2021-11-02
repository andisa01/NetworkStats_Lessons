
###
# Ch 5
###

# Simulating classical random graphs
library(sand)
set.seed(42)
g.er <- erdos.renyi.game(100, 0.02)
plot(g.er, layout = layout.circle, vertex.label = NA)
# edges are scattered between pairs in a random, uniform way. Note that graphs generated in this way are not neccessarily connected.
is.connected(g.er)
hist(degree(g.er))
vcount(decompose.graph(g.er)[[1]])
table(sapply(decompose.graph(g.er), vcount))
# There is one giant component of the graph with 71 verticies.
mean(degree(g.er))
average.path.length(g.er)
diameter(g.er)
transitivity(g.er)
# Relatively vertices on shortest paths between vertex pairs and low clustering.

# We can simulate a graph with the same degree sequence as an empirical network
data(yeast)
degree.sequence.game(degree(yeast), method = "vl")
ecount(yeast) == ecount(degree.sequence.game(degree(yeast), method = "vl"))
all(degree(yeast) == degree(degree.sequence.game(degree(yeast), method = "vl")))
# The edge count and degree sequence are exactly the same, but the empirical network has twice the diameter of the simulated version
diameter(yeast)
diameter(degree.sequence.game(degree(yeast), method = "vl"))
# And there is must less clustering
transitivity(yeast)
transitivity(degree.sequence.game(degree(yeast), method = "vl"))

# The random graphs above do not typically fit the way real-world networks are structured. A different strategy is to start with a lattice structured network and then randomly "rewire" a percentage of the edges.
### This could be an importnat Null model for gene-coexpression network evolution!
g.ws <- watts.strogatz.game(1, 25, 5, 0.05, loops = FALSE) # Arguments are: dimension, size (Nv), neighborhood size, and rewiring probability.
plot(g.ws)
mean(degree(g.ws))
ecount(g.ws)
plot(erdos.renyi.game(25, 125, type = "gnm"))
mean(degree(erdos.renyi.game(25, 125, type = "gnm")))
# Very easy to see the difference between a random graph with the same Nv and mean degree
plot(g.ws, layout=layout.circle, vertex.label=NA)
plot(erdos.renyi.game(25, 125, type = "gnm"), layout=layout.circle, vertex.label=NA)

g.lat100 <- watts.strogatz.game(1, 100, 5, 0) # Here the probability of rewiring is set to 0, so the structure is just the simple lattice.
plot(g.lat100, layout = layout.grid)
transitivity(g.lat100)
diameter(g.lat100)     
average.path.length(g.lat100)
# The effect of rewiring a relatively small number of edges in a random fashion is tonoticeably reduce the distance between vertices, while still maintaining a similarlyhigh level of clustering.
g.ws100 <- watts.strogatz.game(1, 100, 5, 0.05)
plot(g.ws100, layout = layout.grid)
diameter(g.ws100)
average.path.length(g.ws100)
transitivity(g.ws100)

g.ws100 <- watts.strogatz.game(1, 100, 5, 0.05)
plot(watts.strogatz.game(2, 3, 1, 0), layout = layout.grid)
plot(watts.strogatz.game(3, 3, 1, 0))
# Preferential attachment models start with a random graph and then at each timestep, some number of existing vertices are connected to a new vertex chose preferentially by degree (larger degrees attract more attachments, i.e. "the rich get richer").
set.seed(42)
g.ba <- barabasi.game(100, directed=FALSE)
plot(g.ba, layout=layout.circle, vertex.label=NA)
hist(degree(g.ba)) # There are a few "hub" vertices of high degree.
summary(degree(g.ba))
average.path.length(g.ba)
diameter(g.ba)
transitivity(g.ba)

