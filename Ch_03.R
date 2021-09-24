
library(igraph)
library(sand)

g.1 <- graph.lattice(c(5,5,5))
plot(g.1)

data("aidsblog")
summary(aidsblog)

# Circular layouts
igraph.options(vertex.size = 3, vertex.label = NA, edge.arrow.size = 0.2)
par(mfrow = c(1,2))
plot(g.1, layout = layout.circle)
plot(aidsblog, layout=layout.circle)

# Spring embedder layouts
plot(g.1, layout = layout.fruchterman.reingold)
plot(aidsblog, layout = layout.fruchterman.reingold)

# Engery placement layouts
plot(g.1, layout = layout.kamada.kawai)
plot(aidsblog, layout = layout.kamada.kawai)

# Trees
g.tree <- graph.formula(1-+2,1-+3,1-+4,2-+5,2-+6,2-+7,3-+8,3-+9,4-+10)
par(mfrow=c(1, 3))
igraph.options(vertex.size=30, edge.arrow.size=0.5,vertex.label=NULL)
plot(g.tree, layout=layout.circle)
plot(g.tree, layout=layout.reingold.tilford(g.tree,circular=T))
plot(g.tree, layout=layout.reingold.tilford)

# Bipartite
par(mfrow=c(1, 1))
plot(g.bip, layout=-layout.bipartite(g.bip)[,2:1],vertex.size=30, vertex.shape=ifelse(V(g.bip)$type,"rectangle", "circle"),vertex.color=ifelse(V(g.bip)$type, "red", "cyan"))

# Decorated graph
library(igraphdata)
data(karate)
set.seed(42)
l <- layout.kamada.kawai(karate)
igraph.options(vertex.size=10)
plot(karate, layout = l, vertex.label = V(karate))
V(karate)$label <- sub("Actor ", "", V(karate)$name)
V(karate)$shape <- "circle"
V(karate)[c("Mr Hi", "John A")]$shape <- "rectangle"
V(karate)[Faction == 1]$color <- "red"
V(karate)[Faction == 2]$color <- "dodgerblue"
V(karate)$size <- 4*sqrt(graph.strength(karate))
V(karate)$size2 <- V(karate)$size*.5
E(karate)$width <- E(karate)$weight
F1 <- V(karate)[Faction==1]
F2 <- V(karate)[Faction==2]
E(karate)[ F1 %--% F1 ]$color <- "pink"
E(karate)[ F2 %--% F2 ]$color <- "lightblue"
E(karate)[ F1 %--% F2 ]$color <- "yellow"
V(karate)$label.dist <- ifelse(V(karate)$size >= 10, 0, 0.75)
plot(karate, layout=l)


# Rewriting the same plot in tidygraph format with ggraph
library(tidygraph)
library(tidyverse)
K <- as_tbl_graph(karate)
K %>%
  activate(nodes) %>%
  mutate(label = sub("Actor ", "", name),
         shape = ifelse(label == "Mr Hi" | label == "John A", 1, 0),
         node.size = 2*sqrt(graph.strength(.))) %>%
  activate(edges) %>%
  mutate(edge.type = case_when(
    .N()$Faction[from] == 1 & .N()$Faction[to] == 1 ~ "pink",
    .N()$Faction[from] == 2 & .N()$Faction[to] == 2 ~ "lightblue",
    (.N()$Faction[from] == 1 & .N()$Faction[to] == 2) | (.N()$Faction[from] == 2 & .N()$Faction[to] == 1) ~ "yellow")) %>%
ggraph(layout = 'kk') +
  geom_edge_link(aes(col = edge.type, edge_width = weight)) +
  geom_node_point(aes(fill = as.factor(Faction), pch = as.factor(shape), size = node.size), col = "black") +
  geom_node_text(aes(label = label)) +
  scale_edge_color_identity() +
  scale_shape_manual(values = c(21, 22)) +
  scale_fill_manual(values = c("firebrick", "dodgerblue")) +
  scale_size_identity() +
  theme_graph() +
  theme(legend.position = "none")
