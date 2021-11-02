
###
# Ch. 7
###

library(sand)
library(tidyverse)

data(Ecoli.data)
Ecoli.expr %>% as.data.frame() %>% View()
regDB.adj

heatmap(scale(Ecoli.expr))

library(igraph)
g.regDB <- graph.adjacency(regDB.adj, "undirected")
summary(g.regDB)
plot(g.regDB, vertex.size=3, vertex.label=NA)

# Null hypothesis testing of significance
mycorr <- cor(Ecoli.expr)
z <-0.5*log((1 + mycorr) / (1 - mycorr))
z.vec <- z[upper.tri(z)]
n <- dim(Ecoli.expr)[1]
corr.pvals <- 2*pnorm(abs(z.vec), 0, sqrt(1 / (n-3)), lower.tail=FALSE)
corr.pvals.adj <- p.adjust(corr.pvals, "BH") # adjusting for multiple comparissons
length(corr.pvals.adj[corr.pvals < 0.05])
length(corr.pvals.adj[corr.pvals.adj < 0.05]) # Slightly fewer significant correlations.
qqnorm(corr.pvals.adj)

# Data-dependent methods
library(fdrtool)
mycorr.vec <- mycorr[upper.tri(mycorr)]
fdr <- fdrtool(mycorr.vec, statistic="correlation")

# Partial correlation
pcorr.pvals <- matrix(0, dim(mycorr)[1], dim(mycorr)[2])
for(i in seq(1, 153)){
  for(j in seq(1, 153)){
    rowi <- mycorr[i, -c(i, j)]
    rowj <- mycorr[j, -c(i, j)]
    tmp <- (mycorr[i, j] - rowi*rowj)/sqrt((1-rowi^2)*(1-rowj^2))
    tmp.zvals <- (0.5)*log((1+tmp) / (1-tmp))
    tmp.s.zvals <- sqrt(n-4)*tmp.zvals
    tmp.pvals <- 2*pnorm(abs(tmp.s.zvals), 0, 1, lower.tail=FALSE)
    pcorr.pvals[i, j] <- max(tmp.pvals)
  }
}

pcorr.pvals.vec <- pcorr.pvals[lower.tri(pcorr.pvals)]
pcorr.pvals.adj <- p.adjust(pcorr.pvals.vec, "BH")

pcorr.edges <- (pcorr.pvals.adj < 0.05)
length(pcorr.pvals.adj[pcorr.edges])

pcorr.A <- matrix(0, 153, 153)
pcorr.A[lower.tri(pcorr.A)] <- as.numeric(pcorr.edges)
g.pcorr <- graph.adjacency(pcorr.A, "undirected")

str(graph.intersection(g.regDB, g.pcorr, byname=FALSE))

# Now with mixture model in FDRtools
fdr <- fdrtool(pcorr.pvals.vec, statistic="pvalue"), plot=FALSE)
pcorr.edges.2 <- (fdr$qval < 0.05)
length(fdr$qval[pcorr.edges.2]) # Same results
