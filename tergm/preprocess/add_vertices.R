# Load the required packages
library(network)
library(igraph)

net1 <- read.paj('./co-comment/Comments_common50_2.net')
net2 <- read.paj('./co-comment/Comments_common50_3.net')

graph1_ <- asIgraph(net1)
graph2_ <- asIgraph(net2)
plot(net1, label = 'vertex.names')
plot(graph1_, label = 'vertex.names')
net_x <- asNetwork(graph1_)
plot(net_x, label = 'vertex.names')

# Identify extra vertices present in net2 but not in net1
extra_vertices <- setdiff(net2 %v% "vertex.names", net1 %v% "vertex.names")

# Add the extra vertices to net1 with no edges (isolated vertices)
for (vertex in extra_vertices) {
  graph1_ <- igraph::add.vertices(graph1_, vertex.names = vertex, nv = 1)
}

net1 <- asNetwork(graph1_)


plot(net1, label = 'vertex.names', main = "Modified Network 1")
plot(net2, label = 'vertex.names', main = "Network 2")

