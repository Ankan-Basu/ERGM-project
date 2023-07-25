library(network)
library(igraph)
library(network)

preprocess <- function (net1, net2) {
  # net1, net2 are objects of network class
  # net2 has all the vertices of net1 + some more
  # this functions adds the extra vertices to net1
  # so that both the networks have same number of vertices
  # returns the modified version of net1 as iGraph
  
  graph1_ <- asIgraph(net1);
  
  # Identify extra vertices present in net2 but not in net1
  extra_vertices <- setdiff(net2 %v% 'vertex.names', net1 %v% 'vertex.names');
  
  # Add the extra vertices to net1 with no edges (isolated vertices)
  for (vertex in extra_vertices) {
    graph1_ <- igraph::add.vertices(graph1_, vertex.names = vertex, na = FALSE, nv = 1);
  }
  
  #net1 <- asNetwork(graph1_);
  
  return (graph1_);
}

#------------------------------------------------------------------------------#
net_type = 'comments';

if (net_type == 'comments') {
  input_prefix = './co-comment/Comments_common50_';
  output_prefix = 'co-comment/Comments_modif50_';
} else if (net_type == 'changes') {
  input_prefix = './co-change/Changes_common50_';
  output_prefix = 'co-change/Changes_modif50_';
}

output_dir = './tergm/data/';
#------------------------------------------------------------------------------#

file_names_vector = seq(from=2, to=49, by=1);

# we take the last (50th) network as the reference
net2_file_name <- paste(input_prefix, '50.net', sep='');
net2 <- read.paj(net2_file_name);

for (x in file_names_vector) {
  file_name <- paste(input_prefix, x, '.net', sep='');
  net1 <- read.paj(file_name);
  g <- preprocess(net1, net2);
  
  #g <- asIgraph(net2)
  
  outp_file_name <- paste(output_dir, output_prefix, x, '.graphml', sep='');
  write.graph(g, outp_file_name, format='graphml')
}

outp_file_name <- paste(output_dir, output_prefix, 50, '.graphml', sep='');
write.graph(asIgraph(net2), outp_file_name, format='graphml')
