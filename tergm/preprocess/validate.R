library(network)
library(igraph)

## 
# This script is used to verify if the network files have been properly created
##

#------------------------------------------------------------------------------#
net_type = 'changes';

if (net_type == 'comments') {
  input_prefix = './tergm/data/co-comment/Comments_modif50_';
} else if (net_type == 'changes') {
  input_prefix = './tergm/data/co-change/Changes_modif50_';
}

#------------------------------------------------------------------------------#

file_names_vector = seq(from=2, to=49, by=1);

# we take the last (50th) network as the reference
net2_file_name <- paste(input_prefix, '50.graphml', sep='');
net2 <- read_graph(net2_file_name, format='graphml');
net2 <- asNetwork(net2);

net2_vertices <- net2 %v% 'vertex.names'
all_unique <- !any(duplicated(net2_vertices))

if (!all_unique) {
  stop('DUPLICATE VERTEX DETECTED.')
}

for (x in file_names_vector) {
  file_name <- paste(input_prefix, x, '.graphml', sep='');
  net1 <- read_graph(file_name, format='graphml');
  net1 <- asNetwork(net1)
  
  net1_vertices <- net1 %v% 'vertex.names'
  
  all_unique_1 <- !any(duplicated(net1_vertices))
  
  if (!all_unique_1) {
    stop('DUPLICATE VERTEX DETECTED.')
  }
  
  y <- setdiff(net2_vertices, net1_vertices)
  
  if (length(y) != 0) {
    stop('VERTEX SETS DO NOT MATCH.')
  }
  text <- paste(x, 'Passed', sep=' ');
  print(text)
}

