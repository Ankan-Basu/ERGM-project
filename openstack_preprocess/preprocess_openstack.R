library(igraph);
library(network);

#------------------------------------------------------------------------------#

net_type = 'changes';
output_dir = './openstack_codes/data/';

if (net_type == 'comments') {
  input_prefix = './openstack/co-comment/Comments_uuid50_';
  output_prefix = 'co-comment/Comments_modif50_';
} else if (net_type == 'changes') {
  input_prefix = './openstack/co-change/Changes_uuid50_';
  output_prefix = 'co-change/Changes_modif50_';
}

#------------------------------------------------------------------------------#

preprocess <- function(x) {
  file_name <- paste(input_prefix, x, '.net', sep='');
  
  #read as igraph
  graph1 <- read_graph(file_name, format='pajek');
  
  # remove multiple edges and loops
  graph2 <- simplify(graph1, 
                     remove.multiple = TRUE, 
                     remove.loops = TRUE, 
                     edge.attr.comb = "mean"
                     );
  
  outp_file_name <- paste(output_dir, output_prefix, x, '.graphml', sep='');
  write.graph(graph2, outp_file_name, format='graphml')
}

#-----------------------------------Main---------------------------------------#

file_names_vector = seq(from=1, to=50, by=1);

for (x in file_names_vector) {
  preprocess(x);
}
