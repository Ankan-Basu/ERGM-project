library(tergm)
library(tsna)
library(ndtv)
library("network")
library(igraph)
library('intergraph')
library(btergm)

#------------------------------------------------------------------------------#
net_type = 'comments';

if (net_type == 'comments') {
  #input_prefix = './tergm/data/co-comment/Comments_modif50_';
  input_prefix = './openstack/co-comment/Comments_uuid50_';
} else if (net_type == 'changes') {
  #input_prefix = './tergm/data/co-change/Changes_modif50_';
  input_prefix = './openstack/co-change/Changes_uuid50_';
}

#------------------------------------------------------------------------------#

get_network_list <- function(interval, start=1, end=50) {
  file_names_vector = seq(from=start, to=end, by=interval);
  
  networks_list <- list()
  
  for (x in file_names_vector) {
    file_name <- paste(input_prefix, x, '.net', sep='');
    net <- read_graph(file_name, format='pajek');
    net <- asNetwork(net);
    
    networks_list <- append(networks_list, list(net), after=length(networks_list));
  }
  
  return (networks_list);
}


preprocess <- function(networks_list) {
  new_list <- list()
  for (i in c(1:length(networks_list))) {
    #print(networks_list[i])
    
    net_curr <- networks_list[[i]];
    g <- asIgraph(net_curr);

    # Remove isolated nodes
    Isolated <- which(degree(g) == 0);
    g <- igraph::delete.vertices(g, Isolated);
    
    net_curr <- asNetwork(g)
    
    authority <- authority_score(g)
    closeness <- closeness(g)
    #constraint <- constraint(g)
    degree <- igraph::degree(g)
    eccentricity <- eccentricity(g)
    estimate_betw <- estimate_betweenness(g, cutoff = -1)
    local_ef <- local_efficiency(g)
    harmonic_c <-harmonic_centrality(g)
    #knn <- knn(g)
    pagerank <- page.rank(g)
    
    pagerank <- pagerank$vector
    #knn <- knn$knn
    authority <- authority$vector
    #eigen_c <-eigen_c$vector
    #similarity <- similarity[1,]
    
    
    #class(clust_coeff2)
    #clust_coeff2[is.na(clust_coeff2)] <- 0
    
    
    net_curr %v% 'authority' <- authority
    net_curr %v% 'closeness' <- closeness
    #net_curr %v% 'constraint' <- constraint
    net_curr %v% 'degree' <- degree
    net_curr %v% 'eccentricity' <- eccentricity
    net_curr %v% 'estimate_betw' <- estimate_betw
    net_curr %v% 'local_ef' <- local_ef
    net_curr %v% 'harmonic_c' <- harmonic_c
    #net_curr %v% 'knn' <- knn
    net_curr %v% 'pagerank' <- pagerank
    
    new_list <- append(new_list, list(net_curr))
  }
  return (new_list);
}


x <- get_network_list(1, start = 4, end=20);

y <- preprocess(x)

res <- btergm(y ~ nodecov('closeness')  
             + nodecov('degree') + nodecov('eccentricity')
             + nodecov('estimate_betw') + nodecov('local_ef') + nodecov('harmonic_c')
             + nodecov('pagerank'),
             parallel = "multicore", ncpus = 2, 
)

summary(res)

