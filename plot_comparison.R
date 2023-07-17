library("network")
library("igraph")
#install.packages("intergraph")
library('intergraph')
library('ergm')



#-----------------Comments Network-------------------------------------#

get_metrics <- function(network_no) {
  file_name <- paste('./co-comment/Comments_common50_', network_no, '.net', sep='');
  print(file_name)
  #take the network as an input
  my_net1 <- read.paj(file_name)
  
  #removing all the isolated nodes
  g1 <- asIgraph(my_net1)
  Isolated = which(degree(g1)==0)
  g2 = igraph::delete.vertices(g1, Isolated)
  
  my_net1_r <- asNetwork(g2)
  
  #finding the metrics of the changed network
  authority1 <- authority_score(g2)
  closeness1 <- closeness(g2)
  constraint1 <- constraint(g2)
  degree1 <- degree(g2)
  eccentricity1 <- eccentricity(g2)
  estimate_betw1 <- estimate_betweenness(g2, cutoff = -1)
  local_ef1 <- local_efficiency(g2)
  harmonic_c1 <- harmonic_centrality(g2)
  knn1 <- knn(g2)
  pagerank1 <- page.rank(g2)
  similarity1 <- similarity(g2)
  eigen_c1 <- eigen_centrality(g2)
  clust_coeff1 <- transitivity(g2, type="localundirected") 
  
  knn1 <- knn1$knn
  authority1 <- authority1$vector
  eigen_c1 <- eigen_c1$vector
  similarity1 <- similarity1[1,]
  pagerank1 <- pagerank1$vector
  
  class(clust_coeff1)
  clust_coeff1[is.na(clust_coeff1)] <- 0
  
  #adding the metrics to the nodes
  my_net1_r %v% 'authority1' <- authority1
  my_net1_r %v% 'closeness1' <- closeness1
  my_net1_r %v% 'constraint1' <- constraint1
  my_net1_r %v% 'degree1' <- degree1
  my_net1_r %v% 'eccentricity1' <- eccentricity1
  my_net1_r %v% 'estimate_betw1' <- estimate_betw1
  my_net1_r %v% 'local_ef1' <- local_ef1
  my_net1_r %v% 'harmonic_c1' <- harmonic_c1
  my_net1_r %v% 'knn1' <- knn1
  my_net1_r %v% 'pagerank1' <- pagerank1
  my_net1_r %v% 'similarity1' <- similarity1
  my_net1_r %v% 'clust_coeff1' <- clust_coeff1
  my_net1_r %v% 'eigen_c1' <- eigen_c1
  
  #my_net1_r
  
  
  #fitting the ERGM
  ergm_res_without <- ergm(
    my_net1_r~edges+nodecov("closeness1")+nodecov("degree1")
    +nodecov("estimate_betw1")+nodecov("clust_coeff1")
    +nodecov("pagerank1")+nodecov("eigen_c1")
    )
  
  print(ergm_res_without)
  ergm_res_with <- ergm(
    my_net1_r~edges+nodecov("closeness1")+nodecov("authority1")+nodecov("degree1")
    +nodecov("estimate_betw1")+nodecov("clust_coeff1")
    +nodecov("pagerank1")+nodecov("eigen_c1")
  )
  print(ergm_res_with)
  
  res <- list(ergm_res_without, ergm_res_with);
  
  return (res)
}

##----------------------------------Main--------------------------------------##


closeness_without_addn_metric <- c()
degree_without_addn_metric <- c()
estimate_betw_without_addn_metric <- c()
eigen_c_without_addn_metric <- c()
cluster_coeff_without_addn_metric <- c()
pagerank_without_addn_metric <- c()
#authority_without_addn_metric <- c()

closeness_with_addn_metric <- c()
degree_with_addn_metric <- c()
estimate_betw_with_addn_metric <- c()
eigen_c_with_addn_metric <- c()
cluster_coeff_with_addn_metric <- c()
pagerank_with_addn_metric <- c()
#authority_with_addn_metric <- c()

extra_randm <- c()



file_names_vector = seq(from=50, to=50, by=1);

file_names_vector
for (x in file_names_vector) {
  res <- get_metrics(x)
  
  res_without_addn_metric <- res[[1]]
  res_with_addn_metric <- res[[2]]
  
  res_without_addn_metric <- res_without_addn_metric$coefficients
  res_with_addn_metric <- res_with_addn_metric$coefficients
  
  closeness_without_addn_metric <- append(closeness_without_addn_metric, res_without_addn_metric['nodecov.closeness1'])
  degree_without_addn_metric <- append(degree_without_addn_metric, res_without_addn_metric['nodecov.degree1'])
  estimate_betw_without_addn_metric <- append(estimate_betw_without_addn_metric, res_without_addn_metric['nodecov.estimate_betw1'])
  eigen_c_without_addn_metric <- append(eigen_c_without_addn_metric, res_without_addn_metric['nodecov.eigen_c1'])
  cluster_coeff_without_addn_metric <- append(cluster_coeff_without_addn_metric, res_without_addn_metric['nodecov.clust_coeff1'])
  pagerank_without_addn_metric <- append(pagerank_without_addn_metric, res_without_addn_metric['nodecov.pagerank1'])
  #authority_without_addn_metric <- append(authority_without_addn_metric, res_without_addn_metric['nodecov.authority1'])

  closeness_with_addn_metric <- append(closeness_with_addn_metric, res_with_addn_metric['nodecov.closeness1'])
  degree_with_addn_metric <- append(degree_with_addn_metric, res_with_addn_metric['nodecov.degree1'])
  estimate_betw_with_addn_metric <- append(estimate_betw_with_addn_metric, res_with_addn_metric['nodecov.estimate_betw1'])
  eigen_c_with_addn_metric <- append(eigen_c_with_addn_metric, res_with_addn_metric['nodecov.eigen_c1'])
  cluster_coeff_with_addn_metric <- append(cluster_coeff_with_addn_metric, res_with_addn_metric['nodecov.clust_coeff1'])
  pagerank_with_addn_metric <- append(pagerank_with_addn_metric, res_with_addn_metric['nodecov.pagerank1'])
  #authority_with_addn_metric <- append(authority_with_addn_metric, res_with_addn_metric['nodecov.authority1'])  
  
}

#--------------------------------Plotting--------------------------------------#
plot_graph <- function(y, col, file_name) {
  file_name <- paste('./img/', file_name, '.png', sep='');
  png(file=file_name, width=690, height=400)
  plot(x=file_names_vector, y=y,  ylab=deparse(substitute(y)), type = "l", lwd=2, col = col)
  dev.off()
}


#-----------Plotting ERGM without additional metric------------------#
plot_graph(y=closeness_without_addn_metric, col = "red", file_name='closeness_wO')
plot_graph(y=degree_without_addn_metric, col = "red", file_name='degree_wO')
plot_graph(y=estimate_betw_without_addn_metric, col = "red", file_name='estimate_betw_wO')
plot_graph(y=eigen_c_without_addn_metric, col = "red", file_name='eigen_c_wO')
plot_graph(y=cluster_coeff_without_addn_metric, col = "red", file_name='cluster_coeff_wO')
plot_graph(y=pagerank_without_addn_metric, col = "red", file_name='pagerank_wO')
#--------------------------------------------------------------#

#-----------Plotting ERGM with additional metric------------------#
plot_graph(y=closeness_with_addn_metric, col = "darkgreen", file_name='closeness_w')
plot_graph(y=degree_with_addn_metric, col = "darkgreen", file_name='degree_w')
plot_graph(y=estimate_betw_with_addn_metric, col = "darkgreen", file_name='estimate_betw_w')
plot_graph(y=eigen_c_with_addn_metric, col = "darkgreen", file_name='eigen_c_w')
plot_graph(y=cluster_coeff_with_addn_metric, col = "darkgreen", file_name='cluster_coeff_w')
plot_graph(y=pagerank_with_addn_metric, col = "darkgreen", file_name='pagerank_w')
#--------------------------------------------------------------#

