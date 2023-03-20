library("network")
library("igraph")
#install.packages("intergraph")
library('intergraph')
library('ergm')

#-----------------Comments Network--------------------------------------------------------------------------------

fitERGM <- function(filename) {
  #filename <- './co-change/Changes_common50_30.net'
  my_net1 <- read.paj(filename)
  
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
  power_c1 <- power_centrality(g2)
  similarity1 <- similarity(g2)
  #transitivity<-transitivity(g2)
  strength1 <- strength(g2)
  eigen_c1 <- eigen_centrality(g2)
  clust_coeff1 <- transitivity(g2, type="localundirected") 
  
  #?estimate_betweenness
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
  my_net1_r %v% 'power_c1' <- power_c1
  my_net1_r %v% 'similarity1' <- similarity1
  my_net1_r %v% 'strength1' <- strength1
  my_net1_r %v% 'eigen_c1' <- eigen_c1
  my_net1_r %v% 'clust_coeff1' <- clust_coeff1
  
  
  #fitting the ERGM
  
  all_on_edge_comments1 <- ergm(
    my_net1_r~edges+nodecov("authority1")+nodecov("closeness1")+nodecov("constraint1")
    +nodecov("degree1")+nodecov("eccentricity1")+nodecov("estimate_betw1")
    +nodecov("local_ef1")+nodecov("harmonic_c1")+nodecov("knn1")
    +nodecov("pagerank1")+nodecov("power_c1")+nodecov("similarity1")
    +nodecov("clust_coeff1"))
  
  print(summary(all_on_edge_comments1))
}

for (x in seq(from=40, to=50, by=10)) {
  filename <- paste('./co-comment/Comments_common50_', x, '.net', sep='')
  print(paste('For time stamp:', x))
  fitERGM(filename)
}

