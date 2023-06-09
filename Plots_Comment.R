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
  clust_coeff1 <- transitivity(g2, type="localundirected") 
  
  knn1 <- knn1$knn
  authority1 <- authority1$vector
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
  
  #my_net1_r
  
  
  #fitting the ERGM
  ergm_comments_12 <- ergm(
    my_net1_r~edges+nodecov("authority1")+nodecov("closeness1")+nodecov("constraint1")
    +nodecov("degree1")+nodecov("eccentricity1")+nodecov("estimate_betw1")
    +nodecov("local_ef1")+nodecov("harmonic_c1")+nodecov("knn1")
    +nodecov("pagerank1")+nodecov("similarity1")
    +nodecov("clust_coeff1"))
  
  
  ergm_comments_6 <- ergm(
    my_net1_r~edges+nodecov("authority1")+nodecov("closeness1")
    +nodecov("degree1")+nodecov("eccentricity1")+nodecov("estimate_betw1")
    +nodecov("pagerank1"))
  
  ergm_comments_7 <- ergm(
    my_net1_r~edges+nodecov("authority1")+nodecov("closeness1")
    +nodecov("constraint1")+nodecov("degree1")+nodecov("eccentricity1")
    +nodecov("estimate_betw1")+nodecov("pagerank1"))
  
  metrics_list <- list(ergm_comments_6, ergm_comments_7, ergm_comments_12);
  
  return (metrics_list)
}

##----------------------------------Main--------------------------------------##

#-------------ERGM_6 important metrics-----------------------#
closeness_6 <- c()
degree_6 <- c()
estimate_betw_6 <- c()
pagerank_6 <- c()
authority_6 <- c()
#------------------------------------------------------------#

#-------------ERGM_7 important metrics-----------------------#
pagerank_7 <- c()
estimate_betw_7 <- c()
degree_7 <- c()
authority_7 <- c()
closeness_7 <- c()
constraint_7 <- c()
#------------------------------------------------------------#

#-------------ERGM_12 important metrics----------------------#
authority_12 <- c()     
closeness_12 <- c()    
constraint_12 <- c()  
degree_12 <- c()
harmonic_c_12 <- c()     
knn_12 <- c()
#------------------------------------------------------------#


file_names_vector = seq(from=36, to=50, by=1);

for (x in file_names_vector) {
  res <- get_metrics(x)
  
  ergm_6 <- res[[1]]
  ergm_7 <- res[[2]]
  ergm_12 <- res[[3]]
  
  ergm_6 <- ergm_6$coefficients
  ergm_7 <- ergm_7$coefficients
  ergm_12 <- ergm_12$coefficients
  
  
  #----------Adding ERGM_6 important metrics-------------------#
  closeness_6 <- append(closeness_6, ergm_6['nodecov.closeness1'])
  degree_6 <- append(degree_6, ergm_6['nodecov.degree1'])
  estimate_betw_6 <- append(estimate_betw_6, ergm_6['nodecov.estimate_betw1'])
  pagerank_6 <- append(pagerank_6, ergm_6['nodecov.pagerank1'])
  authority_6 <- append(authority_6, ergm_6['nodecov.authority1'])
  #------------------------------------------------------------#
  
  #----------Adding ERGM_7 important metrics-------------------#
  pagerank_7 <- append(pagerank_7, ergm_7['nodecov.pagerank1'])
  estimate_betw_7 <- append(estimate_betw_7, ergm_7['nodecov.estimate_betw1'])
  degree_7 <- append(degree_7, ergm_7['nodecov.degree1'])
  authority_7 <- append(authority_7, ergm_7['nodecov.authority1'])
  closeness_7 <- append(closeness_7, ergm_7['nodecov.closeness1'])
  constraint_7 <- append(constraint_7, ergm_7['nodecov.constraint1'])
  
  #------------------------------------------------------------#
  
  #----------Adding ERGM_12 important metrics------------------#
  authority_12 <- append(authority_12, ergm_12['nodecov.authority1'])  
  closeness_12 <- append(closeness_12, ergm_12['nodecov.closeness1'])
  constraint_12 <- append(constraint_12, ergm_12['nodecov.constraint1'])
  degree_12 <- append(degree_12, ergm_12['nodecov.degree1'])
  harmonic_c_12 <- append(harmonic_c_12, ergm_12['nodecov.harmonic_c1'])
  knn_12 <- append(knn_12, ergm_12['nodecov.knn1'])
  #------------------------------------------------------------#
}

#--------------------------------Plotting--------------------------------------#

#-----------Plotting ERGM_6 important metrics------------------#
plot(x=file_names_vector, y=closeness_6, type = "l", lwd=2, col = "red")
plot(x=file_names_vector, y=degree_6, type = "l", lwd=2, col = "red")
plot(x=file_names_vector, y=estimate_betw_6, type = "l", lwd=2, col = "red")
plot(x=file_names_vector, y=pagerank_6, type = "l", lwd=2, col = "red")
plot(x=file_names_vector, y=authority_6, type = "l", lwd=2, col = "red")
#--------------------------------------------------------------#

#-----------Plotting ERGM_7 important metrics------------------#
plot(x=file_names_vector, y=pagerank_7, type = "l", lwd=2, col = "darkgreen")
plot(x=file_names_vector, y=estimate_betw_7, type = "l", lwd=2, col = "darkgreen")
plot(x=file_names_vector, y=degree_7, type = "l", lwd=2, col = "darkgreen")
plot(x=file_names_vector, y=authority_7, type = "l", lwd=2, col = "darkgreen")
plot(x=file_names_vector, y=closeness_7, type = "l", lwd=2, col = "darkgreen")
plot(x=file_names_vector, y=constraint_7, type = "l", lwd=2, col = "darkgreen")
#--------------------------------------------------------------#

#----------Plotting ERGM_12 important metrics------------------#
plot(x=file_names_vector, y=authority_12, type = "l", lwd=2, col = "blue")
plot(x=file_names_vector, y=closeness_12, type = "l", lwd=2, col = "blue")
plot(x=file_names_vector, y=constraint_12, type = "l", lwd=2, col = "blue")
plot(x=file_names_vector, y=degree_12, type = "l", lwd=2, col = "blue")
plot(x=file_names_vector, y=harmonic_c_12, type = "l", lwd=2, col = "blue")
plot(x=file_names_vector, y=knn_12, type = "l", lwd=2, col = "blue")
#--------------------------------------------------------------#