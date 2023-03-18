my_net2 <- read.paj("./co-change/Changes_common50_50.net")
class(my_net2)

network.size(my_net2)
network.edgecount(my_net2) 
plot(my_net2,displaylabels=F,boxed.labels=F)
summary(my_net2)

#removing all the isolated nodes

gx <- asIgraph(my_net2)
class(gx)
Isolated2 = which(degree(gx)==0)
gx2 = igraph::delete.vertices(gx, Isolated2)

my_net2_r <- asNetwork(gx2)
summary(my_net2_r)
plot(my_net2_r, displaylabels=F, boxed.labels=F)
network.edgecount(my_net2_r) 

#finding the metrics of the changed network

authority2 <- authority_score(gx2)
closeness2 <- closeness(gx2)
constraint2 <- constraint(gx2)
degree2 <- degree(gx2)
eccentricity2 <- eccentricity(gx2)
estimate_betw2 <- estimate_betweenness(gx2, cutoff = -1)
local_ef2 <- local_efficiency(gx2)
harmonic_c2<-harmonic_centrality(gx2)
knn2 <- knn(gx2)
pagerank2 <- page.rank(gx2)
power_c2 <- power_centrality(gx2)
similarity2 <- similarity(gx2)
#transitivity<-transitivity(gx2)
strength2 <- strength(gx2)
eigen_c2 <- eigen_centrality(gx2)
clust_coeff2 <- transitivity(gx2, type="localundirected") 

#?estimate_betweenness

pagerank2 <- pagerank2$vector
knn2 <- knn2$knn
authority2 <- authority2$vector
eigen_c2 <-eigen_c2$vector
similarity2 <- similarity2[1,]


class(clust_coeff2)
clust_coeff2[is.na(clust_coeff2)] <- 0

#adding the metrics to the nodes

my_net2_r %v% 'authority2' <- authority2
my_net2_r %v% 'closeness2' <- closeness2
my_net2_r %v% 'constraint2' <- constraint2
my_net2_r %v% 'degree2' <- degree2
my_net2_r %v% 'eccentricity2' <- eccentricity2
my_net2_r %v% 'estimate_betw2' <- estimate_betw2
my_net2_r %v% 'local_ef2' <- local_ef2
my_net2_r %v% 'harmonic_c2' <- harmonic_c2
my_net2_r %v% 'knn2' <- knn2
my_net2_r %v% 'pagerank2' <- pagerank2
my_net2_r %v% 'power_c2' <- power_c2
my_net2_r %v% 'similarity2' <- similarity2
my_net2_r %v% 'strength2' <- strength2
my_net2_r %v% 'eigen_c2' <- eigen_c2
my_net2_r %v% 'clust_coeff2' <- clust_coeff2

my_net2_r

#exporting the changed graph

write_graph(gx2, "./Eclipse_Changes_Network.net", "pajek")

#fitting the ERGM

all_on_edge_changes2 <- ergm(
  my_net2_r~edges+nodecov("authority2")+nodecov("closeness2")+nodecov("constraint2")
  +nodecov("degree2")+nodecov("eccentricity2")+nodecov("estimate_betw2")
  +nodecov("local_ef2")+nodecov("harmonic_c2")+nodecov("knn2")
  +nodecov("pagerank2")+nodecov("power_c2")+nodecov("similarity2")
  +nodecov("clust_coeff2"))

summary(all_on_edge_changes2)

#Finding the goodness of fit

all_on_edge_changes_gof2<-gof(all_on_edge_changes2~distance)
all_on_edge_changes_gof2
plot(all_on_edge_changes_gof2)
