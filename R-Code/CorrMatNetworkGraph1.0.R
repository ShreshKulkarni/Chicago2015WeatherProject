#Plot the network graph with correlation matrix
require(xts)
require(quantmod)
require(igraph)

total_2015 <- read.csv("~/total_2015.csv")

cor_mat<- cor(total_2015[,-c(24,22,1)])
cor_mat[ lower.tri(cor_mat, diag=TRUE) ]<- 0
cor_mat[ abs(cor_mat) < 0.3]<- 0

graph <- graph.adjacency(cor_mat, weighted=TRUE, mode="upper")
E(graph)$weight<-t(cor_mat)[abs(t(cor_mat))>0.3]

#Assigning colors
colScale<-c("#d73027","#f46d43","#fdae61","#fee090","#e0f3f8","#abd9e9","#74add1","#4575b4")
E(graph)[ weight>0.8 ]$color <- colScale[8] #"#313695" 
E(graph)[ weight>=0.65 & weight<0.8 ]$color <- colScale[7] #"#4575b4" 
E(graph)[ weight>=0.5 & weight<0.65 ]$color <- colScale[6] #"#74add1" 
E(graph)[ weight>=0.3 & weight<0.5 ]$color <- colScale[5] #"#abd9e9"
E(graph)[ weight>=-0.4 & weight< -0.3 ]$color <- colScale[4] #"#fee090"
E(graph)[ weight>=-0.5 & weight< -0.4 ]$color <- colScale[3] #"#fdae61"
E(graph)[ weight>=-0.6 & weight< -0.5 ]$color <- colScale[2] #"#f46d43"
E(graph)[ weight>=-0.7 & weight< -0.6 ]$color <- colScale[1] #"#d73027"

V(graph)$label<- colnames(total_2015[,-c(24,22,1)])#V(graph)$name
graph$layout <- layout.circle


plot(decompose.graph(graph)[[which.max(sapply(decompose.graph(graph), vcount))]],
     edge.width =4,frame=T,vertex.label.degree=75 ) 
legend("bottomleft", title="Correlation Coefficient", cex=0.75,
       fill=rev(colScale), 
       legend=c(">0.8", "0.65-0.8","0.5-0.65","0.3-0.5","-0.3 - -0.4","-0.4 - -0.5","-0.5 - -0.6","-0.6 - -0.7"), 
       ncol=2)

