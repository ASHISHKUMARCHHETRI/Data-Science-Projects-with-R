data.matrix<-matrix(nrow=100,ncol=10)
colnames(data.matrix)<-c(
  paste("wt",1:5,sep = ""),
  paste("ko",1:5,sep=""))
rownames(data.matrix)<-paste("gene",1:100,sep="")
for (i in 1:100){
  wt.values<-rpois(5,lambda = sample(x=10:1000,size=1))
  ko.values<-rpois(5,lambda = sample(x=10:1000,size=1))
  
  data.matrix[i,]<-c(wt.values,ko.values)
}
head(data.matrix)

pca<-prcomp(t(data.matrix),scale = T)

plot(pca$x[,1],pca$x[,2])
pca.var.per<-pca$sdev^2
barplot(pca.var.per,main="Scree Plot",xlab="Principal Component",ylab="Percent Variation",col = "green")
library(ggplot2)
loading_scores<-pca$rotation[,1]
gene_scores<-abs(loading_scores)
gene_scores_ranked<-sort(gene_scores,decreasing = T)
top_10_genes<-names(gene_scores_ranked[1:10])
top_10_genes
pca$rotation[top_10_genes,1]
