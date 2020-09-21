#Check the statistical significance by shuffling the data
N=c()
correlated.genes = unique(correlated.genes[,1])
overlap_array = c()
count = 1000
threshold = 0.9
for(x in 1:count){
  exp.data1 = exp.data[,-1]
  exp.data2 = exp.data1[,sample(ncol(exp.data1))]
  exp.data2 = cbind(exp.data$gene_short_name,exp.data2)
  #Correlation
  correlated.genes.shuffled = data.frame(name = character(0),value = numeric(0))
  colnames(correlated.genes.shuffled) = c("name","value")
  arr.index = c()
  for(i in 1:dim(exp.data2)[1]){
    C = cor(Mean.Expression.Stages,as.vector(unlist(exp.data2[i,2:7])),method = "spearman")
    if(!is.na(C) && abs(C) > threshold){
      nr = data.frame(exp.data2[i,1],as.double(C))
      colnames(nr) = c("name","value") 
      correlated.genes.shuffled = rbind(correlated.genes.shuffled,nr)
      arr.index= c(arr.index,i)
    }
  }
  overlap = as.double(length(intersect(unlist(correlated.genes.shuffled[,1]),unlist(correlated.genes)))/length(union(unlist(correlated.genes.shuffled[,1]),unlist(correlated.genes))))
  print(c(x,overlap))
  overlap_array = c(overlap_array,overlap)
  N = c(dim(correlated.genes.shuffled)[1],N)
  #end of shuffling
}
A=overlap_array
length(A[A>0.05])
png(filename = "~/Desktop/Fig_shuffled_1000.png",width = 600,height = 600)
barplot(overlap_array,ylim=c(0,1))
dev.off()