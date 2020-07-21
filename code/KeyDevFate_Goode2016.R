setwd("~/Desktop/KeyDevelopmentalFate/data/")
data = read.delim("Goode2016_GSE69080_expression_cufflinks.txt",sep="\t")
exp.data = data[,-c(1,2,4)]
#exp.data = exp.data[!is.na(exp.data$ESC) & !is.na(exp.data$MES) & !is.na(exp.data$HB) & !is.na(exp.data$HE) & !is.na(exp.data$HP) & !is.na(exp.data$MAC),]
#Globar regulators
GlobalRegulators = c("Pou5f1","Sox2","Nanog","Esrrb","Cebpb","Elk4","Fli1", "Tal1", "Lmo2","Runx1","Meis1","Gfi1","Gata2","Gfi1b","Gata1","Sfpi1")
GlobalRegulators %in% exp.data[,1]
exp.GR = exp.data[exp.data$gene_short_name %in% GlobalRegulators,]
par(mfrow = c(5,5))
exp.GR.selected = exp.GR[c(1,3,5,6,7,8,12,13,14,15,16,18,19,21),]
png(filename = "./../result/First_study/GR_selected.png",width = 480,height = 700)
par(mfrow = c(4,4))
for(i in 1:dim(exp.GR.selected)[1]){
  plot(seq(1:6),exp.GR.selected[row.names(exp.GR.selected)[i],c(2:7)],type = "l",xlab = exp.GR.selected$gene_short_name[i],ylab = "",col="blue",cex.lab = 1.5)
}
dev.off()

#Patterned candidates
m = 2
n = m + 1

ESC_set = c()
ESC_set_index = c()
for(i in 1:dim(exp.data)[1]){
  S = 0
  if(exp.data[i,2] == max(exp.data[i,-1])){
    if(exp.data[i,4] < exp.data[i,3])
      S = S + 1
    if(exp.data[i,5] < exp.data[i,4])
      S = S + 1
    if(exp.data[i,6] < exp.data[i,5])
      S = S + 1
    if(exp.data[i,7] < exp.data[i,6])
      S = S + 1
  }
  if(S > n){
    ESC_set = c(ESC_set,as.character(exp.data[i,1]))
    ESC_set_index = c(ESC_set_index,i)
  }
}

MES_set = c()
MES_set_index = c()
for(i in 1:dim(exp.data)[1]){
  S = 0
  if(exp.data[i,3] == max(exp.data[i,-1])){
    if(exp.data[i,5] < exp.data[i,4])
      S = S + 1
    if(exp.data[i,6] < exp.data[i,5])
      S = S + 1
    if(exp.data[i,7] < exp.data[i,6])
      S = S + 1
  }
  if(S > m){
    MES_set = c(MES_set,as.character(exp.data[i,1]))
    MES_set_index = c(MES_set_index,i)
  }
}

HB_set = c()
HB_set_index = c()
for(i in 1:dim(exp.data)[1]){
  S = 0
  if(exp.data[i,4] == max(exp.data[i,-1])){
    if(exp.data[i,2] < exp.data[i,3])
      S = S + 1
    if(exp.data[i,6] < exp.data[i,5])
      S = S + 1
    if(exp.data[i,7] < exp.data[i,6])
      S = S + 1
  }
  if(S > m){
    HB_set = c(HB_set,as.character(exp.data[i,1]))
    HB_set_index = c(HB_set_index,i)
  }
}

HE_set = c()
HE_set_index = c()
for(i in 1:dim(exp.data)[1]){
  S = 0
  if(exp.data[i,5] == max(exp.data[i,-1])){
    if(exp.data[i,2] < exp.data[i,3])
      S = S + 1
    if(exp.data[i,3] < exp.data[i,4])
      S = S + 1
    if(exp.data[i,7] < exp.data[i,6])
      S = S + 1
  }
  if(S > m){
    HE_set = c(HE_set,as.character(exp.data[i,1]))
    HE_set_index = c(HE_set_index,i)
  }
}

HP_set = c()
HP_set_index = c()
for(i in 1:dim(exp.data)[1]){
  S = 0
  if(exp.data[i,6] == max(exp.data[i,-1])){
    if(exp.data[i,2] < exp.data[i,3])
      S = S + 1
    if(exp.data[i,3] < exp.data[i,4])
      S = S + 1
    if(exp.data[i,4] < exp.data[i,5])
      S = S + 1
  }
  if(S > m){
    HP_set = c(HP_set,as.character(exp.data[i,1]))
    HP_set_index = c(HP_set_index,i)
  }
}

MAC_set = c()
MAC_set_index = c()
for(i in 1:dim(exp.data)[1]){
  S = 0
  if(exp.data[i,7] == max(exp.data[i,-1])){
    if(exp.data[i,2] < exp.data[i,3])
      S = S + 1
    if(exp.data[i,3] < exp.data[i,4])
      S = S + 1
    if(exp.data[i,4] < exp.data[i,5])
      S = S + 1
    if(exp.data[i,5] < exp.data[i,6])
      S = S + 1
  }
  if(S > n){
    MAC_set = c(MAC_set,as.character(exp.data[i,1]))
    MAC_set_index = c(MAC_set_index,i)
  }
}
#First layer candidates (sorted)
exp.ESC = exp.data[ESC_set_index,]
ESC.1stLayer = exp.ESC[order(-exp.ESC$ESC),]

exp.MES = exp.data[MES_set_index,]
MES.1stLayer = exp.MES[order(-exp.MES$MES),]

exp.HB = exp.data[HB_set_index,]
HB.1stLayer = exp.HB[order(-exp.HB$HB),]

exp.HE = exp.data[HE_set_index,]
HE.1stLayer = exp.HE[order(-exp.HE$HE),]

exp.HP = exp.data[HP_set_index,]
HP.1stLayer = exp.HP[order(-exp.HP$HP),]

exp.MAC = exp.data[MAC_set_index,]
MAC.1stLayer = exp.MAC[order(-exp.MAC$MAC),]

#Write first layer candidates in text files
write.table(ESC.1stLayer,"./../result/First_study/1st_layer/1stLayer_ESC_candidates.csv",sep="\t",row.names = FALSE,col.names = FALSE)
write.table(MES.1stLayer,"./../result/First_study/1st_layer/1stLayer_MES_candidates.csv",sep="\t",row.names = FALSE,col.names = FALSE)
write.table(HB.1stLayer,"./../result/First_study/1st_layer/1stLayer_HB_candidates.csv",sep="\t",row.names = FALSE,col.names = FALSE)
write.table(HE.1stLayer,"./../result/First_study/1st_layer/1stLayer_HE_candidates.csv",sep="\t",row.names = FALSE,col.names = FALSE)
write.table(HP.1stLayer,"./../result/First_study/1st_layer/1stLayer_HP_candidates.csv",sep="\t",row.names = FALSE,col.names = FALSE)
write.table(MAC.1stLayer,"./../result/First_study/1st_layer/1stLayer_MAC_candidates.csv",sep="\t",row.names = FALSE,col.names = FALSE)

#TTRUST database as a reference for mouse TFs
data.TF <- read.delim("./trrust_rawdata.mouse.tsv",sep = "\t",header = FALSE)
TF.mouse <- unique(data.TF[,1])

#TFs in the sorted genes
TF.ESC <- ESC.1stLayer[ESC.1stLayer[,1] %in% TF.mouse,]
TF.MES <- MES.1stLayer[MES.1stLayer[,1] %in% TF.mouse,]
TF.HB <- HB.1stLayer[HB.1stLayer[,1] %in% TF.mouse,]
TF.HE <- HE.1stLayer[HE.1stLayer[,1] %in% TF.mouse,]
TF.HP <- HP.1stLayer[HP.1stLayer[,1] %in% TF.mouse,]
TF.MAC <- MAC.1stLayer[MAC.1stLayer[,1] %in% TF.mouse,]

#Targets of the TFs in the database
targets.ESC = character()
targets.ESC = unique(as.character(data.TF[data.TF[,1] %in% TF.ESC[,1],2]))

targets.MES = character()
targets.MES = unique(as.character(data.TF[data.TF[,1] %in% TF.MES[,1],2]))

targets.HB = character()
targets.HB = unique(as.character(data.TF[data.TF[,1] %in% TF.HB[,1],2]))

targets.HP = character()
targets.HP = unique(as.character(data.TF[data.TF[,1] %in% TF.HP[,1],2]))

targets.HE = character()
targets.HE = unique(as.character(data.TF[data.TF[,1] %in% TF.HE[,1],2]))

targets.MAC = character()
targets.MAC = unique(as.character(data.TF[data.TF[,1] %in% TF.MAC[,1],2]))

#Depiction of the set of TFs for 6 stages of development following the pattern of expression.
png(filename = "./../result/First_study/1st_layer/1stLayer_TF_plot.png",width = 600,height = 600)
par(mfrow=c(2,3))
par(mar = c(6,6,2,2))
plot(seq(1:6),TF.ESC[1,2:7],type="o",xlab = "ESC",ylab = "expression (FPKM)",col ="red",cex.lab = 1.5)
for(i in 2:dim(TF.ESC)[1]){
  lines(seq(1:6),TF.ESC[i,2:7],type="o",xlab = "",ylab ="",col ="red")
}

plot(seq(1:6),TF.MES[1,2:7],type="o",xlab = "MES",ylab = "",col ="gold",cex.lab = 1.5)
for(i in 2:dim(TF.MES)[1]){
  lines(seq(1:6),TF.MES[i,2:7],type="o",xlab = "",ylab = "",col ="gold")
}

plot(seq(1:6),TF.HB[1,2:7],type="o",xlab = "HB",ylab = "",col ="green",cex.lab = 1.5)
for(i in 2:dim(TF.HB)[1]){
  lines(seq(1:6),TF.HB[i,2:7],type="o",xlab = "",ylab = "",col ="green")
}

plot(seq(1:6),TF.HE[1,2:7],type="o",xlab = "HE",ylab = "",col ="cyan",cex.lab = 1.5)
for(i in 2:dim(TF.HE)[1]){
  lines(seq(1:6),TF.HE[i,2:7],type="o",xlab = "",ylab = "",col ="cyan")
}

plot(seq(1:6),TF.HP[1,2:7],type="o",xlab = "HP",ylab = "",col ="blue",cex.lab = 1.5)
for(i in 2:dim(TF.HP)[1]){
  lines(seq(1:6),TF.HP[i,2:7],type="o",xlab = "",ylab = "",col ="blue")
}

plot(seq(1:6),TF.MAC[1,2:7],type="o",xlab = "MAC",ylab = "",col ="magenta",cex.lab = 1.5)
for(i in 2:dim(TF.MAC)[1]){
  lines(seq(1:6),TF.MAC[i,2:7],type="o",xlab = "",ylab = "",col ="magenta")
}
dev.off()

#Second layer candidates 
#print(c(mean(exp.ESC[,2]),mean(exp.MES[,3]),mean(exp.HB[,4]),mean(exp.HE[,5]),mean(exp.HP[,6]),mean(exp.MAC[,7])))
Mean.Expression.Stages = c(mean(exp.ESC[,2]),mean(exp.MES[,3]),mean(exp.HB[,4]),mean(exp.HE[,5]),mean(exp.HP[,6]),mean(exp.MAC[,7]))
png(filename = "./../result/First_study/2nd_layer/2ndLayer_patternPlot.png",width = 600,height = 600)
plot(Mean.Expression.Stages,col = c("red","gold","green","cyan","blue","magenta"),pch=20,cex=4,ylab = "mean.expression",xlab="",xaxt="n",cex.lab=2,yaxt="n")
lines(Mean.Expression.Stages,col="black")
axis(1,at = seq(1,6,by=1), labels = c("ESC","MES","HB","HE","HP","MAC"),cex.axis=1.5)
axis(2,cex.axis=1.5)
dev.off()

#Correlation
correlated.genes = data.frame(name = character(0),value = numeric(0))
colnames(correlated.genes) = c("name","value")
threshold = 0.9
arr.index = c()
arr.perfect = c()
for(i in 1:dim(exp.data)[1]){
  C = cor(Mean.Expression.Stages,as.vector(unlist(exp.data[i,2:7])),method = "spearman")
  if(!is.na(C) && abs(C) > threshold){
    nr = data.frame(exp.data[i,1],as.double(C))
    colnames(nr) = c("name","value") 
    correlated.genes = rbind(correlated.genes,nr)
    arr.index= c(arr.index,i)
    if(C == 1 | C == -1)
      arr.perfect = c(arr.perfect,i)
  }
}
correlated.genes = correlated.genes[order(-correlated.genes$value),]
write.table(correlated.genes,"./../result/First_study/2nd_layer/2ndLayer_candidates.csv",sep="\t",row.names = FALSE,col.names = FALSE)
exp.correlated.genes = exp.data[arr.index,]
exp.correlated.genes.perfect = exp.data[arr.perfect,]
png(filename = "./../result/First_study/2nd_layer/2ndLayer_correlated_pattern.png",width = 600,height = 600)
plot(x=seq(1:6),Mean.Expression.Stages,type = "o",col="red",xlab="",ylab="correlated genes",ylim=c(0,100),xaxt="n",yaxt="n",cex.lab=2)
axis(1,at = seq(1,6,by=1), labels = c("ESC","MES","HB","HE","HP","MAC"),cex.axis=1.5)
axis(2,cex.axis=1.5)
for(i in 1:14){
  lines(seq(1:6),exp.correlated.genes.perfect[i,2:7],type="o",xlab = "",ylab ="",col ="black")
}
dev.off()
#Identify the regulators of the SL genes and the intersected set with TF-specific ESC, MES, HB, HE, HP and MAC
TF.CG <- correlated.genes[correlated.genes$name %in% TF.mouse,]
TF.CG.Regulator = unique(data.TF[data.TF[,2] %in% correlated.genes[,1],1])
write.table(TF.CG.Regulator,"./../result/First_study/2nd_layer/TFregulator.txt",col.names = FALSE,row.names = FALSE,quote = FALSE,sep = "\t")
TF.CG.Regulator.ESC = unique(TF.CG.Regulator[TF.CG.Regulator %in% TF.ESC[,1]])
TF.CG.Regulator.MES = unique(TF.CG.Regulator[TF.CG.Regulator %in% TF.MES[,1]])
TF.CG.Regulator.HB = unique(TF.CG.Regulator[TF.CG.Regulator %in% TF.HB[,1]])
TF.CG.Regulator.HE = unique(TF.CG.Regulator[TF.CG.Regulator %in% TF.HE[,1]])
TF.CG.Regulator.HP = unique(TF.CG.Regulator[TF.CG.Regulator %in% TF.HP[,1]])
TF.CG.Regulator.MAC = unique(TF.CG.Regulator[TF.CG.Regulator %in% TF.MAC[,1]])

#Third layer candidates
TF.CG.Regulator.Target =data.TF[data.TF[,2] %in% correlated.genes[,1],c(1,2)]
write.table(TF.CG.Regulator.Target,"./../result/First_study/3rd_layer/TFtarget.txt",col.names = FALSE,row.names = FALSE,quote = FALSE,sep = "\t")