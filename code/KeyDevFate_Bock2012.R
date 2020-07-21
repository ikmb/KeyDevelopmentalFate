library("readxl")
setwd("~/Desktop/KeyDevelopmentalFate/data/")
data.table = read_excel("Bock2012_1-s2.0-S1097276512005448-mmc3.xlsx",2)
data.table = noquote(data.table)
data.table = data.table[!is.na(data.table$HSC) & !is.na(data.table$MPP1) & !is.na(data.table$MPP2) & !is.na(data.table$CLP) & !is.na(data.table$CD4)
                        & !is.na(data.table$CD8) & !is.na(data.table$B_cell) & !is.na(data.table$CMP) & !is.na(data.table$MEP) & !is.na(data.table$GMP)
                        & !is.na(data.table$Eryth) & !is.na(data.table$Granu) & !is.na(data.table$Mono),]
HSC = noquote(data.table$HSC)
MPP1 = noquote(data.table$MPP1)
MPP2 = noquote(data.table$MPP2)
CLP = noquote(data.table$CLP)
CMP = noquote(data.table$CMP)
CD4 = noquote(data.table$CD4)
CD8 = noquote(data.table$CD8)
BCell = noquote(data.table$B_cell)
CMP = noquote(data.table$CMP)
MEP = noquote(data.table$MEP)
GMP = noquote(data.table$GMP)
Eryth = noquote(data.table$Eryth)
Granu = noquote(data.table$Granu)
Mono = noquote(data.table$Mono)
#Patterned candidates
HSC.KP = data.table[(HSC > MPP1) & (MPP1 > MPP2) & (MPP2 > CLP) & (CLP > CD4) & (CLP > CD8) & (CLP > BCell)
                    & (MPP2 > CMP) & (CMP > GMP) & (CMP > MEP) & (MEP > Eryth) & (GMP > Granu) & (GMP > Mono),c(7:20)]
MPP1.KP = data.table[(HSC < MPP1) & (MPP1 > MPP2) & (MPP2 > CLP) & (CLP > CD4) & (CLP > CD8) & (CLP > BCell)
                    & (MPP2 > CMP) & (CMP > GMP) & (CMP > MEP) & (MEP > Eryth) & (GMP > Granu) & (GMP > Mono),c(7:20)]
MPP2.KP = data.table[(HSC < MPP1) & (MPP1 < MPP2) & (MPP2 > CLP) & (CLP > CD4) & (CLP > CD8) & (CLP > BCell)
                    & (MPP2 > CMP) & (CMP > GMP) & (CMP > MEP) & (MEP > Eryth) & (GMP > Granu) & (GMP > Mono),c(7:20)]
CLP.KP = data.table[(HSC < MPP1) & (MPP1 < MPP2) & (MPP2 < CLP) & (CLP > CD4) & (CLP > CD8) & (CLP > BCell)
                    & (MPP2 > CMP) & (CMP > GMP) & (CMP > MEP) & (MEP > Eryth) & (GMP > Granu) & (GMP > Mono),c(7:20)]
CMP.KP = data.table[(HSC < MPP1) & (MPP1 < MPP2) & (MPP2 > CLP) & (CLP > CD4) & (CLP > CD8) & (CLP > BCell)
                    & (MPP2 < CMP) & (CMP > GMP) & (CMP > MEP) & (MEP > Eryth) & (GMP > Granu) & (GMP > Mono),c(7:20)]
MEP.KP = data.table[(HSC < MPP1) & (MPP1 < MPP2) & (MPP2 > CLP) & (CLP > CD4) & (CLP > CD8) & (CLP > BCell)
                    & (MPP2 < CMP) & (CMP > GMP) & (CMP < MEP) & (MEP > Eryth) & (GMP > Granu) & (GMP > Mono),c(7:20)]
GMP.KP = data.table[(HSC < MPP1) & (MPP1 < MPP2) & (MPP2 > CLP) & (CLP > CD4) & (CLP > CD8) & (CLP > BCell)
                    & (MPP2 < CMP) & (CMP < GMP) & (CMP > MEP) & (MEP > Eryth) & (GMP > Granu) & (GMP > Mono),c(7,8,9,10,12,13,14,18,19,20)]
CD4.KP = data.table[(HSC < MPP1) & (MPP1 < MPP2) & (MPP2 < CLP) & (CLP < CD4) & (CLP > CD8) & (CLP > BCell),c(7,8,9,10,11,15,16,17)]
CD8.KP = data.table[(HSC < MPP1) & (MPP1 < MPP2) & (MPP2 < CLP) & (CLP > CD4) & (CLP < CD8) & (CLP > BCell),c(7,8,9,10,11,15,16,17)]
BCell.KP = data.table[(HSC < MPP1) & (MPP1 < MPP2) & (MPP2 < CLP) & (CLP > CD4) & (CLP > CD8) & (CLP < BCell),c(7,8,9,10,11,15,16,17)]

Eryth.KP = data.table[(HSC < MPP1) & (MPP1 < MPP2) & (MPP2 < CMP) & (CMP > GMP) & (CMP < MEP) & (MEP < Eryth) & (GMP > Granu) & (GMP > Mono),c(7,8,9,10,12,13,14,18,19,20)]
Granu.KP = data.table[(HSC < MPP1) & (MPP1 < MPP2) & (MPP2 < CMP) & (CMP < GMP) & (CMP > MEP) & (MEP > Eryth) & (GMP < Granu) & (GMP > Mono),c(7,8,9,10,12,13,14,18,19,20)]
Mono.KP = data.table[(HSC < MPP1) & (MPP1 < MPP2) & (MPP2 < CMP) & (CMP < GMP) & (CMP > MEP) & (MEP > Eryth) & (GMP > Granu) & (GMP < Mono),c(7,8,9,10,12,13,14,18,19,20)]                    

df = list(HSC.KP[,1],HSC.KP[,1],MPP2.KP[,1],CLP.KP[,1],CMP.KP[,1],MEP.KP[,1],GMP.KP[,1],CD4.KP[,1],CD8.KP[,1],BCell.KP[,1],Eryth.KP[,1],Granu.KP[,1],Mono.KP[,1])
names(df) = c("'HSC'","'MPP1'","'MPP2'","'CLP'","'CMP'","'MEP'","'GMP'","'CD4'","'CD8'","'BCell'","'Eryth'","'Granu'","'Mono'")
for(i in 1:13){
  write.table(t(noquote(as.character(unlist(df[i])))),"./../result/Second_study/1st_layer/1stLayer_candidates.csv",sep = ", ",row.names = names(df)[i],quote = FALSE,col.names = FALSE,append = TRUE)
}
#TTRUST database as a reference for mouse TFs
data.TF <- read.delim("./trrust_rawdata.mouse.tsv",sep = "\t",header = FALSE)
TF.mouse <- unique(data.TF[,1])

#TFs in the KP genes
TF.HSC <- HSC.KP[HSC.KP$geneName %in% TF.mouse,]
TF.MPP1 <- MPP1.KP[MPP1.KP$geneName %in% TF.mouse,]
TF.MPP2 <- MPP2.KP[MPP2.KP$geneName %in% TF.mouse,]
TF.CLP <- CLP.KP[CLP.KP$geneName %in% TF.mouse,]
TF.CMP <- CMP.KP[CMP.KP$geneName %in% TF.mouse,]
TF.MEP <- MEP.KP[MEP.KP$geneName %in% TF.mouse,]
TF.GMP <- GMP.KP[GMP.KP$geneName %in% TF.mouse,]
TF.CD4 <- CD4.KP[CD4.KP$geneName %in% TF.mouse,]
TF.CD8 <- CD8.KP[CD8.KP$geneName %in% TF.mouse,]
TF.Bcell <- BCell.KP[BCell.KP$geneName %in% TF.mouse,]
TF.Eryth <- Eryth.KP[Eryth.KP$geneName %in% TF.mouse,]
TF.Granu <- Granu.KP[Granu.KP$geneName %in% TF.mouse,]
TF.Mono <- Mono.KP[Mono.KP$geneName %in% TF.mouse,]

df = list(TF.HSC[,1],TF.MPP1[,1],TF.MPP2[,1],TF.CLP[,1],TF.CMP[,1],TF.MEP[,1],TF.GMP[,1],TF.CD4[,1],TF.CD8[,1],TF.Bcell[,1],TF.Eryth[,1],TF.Granu[,1],TF.Mono[,1])
names(df) = c("'HSC'","'MPP1'","'MPP2'","'CLP'","'CMP'","'MEP'","'GMP'","'CD4'","'CD8'","'BCell'","'Eryth'","'Granu'","'Mono'")
for(i in 1:13){
  write.table(t(noquote(as.character(unlist(df[i])))),"./../result/Second_study/1st_layer/1stLayer_TFcandidates.csv",sep = ", ",row.names = names(df)[i],quote = FALSE,col.names = FALSE,append = TRUE)
}
targets.HSC = character()
targets.HSC =  unique(as.character(data.TF[data.TF[,1] %in% as.character(unlist(TF.HSC[,1])),2]))

targets.MPP1 = character()
targets.MPP1 = unique(as.character(data.TF[data.TF[,1] %in% TF.MPP1[,1],2]))

targets.MPP2 = character()
targets.MPP2 = unique(as.character(data.TF[data.TF[,1] %in% TF.MPP2[,1],2]))

targets.CLP = character()
targets.CLP = unique(as.character(data.TF[data.TF[,1] %in% TF.CLP[,1],2]))

targets.CMP = character()
targets.CMP = unique(as.character(data.TF[data.TF[,1] %in% TF.CMP[,1],2]))

targets.MEP = character()
targets.MEP = unique(as.character(data.TF[data.TF[,1] %in% TF.MEP[,1],2]))

targets.GMP = character()
targets.GMP = unique(as.character(data.TF[data.TF[,1] %in% TF.GMP[,1],2]))

targets.CD4 = character()
targets.CD4 = unique(as.character(data.TF[data.TF[,1] %in% TF.CD4[,1],2]))

targets.CD8 = character()
targets.CD8 = unique(as.character(data.TF[data.TF[,1] %in% TF.CD8[,1],2]))

targets.Bcell = character()
targets.Bcell = unique(as.character(data.TF[data.TF[,1] %in% TF.Bcell[,1],2]))

targets.Eryth = character()
targets.Eryth = unique(as.character(data.TF[data.TF[,1] %in% TF.Eryth[,1],2]))

targets.Granu = character()
targets.Granu = unique(as.character(data.TF[data.TF[,1] %in% TF.Granu[,1],2]))

targets.Mono = character()
targets.Mono = unique(as.character(data.TF[data.TF[,1] %in% TF.Mono[,1],2]))

#Second and third layer candidates
mean.HSC = mean(as.numeric(HSC.KP$HSC))
mean.MPP1 = mean(as.numeric(MPP1.KP$MPP1))
mean.MPP2 = mean(as.numeric(MPP2.KP$MPP2))
mean.CLP = mean(as.numeric(CLP.KP$CLP))
mean.CMP = mean(as.numeric(CMP.KP$CMP))
mean.GMP = mean(as.numeric(GMP.KP$GMP))
mean.MEP = mean(as.numeric(MEP.KP$MEP))
mean.CD4 = mean(as.numeric(CD4.KP$CD4))
mean.CD8 = mean(as.numeric(CD8.KP$CD8))
mean.BCell = mean(as.numeric(BCell.KP$B_cell))
mean.Mono = mean(as.numeric(Mono.KP$Mono))
mean.Granu = mean(as.numeric(Granu.KP$Granu))
mean.Eryth = mean(as.numeric(Eryth.KP$Eryth))
mean.CD4.lineage = c(mean.HSC,mean.MPP1,mean.MPP2,mean.CLP,mean.CD4)
mean.CD8.lineage = c(mean.HSC,mean.MPP1,mean.MPP2,mean.CLP,mean.CD8)
mean.Bcell.lineage = c(mean.HSC,mean.MPP1,mean.MPP2,mean.CLP,mean.BCell)
mean.Mono.lineage = c(mean.HSC,mean.MPP1,mean.MPP2,mean.CMP,mean.GMP,mean.Mono)
mean.Granu.lineage = c(mean.HSC,mean.MPP1,mean.MPP2,mean.CMP,mean.GMP,mean.Granu)
mean.Eryth.lineage = c(mean.HSC,mean.MPP1,mean.MPP2,mean.CMP,mean.MEP,mean.Eryth)

png(filename = "./../result/Second_study/2nd_layer/2ndLayer_patternPlot.png",width = 1200,height = 600)
par(mfrow=c(2,3))
par(mar = c(6,6,2,2))
#CD4
plot(mean.CD4.lineage,col = c("red","gold","green","cyan","blue"),pch=20,cex=4,ylab = "mean.expression",xlab="",xaxt="n",cex.lab=2,yaxt="n",type="o",main = "CD4",cex.main=2)
lines(mean.CD4.lineage,col="black")
axis(1,at = seq(1,5,by=1), labels = c("HSC","MPP1","MPP2","CLP","CD4"),cex.axis=1.5)
axis(2,cex.axis=1.5)
#CD8
plot(mean.CD8.lineage,col = c("red","gold","green","cyan","blue"),pch=20,cex=4,ylab = "",xlab="",xaxt="n",cex.lab=2,yaxt="n",type="o",main = "CD8",cex.main=2)
lines(mean.CD8.lineage,col="black")
axis(1,at = seq(1,5,by=1), labels = c("HSC","MPP1","MPP2","CLP","CD8"),cex.axis=1.5)
axis(2,cex.axis=1.5)
#BCell
plot(mean.Bcell.lineage,col = c("red","gold","green","cyan","blue"),pch=20,cex=4,ylab = "",xlab="",xaxt="n",cex.lab=2,yaxt="n",type="o",main = "B-cell",cex.main=2)
lines(mean.Bcell.lineage,col="black")
axis(1,at = seq(1,5,by=1), labels = c("HSC","MPP1","MPP2","CLP","B-cell"),cex.axis=1.5)
axis(2,cex.axis=1.5)
#Eryth
plot(mean.Eryth.lineage,col = c("red","gold","green","cyan","blue","purple"),pch=20,cex=4,ylab = "",xlab="",xaxt="n",cex.lab=2,yaxt="n",type="o",main = "Eryth",cex.main=2)
lines(mean.Eryth.lineage,col="black")
axis(1,at = seq(1,6,by=1), labels = c("HSC","MPP1","MPP2","CMP","MEP","Eryth"),cex.axis=1.5)
axis(2,cex.axis=1.5)
#Granu
plot(mean.Granu.lineage,col = c("red","gold","green","cyan","blue","purple"),pch=20,cex=4,ylab = "",xlab="",xaxt="n",cex.lab=2,yaxt="n",type="o",main = "Granu",cex.main=2)
lines(mean.Granu.lineage,col="black")
axis(1,at = seq(1,6,by=1), labels = c("HSC","MPP1","MPP2","CMP","GMP","Granu"),cex.axis=1.5)
axis(2,cex.axis=1.5)
#Mono
plot(mean.Mono.lineage,col = c("red","gold","green","cyan","blue","purple"),pch=20,cex=4,ylab = "",xlab="",xaxt="n",cex.lab=2,yaxt="n",type="o",main = "Mono",cex.main=2)
lines(mean.Mono.lineage,col="black")
axis(1,at = seq(1,6,by=1), labels = c("HSC","MPP1","MPP2","CMP","GMP","Mono"),cex.axis=1.5)
axis(2,cex.axis=1.5)
dev.off()

#Correlation-CD4
correlated.genes.CD4 = data.frame(name = character(0),value = numeric(0))
colnames(correlated.genes.CD4) = c("name","value")
threshold = 0.9
arr.index.CD4 = c()
arr.perfect.CD4 = c()
for(i in 1:dim(data.table)[1]){
  C = cor(mean.CD4.lineage,as.numeric(unlist(data.table[i,c(8,9,10,11,15)])),method = "spearman")
  if(!is.na(C) && abs(C) > threshold){
    nr = data.frame(data.table[i,7],as.double(C))
    colnames(nr) = c("name","value") 
    correlated.genes.CD4 = rbind(correlated.genes.CD4,nr)
    arr.index.CD4= c(arr.index.CD4,i)
  }
}
exp.correlated.genes.CD4 = data.table[arr.index.CD4,]
TF.CG.CD4 <- correlated.genes.CD4[correlated.genes.CD4$name %in% TF.mouse,]
correlated.genes.CD4 = correlated.genes.CD4[order(correlated.genes.CD4$value),]
TF.CG.Regulator.CD4 = unique(data.TF[data.TF[,2] %in% correlated.genes.CD4[,1],1])
write.table(TF.CG.Regulator.CD4,"./../result/Second_study/2nd_layer/TFregulatorCD4.txt",col.names = FALSE,row.names = FALSE,quote = FALSE,sep = "\t")
TF.CG.Regulator.Target.CD4 = data.TF[data.TF[,2] %in% correlated.genes.CD4[,1],c(1,2)]
write.table(TF.CG.Regulator.Target.CD4,"./../result/Second_study/3rd_layer/TFtargetCD4.txt",col.names = FALSE,row.names = FALSE,quote = FALSE,sep = "\t")
#Correlation-CD8
correlated.genes.CD8 = data.frame(name = character(0),value = numeric(0))
colnames(correlated.genes.CD8) = c("name","value")
threshold = 0.9
arr.index.CD8 = c()
arr.perfect.CD8 = c()
for(i in 1:dim(data.table)[1]){
  C = cor(mean.CD8.lineage,as.numeric(unlist(data.table[i,c(8,9,10,11,16)])),method = "spearman")
  if(!is.na(C) && abs(C) > threshold){
    nr = data.frame(data.table[i,7],as.double(C))
    colnames(nr) = c("name","value") 
    correlated.genes.CD8 = rbind(correlated.genes.CD8,nr)
    arr.index.CD8= c(arr.index.CD8,i)
    if(C == 1 | C == -1)
      arr.perfect.CD8 = c(arr.perfect.CD8,i)
  }
}
exp.correlated.genes.CD8 = data.table[arr.index.CD8,]
TF.CG.CD8 <- correlated.genes.CD8[correlated.genes.CD8$name %in% TF.mouse,]
correlated.genes.CD8 = correlated.genes.CD8[order(correlated.genes.CD8$value),]
TF.CG.Regulator.CD8 = unique(data.TF[data.TF[,2] %in% correlated.genes.CD8[,1],1])
write.table(TF.CG.Regulator.CD8,"./../result/Second_study/2nd_layer/TFregulatorCD8.txt",col.names = FALSE,row.names = FALSE,quote = FALSE,sep = "\t")
TF.CG.Regulator.Target.CD8 = data.TF[data.TF[,2] %in% correlated.genes.CD8[,1],c(1,2)]
write.table(TF.CG.Regulator.Target.CD8,"./../result/Second_study/3rd_layer/TFtargetCD8.txt",col.names = FALSE,row.names = FALSE,quote = FALSE,sep = "\t")
###
#Correlation-Bcell
correlated.genes.Bcell = data.frame(name = character(0),value = numeric(0))
colnames(correlated.genes.Bcell) = c("name","value")
threshold = 0.9
arr.index.Bcell = c()
arr.perfect.Bcell = c()
for(i in 1:dim(data.table)[1]){
  C = cor(mean.Bcell.lineage,as.numeric(unlist(data.table[i,c(8,9,10,11,17)])),method = "spearman")
  if(!is.na(C) && abs(C) > threshold){
    nr = data.frame(data.table[i,7],as.double(C))
    colnames(nr) = c("name","value") 
    correlated.genes.Bcell = rbind(correlated.genes.Bcell,nr)
    arr.index.Bcell= c(arr.index.Bcell,i)
  }
}
exp.correlated.genes.Bcell = data.table[arr.index.Bcell,]
TF.CG.Bcell <- correlated.genes.Bcell[correlated.genes.Bcell$name %in% TF.mouse,]
correlated.genes.Bcell = correlated.genes.Bcell[order(correlated.genes.Bcell$value),]
TF.CG.Regulator.Bcell = unique(data.TF[data.TF[,2] %in% correlated.genes.Bcell[,1],1])
write.table(TF.CG.Regulator.Bcell,"./../result/Second_study/2nd_layer/TFregulatorBcell.txt",col.names = FALSE,row.names = FALSE,quote = FALSE,sep = "\t")
TF.CG.Regulator.Target.Bcell = data.TF[data.TF[,2] %in% correlated.genes.Bcell[,1],c(1,2)]
write.table(TF.CG.Regulator.Target.Bcell,"./../result/Second_study/3rd_layer/TFtargetBcell.txt",col.names = FALSE,row.names = FALSE,quote = FALSE,sep = "\t")
###
#Correlation-Eryth
correlated.genes.Eryth = data.frame(name = character(0),value = numeric(0))
colnames(correlated.genes.Eryth) = c("name","value")
threshold = 0.9
arr.index.Eryth = c()
arr.perfect.Eryth = c()
for(i in 1:dim(data.table)[1]){
  C = cor(mean.Eryth.lineage,as.numeric(unlist(data.table[i,c(8,9,10,12,14,18)])),method = "spearman")
  if(!is.na(C) && abs(C) > threshold){
    nr = data.frame(data.table[i,7],as.double(C))
    colnames(nr) = c("name","value") 
    correlated.genes.Eryth = rbind(correlated.genes.Eryth,nr)
    arr.index.Eryth= c(arr.index.Eryth,i)
  }
}
exp.correlated.genes.Eryth = data.table[arr.index.Eryth,]
TF.CG.Eryth <- correlated.genes.Eryth[correlated.genes.Eryth$name %in% TF.mouse,]
correlated.genes.Eryth = correlated.genes.Eryth[order(correlated.genes.Eryth$value),]
TF.CG.Regulator.Eryth = unique(data.TF[data.TF[,2] %in% correlated.genes.Eryth[,1],1])
write.table(TF.CG.Regulator.Eryth,"./../result/Second_study/2nd_layer/TFregulatorEryth.txt",col.names = FALSE,row.names = FALSE,quote = FALSE,sep = "\t")
TF.CG.Regulator.Target.Eryth = data.TF[data.TF[,2] %in% correlated.genes.Eryth[,1],c(1,2)]
write.table(TF.CG.Regulator.Target.Eryth,"./../result/Second_study/3rd_layer/TFtargetEryth.txt",col.names = FALSE,row.names = FALSE,quote = FALSE,sep = "\t")
###
#Correlation-Granu
correlated.genes.Granu = data.frame(name = character(0),value = numeric(0))
colnames(correlated.genes.Granu) = c("name","value")
threshold = 0.9
arr.index.Granu = c()
arr.perfect.Granu = c()
for(i in 1:dim(data.table)[1]){
  C = cor(mean.Granu.lineage,as.numeric(unlist(data.table[i,c(8,9,10,12,13,19)])),method = "spearman")
  if(!is.na(C) && abs(C) > threshold){
    nr = data.frame(data.table[i,7],as.double(C))
    colnames(nr) = c("name","value") 
    correlated.genes.Granu = rbind(correlated.genes.Granu,nr)
    arr.index.Granu= c(arr.index.Granu,i)
  }
}
exp.correlated.genes.Granu = data.table[arr.index.Granu,]
TF.CG.Granu <- correlated.genes.Granu[correlated.genes.Granu$name %in% TF.mouse,]
correlated.genes.Granu = correlated.genes.Granu[order(correlated.genes.Granu$value),]
TF.CG.Regulator.Granu = unique(data.TF[data.TF[,2] %in% correlated.genes.Granu[,1],1])
write.table(TF.CG.Regulator.Granu,"./../result/Second_study/2nd_layer/TFregulatorGranu.txt",col.names = FALSE,row.names = FALSE,quote = FALSE,sep = "\t")
TF.CG.Regulator.Target.Granu = data.TF[data.TF[,2] %in% correlated.genes.Granu[,1],c(1,2)]
write.table(TF.CG.Regulator.Target.Granu,"./../result/Second_study/3rd_layer/TFtargetGranu.txt",col.names = FALSE,row.names = FALSE,quote = FALSE,sep = "\t")
###
#Correlation-Mono
correlated.genes.Mono = data.frame(name = character(0),value = numeric(0))
colnames(correlated.genes.Mono) = c("name","value")
threshold = 0.9
arr.index.Mono = c()
arr.perfect.Mono = c()
for(i in 1:dim(data.table)[1]){
  C = cor(mean.Mono.lineage,as.numeric(unlist(data.table[i,c(8,9,10,12,13,20)])),method = "spearman")
  if(!is.na(C) && abs(C) > threshold){
    nr = data.frame(data.table[i,7],as.double(C))
    colnames(nr) = c("name","value") 
    correlated.genes.Mono = rbind(correlated.genes.Mono,nr)
    arr.index.Mono= c(arr.index.Mono,i)
  }
}
exp.correlated.genes.Mono = data.table[arr.index.Mono,]
TF.CG.Mono <- correlated.genes.Mono[correlated.genes.Mono$name %in% TF.mouse,]
correlated.genes.Mono = correlated.genes.Mono[order(correlated.genes.Mono$value),]
TF.CG.Regulator.Mono = unique(data.TF[data.TF[,2] %in% correlated.genes.Mono[,1],1])
write.table(TF.CG.Regulator.Mono,"./../result/Second_study/2nd_layer/TFregulatorMono.txt",col.names = FALSE,row.names = FALSE,quote = FALSE,sep = "\t")
TF.CG.Regulator.Target.Mono = data.TF[data.TF[,2] %in% correlated.genes.Mono[,1],c(1,2)]
write.table(TF.CG.Regulator.Target.Mono,"./../result/Second_study/3rd_layer/TFtargetMono.txt",col.names = FALSE,row.names = FALSE,quote = FALSE,sep = "\t")