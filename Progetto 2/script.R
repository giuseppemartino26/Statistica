estu<-read.csv("tabella.csv")
library(MASS)
source("s2_cmroc.r")

#Trasformo attributi in numerici
estu$Estuary_volume..ML.<-gsub(",","",estu$Estuary_volume..ML.)
estu$Perimeter..km.<-gsub(",","",estu$Perimeter..km.)
estu$Total_flush_time..days.<-gsub(",","",estu$Total_flush_time..days.)
estu$Catchment_area..km.2.<-gsub(",","",estu$Catchment_area..km.2.)

estu$Estuary_volume..ML.<-as.numeric(estu$Estuary_volume..ML.)
estu$Perimeter..km.<-as.numeric(estu$Perimeter..km.)
estu$Total_flush_time..days.<-as.numeric(estu$Total_flush_time..days.)
estu$Catchment_area..km.2.<-as.numeric(estu$Catchment_area..km.2.)

#Elimino attributi non considerati utili e fattori percentuali
estu<-estu[,-c(1,2,6,26,27)]

#elimino osservazioni con valori mancanti
estu<-na.omit(estu)

#Assegno 1 se la foce è di tipo BDL, 0 altrimenti
estu$Estuary_type[which(estu$Estuary_type != "BDL")]<-0
estu$Estuary_type[which(estu$Estuary_type == "BDL")]<-1
estu$Estuary_type<-as.numeric(estu$Estuary_type)

#estu$Disturbance_class<-factor(estu$Disturbance_class)
estu$Disturbance_class[which(estu$Disturbance_class == "VL")]<-1
estu$Disturbance_class[which(estu$Disturbance_class == "L")]<-2
estu$Disturbance_class[which(estu$Disturbance_class == "M")]<-3
estu$Disturbance_class[which(estu$Disturbance_class == "H")]<-4
estu$Disturbance_class[which(estu$Disturbance_class == "VH")]<-5
estu$Disturbance_class<-as.numeric(estu$Disturbance_class)

#rinomino gli attributi
colnames(estu)[c(4,6,7,11,12,13,14,16,17,18,20,23)] <- c("Temp","Salinity","Turbidity","Seagrass_area","Mangrove_area","Total_estuary_area","Estuary_volume","Total_area_saltmarsh","Average_depth","Perimeter","Total_flush_time","Catchment_area")


str(estu)
#ANALISI DISCRIMINANTE LINEARE---------------------------------------------------------
estu.lda=lda(Estuary_type~.,data = estu,CV=F)

#correlazioni
correlation<-round(cor(estu),2)
library(ggcorrplot)
ggcorrplot(correlation, lab = TRUE)

#lm
estu.lm=lm(Estuary_type~.,data = estu)
summary(estu.lm)
estu.lm2=lm(Estuary_type~.-Catchment_area,data = estu)
summary(estu.lm2)
estu.lm3=lm(Estuary_type~.-Catchment_area-Summer,data = estu)
summary(estu.lm3)
estu.lm4=lm(Estuary_type~.-Catchment_area-Summer-Total_area_saltmarsh,data = estu)
summary(estu.lm4)

#risolvo le collinearità eliminando gli attributi con correlazione 1
estu<-estu[,-c(3,16,23)]

estu.lda=lda(Estuary_type~.,data = estu,CV=F)
estu.lda.p=predict(estu.lda)
estu.lda.post=estu.lda.p$posterior[,2]

#accuratezza
sum((estu.lda.post>0.5)==(estu$Estuary_type>0.5))/length(estu$Estuary_type)

#matrice di confusione
s2_confusion(estu$Estuary_type,estu.lda.post)

#area sotto la curva
estu.lda.roc=s2_roc(estu$Estuary_type,estu.lda.post)
s2_auc(estu.lda.roc)


#ANALISI DISCRIMINANTE QUADRATICA-------------------------------------------------------
estu.qda=qda(Estuary_type~.,data=estu,CV=F)

estu.qda=qda(Estuary_type~.,data=estu,CV=F)
estu.qda.p=predict(estu.qda)
estu.qda.post=estu.qda.p$posterior[,2]

#accuratezza qda
sum((estu.qda.post>0.5)==(estu$Estuary_type>0.5))/length(estu$Estuary_type)

#matrice di confusione qda
s2_confusion(estu$Estuary_type,estu.qda.post)

#area sotto la curva
estu.qda.roc=s2_roc(estu$Estuary_type,estu.qda.post)
s2_auc(estu.qda.roc)



#REGRESSIONE LOGISTICA-----------------------------------------------------------------
estu.glm=glm(Estuary_type~.,data = estu,family = binomial)
estu.glm.p=predict(estu.glm,type = "response")

#accuratezza
sum((estu.glm.p>0.5)==(estu$Estuary_type>0.5))/2968

#matrice di confusione
s2_confusion(estu$Estuary_type,estu.glm.p)

#curva ROC
glm.roc<-s2_roc(estu$Estuary_type,estu.glm.p)
s2_auc(glm.roc) #area sotto la curva


#CURVE ROC----------------------------------------------------------------------------
s2_roc.plot(estu.lda.roc)
s2_roc.lines(estu.qda.roc, col="blue")
s2_roc.lines(glm.roc, col="green")
legend("bottomright",inset=0.02,c("lda, AUC=0,933","qda, AUC=0,953", "glm, AUC=0,968"),col=c("black","blue","green"),lty = 1,bg="white",cex=0.6)


#AUTOVALUTAZIONE----------------------------------------------------------------------
l=length(estu$Estuary_type)
acc=matrix(0,500,3)
for (i in 1:500) {
  idx=sample(l,500)
  estucv=estu[-idx,]
  estu.lda=lda(Estuary_type~.,data =estucv)
  estu.lda.p=predict(estu.lda,estu[idx,])$posterior[,2]
  acc[i,1]=sum((estu.lda.p>0.5)==(estu$Estuary_type[idx]>0.5))/500
  estu.qda=qda(Estuary_type~.,data = estucv)
  estu.qda.p=predict(estu.qda,estu[idx,])$posterior[,2]
  acc[i,2]=sum((estu.qda.p>0.5)==(estu$Estuary_type[idx]>0.5))/500
  estu.glm=glm(Estuary_type~.,family = binomial,data = estucv)
  estu.glm.p=predict(estu.glm,estu[idx,],type = "response")
  acc[i,3]=sum((estu.glm.p>0.5)==(estu$Estuary_type[idx]>0.5))/500
 }
plot(acc[,1],type="b",pch=20,col="red")
lines(acc[,2],type="b",pch=20,col="blue")
lines(acc[,3],type="b",pch=20,col="green")
legend("bottomright",inset=0.02,c("lda","qda", "glm"),col=c("red","blue","green"),pch=c(19,19),bg="white",cex=1)

#lda
mean(acc[,1])
sd(acc[,1])
#qda
mean(acc[,2])
sd(acc[,2])
#glm
mean(acc[,3])
sd(acc[,3])








