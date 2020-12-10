  #Importazione e pulizia del dataset
    Prices<-read.csv("Prices.csv")
    Prices$Year<-NULL
    Prices$ï..City<-NULL
    
  #Grafico delle correlazioni 
    correlation<-round(cor(Prices),2)
    library(ggcorrplot)
    ggcorrplot(correlation, lab = TRUE)
    
  #Calcolo modello lineare
    Prices.lm= lm(Rent~.,data=Prices)
    summary(Prices.lm)
    
  #Riduzione modello
    Prices.lm2= lm(Rent~.-Weekend.Getaway,data=Prices)
    summary(Prices.lm2)
    
    Prices.lm3= lm(Rent~.-Weekend.Getaway-Bad.Habits,data=Prices)
    summary(Prices.lm3)
    
    Prices.lm4= lm(Rent~.-Weekend.Getaway-Bad.Habits-Haircut,data=Prices)
    summary(Prices.lm4)
    
    Prices.lm5= lm(Rent~.-Weekend.Getaway-Bad.Habits-Haircut-Cheap.Date,data=Prices)
    summary(Prices.lm5)
    
    Prices.lm6= lm(Rent~.-Weekend.Getaway-Bad.Habits-Haircut-Cheap.Date-Ticket.Public.Transport,data=Prices)
    summary(Prices.lm6)
    
    Prices.lm7= lm(Rent~.-Weekend.Getaway-Bad.Habits-Haircut-Cheap.Date-Ticket.Public.Transport-Gas,data=Prices)
    summary(Prices.lm7)
    
    Prices.lm8= lm(Rent~.-Weekend.Getaway-Bad.Habits-Haircut-Cheap.Date-Ticket.Public.Transport-Gas-Gym,data=Prices)
    summary(Prices.lm8)
    
   #Variazione varianza spiegata passo dopo passo
    r=matrix(nrow = 8,ncol = 2)
    r[1,]=c(summary(Prices.lm)$r.squared,summary(Prices.lm)$adj.r.squared)
    r[2,]=c(summary(Prices.lm2)$r.squared,summary(Prices.lm)$adj.r.squared)
    r[3,]=c(summary(Prices.lm3)$r.squared,summary(Prices.lm)$adj.r.squared)
    r[4,]=c(summary(Prices.lm4)$r.squared,summary(Prices.lm)$adj.r.squared)
    r[5,]=c(summary(Prices.lm5)$r.squared,summary(Prices.lm)$adj.r.squared)
    r[2,]=c(summary(Prices.lm2)$r.squared,summary(Prices.lm2)$adj.r.squared)
    r[3,]=c(summary(Prices.lm3)$r.squared,summary(Prices.lm3)$adj.r.squared)
    r[4,]=c(summary(Prices.lm4)$r.squared,summary(Prices.lm4)$adj.r.squared)
    r[5,]=c(summary(Prices.lm5)$r.squared,summary(Prices.lm5)$adj.r.squared)
    r[6,]=c(summary(Prices.lm6)$r.squared,summary(Prices.lm6)$adj.r.squared)
    r[7,]=c(summary(Prices.lm7)$r.squared,summary(Prices.lm7)$adj.r.squared)
    r[8,]=c(summary(Prices.lm8)$r.squared,summary(Prices.lm8)$adj.r.squared)
    ymin=min(r)
    ymax=max(r)
    plot(r[,1],pch=19,type = "b",col="red",ylim = c(ymin,ymax))
    lines(r[,2],pch=19,type = "b",col="blue")
    legend("bottomleft",inset=0.02,c("varianza spiegata","varianza spiegata corretta"),pt.bg =c("red","blue"),pch=c(22,22),bg="white",cex=0.9)
    
    
  #Modello non lineare
    lPrices=log(Prices)
    
    correlation2<-round(cor(lPrices),2)
    ggcorrplot(correlation2,lab = 2)
    
    lPrices.lm=lm(Rent~.,data = lPrices)
    summary(lPrices.lm)
    
    lPrices.lm2=lm(Rent~.-Bad.Habits,data = lPrices)
    summary(lPrices.lm2)
    
    lPrices.lm3=lm(Rent~.-Bad.Habits-Cheap.Date,data = lPrices)
    summary(lPrices.lm3)
    
    lPrices.lm4=lm(Rent~.-Bad.Habits-Cheap.Date-Weekend.Getaway,data = lPrices)
    summary(lPrices.lm4)
    
    lPrices.lm5=lm(Rent~.-Bad.Habits-Cheap.Date-Weekend.Getaway-Gas,data = lPrices)
    summary(lPrices.lm5)
    
    lPrices.lm6=lm(Rent~.-Bad.Habits-Cheap.Date-Weekend.Getaway-Gas-Ticket.Public.Transport,data = lPrices)
    summary(lPrices.lm6)
  
    lPrices.lm7=lm(Rent~.-Bad.Habits-Cheap.Date-Weekend.Getaway-Gas-Ticket.Public.Transport-Haircut,data = lPrices)
    summary(lPrices.lm7)
    
    r2=matrix(nrow = 7,ncol = 2)
    r2[1,]=c(summary(lPrices.lm)$r.squared,summary(lPrices.lm)$adj.r.squared)
    r2[2,]=c(summary(lPrices.lm2)$r.squared,summary(lPrices.lm)$adj.r.squared)
    r2[3,]=c(summary(lPrices.lm3)$r.squared,summary(lPrices.lm)$adj.r.squared)
    r2[4,]=c(summary(lPrices.lm4)$r.squared,summary(lPrices.lm)$adj.r.squared)
    r2[5,]=c(summary(lPrices.lm5)$r.squared,summary(lPrices.lm)$adj.r.squared)
    r2[2,]=c(summary(lPrices.lm2)$r.squared,summary(lPrices.lm2)$adj.r.squared)
    r2[3,]=c(summary(lPrices.lm3)$r.squared,summary(lPrices.lm3)$adj.r.squared)
    r2[4,]=c(summary(lPrices.lm4)$r.squared,summary(lPrices.lm4)$adj.r.squared)
    r2[5,]=c(summary(lPrices.lm5)$r.squared,summary(lPrices.lm5)$adj.r.squared)
    r2[6,]=c(summary(lPrices.lm6)$r.squared,summary(lPrices.lm6)$adj.r.squared)
    r2[7,]=c(summary(lPrices.lm7)$r.squared,summary(lPrices.lm7)$adj.r.squared)
    ymin=min(r2)
    ymax=max(r2)
    plot(r2[,1],pch=19,type = "b",col="red",ylim = c(ymin,ymax))
    lines(r2[,2],pch=19,type = "b",col="blue")
    legend("bottomleft",inset=0.02,c("varianza spiegata","varianza spiegata corretta"),pt.bg =c("red","blue"),pch=c(22,22),bg="white",cex=0.9)
    
    
  #Confronto tra modelli e autovalutazione  
  n=20
  err_lin=rep(0,n)
  err_log=rep(0,n)
  err_lin_red=rep(0,n)
  err_log_red=rep(0,n)
   

    for (i in 1:n) {
       testset<-sort(sample(98,5))
       trainingset<-Prices[-testset,]
       test<-Prices[testset,]
       trainingset.log<-lPrices[-testset,]
       test.log<-lPrices[testset,]
         
       trainingset.lm=lm(Rent~.,data = trainingset)
       trainingset.red.lm=lm(Rent~.-Weekend.Getaway-Bad.Habits-Haircut-Cheap.Date,data = trainingset)
       trainingset.log.lm=lm(Rent~.,data =trainingset.log)
       trainingset.log.red.lm=lm(Rent~.-Weekend.Getaway-Bad.Habits-Gas-Cheap.Date,data = trainingset.log)
           
       prediction<-predict(trainingset.lm,test)
       prediction.red<-predict(trainingset.red.lm,test)
       prediction.log<-predict(trainingset.log.lm,test.log)
       prediction.red.log<-predict(trainingset.log.red.lm,test.log)
             
       err_lin[i] = sqrt(mean((prediction - test$Rent)^2))
       err_lin_red[i] = sqrt(mean((prediction.red - test$Rent)^2))
       err_log[i] = sqrt(mean((exp(prediction.log) - test$Rent)^2))
       err_log_red[i] = sqrt(mean((exp(prediction.red.log) - test$Rent)^2))
       
    }
  
  mean(err_lin)
  sd(err_lin)
  
  mean(err_log)
  sd(err_log)
  
  mean(err_lin_red)
  sd(err_lin_red)
  
  mean(err_log_red)
  sd(err_log_red)
 
  
  gmin=min(err_lin,err_lin_red,err_log,err_log_red)
  gmax=max(err_lin,err_lin_red,err_log,err_log_red)
  
  plot(err_lin,type="b",pch=20,col="blue",ylim=c(gmin,gmax))
  points(err_log,type="b",pch=20,col="red")
  points(err_lin_red,type="b",pch=20,col="deepskyblue2")
  points(err_log_red,type="b",pch=20,col="tan1")
  legend("topleft",inset=0.02,c("modello lineare","modello lineare ridotto","modello logaritmico","modello logaritmico ridotto"),col=c("blue","deepskyblue2","red","tan1"),pch=c(19,19),bg="white",cex=0.8)
  
  #Analisi dei residui 
  Prices.lm.r=residuals(Prices.lm5)
  plot(fitted(Prices.lm5),Prices.lm.r,pch=19)
  Prices.log.lm.r=residuals(lPrices.lm5)
  plot(fitted(lPrices.lm5),Prices.log.lm.r,pch=19)  
  
  qqnorm(Prices.lm.r,pch=19)
  qqline(Prices.lm.r,col="red")
  
  qqnorm(Prices.log.lm.r,pch=19)
  qqline(Prices.log.lm.r,col="red")
  
  hist(Prices.log.lm.r,20,freq = F)
  lines(density(Prices.log.lm.r),col="red")
  m=mean(Prices.log.lm.r)
  s=sd(Prices.log.lm.r)
  lines(sort(Prices.log.lm.r),dnorm(sort(Prices.log.lm.r),m,s),col="blue")
  
  
  hist(Prices.lm.r,20,freq = F)
  lines(density(Prices.lm.r),col="red")
  m=mean(Prices.lm.r)
  s=sd(Prices.lm.r)
  lines(sort(Prices.lm.r),dnorm(sort(Prices.lm.r),m,s),col="blue")
  
  shapiro.test(Prices.lm.r)
  shapiro.test(Prices.log.lm.r)
  
  library(ggfortify)
  autoplot(lPrices.lm5)
  autoplot(Prices.lm5)
  
  boxplot(Prices.log.lm.r)$out
    
  
#dopo eliminazione outliers ouliers
  
lPrices<-lPrices[-c(28,52,93),]

  n=20
   
  err_log=rep(0,n)
  err_log_red=rep(0,n)
  
  err_rel=rep(0,n)
  err_red_rel=rep(0,n)
  
   
  for (i in 1:n) {
   testset<-sort(sample(95,4))
   
   trainingset.log<-lPrices[-testset,]
   test.log<-lPrices[testset,]
       
   trainingset.log.lm=lm(Rent~.,data =trainingset.log)
   trainingset.log.red.lm=lm(Rent~.-Weekend.Getaway-Bad.Habits-Gas-Cheap.Date,data = trainingset.log)
       
   prediction.log<-predict(trainingset.log.lm,test.log)
   prediction.red.log<-predict(trainingset.log.red.lm,test.log)
     
   err_log[i] = sqrt(mean((exp(prediction.log) - exp(test.log$Rent))^2))
   err_log_red[i] = sqrt(mean((exp(prediction.red.log) - exp(test.log$Rent))^2))
   
   err_rel[i]=sqrt(mean((exp(prediction.log) - exp(test.log$Rent))^2)/mean((exp(test.log$Rent))^2))
   err_red_rel[i]=sqrt(mean((exp(prediction.red.log) - exp(test.log$Rent))^2)/mean((exp(test.log$Rent))^2))
       
   }
   
   mean(err_log)
   mean(err_log_red)
   
   sd(err_log)
   sd(err_log_red)
   
   mean(err_rel)
   mean(err_red_rel)
  

  
   gmin=min(err_log,err_log_red)
   gmax=max(err_log,err_log_red)
   
   plot(err_log,type="b",pch=20,col="red",ylim=c(gmin,gmax))
   points(err_log_red,type="b",pch=20,col="tan1")
   legend("topright",inset=0.02,c("modello logaritmico","modello logaritmico ridotto"),col=c("red","tan1"),pch=c(19,19),bg="white",cex=0.8)
  
  
  
  
    
    