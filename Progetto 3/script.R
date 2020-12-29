trf<-read.csv("tabella.csv")

#elimino sabati e domeniche
trf<-s[-c(97:144,265:312,433:480,601:648,769:816,937:984,1105:1152,1273:1320,1441:1464),]

#visualizzazione della serie e funzione di autocorrelazione
trf.ts = ts(trf$Volume, frequency = 24)
par(mfrow=c(2,1))
plot(trf.ts)
acf(trf.ts,60)

#andamento giornaliero
par(mfrow=c(1,1))
m_trf = matrix(trf.ts, 24, 44)
par(bg = "black")
ts.plot(scale(m_trf, scale = F), col = heat.colors(44))
lines(rowMeans(scale(m_trf, scale = F)), lwd = 3, col = "white")

par(bg="white")
trf.sd=vector("numeric",24)
for (i in 1:24) {
  trf.sd[i]=sd(m_trf[i,])}
trf.m=rowMeans(m_trf)
plot(trf.m,pch=20,type = "b",ylim = range(c(trf.m-3*trf.sd,trf.m+3*trf.sd)))
arrows(1:24,trf.m- trf.sd,1:24,trf.m+trf.sd,length = 0.02,angle = 90,code = 3,col = "green3")

#DECOMPOSIZIONE-----------------------------------------------------------------------------

#decomposizione additiva
dec.a<-decompose(trf.ts)
plot(dec.a)

#decomposizione moltiplicativa
dec.m<-decompose(trf.ts,type = "multiplicative")
plot(dec.m)

#confronto stagionalità tra add e molt
plot(dec.a$seasonal)
lines(mean(dec.m$trend,na.rm =T)*(dec.m$seasonal-1),col="red")

#residui modello additivo
res.a<-as.vector(window(dec.a$random,c(1,13),c(44,12)))
plot(res.a,pch=20)
var(res.a)/var(window(trf.ts,c(1,13),c(44,12)))
acf(res.a)

layout(t(1:2))
hist(res.a, 20, freq = F)
lines(density(res.a), col = "blue")
lines(sort(res.a), dnorm(sort(res.a), mean(res.a), sd(res.a)), col = "red")
qqnorm(res.a)
qqline(res.a)
layout(1)
shapiro.test(res.a)

#residui modello moltiplicativo
res.m<-as.vector(window(dec.m$random,c(1,13),c(44,12)))
plot(log(res.m),pch=20)

res.m.l=log(res.m)
var(res.m.l)/var(window(log(trf.ts),c(1,13),c(44,12)))
acf(res.m.l,60)

layout(t(1:2))
hist(res.m.l, 20, freq = F)
lines(density(res.m.l), col = "blue")
lines(sort(res.m.l), dnorm(sort(res.m.l), mean(res.m.l), sd(res.m.l)), col = "red")
qqnorm(res.m.l)
qqline(res.m.l)
layout(1)
shapiro.test(res.m.l)

#stl
trf.stl<-stl(trf.ts,7)
plot(trf.stl)
res.stl<-as.vector(window(trf.stl$time.series[,3],c(1,13),c(44,12)))
plot(res.stl,pch=20)
acf(res.stl,60)
var(res.stl)/var(window(trf.ts,c(1,13),c(44,12)))

#variabilità dei residui a confronto
sd(acf(res.a,plot = F)$acf)
sd(acf(res.m.l,plot = F)$acf)
sd(acf(res.stl,plot = F)$acf)



#HOLT-WINTERS MOLTIPLICATIVO----------------------------------------------------------------
trf.hwm = HoltWinters(trf.ts,alpha = 0.01, gamma = 0.37, seasonal = "multiplicative")
plot(trf.hwm)
trf.hwm

#previsioni
plot(trf.hwm, predict(trf.hwm, 24), main = "Previsione a 1 giorno")

#residui
trf.hwm.r = residuals(trf.hwm)
layout(t(1:2))
plot(trf.hwm.r, type = "p", pch = 20)
plot(trf.hwm$fitted[, 1], trf.hw.r, pch = 20)
layout(1)

var(trf.hwm.r)/var(window(trf.ts, c(1, 1)))
acf(trf.hwm.r)
layout(t(1:2))
hist(trf.hwm.r, 20, freq = F)
lines(density(trf.hwm.r),col="red")
lines(sort(trf.hwm.r), dnorm(sort(trf.hwm.r), mean(trf.hwm.r), sd(trf.hwm.r)), col = "blue")
qqnorm(trf.hwm.r, pch = 20)
qqline(trf.hwm.r)
shapiro.test(trf.hwm.r)
layout(1)

#incertezze sulle previsioni
trf.hwm.p = predict(trf.hwm, 24)
y.max=max(trf.hwm.p+quantile(trf.hwm.r,0.975))
y.min=min(trf.hwm.p+quantile(trf.hwm.r,0.025))
ts.plot(trf.hwm.p,ylim=c(y.min,y.max))
lines(trf.hwm.p+quantile(trf.hwm.r,0.975),col="red")
lines(trf.hwm.p+quantile(trf.hwm.r,0.025),col="red")



#MINIMI QUADRATI--------------------------------------------------------------------------
trf.ls = ar(trf.ts, method = "ols")
trf.ls$order

#previsione
trf.ls.p= predict(trf.ls, n.ahead = 24, se.fit = FALSE)
ts.plot(window(trf.ts, 1), window(trf.ts - trf.ls$resid, 1), trf.ls.p, col = c("black", 
                                                                               "blue", "red"))
#var non spiegata
var(na.omit(trf.ls$resid))/var(trf.ts[27:1056])

#residui
layout(t(1:2))
trf.ls.r = as.double(na.omit(trf.ls$resid))
trf.ls.fitted = as.double(na.omit(trf.ts - trf.ls$resid))
plot(trf.ls.r, pch = 20)
plot(trf.ls.fitted, trf.ls.r, pch = 20)
var(trf.ls.r)/var(trf.ls.r + trf.ls.fitted)
acf(trf.ls.r)
pacf(trf.ls.r)
hist(trf.ls.r, 20, freq = F)
lines(density(trf.ls.r), col = "red")
lines(sort(trf.ls.r), dnorm(sort(trf.ls.r), mean(trf.ls.r), sd(trf.ls.r)), col = "blue")
qqnorm(trf.ls.r, pch = 20)
qqline(trf.ls.r)
shapiro.test(trf.ls.r)
layout(1)



#incertezze sulle previsioni
y.max=max(trf.ls.p+quantile(trf.ls.r,0.975))
y.min=min(trf.ls.p+quantile(trf.ls.r,0.025))
ts.plot(trf.ls.p,ylim=c(y.min,y.max))
lines(trf.ls.p+quantile(trf.ls.r,0.975),col="red")
lines(trf.ls.p+quantile(trf.ls.r,0.025),col="red")

ts.plot(trf.ls.p,trf.hwm.p,col=c("red","blue"))


#autovalutazione e confronto con holt-winters moltiplicativo
train = window(trf.ts, end = c(43, 24))
test = window(trf.ts, 44)
trf.hwm <- HoltWinters(train,alpha = 0.01, gamma = 0.37, seasonal = "multiplicative")
trf.hwm.p = predict(trf.hwm, 24)
trf.ls.p = predict(ar(train, method = "ols"), n.ahead = 24, se.fit = FALSE)
ts.plot(test, trf.ls.p,trf.hwm.p, col = c("black", "red","blue"))
legend("topleft",inset=0.02,c("Holt-Winters","Minimi Quadrati","Valori effettivi"),col=c("blue","red","black"),pch=c(19,19),bg="white",cex=1)
sqrt(mean((trf.ls.p - test)^2))
sqrt(mean((trf.hwm.p - test)^2))


#HOLT-WINTERS (additivo)---------------------------------------------------------------------------------
trf.hw = HoltWinters(trf.ts)
plot(trf.hw)

#previsioni
layout(t(1:2))
plot(trf.hw, predict(trf.hw, 24), main = "Previsione a 1 giorno")
plot(trf.hw, predict(trf.hw, 48), main = "Previsione a 2 giorni")
layout(1)

#residui
trf.hw.r = residuals(trf.hw)
plot(trf.hw.r, type = "p", pch = 20)
plot(trf.hw$fitted[, 1], trf.hw.r, pch = 20)

start(trf.ts)
end(trf.ts)
start(trf.hw.r)
end(trf.hw.r)
var(trf.hw.r)/var(window(trf.ts, c(1, 1)))
acf(trf.hw.r)
hist(trf.hw.r, 20, freq = F)
lines(density(trf.hw.r))
lines(sort(trf.hw.r), dnorm(sort(trf.hw.r), mean(trf.hw.r), sd(trf.hw.r)), col = "red")
qqnorm(trf.hw.r, pch = 20)
qqline(trf.hw.r)
shapiro.test(trf.hw.r)


#incertezze sulle previsioni
trf.hw.p = predict(trf.hw, 24)
y.max=max(trf.hw.p+quantile(trf.hw.r,0.975))
y.min=min(trf.hw.p+quantile(trf.hw.r,0.025))
ts.plot(trf.hw.p,ylim=c(y.min,y.max))
lines(trf.hw.p+quantile(trf.hw.r,0.975),col="red")
lines(trf.hw.p+quantile(trf.hw.r,0.025),col="red")


#autovalidazione
train = window(trf.ts, end = c(43, 24))
test = window(trf.ts, 44)
trf.hw <- HoltWinters(train)
trf.hw.p = predict(trf.hw, 24)
sqrt(mean((trf.hw.p - test)^2))
ts.plot(test, trf.hw.p, col = c("black", "red"))



#AUTOREGRESSIONE MODELLO DIRETTO----------------------------------------------------------------
pacf(trf.ts)

L = length(trf.ts)
l = 25  # numero di lag in ingresso
mnt = matrix(nrow = L - l, ncol = l + 1)
for (i in 1:(l + 1)) {
  mnt[, i] = trf.ts[i:(L - l - 1 + i)]
}
mnt <- data.frame(mnt)
trf.lm <- lm(X26 ~ ., data = mnt)  # X14 perché 13 lag in ingresso
summary(trf.lm)


giorni = 2
pt = rep(0, L + 24 * giorni)
pt[1:L] = trf.ts
for (i in 1:(24 * giorni)) {
     pt[L + i] = coef(trf.lm) %*% c(1, rev(pt[L + i - 1:l]))
 }
trf.lm.pt = ts(pt, frequency = 24, start = c(1, 1))
trf.lm.a = window(trf.ts, c(2, 2)) - resid(trf.lm)

ts.plot(trf.ts, trf.lm.a, window(trf.lm.pt, c(44, 24)), col = c("black", "blue", "red"))
                                                                                                                      
trf.lm.r=resid(trf.lm)
var(trf.lm.r)/var(window(trf.ts,1))

acf(trf.lm.r,60)

plot(trf.lm.a, trf.lm.r,pch=20)
hist(trf.lm.r, 20, freq = F)
lines(density(trf.lm.r), col = "blue")
lines(sort(trf.lm.r), dnorm(sort(trf.lm.r), mean(trf.lm.r), sd(trf.lm.r)), col = "red")
 # quantili
qqnorm(trf.lm.r, pch = 20)
qqline(trf.lm.r)
 # test
shapiro.test(trf.lm.r)

# training e test set
train = window(trf.ts, end = c(43, 24))
test = window(trf.ts, c(44, 1))
 
 # previsione del modello autoregressivo
train.lm = lm(X26 ~ ., data = mnt)
pt = rep(0, L)
pt[1:(L - l + 1)] = trf.ts[1:(L - l + 1)]
for (i in 1:24) {
     pt[L - l + 1 + i] = coef(train.lm) %*% c(1, rev(pt[L - l + 1 + i - 1:l]))
 }
train.lm.pt = window(ts(pt, frequency = 24, start = c(1, 1)), 44)
 # errore nelle predizioni
 sqrt(mean((train.lm.pt - test)^2))

 # confronto grafico
 ts.plot(test, train.lm.pt, col = c("black", "red"))
 
#YULE-WALKER--------------------------------------------------------------------------
 trf.ar = ar(trf.ts)
 trf.ar
 
 #prop var non spiegata
 trf.ar$var/var(trf.ts[29:length(trf.ts)])
 
 #analisi
 ts.plot(trf.ts, trf.ts - trf.ar$resid, col = c("black", "red"))
 
 #previsione
 trf.ar.pt = predict(trf.ar, n.ahead = 24, se.fit = FALSE)
 plot(trf.ar.pt)
 
 #residui
trf.ar.r=trf.ar$resid[29:1056]
plot(trf.ar.r,pch=20)
acf(trf.ar.r,60)

hist(trf.ar.r, 20, freq = F)
lines(density(trf.ar.r), col = "blue")
lines(sort(trf.ar.r), dnorm(sort(trf.ar.r), mean(trf.ar.r), sd(trf.ar.r)), col = "red")
# quantili
qqnorm(trf.ar.r, pch = 20)
qqline(trf.ar.r)
# test
shapiro.test(trf.ar.r)

#autovalutazione
train = window(trf.ts, end = c(43, 24))
test = window(trf.ts, 44)
trf.ar.p = predict(ar(train), n.ahead = 24, se.fit = FALSE)
ts.plot(test, trf.ar.p, col = c("black", "red"))
sqrt(mean((test - trf.ar.p)^2))





 