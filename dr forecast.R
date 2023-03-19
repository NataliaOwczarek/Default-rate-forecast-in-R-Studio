install.packages("forecast")
library(forecast)
install.packages("lattice")
library(lattice)
install.packages("tseries")
library(tseries)
install.packages("mFilter")
library(mFilter)
install.packages("openxlsx")
library(openxlsx)
install.packages("dplyr")
library(dplyr)
install.packages("FRAPO")
library(FRAPO)
install.packages("stats")
library(stats)
setwd("C:/Users/owcza/Desktop/studia/.ekonomia/r")
dane <- read.xlsx("DR_case_study_in_sample(2).xlsx")
head(dane)
length(dane)
View(dane)
#dane.s to pobrane DR z danych xlsx
defaultrate.s<-select(dane, DR)
defaultrate.ts<- ts(defaultrate.s, start = c(2010,1),frequency = 4)
start(defaultrate.ts)
end(defaultrate.ts)
frequency(defaultrate.ts)
defaultrate.ts


f <- FRAPO::trdhp(defaultrate.ts, lambda=1600)

par(mfcol=c(2,1),mar=c(4,4,1,1)+0.1, mgp=c(3,0.6,0),las=1)
plot(defaultrate.ts,col="SteelBlue",
     main="Hodrick-Prescott filtr")
lines(f,col="YellowGreen")
plot(defaultrate.ts-f,col="SteelBlue",
     main="składnik cykliczny po usunięciu trendu")






is.ts(defaultrate.ts) #f sprawdza, czy szereg jest czasowy

plot(defaultrate.ts, main="współczynnik niewykonania zobowiązania 
     (DR) w latach 2010-2019", xlab="Rok")
grid()

par(mar=rep(2, 4))
par(mfrow=c(2,1))
seasonplot(defaultrate.ts,year.labels= TRUE, 
           col = rainbow(5),
           main = "współczynnik niewykonania zobowiązania (DR) w podziale na lata")

Acf(defaultrate.ts)

defaultrate.ts.dekompozycja.add <- decompose(defaultrate.ts,"additive")
plot(defaultrate.ts.dekompozycja.add)

defaultrate.ts<- ts(defaultrate.s, start = c(2010,1),frequency = 4)
xyplot(defaultrate.ts,strip=TRUE,cut=list(number=3,overlap=0.5), col = rainbow(5),main = ("wykres sezonowy"))

par(mfrow=c(1,1))
Acf(defaultrate.ts)

defaultrate.ts.dekompozycja.add <- decompose(defaultrate.ts,"additive")
plot(defaultrate.ts.dekompozycja.add)

lag.plot(defaultrate.ts,lags=4,do.lines=FALSE,main="Wpółczynnik niewykonania zobowiązania-wykres opóźnień")
defaultrate.ts.dekompozycja.add <- decompose(defaultrate.ts,"additive")

#dev.new(noRStudioGD = TRUE)
#par(mfrow=c(2,1))
#Acf(defaultrate.ts)
#Acf(wahania.losowe)
#dev.new(noRStudioGD = TRUE)
#pacf(defaultrate.ts)
#pacf(wahania.losowe)



#dodajemy stopę procentową
dane2 <- read.xlsx("stopa proc.xlsx")
dane2.s<-select(dane2, R)
stopa_proc.ts<-ts(dane2.s,start=c(2010,1),frequency=4)

plot(stopa_proc.ts, main="Realna stopa procentowa w latach 2010-2019")

stopa_proc.ts.dekompozycja.add <- decompose(stopa_proc.ts,"additive")
plot(stopa_proc.ts.dekompozycja.add)

#H0:szereg niestacjonarny
#H1:szereg stacjonarny
adf.test(stopa_proc.ts)
#P-value>=5%->odrzucenie H0->szereg stacjonarny
#odrzucam H0 na rzecz H1, szereg jest STACJONARNY
#Phillips-Perron Test
pp.test(stopa_proc.ts)
#Phipips-Perron test ->stacjonarny
#KPSS test
#HO stacjonarność
#H1 brak stacjonarności
kpss.test(stopa_proc.ts,null="Trend")
#p-value<5%->brak podstaw do odrzucenia H0
#p-value>0,05,odrzucam H0 na rzecz H1

#Box-PierceQ test i Ljung -Box test
#H0 autokorelacja rzędu k=0
Box.test (defaultrate.ts,lag=4,type="Box-Pierce")
Box.test(stopa_proc.ts,lag=4,type="Box-Pierce")
Box.test(defaultrate.ts,lag=4,type="Ljung-Box")
Box.test(stopa_proc.ts,lag=4,type="Ljung-Box")

#Trnsformacja Boxa-Coxa
dev.new(noRStudioGD = TRUE)
par(mfrow=c(3,1))
plot(defaultrate.ts,main="Oryginalne dane",xlab="",ylab="")
grid()
plot(BoxCox(defaultrate.ts
            
            
            ,lambda=0.5),main="Dane po transformacji Boxa-Coxa, lambda=0.5",xlab="",ylab="")
grid()
plot(BoxCox(defaultrate.ts,lambda=0),main="Dane po transformacji Boxa-Coxa, lambda=0",xlab="",ylab="")
grid()

#Odwracanie
mean(defaultrate.ts)
bc<-BoxCox(defaultrate.ts,lambda=0.5)
mean(bc)
bc.inv<-InvBoxCox(bc,lambda=0.5)
mean(bc.inv)

#Różnicwowanie
stopa_proc.ts.diff1<-diff(stopa_proc.ts,differences=1)
dev.new(noRStudioGD = TRUE)
tsdisplay(stopa_proc.ts)
dev.new(noRStudioGD = TRUE)
tsdisplay(stopa_proc.ts.diff1)

#różnicowanie z opoznieniem okresowym
#różnicowanie
defaultrate.ts.diff4 <- diff(defaultrate.ts, lag=4)
dev.new(noRStudioGD = TRUE)
tsdisplay(defaultrate.ts)
dev.new(noRStudioGD = TRUE)
tsdisplay(defaultrate.ts.diff4)
#szereg defaultrate.ts nie jest stacjonarny i w defaultrate.ts.diff12 nadal wystepuje trend 
#wystepują tez odchylenia, warianca nie jest stała w czasie, wiec szereg nie jest stacjonarny

#podwójne różnicowanie z opoznieniem okresowym
defaultrate.ts.diff4.1 <- diff(defaultrate.ts.diff4, lag=1)
dev.new(noRStudioGD = TRUE)
tsdisplay(defaultrate.ts.diff4)
head(defaultrate.ts.diff4)
dev.new(noRStudioGD = TRUE)
tsdisplay(defaultrate.ts.diff4.1)

#odwrócenie, nie działa ale nwm po co to jest
#diffinv(defaultrate.ts.diff4.1,lag = 1, xi=0.0423)
#diffinv(defaultrate.ts.diff4, lag=4, xi=c(0.035,0.0438,0.0409,0.0333)
        
plot(defaultrate.ts)

#dodajemy bezrobocie
dane3 <- read.xlsx("dane_stopa_bezrobocia1.xlsx")
dane3.s<-select(dane3, U)
bezrobocie.ts<-ts(dane3.s,start=c(2010,1),frequency=4)

#H0:szereg niestacjonarny
#H1:szereg stacjonarny
adf.test(bezrobocie.ts)
#P-value>=5%->odrzucenie H0->szereg stacjonarny
#odrzucam H0 na rzecz H1, szereg jest STACJONARNY
#Phillips-Perron Test
pp.test(bezrobocie.ts)
#Phipips-Perron test ->stacjonarny
#KPSS test
#HO stacjonarność
#H1 brak stacjonarności
kpss.test(bezrobocie.ts,null="Trend")
#p-value<5%->brak podstaw do odrzucenia H0
#p-value>0,05,odrzucam H0 na rzecz H1, szereg nie jest stacjonarny.
#Box-PierceQ test i Ljung -Box test
#H0 autokorelacja rzędu k=0
# Box.test (defaultrate.ts,lag=4,type="Box-Pierce")
# Box.test(bezrobocie.ts,lag=4,type="Box-Pierce")
# Box.test(defaultrate.ts,lag=4,type="Ljung-Box")
# Box.test(bezrobocie.ts,lag=4,type="Ljung-Box")
# #Trnsformacja Boxa-Coxa
# dev.new(noRStudioGD = TRUE)
# par(mfrow=c(3,1))
# plot(defaultrate.ts,main="Oryginalne dane",xlab="",ylab="")
# grid()
# plot(BoxCox(defaultrate.ts,lambda=0.5),main="Dane po transformacji Boxa-Coxa, lambda=0.5",xlab="",ylab="")
# grid()
# plot(BoxCox(defaultrate.ts,lambda=0),main="Dane po transformacji Boxa-Coxa, lambda=0",xlab="",ylab="")
# grid()
# #Odwracanie
# mean(defaultrate.ts)
# bc<-BoxCox(defaultrate.ts,lambda=0.5)
# mean(bc)
# bc.inv<-InvBoxCox(bc,lambda=0.5)
# mean(bc.inv)
# #Różnicwowanie
# bezrobocie.ts.diff1<-diff(bezrobocie.ts,differences=1)
# dev.new(noRStudioGD = TRUE)
# tsdisplay(bezrobocie.ts)
# dev.new(noRStudioGD = TRUE)
# tsdisplay(bezrobocie.ts.diff1)
# #różnicowanie
# defaultrate.ts.diff12<-diff(defaultrate.ts,lag=12)
# dev.new(noRStudioGD = TRUE)
# tsdisplay(defaultrate.ts)
# dev.new(noRStudioGD = TRUE)
# tsdisplay(defaultrate.ts.diff12)
# #różnicowanie
# defaultrate.ts.diff12.1<-diff(defaultrate.ts.diff12, lag=1)
# dev.new(noRStudioGD = TRUE)
# tsdisplay(defaultrate.ts.diff12)
# dev.new(noRStudioGD = TRUE)
# tsdisplay(defaultrate.ts.diff12.1)
# #odwrócenie, nie działa ale nwm po co to jest
# #diffinv(defaultrate.ts.diff12.1,lag = 1, xi=0.0364)
# #diffinv(defaultrate.ts.diff12, lag=12, xi=c(0.035,0.0438,0.0409,0.0333,0.0323,0.0371,0.0352,0.0373,0.0388,0.0352,0.0324,0.0364
# )
#Dekompozycja
m3<-ma(defaultrate.ts.diff4,order=3,centre=TRUE)
m11<-ma(defaultrate.ts.diff4,order=11,centre=TRUE)
m21<-ma(defaultrate.ts.diff4,order=21,centre=TRUE)
dev.new(noRStudioGD = TRUE)
plot(m3,main="symetryczna średnia ruchoma",col="blue",lty=2,ylim=c(-0.04,0.03))
lines(m11,col="red",lty=2)
lines(m21,col="green",lty=2)
lines(defaultrate.ts.diff4,col="black",lty=1)
grid()
legend("bottomleft", legend=c("wyjsciowy szereg","ruchoma srednia", "ruchoma srednia q5", "ruchoma srednia q10"), col=c("black", "blue", "red", "green"),lty=c(1,2,2,2))


defaultrate.hp <-hpfilter(defaultrate.ts.diff4, freq=4, type="frequency", drift = TRUE)
defaultrate.bk <-bkfilter(defaultrate.ts.diff4, pl=2, pu=4, nfix=2, drift= TRUE)
defaultrate.cf <-cffilter(defaultrate.ts.diff4, pl=2, pu=4, drift= TRUE, root=TRUE)
dev.new(noRStudioGD = TRUE)
plot(defaultrate.hp$trend, main="filtrowanie", col= "blue", lty=2, ylim=c(-0.04, 0.08))
lines(defaultrate.bk$trend, col="red", lty=2)
lines(defaultrate.cf$trend, col="green", lty=2)
lines(defaultrate.ts.diff4, col="black", lty=1)
grid()


#dekompozycja szeregu
defaultrate.ts.dekomp.add<-decompose(defaultrate.ts, type="additive")
dev.new(noRStudioGD = TRUE)
plot(defaultrate.ts.dekomp.add)
dev.new(noRStudioGD =  TRUE)
tsdisplay(defaultrate.ts.dekomp.add$seasonal)

defaultrate.ts.dekomp.mult <-decompose(defaultrate.ts, type="multiplicative")
dev.new(noRStudioGD = TRUE)
plot(defaultrate.ts.dekomp.mult)
dev.new(noRStudioGD =  TRUE)
tsdisplay(defaultrate.ts.dekomp.mult$seasonal)


# dekompozycja na podsatwie modelu regresji
#sezonowos kodujemy za pomocą zmiennych 0-1
head(seasonaldummy(defaultrate.ts),4)
defaultrate.ts.trend <- tslm(defaultrate.ts ~ trend)
summary(defaultrate.ts.trend)
#intercept-wyraz wolny, kluczowe są gwiazdki i parametr tak jak w ekonometrii

dev.new(noRStudioGD = TRUE)
tsdisplay(residuals(defaultrate.ts.trend), main="reszty losowe")

#dane i dopasowanie trendu
dev.new(noRStudioGD = TRUE)
plot(defaultrate.ts)
lines(fitted(defaultrate.ts.trend), col="green", lty=2)
dev.new(noRStudioGD = TRUE)
tsdisplay(residuals(defaultrate.ts.trend), main="reszty losowe")


defaultrate.ts.trend.sezonowosc <- tslm(defaultrate.ts ~ trend + season)
summary(defaultrate.ts.trend.sezonowosc)
dev.new(noRStudioGD = TRUE)
plot(defaultrate.ts)
lines(fitted(defaultrate.ts.trend.sezonowosc), col="green", lty=2)

dev.new(noRStudioGD = TRUE)
tsdisplay(residuals(defaultrate.ts.trend.sezonowosc), main="reszty losowe")

# dokładamy boxa- coxa
defaultrate.ts.trend.sezonowosc.bc <- tslm(defaultrate.ts ~ trend, lambda = 0)
summary(defaultrate.ts.trend.sezonowosc.bc)

dev.new(noRStudioGD = TRUE)
plot(defaultrate.ts)
lines(fitted(defaultrate.ts.trend.sezonowosc.bc), col="green", lty=2)
dev.new(noRStudioGD = TRUE)
tsdisplay(residuals(defaultrate.ts.trend.sezonowosc.bc), main="reszty losowe")

#trend wielomianowy i sezonowosc 
defaultrate.ts.trend2.sezonowosc <- tslm(defaultrate.ts ~ trend +I(trend^2))
summary(defaultrate.ts.trend2.sezonowosc)
dev.new(noRStudioGD = TRUE)
plot(defaultrate.ts)
lines(fitted(defaultrate.ts.trend2.sezonowosc), col="green", lty=2)
dev.new(noRStudioGD = TRUE)
tsdisplay(residuals(defaultrate.ts.trend2.sezonowosc), main="reszty losowe")

defaultrate.ts.trend4.sezonowosc <- tslm(defaultrate.ts ~ trend +season+ poly(trend, raw = TRUE, degree = 30))
summary(defaultrate.ts.trend4.sezonowosc)
dev.new(noRStudioGD = TRUE)
plot(defaultrate.ts)
lines(fitted(defaultrate.ts.trend4.sezonowosc), col="green", lty=2)
dev.new(noRStudioGD = TRUE)
tsdisplay(residuals(defaultrate.ts.trend4.sezonowosc), main="reszty losowe")

#który model jest najlepszy?
AIC(defaultrate.ts.trend, defaultrate.ts.trend.sezonowosc, defaultrate.ts.trend2.sezonowosc, defaultrate.ts.trend4.sezonowosc)
BIC(defaultrate.ts.trend, defaultrate.ts.trend.sezonowosc, defaultrate.ts.trend2.sezonowosc, defaultrate.ts.trend4.sezonowosc)
anova(defaultrate.ts.trend, defaultrate.ts.trend.sezonowosc, defaultrate.ts.trend2.sezonowosc, defaultrate.ts.trend4.sezonowosc)
#anova porównuje każdy model z modelem pierwszym, kazdy do kolejnego 

anova(defaultrate.ts.trend4.sezonowosc)

#identyfikacja rzędu modelu ARIMA(p,d,q)
#identyfikacja rzędu MA 
par(mar=rep(2,4))
Acf(defaultrate.ts)

#usuwamy sezonowosc róznicując 
Acf(diff(defaultrate.ts, lag=4))
#wychodzi, ze mamy do czynienia z MA(5)

#identyfikacja rzędu AR 
pacf(diff(defaultrate.ts, lag=4))
#czyli wychodzi, że mamy do czynienia z AR(5)

#estymacja parametrów modelu
#model ar(p)
defaultrate.ts.diff<-diff(diff(defaultrate.ts, lag=4),1)
pacf(defaultrate.ts.diff)
acf(defaultrate.ts.diff)
#estymacja metodą youle'a-walkera
ar.model.yw <- ar(defaultrate.ts.diff, order.max = 3, aic = FALSE, method = "yule-walker")
print(ar.model.yw)

#estymacja metodą największej wiarygodności - NIE DZIAŁA
#ar.model.mle <- ar(defaultrate.ts.diff, order.max = 3, aic = FALSE, method = "mle")
#print(ar.model.mle)

#niech algorytm wybierze za nas
ar.model.aic <- ar(defaultrate.ts.diff, aic = FALSE)
print(ar.model.aic)

#wykres kryterium AIC dla równych rzędów p
ar.aic <- ar.model.aic$aic
print(ar.aic)
plot(as.numeric(names(ar.aic)), ar.aic, xlab='rząd modelu autoregresji(p)', ylab="AIC(P)")
grid()


#dopasowanie modelu SARIMA
arima.model1 <- Arima(defaultrate.ts, order = c(4,1,4), seasonal = list(order =c(0,1,0), period=4))
summary(arima.model1)
plot(defaultrate.ts)
lines(fitted(arima.model1), col="green", lty=2)

auto.arima(defaultrate.ts)

#testowo co by było gdybyśmy zamiast sarima przyjęli arima
arima.model2<-Arima(defaultrate.ts,order=c(4,1,4))
summary(arima.model2)
plot(defaultrate.ts)
lines(fitted(arima.model2),col="green",lty=2)
lines(fitted(arima.model1),col="blue",lty=2)
legend("bottomleft", legend=c("ARIMA","SARIMA"), col=c("green", "blue"),lty=c(2,2))

#formalne testy na losowość reszt
ar.reszty<-ar.model.aic$resid
arima1.reszty<-arima.model1$residuals
arima2.reszty<-arima.model2$residuals
par(mar=rep(2,4))
par(mfrow=c(1,1))
plot(ar.reszty,main="AR(1)reszty")
plot(arima1.reszty,main="SARIMA reszty")
plot(arima2.reszty,main="ARIMA reszty")

par(mfrow=c(1,1))
Acf(ar.reszty,main="AR ACF")
Acf(arima1.reszty,main="SARIMA ACF")
Acf(arima2.reszty,main="ARIMA ACF")

Box.test(ar.reszty,lag=1,type="Ljung-Box")
Box.test(arima1.reszty,lag=1,type="Ljung-Box")
Box.test(arima2.reszty,lag=1,type="Ljung-Box")
#<5% nie ma podstaw do odrzucenia H0, tutaj jest powyżej więc nie ma autokor
#histogramy reszt
dev.new()
par(mfrow=c(3,1))
hist(ar.reszty,main="AR(1)reszty")
hist(arima1.reszty,main="SARIMA reszty")
hist(arima2.reszty,main="ARIMA reszty")
#qqplot reszt (sytuacja idealna wykresy się nakładają reszty pochodzą z rozkładu normalnego)

hist(defaultrate.ts)+stat_function(aes(x=kredyt), geom='line', fun=dnorm)

dev.new()
par(mfrow=c(3,1))
qqnorm(ar.reszty,main="AR(1)reszty")
qqline(ar.reszty)
qqnorm(arima1.reszty,main="SARIMA reszty")
qqline(arima1.reszty)
qqnorm(arima2.reszty,main="ARIMA reszty")
qqline(arima2.reszty)

arima.model1.aic<-arima.model1$aic
arima.model1.bic<-arima.model1$bic
arima.model1.aicc<-arima.model1$aicc

arima.model2.aic<-arima.model2$aic
arima.model2.bic<-arima.model2$bic
arima.model2.aicc<-arima.model2$aicc

cat("model 1 AIC:",arima.model1.aic,"\n","model 2 AIC:",arima.model2.aic,"\n",
    "model 1 BIC:",arima.model1.bic,"\n","model 2 BIC:",arima.model2.bic,"\n",
    "model 1 AICC:",arima.model1.aicc,"\n","model 2 AICC:",arima.model2.aicc)
#błedy prognoz
horyzont<-length(defaultrate.ts)
arima.model1.prognozy<-forecast(arima.model1,h=horyzont)
arima.model2.prognozy<-forecast(arima.model2,h=horyzont)

kryteria<-c("RMSE","MAE","MAPE","MASE")
cat("model 1:",accuracy(arima.model1.prognozy)[,kryteria],"\n",
    "model 2:",accuracy(arima.model2.prognozy)[,kryteria])
#automatyczny dobór modelu
arima.auto.aic<-auto.arima(defaultrate.ts,ic="aic")
summary(arima.auto.aic)
arima.auto.aic.prognozy<-forecast(arima.auto.aic,h=horyzont)
cat("model 1:",accuracy(arima.model1.prognozy)[,kryteria],"\n",
    "model 2:",accuracy(arima.model2.prognozy)[,kryteria],"\n",
    "model 3:",accuracy(arima.auto.aic.prognozy)[,kryteria])
#najlepszy jest model, bo błedy się minimalizuje 

#zmienne makro
dane2 <- read.xlsx("stopa proc.xlsx")
dane2.s<-select(dane2, R)
stopa_proc.ts<-ts(dane2.s,start=c(2009,1), end=c(2019,1),frequency=4)

dane3 <- read.xlsx("PKB.xlsx")
dane3.s<-select(dane3, PKB)
PKB.ts<-ts(dane3.s,start=c(2009,1),end=c(2019,1),frequency=4)

dane4 <- read.xlsx("inflacja.xlsx")
dane4.s<-select(dane4, HPI)
inflacja.ts<-ts(dane4.s,start=c(2009,1),end=c(2019,1),frequency=4)

dane5 <- read.xlsx("dane_stopa_bezrobocia.xlsx")
dane5.s<-select(dane5, U)
bezrobocie.ts<-ts(dane5.s,start=c(2009,1),end=c(2019,1),frequency=4)

plot(stopa_proc.ts, main="Realna stopa procentowa w latach 2009-2019")

stopa_proc.ts.dekompozycja.add <- decompose(stopa_proc.ts,"additive")
plot(stopa_proc.ts.dekompozycja.add)
#H0:szereg niestacjonarny
#H1:szereg stacjonarny
adf.test(stopa_proc.ts)
#P-value>=5%->odrzucenie H0->szereg stacjonarny
#odrzucam H0 na rzecz H1, szereg jest STACJONARNY
#Phillips-Perron Test
pp.test(stopa_proc.ts)
#Phipips-Perron test ->stacjonarny
#KPSS test
#HO stacjonarność
#H1 brak stacjonarności
kpss.test(stopa_proc.ts,null="Trend")
#p-value<5%->brak podstaw do odrzucenia H0
#p-value>0,05,odrzucam H0 na rzecz H1

acf(stopa_proc.ts)

stopa_proc.ts.diff1<-diff(stopa_proc.ts,differences=1)
dev.new(noRStudioGD = TRUE)
tsdisplay(stopa_proc.ts)
dev.new(noRStudioGD = TRUE)
tsdisplay(stopa_proc.ts.diff1)

adf.test(stopa_proc.ts.diff1)
kpss.test(stopa_proc.ts.diff1,null="Trend")

install.packages("tempdisagg")
library(tempdisagg)


#automatyczny dobor modelu
arima.auto.aic.newvar<-auto.arima(defaultrate.ts, ic="aic", xreg=as.matrix(stopa_proc.ts.diff1))
summary(arima.auto.aic.newvar)


plot(PKB.ts)
PKB.ts.diff1<-diff(PKB.ts,differences=1)
dev.new(noRStudioGD = TRUE)
tsdisplay(PKB.ts)
dev.new(noRStudioGD = TRUE)
tsdisplay(PKB.ts.diff1)

#połączenie makro
stopa_proc_i_PKB.ts<-ts.union(PKB.ts.diff1, stopa_proc.ts.diff1)

#automatyczny dobor modelu
arima.auto.aic.newvar1<-auto.arima(defaultrate.ts, ic="aic", xreg=as.matrix(stopa_proc_i_PKB.ts))
summary(arima.auto.aic.newvar1)


inflacja.ts.diff1<-diff(inflacja.ts,differences=1)
dev.new(noRStudioGD = TRUE)
tsdisplay(inflacja.ts)
dev.new(noRStudioGD = TRUE)
tsdisplay(inflacja.ts.diff1)

#połączenie makro
stopa_proc_i_PKB_inflacja.ts<-ts.union(PKB.ts.diff1, stopa_proc.ts.diff1, inflacja.ts.diff1)

#automatyczny dobor modelu
arima.auto.aic.newvar2<-auto.arima(defaultrate.ts, ic="aic", xreg=as.matrix(stopa_proc_i_PKB_inflacja.ts))
summary(arima.auto.aic.newvar2)



bezrobocie.ts.diff1<-diff(bezrobocie.ts,differences=1)
dev.new(noRStudioGD = TRUE)
tsdisplay(bezrobocie.ts)
dev.new(noRStudioGD = TRUE)
tsdisplay(bezrobocie.ts.diff1)


f <- FRAPO::trdhp(bezrobocie.ts, lambda=1600)

par(mfcol=c(2,1),mar=c(4,4,1,1)+0.1, mgp=c(3,0.6,0),las=1)
plot(bezrobocie.ts,col="SteelBlue",
     main="Hodrick-Prescott filtr")
lines(f,col="YellowGreen")
plot(bezrobocie.ts-f,col="SteelBlue",
     main="składnik cykliczny po usunięciu trendu")

bezrobocie.ts_f.diff1<-diff(bezrobocie.ts-f,differences=1)
dev.new(noRStudioGD = TRUE)
tsdisplay(bezrobocie.ts-f)
dev.new(noRStudioGD = TRUE)
tsdisplay(bezrobocie.ts_f.diff1)

mabezrobocie.ts<-ma(bezrobocie.ts-f, order=3)
plot(mabezrobocie.ts)
tsdisplay(mabezrobocie.ts)

mabezrobocie.ts.diff1<-diff(mabezrobocie.ts,differences=2)
dev.new(noRStudioGD = TRUE)
tsdisplay(mabezrobocie.ts)
dev.new(noRStudioGD = TRUE)
tsdisplay(mabezrobocie.ts.diff1)

bez.ts<-stl(bezrobocie.ts, s.window = ("periodic"))
plot(bez.ts)
seasonal <-bez.ts$seasonal

bezrobocie.bk<-bkfilter(bezrobocie.ts.diff1,pl=2,pu=4,nfix=2,drift=TRUE)
dev.new(noRStudioGD = TRUE)
plot(bezrobocie.hp$trend,main="filtrowanie",col="blue",lty=2,ylim=c(-5,5))
lines(bezrobocie.bk$trend,col="red",lty=2)
grid()

acf(bezrobocie.bk)

bezrobociefilter.ts<-bkfilter(bezrobocie.ts,pl=NULL,pu=NULL,nfix=NULL,type=c("fixed","variable"),drift=FALSE)

yth_filter(bezrobocie.ts, h=8,p=4, output("bezrobocie.ts","trend"))

arima.auto.aic.newvar5<-arima(defaultrate.ts, order = c(4,1,4), seasonal = list(order =c(0,1,0),xreg=as.matrix(stopa_proc_i_PKB_inflacja.ts))
summary(arima.auto.aic.newvar5)

arima


arima.model1 <- Arima(defaultrate.ts, order = c(4,1,4), seasonal = list(order =c(0,1,0), period=4))
summary(arima.model1)
plot(defaultrate.ts)
lines(fitted(arima.model1), col="green", lty=2)

arima.auto <-auto.arima(defaultrate.ts)
summary(arima.auto)


#błedy prognoz
horyzont<-length(defaultrate.ts)
arima.model1.prognozy<-forecast(arima.model1,h=horyzont)
arima.model2.prognozy<-forecast(arima.model2,h=horyzont)

kryteria<-c("RMSE","MAE","MAPE","MASE")
cat("model 1:",accuracy(arima.model1.prognozy)[,kryteria],"\n",
    "model 2:",accuracy(arima.model2.prognozy)[,kryteria])

#automatyczny dobór modelu
4

#arima.auto.aic.newvar<-auto.arima(defaultrate.ts, ic="aic", xreg=as.matrix(stopa_proc_i_PKB_inflacja.ts))
#summary(arima.auto.aic.newvar)
#arima.auto.aic.newvar.prognozy <-forecast(arima.auto.aic.newvar, h= horyzont, xreg=as.matrix(stopa_proc_i_PKB_inflacja.ts))
#cat("Model 1:", accuracy(arima.model1.prognozy)[, kryteria],"\n",
    "Model 2:", accuracy(arima.model2.prognozy)[, kryteria],"\n",
    "Model 4:", accuracy(arima.auto.aic.newvar.prognozy)[, kryteria])

horyzont<-length(defaultrate.ts)

arima.auto.aic.newvar3<-Arima(defaultrate.ts,order=c(4,1,4), season=c(0,1,0), xreg=as.matrix(stopa_proc_i_PKB_inflacja.ts))
summary(arima.auto.aic.newvar3)
arima.auto.aic.newvar4<-Arima(defaultrate.ts,order=c(4,1,4), xreg=as.matrix(stopa_proc_i_PKB_inflacja.ts))
summary(arima.auto.aic.newvar4)
arima.auto.aic.newvar2<-auto.arima(defaultrate.ts, ic="aic", xreg=as.matrix(stopa_proc_i_PKB_inflacja.ts))
summary(arima.auto.aic.newvar2)
arima.auto.aic.newvar2.prognozy <-forecast(arima.auto.aic.newvar2, h= horyzont, xreg=as.matrix(stopa_proc_i_PKB_inflacja.ts))
arima.auto.aic.newvar3.prognozy<-forecast(arima.auto.aic.newvar3,h=horyzont,xreg=as.matrix(stopa_proc_i_PKB_inflacja.ts) )
arima.auto.aic.newvar4.prognozy<-forecast(arima.auto.aic.newvar4,h=horyzont,xreg=as.matrix(stopa_proc_i_PKB_inflacja.ts) )
cat("Model 1:", accuracy(arima.model1.prognozy)[, kryteria],"\n",
    "Model 2:", accuracy(arima.model2.prognozy)[, kryteria],"\n",
    "Model 3:", accuracy(arima.auto.aic.newvar2.prognozy)[, kryteria],"\n",
    "Model 4:", accuracy(arima.auto.aic.newvar3.prognozy)[, kryteria],"\n",
    "Model 5:", accuracy(arima.auto.aic.newvar4.prognozy)[, kryteria])

horyzont<-length(defaultrate.test)

#prognozowanie
#srednia ruchoma na podstawie wszystkich obserwakcjio z szeregu
defaultrate.srednia <- meanf(x=defaultrate.ts, h=4)
defaultrate.srednia
plot(defaultrate.srednia)

#metoda naiwna i sezonowa metoda naiwna
defaultrate.naiwna<-naive(x=defaultrate.ts, h=4)
defaultrate.naiwna
plot(defaultrate.naiwna)

defaultrate.naiwna=snaive(x=defaultrate.ts,h=12)
defaultrate.naiwna
plot(defaultrate.naiwna)

#metoda naiwna z dryfem
defaultrate.naiwna.dryf<-rwf(x=defaultrate.ts, drift=TRUE, h=12)
defaultrate.naiwna.dryf
plot(defaultrate.naiwna.dryf)



#podział danych na zbior uczacy i testowy
defaultrate.learn <-window(defaultrate.ts, end=c(2017,3))
start(defaultrate.learn)
end(defaultrate.learn)
length(defaultrate.learn)
defaultrate.test <-window(defaultrate.ts, start=c(2017,4))
start(defaultrate.test)
end(defaultrate.test)
length(defaultrate.test)


#podział danych na zbior uczacy i testowy
#defaultrate.learn <-window(arima.auto.aic.newvar3.prognozy, end=c(2017,3))
#start(defaultrate.learn)
#end(defaultrate.learn)
#length(defaultrate.learn)
#defaultrate.test <-window(arima.auto.aic.newvar3.prognozy, start=c(2017,4))
#start(defaultrate.test)
#end(defaultrate.test)
#length(defaultrate.test)

plot(forecast(arima.auto.aic.newvar4.prognozy,h=horyzont))
lines(defaultrate.ts,col="red",lty=2)

#wykres
ts.plot(defaultrate.learn, defaultrate.test, col=1:12, lty=c(1,2))
abline(v=time(defaultrate.test)[1], lty=3)
legend("topleft", legend=c("training set", "test set"),
       col=1:2, lty=c(1,2), cex=0.5)

horyzont<-length(defaultrate.test)

#prognoza naiwna
defaultrate.naiwna= snaive(x=defaultrate.learn,h=horyzont)
#porównanie prognoz i wartości rzeczywistych 
plot(defaultrate.naiwna)
lines(defaultrate.ts,col="red",lty=2)

#analiza własności reszt
reszty.naiwna<-residuals(defaultrate.naiwna)
plot(reszty.naiwna)
Acf(reszty.naiwna)
Box.test(reszty.naiwna,lag=4,type="Ljung-Box")
#>=5% czyli nie ma podstaw do odrzucenia hipotezy zerowej

# Model ARIMA automat
defaultrate.arima.auto<-auto.arima(defaultrate.learn)
summary(defaultrate.arima.auto)
#porównanie prognoz i wartości rzeczywistych 
plot(forecast(defaultrate.arima.auto,h=horyzont))
lines(defaultrate.ts,col="red",lty=2)

#analiza własności reszt
reszty.arima.auto<-residuals(defaultrate.arima.auto)
plot(reszty.arima.auto)
Acf(reszty.arima.auto)
Box.test(reszty.arima.auto,lag=4,type="Ljung-Box")
#>=5% czyli nie ma podstaw do odrzucenia hipotezy zerowej (0.2977-nie ma podstaw do odrzucenia, czyli nie ma autokorelacji)

acf(defaultrate.learn)

#ARIMA ręczna
#defaultrate.arima.manual<-Arima(defaultrate.learn,order=c(1,0,0),season=c(0,1,0))
#summary(defaultrate.arima.manual)

pacf(defaultrate.learn)
acf(defaultrate.learn)


length(stopa_proc_i_PKB_inflacja.ts)
length(defaultrate.learn)

#skracanie makro
stopa_proc_i_PKB_inflacja_obcięte.ts<-ts(stopa_proc_i_PKB_inflacja.ts,start=c(2010,1),end=c(2017,3),frequency=4)
length(stopa_proc_i_PKB_inflacja_obcięte.ts)


#ARIMA ręczna ze zmiennymi makro
defaultrate.arima.manual.makro<-Arima(defaultrate.learn,order=c(4,1,4),season = c(0,1,0), xreg=as.matrix(stopa_proc_i_PKB_inflacja_obcięte.ts))
summary(defaultrate.arima.manual.makro)


defaultrate.arima.manual.makro1<-Arima(defaultrate.learn,order=c(4,1,4), xreg=as.matrix(stopa_proc_i_PKB_inflacja_obcięte.ts))
summary(defaultrate.arima.manual.makro1)


defaultrate.arima.manual.makro.prognozy<-forecast(defaultrate.arima.manual.makro,h=horyzont,xreg=as.matrix(stopa_proc_i_PKB_inflacja_obcięte.ts) )
plot(defaultrate.arima.manual.makro.prognozy)
defaultrate.arima.manual.makro


defaultrate.arima.manual.makro1.prognozy<-forecast(defaultrate.arima.manual.makro1,h=horyzont,xreg=as.matrix(stopa_proc_i_PKB_inflacja_obcięte.ts) )
plot(defaultrate.arima.manual.makro1.prognozy)


#ARIMA ręczna
defaultrate.arima.manual<-Arima(defaultrate.learn,order=c(2,1,4),season=c(0,1,0))
summary(defaultrate.arima.manual)

#porównanie prognoz i wartości rzeczywistych 
plot(forecast(defaultrate.arima.manual,h=horyzont))
lines(defaultrate.ts,col="red",lty=2)

#porównanie prognoz i wartości rzeczywistych makro
plot(defaultrate.arima.manual.makro.prognozy,h=horyzont)
lines(defaultrate.ts,col="red",lty=2)

#analiza własności reszt
reszty.arima.manual<-residuals(defaultrate.arima.manual)
plot(reszty.arima.manual)
Acf(reszty.arima.manual)
Box.test(reszty.arima.manual,lag=4,type="Ljung-Box")
#>=5% czyli nie ma podstaw do odrzucenia hipotezy zerowej
#na wykresie autoregresja, jest to spowodowane 1 i 0 na innych miejscach 1,0,0 brak róznicowania bo 0 na drugim miejscu)

#analiza własności reszt makro
reszty.arima.manual.makro<-residuals(defaultrate.arima.manual.makro)
plot(reszty.arima.manual.makro)
Acf(reszty.arima.manual.makro)
Box.test(reszty.arima.manual.makro,lag=4,type="Ljung-Box")


#analiza własności reszt makro1
reszty.arima.manual.makro1<-residuals(defaultrate.arima.manual.makro1)
plot(reszty.arima.manual.makro1)
Acf(reszty.arima.manual.makro1)
Box.test(reszty.arima.manual.makro1,lag=4,type="Ljung-Box")

#modele wygładzania wykładniczego 
defaultrate.ets<-ets(defaultrate.learn,opt.crit="mse",ic="aicc")
summary(defaultrate.ets)
#porównanie prognoz i wartości rzeczywistych 
plot(forecast(defaultrate.ets,h=horyzont))
lines(defaultrate.ts,col="red",lty=2)



#analiza własności reszt
reszty.ets<-residuals(defaultrate.ets)
plot(reszty.ets)
Acf(reszty.ets)
Box.test(reszty.ets,lag=12,type="Ljung-Box")

#prognozy oparte na dekompozycji
defaultrate.dekompozycja.addytywna<-tslm(defaultrate.learn ~ trend+season)
summary(defaultrate.dekompozycja.addytywna)

#porównanie prognoz i wartości rzeczywistych 
plot(forecast(defaultrate.dekompozycja.addytywna,h=horyzont))
lines(defaultrate.ts,col="red",lty=2)

#analiza własności reszt
reszty.dekompozycja.addytywna<-residuals(defaultrate.dekompozycja.addytywna)
plot(reszty.dekompozycja.addytywna)
Acf(reszty.dekompozycja.addytywna)
Box.test(reszty.dekompozycja.addytywna,lag=4,type="Ljung-Box")
#>=5% czyli nie ma podstaw do odrzucenia hipotezy zerowej

horyzont<-length(defaultrate.test)
#porównajmy prognozy na jednym wykresie
par(mar=rep(2,4))
par(mfrow=c(1,1))
prognozy.arima.auto<-forecast(defaultrate.arima.auto,h=horyzont)
prognozy.arima.manual<-forecast(defaultrate.arima.manual,h=horyzont)
defaultrate.arima.manual.makro.prognozy<-forecast(defaultrate.arima.manual.makro,h=horyzont,xreg=as.matrix(stopa_proc_i_PKB_inflacja_obcięte.ts) )
prognozy.ets<-forecast(defaultrate.ets,h=horyzont)
prognozy.dekompozycja.addytywna<-forecast(defaultrate.dekompozycja.addytywna,h=horyzont)
ts.plot(defaultrate.naiwna$mean,prognozy.arima.auto$mean,prognozy.arima.manual$mean,prognozy.dekompozycja.addytywna$mean,defaultrate.arima.manual.makro.prognozy$mean ,col=c("red","green","blue","orange", "pink"))
grid()
legend("bottomleft", legend=c("prognozy.naiwna","prognozy.arima.auto","prognozy.arima.manual", "prognozy.addytywna", "prognozy.makro"), col=c("red", "green", "blue", "orange", "pink"), lty = c(1,1,1,1,1))


#dokładność prognoz
cat("Miara: MAE trening,MAE test","\n",
    "ARIMA auto:",accuracy(prognozy.arima.auto,defaultrate.test)[,"MAE"],"\n",
    "ARIMA ręczna:",accuracy(prognozy.arima.manual,defaultrate.test)[,"MAE"],"\n",
    "ARIMA ręczna z makro:",accuracy(defaultrate.arima.manual.makro.prognozy,defaultrate.test)[,"MAE"],"\n",
    "ARIMA ręczna z makro bez sezonowości:",accuracy(defaultrate.arima.manual.makro1.prognozy,defaultrate.test)[,"MAE"],"\n",
    "Metoda naiwna:",accuracy(defaultrate.naiwna,defaultrate.test)[,"MAE"],"\n",
    "ETS:",accuracy(prognozy.ets,defaultrate.test)[,"MAE"],"\n",
    "Dekompozycja addytywna:",accuracy(prognozy.dekompozycja.addytywna,defaultrate.test)[,"MAE"])

cat("Miara: RMSE trening, RMSE test","\n",
    "ARIMA auto:",accuracy(prognozy.arima.auto,defaultrate.test)[,"RMSE"],"\n",
    "ARIMA ręczna:",accuracy(prognozy.arima.manual,defaultrate.test)[,"RMSE"],"\n",
    "ARIMA ręczna z makro:",accuracy(defaultrate.arima.manual.makro.prognozy,defaultrate.test)[,"MAE"],"\n",
    "Metoda naiwna:",accuracy(defaultrate.naiwna,defaultrate.test)[,"RMSE"],"\n",
    "ETS:",accuracy(prognozy.ets,defaultrate.test)[,"RMSE"],"\n",
    "Dekompozycja addytywna:",accuracy(prognozy.dekompozycja.addytywna,defaultrate.test)[,"RMSE"])
#Błędy muszą być najmniejsze na zbiorze testowym (arima ręczna jest najlepsza w obu modelach bo ma najmnijesze wartości)




