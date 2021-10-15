library(vars)
library(stargazer)
library(tseries)
library(ggfortify)
y <- read.csv("qtr.csv")

lr_gold <- ts(y$LR..not.divide, start = c(1996, 1), end = c(2020, 1), frequency = 4)
lr_gold
rv_gold <- ts(y$RV_new, start = c(1996, 1), end = c(2020, 1), frequency = 4)

lr_crude <- ts(y$LR...not.divide, start = c(1996, 1), end = c(2020, 1),frequency = 4 )
rv_crude <- ts(y$RV_new.1, start = c(1996, 1), end = c(2020, 1), frequency = 4)

wpui <- ts(y$World.Pandemic.Uncertainty.Index..WPUI...equally.weighted.average, start = c(1996, 1), end = c(2020, 1), frequency = 4) # Pandemic Uncertainty in G15

lwip <- ts(y$LWIP.QTR, start = c(1996, 1), end = c(2020, 1), frequency = 4)
lgpr <- ts(y$LGPR.QTR, start = c(1996, 1), end = c(2020, 1), frequency = 4)
ts.plot(lwip)
ts.plot(lgpr)
wui_simple <-  ts(y$Simple, start = c(1996, 1), end = c(2020, 1), frequency = 4)
wui <- ts(y$GDP.Weighted, start = c(1996, 1), end = c(2020, 1), frequency = 4) 

amat <- diag(5)
amat

amat[2, 1] <- NA
amat[3, 1] <- NA
amat[3, 2] <- NA
amat[4, 1] <- NA
amat[4, 2] <- NA
amat[4, 3] <- NA
amat[5, 1] <- NA
amat[5, 2] <- NA
amat[5, 3] <- NA
amat[5, 4] <- NA
amat

#SVAR Gold  Uncertainty Index
sv<- cbind(lwip, lr_gold, rv_gold,lgpr,wui)
colnames(sv) <- cbind('LWIP','LR', 'RV','LGPR', 'WUI')

lagselect <- VARselect(sv)
lagselect$selection
?VAR
estim1 <- VAR(sv, lag = 2)
estim1
svar1 <- SVAR(estim1, Amat = amat)
stargazer(svar1[["A"]])

irf1 <- irf(svar1, impulse = 'WPUI', response = 'RV', runs = 100, ci = 0.68)
irf1
plot(irf1, main = "Gold Volatility response to uncertainty shock in G15")

#SVAR Gold Pandemic Uncertainty
sv_wpui<- cbind(lwip, lr_gold, rv_gold,lgpr,wpui)
colnames(sv_wpui) <- cbind('LWIP','LR', 'RV','LGPR','WPUI')
sv
lagselect_wpui <- VARselect(sv_wpui)
lagselect_wpui$selection

estim1_wpui <- VAR(sv_wpui, lag = 2)

svar1_wpui <- SVAR(estim1_wpui, Amat = amat)
stargazer(svar1_wpui[["A"]])

irf1_wpui <- irf(svar1_wpui, impulse = 'WPUI', response = 'RV', runs = 100, ci = 0.68)
irf1_wpui
plot(irf1_wpui, main = "Gold Volatility response to pandemic uncertainty shock in G15")

#SVAR Crude Oil Uncertainty Index
sv_<-cbind(lwip, lr_crude, rv_crude,lgpr, wui)
colnames(sv_) <- cbind('LWIP','LR', 'RV','LGPR','WUI')


lagselect_ <- VARselect(sv_)
lagselect_$selection

estim3 <- VAR(sv_, lag = 2)
svar3 <- SVAR(estim3, Amat = amat)
stargazer(svar3[["A"]])

irf2 <- irf(svar3, impulse = 'WUI', response = 'RV', runs = 100, ci = 0.68)
irf2
plot(irf2, main = "Crude Volatility response to uncertainty shock in G15")
acf(ts.intersect(wui, unemp, lwip, lgpr, gdp, rv_crude, lr_crude))

#SVAR Crude oil Pandemic Uncertainty
sv_wpui_crude<-cbind(lwip, lr_crude, rv_crude,lgpr, wpui)
colnames(sv_wpui_crude) <- cbind('LWIP','LR', 'RV','LGPR' ,'WPUI')


lagselect_wpui_crude <- VARselect(sv_wpui_crude)
lagselect_wpui_crude$selection

estim3_wpui <- VAR(sv_wpui_crude, lag = 2)
estim3_wpui
svar3_wpui <- SVAR(estim3_wpui, Amat = amat)
stargazer(svar3_wpui[["A"]])

irf2_wpui <- irf(svar3_wpui, impulse = 'WPUI', response = 'RV', runs = 100, ci = 0.68)

irf2_wpui
plot(irf2_wpui, main = "Crude Volatility response to pandemic uncertainty shock in G15")
