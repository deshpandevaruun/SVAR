library(vars)
library(stargazer)
library(tseries)
library(ggfortify)
y <- read.csv("allcountries.csv")

lr_gold <- ts(y$GoldLR., start = 1996, end = 2020)
rv_gold <- ts(y$GoldRV, start = 1996, end = 2020)
lr_crude <- ts(y$CrudeLR., start = 1996, end = 2020)
rv_crude <- ts(y$CrudeRV, start = 1996, end = 2020)

wpui <- ts(y$BRICS.6, start = 1996, end = 2020) # Pandemic Uncertainty in BRICS

unemp <- ts(y$BRICS, start = 1996, end = 2020) # Unemployment in BRICS

gdp <- ts(y$BRICS.1, start = 1996, end = 2020) # GDP in BRICS

lwip <- ts(y$LWIP, start = 1996, end = 2020)
lgpr <- ts(y$LGPR, start = 1996, end = 2020)

wui<- ts(y$BRICS.4, start = 1996, end = 2020) # Uncertainty in BRICS
cpi<- ts(y$BRICS.2, start = 1996, end = 2020) # Inflation in BRICS
wpui <- log(wpui)
wpui
adf.test(wpui)
diff_wpui <- diff(wpui, lag = 2)
diff_wpui
adf.test(diff_wpui)
amat <- diag(8)
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
amat[6, 1] <- NA
amat[6, 2] <- NA
amat[6, 3] <- NA
amat[6, 4] <- NA
amat[6, 5] <- NA

amat[7, 1] <- NA
amat[7, 2] <- NA
amat[7, 3] <- NA
amat[7, 4] <- NA
amat[7, 5] <- NA
amat[7, 6] <- NA

amat[8, 1] <- NA
amat[8, 2] <- NA
amat[8, 3] <- NA
amat[8, 4] <- NA
amat[8, 5] <- NA
amat[8, 6] <- NA
amat[8, 7] <- NA
amat

# SVAR Gold Uncertainty Index
sv<- cbind(lwip, lr_gold, rv_gold,lgpr,unemp,gdp, cpi,wui)
colnames(sv) <- cbind('LWIP','LR', 'RV','LGPR', 'U','GDP', "CPI", 'WUI')

lagselect <- VARselect(sv)
lagselect$selection

estim1 <- VAR(sv, lag = 2, type = 'none')

svar1 <- SVAR(estim1, Amat = amat)
stargazer(svar1[["A"]])

irf1 <- irf(svar1, impulse = 'WUI', response = 'RV', runs = 100, ci = 0.68)
irf1
plot(irf1, main = "Gold Volatility response to uncertainty shock in BRICS")

#SVAR Gold Pandemic Uncertainty
sv_wpui<- cbind(lwip, lr_gold, rv_gold,lgpr,unemp,gdp, cpi,wpui)
colnames(sv_wpui) <- cbind('LWIP','LR', 'RV','LGPR', 'U','GDP', "CPI", 'WPUI')
lagselect_wpui <- VARselect(sv_wpui)
lagselect_wpui$selection

estim1_wpui <- VAR(sv_wpui, lag = 1)
svar1_wpui <- SVAR(estim1_wpui, Amat = amat)
stargazer(svar1_wpui[["A"]])

irf1_wpui <- irf(svar1_wpui, impulse = 'WPUI', response = 'RV', runs = 100, ci = 0.68, n.ahead =  8)
irf1_wpui
plot(irf1_wpui, ylim = c(-30, 30), main = "Gold Volatility response to pandemic uncertainty shock in BRICS")
irf1_wpui[["irf"]]
str(irf1_wpui)
vars:::plot.varirf 
#SVAR Crude oil Uncertainty
sv_<-cbind(lwip, lr_crude, rv_crude,lgpr,unemp,gdp, cpi, wui)
colnames(sv_) <- cbind('LWIP','LR', 'RV','LGPR', 'U','GDP', 'CPI' ,'WUI')


lagselect_ <- VARselect(sv_)
lagselect_$selection

estim3 <- VAR(sv_, lag = 1, type = 'none')
svar3 <- SVAR(estim3, Amat = amat)
stargazer(svar3[["A"]])

irf2 <- irf(svar3, impulse = 'WUI', response = 'RV', runs = 100, ci = 0.68)
irf2
plot(irf2, main = "Crude Volatility response to uncertainty shock in BRICS")
acf(ts.intersect(wui, unemp, lwip, lgpr, gdp, rv_crude, lr_crude))

#SVAR Crude oil Pandemic Uncertainty
sv_wpui_crude<-cbind(lwip, lr_crude, rv_crude,lgpr,unemp,gdp, cpi, wpui)
colnames(sv_wpui_crude) <- cbind('LWIP','LR', 'RV','LGPR', 'U','GDP', 'CPI' ,'WPUI')


lagselect_wpui_crude <- VARselect(sv_wpui_crude)
lagselect_wpui_crude$selection

estim3_wpui <- VAR(sv_wpui_crude, lag = 1)
svar3_wpui <- SVAR(estim3_wpui, Amat = amat)
stargazer(svar3_wpui[["A"]])

irf2_wpui <- irf(svar3_wpui, impulse = 'WPUI', response = 'RV', runs = 100)
irf2_wpui
plot(irf2_wpui, ylim = c(-30, 30), main = "Crude Volatility response to pandemic uncertainty shock in BRICS")
irf2_wpui$irf
plot(irf2_wpui$irf)
