library(vars)
library(stargazer)
library(tseries)
library(ggfortify)
y <- read.csv("allcountries.csv")

lr_gold <- ts(y$GoldLR., start = 1996, end = 2020) #gold log returns (common for all)
rv_gold <- ts(y$GoldRV, start = 1996, end = 2020) # gold RV (common for all)


lr_crude <- ts(y$CrudeLR., start = 1996, end = 2020) # crude log returns (common for all)
rv_crude <- ts(y$CrudeRV, start = 1996, end = 2020)# crude RV (common for all)

wpui <- ts(y$G10.5, start = 1996, end = 2020) # Pandemic Uncertainty in G10
ts.plot(wpui)
wpui
unemp <- ts(y$G10, start = 1996, end = 2020) # Unemployment in G10
gdp <- ts(y$G10.1, start = 1996, end = 2020) # GDP in G10
lwip <- ts(y$LWIP, start = 1996, end = 2020) # log of world industrial production index (common for all)
lgpr <- ts(y$LGPR, start = 1996, end = 2020) # log of geopolitical risk index (common for all)
wui<- ts(y$G10.4, start = 1996, end = 2020) # Uncertainty in G10
cpi <- ts(y$G10.2, start = 1996, end = 2020) #Inflation in G10

amat <- diag(8) # 8X8 identity matrix to define the SVAR ordering

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

# SVAR Gold using Uncertainty Index
sv<- cbind(lwip, lr_gold, rv_gold,lgpr,unemp,gdp, cpi,wui)
colnames(sv) <- cbind('LWIP','LR', 'RV','LGPR', 'U','GDP', "CPI", 'WUI')
sv
lagselect <- VARselect(sv)
lagselect$selection
?VAR
estim1 <- VAR(sv, lag = 2, type = 'none')
estim1
svar1 <- SVAR(estim1, Amat = amat)
stargazer(svar1[["A"]]) # SVAR Table Result
irf1 <- irf(svar1, impulse = 'WUI', response = 'RV', runs = 100, ci = 0.68)
irf1
plot(irf1, main = "Gold Volatility response to uncertainty shock in G10")

# SVAR Gold Pandemic Uncertainty INdex
sv_wpui<- cbind(lwip, lr_gold, rv_gold,lgpr,unemp,gdp, cpi,wpui)
colnames(sv_wpui) <- cbind('LWIP','LR', 'RV','LGPR', 'U','GDP', "CPI", 'WPUI')

lagselect_wpui <- VARselect(sv_wpui)
lagselect_wpui$selection

estim1_wpui <- VAR(sv_wpui, lag = 1)
estim1
svar1_wpui <- SVAR(estim1_wpui, Amat = amat)
stargazer(svar1_wpui[["A"]])

irf1_wpui <- irf(svar1_wpui, impulse = 'WPUI', response = 'RV', runs = 100, ci = 0.68)
irf1_wpui
plot(irf1_wpui, main = "Gold Volatility response to pandemic uncertainty shock in G10")

# SVAR Crude Oil Uncertainty Index
sv_<-cbind(lwip, lr_crude, rv_crude,lgpr,unemp,gdp, cpi, wui)
colnames(sv_) <- cbind('LWIP','LR', 'RV','LGPR', 'U','GDP', 'CPI' ,'WUI')


lagselect_ <- VARselect(sv_)
lagselect_$selection

estim3 <- VAR(sv_, lag = 1, type = 'none')
estim3
svar3 <- SVAR(estim3, Amat = amat)
stargazer(svar3[["A"]])
svar3

irf2 <- irf(svar3, impulse = 'WUI', response = 'RV', runs = 100, ci = 0.68)
irf2
plot(irf2, main = "Crude Volatility response to uncertainty shock in G10")
acf(ts.intersect(wui, unemp, lwip, lgpr, gdp, rv_crude, lr_crude))


#SVAR Crude oil Pandemic Uncertainty
sv_wpui_crude<-cbind(lwip, lr_crude, rv_crude,lgpr,unemp,gdp, cpi, wpui)
colnames(sv_wpui_crude) <- cbind('LWIP','LR', 'RV','LGPR', 'U','GDP', 'CPI' ,'WPUI')


lagselect_wpui_crude <- VARselect(sv_wpui_crude)
lagselect_wpui_crude$selection

estim3_wpui <- VAR(sv_wpui_crude, lag = 2)

svar3_wpui <- SVAR(estim3_wpui, Amat = amat)
svar3_wpui
stargazer(svar3_wpui[["A"]])
svar3

irf2_wpui <- irf(svar3_wpui, impulse = 'WPUI', response = 'RV', runs = 100)
irf2_wpui
plot(irf2_wpui, main = "Crude Volatility response to pandemic uncertainty shock in G10")

acf(ts.intersect(wui, unemp, lwip, lgpr, gdp, rv_crude, lr_crude))
cor(wpui, unemp, method = c('pearson', 'kendall', 'spearman'))
cor.test(wpui, unemp)
