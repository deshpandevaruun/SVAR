# SVAR Estimation

## Description
###This project estimates the 8 variable SVAR model to study the impact of uncertainty shocks on volatility of crude oil and gold prices. The SVAR ordering is [LWIP, LR, RV,LGPR,unemp,GDP, CPI, WPUI]. 

## Installations
###Requires installtion of "vars" package to estimate the VAR model, "stargazer" package for converting SVAR output tables to LaTeX code, and "tseries" package to handle time series data.

## Initialization
###Initialize structure of the SVAR matrix to denote the contemporaneous relation among variables.
###Order the data according to the required var ordering 

## Estimation
### Select the optimal lag order 
>lagselect <- VARselect(Data)
>lagselect$selection

### Estimate the VAR model 
> estimateVar <- VAR(Data, lag = 2, type = 'none')
### Estimate SVAR by imposing the matrix restrictions
> svar <- SVAR(estimateVar, Amat = RestrictionMatrix)

