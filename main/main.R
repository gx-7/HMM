---
title: 'Hidden Markov Model with ldhm R package'
author: "Guillaume Ostrom"
output: pdf_document
---
***

# HMM with the S&P500

ts = ldhmm.ts_log_rtn ("spx", on="days")
fred.data=TRUE
sapply (0:10,  function(drop) kurtosis(ldhmm.drop_outliers(ts$x , drop )))


# ACF
ldhmm.ts_abs_acf(ts$x , drop=0, lag.max =6)
ldhmm.ts_abs_acf(ts$x , drop=10, lag.max =6)

m = 2
param0 = matrix(c(
mu_1 , sigma_1 ,
mu_2 , sigma_2), m, 2, byrow=TRUE)
gamma0 = ldhmm.gamma_init(m)
h <- ldhmm(m, param0 , gamma0 , stationary=TRUE)
hd <- ldhmm.mle(h, ts$x , decode=TRUE , print.level =2)

hd@param
hd@gamma
hd@delta
ldhmm.ld_stats(hd)
hd@states.local.stats

# Volatility prediction

hs <- ldhmm.read_sample_object ()
spx  <- ldhmm.ts_log_rtn(on="days")
hss  <- ldhmm.decoding(hs , spx$x)
V <- ldhmm.decode_stats_history(hss , annualize=TRUE)[,"V"]
plot(spx$d , V, type="l")

hs <- ldhmm.read_sample_object ()
ldhmm.oxford_man_plot_obs(hs)

spx2  <- ldhmm.oxford_man_ts ("SPX2.r", log=TRUE)

Result :
xf <- seq(-0.02, 0.02, by =0.005)
ldhmm.forecast_volatility(hd , spx2$x , xf)

