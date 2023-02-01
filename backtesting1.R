library(quantmod)
library(TTR)
library(PerformanceAnalytics)

# Getting stock prices of AAPL, TSLA and NFLX
getSymbols('AAPL', src = 'yahoo', from = '2019-01-01')
getSymbols('TSLA', src = 'yahoo', from = '2019-01-01')
getSymbols('NFLX', src = 'yahoo', from = '2019-01-01')

# Basic plot of the three stocks
barChart(AAPL, theme = chartTheme('black'))
barChart(TSLA, theme = chartTheme('black'))
barChart(NFLX, theme = chartTheme('black'))

# Creating Leading and Lagging Technical Indicators

# a. Simple Moving Average (SMA)

# 1. AAPL
sma20_aapl <- SMA(AAPL$AAPL.Close, n = 20)
sma50_aapl <- SMA(AAPL$AAPL.Close, n = 50)
lineChart(AAPL, theme = chartTheme('black'))
addSMA(n = 20, col = 'blue')
addSMA(n = 50, col = 'orange')
legend('left', col = c('green','blue','orange'),
       legend = c('AAPL','SMA20','SMA50'), lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)
# 2. TSLA
sma20_tsla <- SMA(TSLA$TSLA.Close, n = 20)
sma50_tsla <- SMA(TSLA$TSLA.Close, n = 50)
lineChart(TSLA, theme = 'black')
addSMA(n = 20, col = 'blue')
addSMA(n = 50, col = 'orange')
legend('left', col = c('green','blue','orange'),
       legend = c('AAPL','SMA20','SMA50'), lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)
# 3. NFLX
sma20_nflx <- SMA(NFLX$NFLX.Close, n = 20)
sma50_nflx <- SMA(NFLX$NFLX.Close, n = 50)
lineChart(NFLX, theme = 'black')
addSMA(n = 20, col = 'blue')
addSMA(n = 50, col = 'orange')
legend('left', col = c('green','blue','orange'),
       legend = c('AAPL','SMA20','SMA50'), lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)


# b.  Parabolic Stop And Reverse (SAR)

# 1. AAPL
sar_aapl <- SAR(cbind(Hi(AAPL),Lo(AAPL)), accel = c(0.02, 0.2))
barChart(AAPL, theme = 'black')
addSAR(accel = c(0.02, 0.2), col = 'lightblue')
# 2. TSLA
sar_tsla <- SAR(cbind(Hi(TSLA),Lo(TSLA)), accel = c(0.02, 0.2))
barChart(TSLA, theme = 'black')
addSAR(accel = c(0.02, 0.2), col = 'lightblue')
# 3. NFLX
sar_nflx <- SAR(cbind(Hi(NFLX),Lo(NFLX)), accel = c(0.02, 0.2))
barChart(NFLX, theme = 'black')
addSAR(accel = c(0.02, 0.2), col = 'lightblue')

# c. Commodity Channel Index (CCI)

# 1. AAPL
cci_aapl <- CCI(HLC(AAPL), n = 20, c = 0.015)
barChart(AAPL, theme = 'black')
addCCI(n = 20, c = 0.015)
# 2. TSLA
cci_tsla <- CCI(HLC(TSLA), n = 20, c = 0.015)
barChart(TSLA, theme = 'black')
addCCI(n = 20, c = 0.015)
# 3. NFLX
cci_nflx <- CCI(HLC(NFLX), n = 20, c = 0.015)
barChart(NFLX, theme = 'black')
addCCI(n = 20, c = 0.015)

# d. Rate of Change (ROC)

# 1. AAPL
roc_aapl <- ROC(AAPL$AAPL.Close, n = 25)
barChart(AAPL, theme = 'black')
addROC(n = 25)
legend('left', col = 'red', legend = 'ROC(25)', lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)
# 1. TSLA
roc_tsla <- ROC(TSLA$TSLA.Close, n = 25)
barChart(TSLA, theme = 'black')
addROC(n = 25)
legend('left', col = 'red', legend = 'ROC(25)', lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)
# 1. NFLX
roc_nflx <- ROC(NFLX$NFLX.Close, n = 25)
barChart(NFLX, theme = 'black')
addROC(n = 25)
legend('right', col = 'red', legend = 'ROC(25)', lty = 1, bty = 'n',
       text.col = 'white', cex = 0.8)

# e. Stochastic Momentum Index (SMI)

# 1. AAPL
smi_aapl <- SMI(HLC(AAPL),
                n = 13, nFast = 2, nSlow = 25, nSig = 9)
barChart(AAPL, theme = 'black')
addSMI(n = 13, fast = 2, slow = 2, signal = 9)
# 2. TSLA
smi_tsla <- SMI(HLC(TSLA),
                n = 13, nFast = 2, nSlow = 25, nSig = 9)
barChart(TSLA, theme = 'black')
addSMI(n = 13, fast = 2, slow = 2, signal = 9)
# 3. NFLX
smi_nflx <- SMI(HLC(NFLX),
                n = 13, nFast = 2, nSlow = 25, nSig = 9)
barChart(NFLX, theme = 'black')
addSMI(n = 13, fast = 2, slow = 2, signal = 9)

# f. Williams %R

# 1. AAPL
wpr_aapl <- WPR(HLC(AAPL), n = 14)
colnames(wpr_aapl) <- 'wpr'
barChart(AAPL, theme = 'black')
addWPR(n = 14)
# 1. TSLA
wpr_tsla <- WPR(HLC(TSLA), n = 14)
colnames(wpr_tsla) <- 'wpr'
barChart(TSLA, theme = 'black')
addWPR(n = 14)
# 1. NFLX
wpr_nflx <- WPR(HLC(NFLX), n = 14)
colnames(wpr_nflx) <- 'wpr'
barChart(NFLX, theme = 'black')
addWPR(n = 14)

# Creating Trading signal with Indicators

# SMA 

# a. AAPL
# SMA 20 Crossover Signal 
sma20_aapl_ts <- Lag(
  ifelse(Lag(Cl(AAPL)) < Lag(sma20_aapl) & Cl(AAPL) > sma20_aapl,1,
         ifelse(Lag(Cl(AAPL)) > Lag(sma20_aapl) & Cl(AAPL) < sma20_aapl,-1,0)))
sma20_aapl_ts[is.na(sma20_aapl_ts)] <- 0
# SMA 50 Crossover Signal
sma50_aapl_ts <- Lag(
  ifelse(Lag(Cl(AAPL)) < Lag(sma50_aapl) & Cl(AAPL) > sma50_aapl,1,
         ifelse(Lag(Cl(AAPL)) > Lag(sma50_aapl) & Cl(AAPL) < sma50_aapl,-1,0)))
sma50_aapl_ts[is.na(sma50_aapl_ts)] <- 0
# SMA 20 and SMA 50 Crossover Signal
sma_aapl_ts <- Lag(
  ifelse(Lag(sma20_aapl) < Lag(sma50_aapl) & sma20_aapl > sma50_aapl,1,
         ifelse(Lag(sma20_aapl) > Lag(sma50_aapl) & sma20_aapl < sma50_aapl,-1,0)))
sma_aapl_ts[is.na(sma_aapl_ts)] <- 0

# b. TSLA
# SMA 20 Crossover Signal 
sma20_tsla_ts <- Lag(
  ifelse(Lag(Cl(TSLA)) < Lag(sma20_tsla) & Cl(TSLA) > sma20_tsla,1,
         ifelse(Lag(Cl(TSLA)) > Lag(sma20_tsla) & Cl(TSLA) < sma20_tsla,-1,0)))
sma20_tsla_ts[is.na(sma20_tsla_ts)] <- 0
# SMA 50 Crossover Signal
sma50_tsla_ts <- Lag(
  ifelse(Lag(Cl(TSLA)) < Lag(sma50_tsla) & Cl(TSLA) > sma50_tsla,1,
         ifelse(Lag(Cl(TSLA)) > Lag(sma50_tsla) & Cl(TSLA) < sma50_tsla,-1,0)))
sma50_tsla_ts[is.na(sma50_tsla_ts)] <- 0
# SMA 20 and SMA 50 Crossover Signal
sma_tsla_ts <- Lag(
  ifelse(Lag(sma20_tsla) < Lag(sma50_tsla) & sma20_tsla > sma50_tsla,1,
         ifelse(Lag(sma20_tsla) > Lag(sma50_tsla) & sma20_tsla < sma50_tsla,-1,0)))
sma_tsla_ts[is.na(sma_tsla_ts)] <- 0

# c. NFLX
# SMA 20 Crossover Signal 
sma20_nflx_ts <- Lag(
  ifelse(Lag(Cl(NFLX)) < Lag(sma20_nflx) & Cl(NFLX) > sma20_nflx,1,
         ifelse(Lag(Cl(NFLX)) > Lag(sma20_nflx) & Cl(NFLX) < sma20_nflx,-1,0)))
sma20_nflx_ts[is.na(sma20_nflx_ts)] <- 0
# SMA 50 Crossover Signal
sma50_nflx_ts <- Lag(
  ifelse(Lag(Cl(NFLX)) < Lag(sma50_nflx) & Cl(NFLX) > sma50_nflx,1,
         ifelse(Lag(Cl(NFLX)) > Lag(sma50_nflx) & Cl(NFLX) < sma50_nflx,-1,0)))
sma50_nflx_ts[is.na(sma50_nflx_ts)] <- 0
# SMA 20 and SMA 50 Crossover Signal
sma_nflx_ts <- Lag(
  ifelse(Lag(sma20_nflx) < Lag(sma50_nflx) & sma20_nflx > sma50_nflx,1,
         ifelse(Lag(sma20_nflx) > Lag(sma50_nflx) & sma20_nflx < sma50_nflx,-1,0)))
sma_nflx_ts[is.na(sma_nflx_ts)] <- 0

# 2. Parabolic Stop And Reverse (SAR) 

# a. AAPL
sar_aapl_ts <- Lag(
  ifelse(Lag(Cl(AAPL)) < Lag(sar_aapl) & Cl(AAPL) > sar_aapl,1,
         ifelse(Lag(Cl(AAPL)) > Lag(sar_aapl) & Cl(AAPL) < sar_aapl,-1,0)))
sar_aapl_ts[is.na(sar_aapl_ts)] <- 0
# b. TSLA
sar_tsla_ts <- Lag(
  ifelse(Lag(Cl(TSLA)) < Lag(sar_tsla) & Cl(TSLA) > sar_tsla,1,
         ifelse(Lag(Cl(TSLA)) > Lag(sar_tsla) & Cl(TSLA) < sar_tsla,-1,0)))
sar_tsla_ts[is.na(sar_tsla_ts)] <- 0
# c. NFLX
sar_nflx_ts <- Lag(
  ifelse(Lag(Cl(NFLX)) < Lag(sar_nflx) & Cl(NFLX) > sar_nflx,1,
         ifelse(Lag(Cl(NFLX)) > Lag(sar_nflx) & Cl(NFLX) < sar_nflx,-1,0)))
sar_nflx_ts[is.na(sar_nflx_ts)] <- 0

# 3. Commodity Channel Index  (CCI)

# a. AAPL
cci_aapl_ts <- Lag(
  ifelse(Lag(cci_aapl) < (-100) & cci_aapl > (-100),1,
         ifelse(Lag(cci_aapl) < (100) & cci_aapl > (100),-1,0)))
cci_aapl_ts[is.na(cci_aapl_ts)] <- 0
# b. TSLA
cci_tsla_ts <- Lag(
  ifelse(Lag(cci_tsla) < (-100) & cci_tsla > (-100),1,
         ifelse(Lag(cci_tsla) < (100) & cci_tsla > (100),-1,0)))
cci_tsla_ts[is.na(cci_tsla_ts)] <- 0
# c. NFLX
cci_nflx_ts <- Lag(
  ifelse(Lag(cci_nflx) < (-100) & cci_nflx > (-100),1,
         ifelse(Lag(cci_nflx) < (100) & cci_nflx > (100),-1,0)))
cci_nflx_ts[is.na(cci_nflx_ts)] <- 0

# 4. Rate of Change (ROC)

# a. AAPL
roc_aapl_ts <- Lag(
  ifelse(Lag(roc_aapl) < (-0.05) & roc_aapl > (-0.05),1,
         ifelse(Lag(roc_aapl) < (0.05) & roc_aapl > (0.05),-1,0)))
roc_aapl_ts[is.na(roc_aapl_ts)] <- 0
# b. TSLA
roc_tsla_ts <- Lag(
  ifelse(Lag(roc_tsla) < (-0.05) & roc_tsla > (-0.05),1,
         ifelse(Lag(roc_tsla) < (0.05) & roc_tsla > (0.05),-1,0)))
roc_tsla_ts[is.na(roc_tsla_ts)] <- 0
# c. NFLX
roc_nflx_ts <- Lag(
  ifelse(Lag(roc_nflx) < (-0.05) & roc_nflx > (-0.05),1,
         ifelse(Lag(roc_nflx) < (0.05) & roc_nflx > (0.05),-1,0)))
roc_nflx_ts[is.na(roc_nflx_ts)] <- 0

# 5. Stochastic Momentum Index (SMI)

# a. AAPL
smi_aapl_ts <- Lag(
  ifelse(Lag(smi_aapl[,1]) < Lag(smi_aapl[,2]) & smi_aapl[,1] > smi_aapl[,2],1, 
         ifelse(Lag(smi_aapl[,1]) > Lag(smi_aapl[,2]) & smi_aapl[,1] < smi_aapl[,2],-1,0)))
smi_aapl_ts[is.na(smi_aapl_ts)] <- 0
# b. TSLA
smi_tsla_ts <- Lag(
  ifelse(Lag(smi_tsla[,1]) < Lag(smi_tsla[,2]) & smi_tsla[,1] > smi_tsla[,2],1, 
         ifelse(Lag(smi_tsla[,1]) > Lag(smi_tsla[,2]) & smi_tsla[,1] < smi_tsla[,2],-1,0)))
smi_tsla_ts[is.na(smi_tsla_ts)] <- 0
# a. NFLX
smi_nflx_ts <- Lag(
  ifelse(Lag(smi_nflx[,1]) < Lag(smi_nflx[,2]) & smi_nflx[,1] > smi_nflx[,2],1, 
         ifelse(Lag(smi_nflx[,1]) > Lag(smi_nflx[,2]) & smi_nflx[,1] < smi_nflx[,2],-1,0)))
smi_nflx_ts[is.na(smi_nflx_ts)] <- 0

# 6. williams %R

# a. AAPL
wpr_aapl_ts <- Lag(
  ifelse(Lag(wpr_aapl) > 0.8 & wpr_aapl < 0.8,1,
         ifelse(Lag(wpr_aapl) > 0.2 & wpr_aapl < 0.2,-1,0)))
wpr_aapl_ts[is.na(wpr_aapl_ts)] <- 0
# b. TSLA
wpr_tsla_ts <- Lag(
  ifelse(Lag(wpr_tsla) > 0.8 & wpr_tsla < 0.8,1,
         ifelse(Lag(wpr_tsla) > 0.2 & wpr_tsla < 0.2,-1,0)))
wpr_tsla_ts[is.na(wpr_tsla_ts)] <- 0
# c. NFLX
wpr_nflx_ts <- Lag(
  ifelse(Lag(wpr_nflx) > 0.8 & wpr_nflx < 0.8,1,
         ifelse(Lag(wpr_nflx) > 0.2 & wpr_nflx < 0.2,-1,0)))
wpr_nflx_ts[is.na(wpr_nflx_ts)] <- 0

# Creating Trading Strategies using Signals

# 1. SMA 20 and SMA 50 Crossover Strategy

# a. AAPL
sma_aapl_strat <- ifelse(sma_aapl_ts > 1,0,1)
for (i in 1 : length(Cl(AAPL))) {
  sma_aapl_strat[i] <- ifelse(sma_aapl_ts[i] == 1,1,ifelse(sma_aapl_ts[i] == -1,0,sma_aapl_strat[i-1]))
}
sma_aapl_strat[is.na(sma_aapl_strat)] <- 1
sma_aapl_stratcomp <- cbind(sma20_aapl, sma50_aapl, sma_aapl_ts, sma_aapl_strat)
colnames(sma_aapl_stratcomp) <- c('SMA(20)','SMA(50)','SMA SIGNAL','SMA POSITION')
# b. TSLA
sma_tsla_strat <- ifelse(sma_tsla_ts > 1,0,1)
for (i in 1 : length(Cl(TSLA))) {
  sma_tsla_strat[i] <- ifelse(sma_tsla_ts[i] == 1,1,ifelse(sma_tsla_ts[i] == -1,0,sma_tsla_strat[i-1]))
}
sma_tsla_strat[is.na(sma_tsla_strat)] <- 1
sma_tsla_stratcomp <- cbind(sma20_tsla, sma50_tsla, sma_tsla_ts, sma_tsla_strat)
colnames(sma_tsla_stratcomp) <- c('SMA(20)','SMA(50)','SMA SIGNAL','SMA POSITION')
# c. NFLX
sma_nflx_strat <- ifelse(sma_nflx_ts > 1,0,1)
for (i in 1 : length(Cl(NFLX))) {
  sma_nflx_strat[i] <- ifelse(sma_nflx_ts[i] == 1,1,ifelse(sma_nflx_ts[i] == 'SEL',0,sma_nflx_strat[i-1]))
}
sma_nflx_strat[is.na(sma_nflx_strat)] <- 1
sma_nflx_stratcomp <- cbind(sma20_nflx, sma50_nflx, sma_nflx_ts, sma_nflx_strat)
colnames(sma_nflx_stratcomp) <- c('SMA(20)','SMA(50)','SMA SIGNAL','SMA POSITION')

# Parabolic SAR Strategy 

# a. AAPL
sar_aapl_strat <- ifelse(sar_aapl_ts > 1,0,1)
for (i in 1 : length(Cl(AAPL))) {
  sar_aapl_strat[i] <- ifelse(sar_aapl_ts[i] == 1,1,ifelse(sar_aapl_ts[i] == -1,0,sar_aapl_strat[i-1]))
}
sar_aapl_strat[is.na(sar_aapl_strat)] <- 1
sar_aapl_stratcomp <- cbind(Cl(AAPL), sar_aapl, sar_aapl_ts, sar_aapl_strat)
colnames(sar_aapl_stratcomp) <- c('Close','SAR','SAR SIGNAL','SAR POSITION')
# b. TSLA
sar_tsla_strat <- ifelse(sar_tsla_ts > 1,0,1)
for (i in 1 : length(Cl(TSLA))) {
  sar_tsla_strat[i] <- ifelse(sar_tsla_ts[i] == 1,1,ifelse(sar_tsla_ts[i] == -1,0,sar_tsla_strat[i-1]))
}
sar_tsla_strat[is.na(sar_tsla_strat)] <- 1
sar_tsla_stratcomp <- cbind(Cl(TSLA), sar_tsla, sar_tsla_ts, sar_tsla_strat)
colnames(sar_tsla_stratcomp) <- c('Close','SAR','SAR SIGNAL','SAR POSITION')
# c. NFLX
sar_nflx_strat <- ifelse(sar_nflx_ts > 1,0,1)
for (i in 1 : length(Cl(NFLX))) {
  sar_nflx_strat[i] <- ifelse(sar_nflx_ts[i] == 1,1,ifelse(sar_nflx_ts[i] == -1,0,sar_nflx_strat[i-1]))
}
sar_nflx_strat[is.na(sar_nflx_strat)] <- 1
sar_nflx_stratcomp <- cbind(Cl(NFLX), sar_nflx, sar_nflx_ts, sar_nflx_strat)
colnames(sar_nflx_stratcomp) <- c('Close','SAR','SAR SIGNAL','SAR POSITION')

# CCI

# a. AAPL
cci_aapl_strat <- ifelse(cci_aapl_ts > 1,0,1)
for (i in 1 : length(Cl(AAPL))) {
  cci_aapl_strat[i] <- ifelse(cci_aapl_ts[i] == 1,1,ifelse(cci_aapl_ts[i] == -1,0,cci_aapl_strat[i-1]))
}
cci_aapl_strat[is.na(cci_aapl_strat)] <- 1
cci_aapl_stratcomp <- cbind(cci_aapl, cci_aapl_ts, cci_aapl_strat)
colnames(cci_aapl_stratcomp) <- c('CCI','CCI SIGNAL','CCI POSITION')
# b. TSLA
cci_tsla_strat <- ifelse(cci_tsla_ts > 1,0,1)
for (i in 1 : length(Cl(TSLA))) {
  cci_tsla_strat[i] <- ifelse(cci_tsla_ts[i] == 1,1,ifelse(cci_tsla_ts[i] == -1,0,cci_tsla_strat[i-1]))
}
cci_tsla_strat[is.na(cci_tsla_strat)] <- 1
cci_tsla_stratcomp <- cbind(cci_tsla, cci_tsla_ts, cci_tsla_strat)
colnames(cci_tsla_stratcomp) <- c('CCI','CCI SIGNAL','CCI POSITION')
# c. NFLX
cci_nflx_strat <- ifelse(cci_nflx_ts > 1,0,1)
for (i in 1 : length(Cl(NFLX))) {
  cci_nflx_strat[i] <- ifelse(cci_nflx_ts[i] == 1,1,ifelse(cci_nflx_ts[i] == -1,0,cci_nflx_strat[i-1]))
}
cci_nflx_strat[is.na(cci_nflx_strat)] <- 1
cci_nflx_stratcomp <- cbind(cci_nflx, cci_nflx_ts, cci_nflx_strat)
colnames(cci_nflx_stratcomp) <- c('CCI','CCI SIGNAL','CCI POSITION')

# ROC

# a. AAPL
roc_aapl_strat <- ifelse(roc_aapl_ts > 1,0,1)
for (i in 1 : length(Cl(AAPL))) {
  roc_aapl_strat[i] <- ifelse(roc_aapl_ts[i] == 1,1,ifelse(roc_aapl_ts[i] == -1,0,roc_aapl_strat[i-1]))
}
roc_aapl_strat[is.na(roc_aapl_strat)] <- 1
roc_aapl_stratcomp <- cbind(roc_aapl, roc_aapl_ts, roc_aapl_strat)
colnames(roc_aapl_stratcomp) <- c('ROC(25)','ROC SIGNAL','ROC POSITION')
# b. TSLA
roc_tsla_strat <- ifelse(roc_tsla_ts > 1,0,1)
for (i in 1 : length(Cl(TSLA))) {
  roc_tsla_strat[i] <- ifelse(roc_tsla_ts[i] == 1,1,ifelse(roc_tsla_ts[i] == -1,0,roc_tsla_strat[i-1]))
}
roc_tsla_strat[is.na(roc_tsla_strat)] <- 1
roc_tsla_stratcomp <- cbind(roc_tsla, roc_tsla_ts, roc_tsla_strat)
colnames(roc_tsla_stratcomp) <- c('ROC(25)','ROC SIGNAL','ROC POSITION')
# c. NFLX
roc_nflx_strat <- ifelse(roc_nflx_ts > 1,0,1)
for (i in 1 : length(Cl(NFLX))) {
  roc_nflx_strat[i] <- ifelse(roc_nflx_ts[i] == 1,1,ifelse(roc_nflx_ts[i] == -1,0,roc_nflx_strat[i-1]))
}
roc_nflx_strat[is.na(roc_nflx_strat)] <- 1
roc_nflx_stratcomp <- cbind(roc_nflx, roc_nflx_ts, roc_nflx_strat)
colnames(roc_nflx_stratcomp) <- c('ROC(25)','ROC SIGNAL','ROC POSITION')

# SMI

# a. AAPL
smi_aapl_strat <- ifelse(smi_aapl_ts > 1,0,1)
for (i in 1 : length(Cl(AAPL))) {
  smi_aapl_strat[i] <- ifelse(smi_aapl_ts[i] == 1,1,ifelse(smi_aapl_ts[i] == -1,0,smi_aapl_strat[i-1]))
}
smi_aapl_strat[is.na(smi_aapl_strat)] <- 1
smi_aapl_stratcomp <- cbind(smi_aapl[,1],smi_aapl[,2],smi_aapl_ts,smi_aapl_strat)
colnames(smi_aapl_stratcomp) <- c('SMI','SMI(S)','SMI SIGNAL','SMI POSITION')
# b. TSLA
smi_tsla_strat <- ifelse(smi_tsla_ts > 1,0,1)
for (i in 1 : length(Cl(TSLA))) {
  smi_tsla_strat[i] <- ifelse(smi_tsla_ts[i] == 1,1,ifelse(smi_tsla_ts[i] == -1,0,smi_tsla_strat[i-1]))
}
smi_tsla_strat[is.na(smi_tsla_strat)] <- 1
smi_tsla_stratcomp <- cbind(smi_tsla[,1],smi_tsla[,2],smi_tsla_ts,smi_tsla_strat)
colnames(smi_tsla_stratcomp) <- c('SMI','SMI(S)','SMI SIGNAL','SMI POSITION')
# c. NFLX
smi_nflx_strat <- ifelse(smi_nflx_ts > 1,0,1)
for (i in 1 : length(Cl(NFLX))) {
  smi_nflx_strat[i] <- ifelse(smi_nflx_ts[i] == 1,1,ifelse(smi_nflx_ts[i] == -1,0,smi_nflx_strat[i-1]))
}
smi_nflx_strat[is.na(smi_nflx_strat)] <- 1
smi_nflx_stratcomp <- cbind(smi_nflx[,1],smi_nflx[,2],smi_nflx_ts,smi_nflx_strat)
colnames(smi_nflx_stratcomp) <- c('SMI','SMI(S)','SMI SIGNAL','SMI POSITION')

# WPR

# a. AAPL
wpr_aapl_strat <- ifelse(wpr_aapl_ts > 1,0,1)
for (i in 1 : length(Cl(AAPL))) {
  wpr_aapl_strat[i] <- ifelse(wpr_aapl_ts[i] == 1,1,ifelse(wpr_aapl_ts[i] == -1,0,wpr_aapl_strat[i-1]))
}
wpr_aapl_strat[is.na(wpr_aapl_strat)] <- 1
wpr_aapl_stratcomp <- cbind(wpr_aapl, wpr_aapl_ts, wpr_aapl_strat)
colnames(wpr_aapl_stratcomp) <- c('WPR(14)','WPR SIGNAL','WPR POSITION')
# b. TSLA
wpr_tsla_strat <- ifelse(wpr_tsla_ts > 1,0,1)
for (i in 1 : length(Cl(TSLA))) {
  wpr_tsla_strat[i] <- ifelse(wpr_tsla_ts[i] == 1,1,ifelse(wpr_tsla_ts[i] == -1,0,wpr_tsla_strat[i-1]))
}
wpr_tsla_strat[is.na(wpr_tsla_strat)] <- 1
wpr_tsla_stratcomp <- cbind(wpr_tsla, wpr_tsla_ts, wpr_tsla_strat)
colnames(wpr_tsla_stratcomp) <- c('WPR(14)','WPR SIGNAL','WPR POSITION')
# c. NFLX
wpr_nflx_strat <- ifelse(wpr_nflx_ts > 1,0,1)
for (i in 1 : length(Cl(NFLX))) {
  wpr_nflx_strat[i] <- ifelse(wpr_nflx_ts[i] == 1,1,ifelse(wpr_nflx_ts[i] == -1,0,wpr_nflx_strat[i-1]))
}
wpr_nflx_strat[is.na(wpr_nflx_strat)] <- 1
wpr_nflx_stratcomp <- cbind(wpr_nflx, wpr_nflx_ts, wpr_nflx_strat)
colnames(wpr_nflx_stratcomp) <- c('WPR(14)','WPR SIGNAL','WPR POSITION')

# Trading Strategy Performance 

# Calculating Returns & setting Benchmark for companies
ret_aapl <- diff(log(Cl(AAPL)))
ret_tsla <- diff(log(Cl(TSLA)))
ret_nflx <- diff(log(Cl(NFLX)))
benchmark_aapl <- ret_aapl
benchmark_tsla <- ret_tsla
benchmark_nflx <- ret_nflx

# SMA 

# 1. AAPL
sma_aapl_ret <- ret_aapl*sma_aapl_strat
sma_aapl_ret_commission_adj <- ifelse((sma_aapl_ts == 1|sma_aapl_ts == -1) & sma_aapl_strat != Lag(sma_aapl_ts), (ret_aapl-0.05)*sma_aapl_strat, ret_aapl*sma_aapl_strat)
sma_aapl_comp <- cbind(sma_aapl_ret, sma_aapl_ret_commission_adj, benchmark_aapl)
colnames(sma_aapl_comp) <- c('SMA','SMA Commission Adj','Apple Benchmark')
charts.PerformanceSummary(sma_aapl_comp, main = 'Apple SMA Performance')
sma_aapl_comp_table <- table.AnnualizedReturns(sma_aapl_comp)
# 2. TSLA
sma_tsla_ret <- ret_tsla*sma_tsla_strat
sma_tsla_ret_commission_adj <- ifelse((sma_tsla_ts == 1|sma_tsla_ts == -1) & sma_tsla_strat != Lag(sma_tsla_ts), (ret_tsla-0.05)*sma_tsla_strat, ret_tsla*sma_tsla_strat)
sma_tsla_comp <- cbind(sma_tsla_ret, sma_tsla_ret_commission_adj, benchmark_tsla)
colnames(sma_tsla_comp) <- c('SMA','SMA Commission Adj','Tesla Benchmark')
charts.PerformanceSummary(sma_tsla_comp, main = 'Tesla SMA Performance')
sma_tsla_comp_table <- table.AnnualizedReturns(sma_tsla_comp)
# 3. NFLX
sma_nflx_ret <- ret_nflx*sma_nflx_strat
sma_nflx_ret_commission_adj <- ifelse((sma_nflx_ts == 1|sma_nflx_ts == -1) & sma_nflx_strat != Lag(sma_nflx_ts), (ret_nflx-0.05)*sma_nflx_strat, ret_nflx*sma_nflx_strat)
sma_nflx_comp <- cbind(sma_nflx_ret, sma_nflx_ret_commission_adj, benchmark_nflx)
colnames(sma_nflx_comp) <- c('SMA','SMA Commission Adj','Netflix Benchmark')
charts.PerformanceSummary(sma_nflx_comp, main = 'Netflix SMA Performance')
sma_nflx_comp_table <- table.AnnualizedReturns(sma_nflx_comp)

# Parabolic SAR 

# 1. AAPL
sar_aapl_ret <- ret_aapl*sar_aapl_strat
sar_aapl_ret_commission_adj <- ifelse((sar_aapl_ts == 1|sar_aapl_ts == -1) & sar_aapl_strat != Lag(sar_aapl_ts), (ret_aapl-0.05)*sar_aapl_strat, ret_aapl*sar_aapl_strat)
sar_aapl_comp <- cbind(sar_aapl_ret, sar_aapl_ret_commission_adj, benchmark_aapl)
colnames(sar_aapl_comp) <- c('SAR','SAR Commission Adj','Apple Benchmark')
charts.PerformanceSummary(sar_aapl_comp, main = 'Apple Parabolic SAR Performance')
sar_aapl_comp_table <- table.AnnualizedReturns(sar_aapl_comp)
# 2. TSLA
sar_tsla_ret <- ret_tsla*sar_tsla_strat
sar_tsla_ret_commission_adj <- ifelse((sar_tsla_ts == 1|sar_tsla_ts == -1) & sar_tsla_strat != Lag(sar_tsla_ts), (ret_tsla-0.05)*sar_tsla_strat, ret_tsla*sar_tsla_strat)
sar_tsla_comp <- cbind(sar_tsla_ret, sar_tsla_ret_commission_adj, benchmark_tsla)
colnames(sar_tsla_comp) <- c('SAR','SAR Commission Adj','Tesla Benchmark')
charts.PerformanceSummary(sar_tsla_comp, main = 'Tesla Parabolic SAR Performance')
sar_tsla_comp_table <- table.AnnualizedReturns(sar_tsla_comp)
# 3. NFLX
sar_nflx_ret <- ret_nflx*sar_nflx_strat
sar_nflx_ret_commission_adj <- ifelse((sar_nflx_ts == 1|sar_nflx_ts == -1) & sar_nflx_strat != Lag(sar_nflx_ts), (ret_nflx-0.05)*sar_nflx_strat, ret_nflx*sar_nflx_strat)
sar_nflx_comp <- cbind(sar_nflx_ret, sar_nflx_ret_commission_adj, benchmark_nflx)
colnames(sar_nflx_comp) <- c('SAR','SAR Commission Adj','Netflix Benchmark')
charts.PerformanceSummary(sar_nflx_comp, main = 'Netflix Parabolic SAR Performance')
sar_nflx_comp_table <- table.AnnualizedReturns(sar_nflx_comp)

# CCI  

# 1. AAPL
cci_aapl_ret <- ret_aapl*cci_aapl_strat
cci_aapl_ret_commission_adj <- ifelse((cci_aapl_ts == 1|cci_aapl_ts == -1) & cci_aapl_strat != Lag(cci_aapl_ts), (ret_aapl-0.05)*cci_aapl_strat, ret_aapl*cci_aapl_strat)
cci_aapl_comp <- cbind(cci_aapl_ret, cci_aapl_ret_commission_adj, benchmark_aapl)
colnames(cci_aapl_comp) <- c('CCI','CCI Commission Adj','Apple Benchmark')
charts.PerformanceSummary(cci_aapl_comp, main = 'Apple CCI Performance')
cci_aapl_comp_table <- table.AnnualizedReturns(cci_aapl_comp)
# 2. TSLA
cci_tsla_ret <- ret_tsla*cci_tsla_strat
cci_tsla_ret_commission_adj <- ifelse((cci_tsla_ts == 1|cci_tsla_ts == -1) & cci_tsla_strat != Lag(cci_tsla_ts), (ret_tsla-0.05)*cci_tsla_strat, ret_tsla*cci_tsla_strat)
cci_tsla_comp <- cbind(cci_tsla_ret, cci_tsla_ret_commission_adj, benchmark_tsla)
colnames(cci_tsla_comp) <- c('CCI','CCI Commission Adj','Tesla Benchmark')
charts.PerformanceSummary(cci_tsla_comp, main = 'Tesla CCI Performance')
cci_tsla_comp_table <- table.AnnualizedReturns(cci_tsla_comp)
# 3. NFLX
cci_nflx_ret <- ret_nflx*cci_nflx_strat
cci_nflx_ret_commission_adj <- ifelse((cci_nflx_ts == 1|cci_nflx_ts == -1) & cci_nflx_strat != Lag(cci_nflx_ts), (ret_nflx-0.05)*cci_nflx_strat, ret_nflx*cci_nflx_strat)
cci_nflx_comp <- cbind(cci_nflx_ret, cci_nflx_ret_commission_adj, benchmark_nflx)
colnames(cci_nflx_comp) <- c('CCI','CCI Commission Adj','Netflix Benchmark')
charts.PerformanceSummary(cci_nflx_comp, main = 'Netflix CCI Performance')
cci_nflx_comp_table <- table.AnnualizedReturns(cci_nflx_comp)

# ROC  

# 1. AAPL
roc_aapl_ret <- ret_aapl*roc_aapl_strat
roc_aapl_ret_commission_adj <- ifelse((roc_aapl_ts == 1|roc_aapl_ts == -1) & roc_aapl_strat != Lag(roc_aapl_ts), (ret_aapl-0.05)*roc_aapl_strat, ret_aapl*roc_aapl_strat)
roc_aapl_comp <- cbind(roc_aapl_ret, roc_aapl_ret_commission_adj, benchmark_aapl)
colnames(roc_aapl_comp) <- c('ROC','ROC Commission Adj','Apple Benchmark')
charts.PerformanceSummary(roc_aapl_comp, main = 'Apple ROC Performance')
roc_aapl_comp_table <- table.AnnualizedReturns(roc_aapl_comp)
# 2. TSLA
roc_tsla_ret <- ret_tsla*roc_tsla_strat
roc_tsla_ret_commission_adj <- ifelse((roc_tsla_ts == 1|roc_tsla_ts == -1) & roc_tsla_strat != Lag(roc_tsla_ts), (ret_tsla-0.05)*roc_tsla_strat, ret_tsla*roc_tsla_strat)
roc_tsla_comp <- cbind(roc_tsla_ret, roc_tsla_ret_commission_adj, benchmark_tsla)
colnames(roc_tsla_comp) <- c('ROC','ROC Commission Adj','Tesla Benchmark')
charts.PerformanceSummary(roc_tsla_comp, main = 'Tesla ROC Performance')
roc_tsla_comp_table <- table.AnnualizedReturns(roc_tsla_comp)
# 3. NFLX
roc_nflx_ret <- ret_nflx*roc_nflx_strat
roc_nflx_ret_commission_adj <- ifelse((roc_nflx_ts == 1|roc_nflx_ts == -1) & roc_nflx_strat != Lag(roc_nflx_ts), (ret_nflx-0.05)*roc_nflx_strat, ret_nflx*roc_nflx_strat)
roc_nflx_comp <- cbind(roc_nflx_ret, roc_nflx_ret_commission_adj, benchmark_nflx)
colnames(roc_nflx_comp) <- c('ROC','ROC Commission Adj','Netflix Benchmark')
charts.PerformanceSummary(roc_nflx_comp, main = 'Netflix ROC Performance')
roc_nflx_comp_table <- table.AnnualizedReturns(roc_nflx_comp)

# SMI  

# 1. AAPL
smi_aapl_ret <- ret_aapl*smi_aapl_strat
smi_aapl_ret_commission_adj <- ifelse((smi_aapl_ts == 1|smi_aapl_ts == -1) & smi_aapl_strat != Lag(smi_aapl_ts), (ret_aapl-0.05)*smi_aapl_strat, ret_aapl*smi_aapl_strat)
smi_aapl_comp <- cbind(smi_aapl_ret, smi_aapl_ret_commission_adj, benchmark_aapl)
colnames(smi_aapl_comp) <- c('SMI','SMI Commission Adj','Apple Benchmark')
charts.PerformanceSummary(smi_aapl_comp, main = 'Apple SMI Performance')
smi_aapl_comp_table <- table.AnnualizedReturns(smi_aapl_comp)
# 2. TSLA
smi_tsla_ret <- ret_tsla*smi_tsla_strat
smi_tsla_ret_commission_adj <- ifelse((smi_tsla_ts == 1|smi_tsla_ts == -1) & smi_tsla_strat != Lag(smi_tsla_ts), (ret_tsla-0.05)*smi_tsla_strat, ret_tsla*smi_tsla_strat)
smi_tsla_comp <- cbind(smi_tsla_ret, smi_tsla_ret_commission_adj, benchmark_tsla)
colnames(smi_tsla_comp) <- c('SMI','SMI Commission Adj','Tesla Benchmark')
charts.PerformanceSummary(smi_tsla_comp, main = 'Tesla SMI Performance')
smi_tsla_comp_table <- table.AnnualizedReturns(smi_tsla_comp)
# 3. NFLX
smi_nflx_ret <- ret_nflx*smi_nflx_strat
smi_nflx_ret_commission_adj <- ifelse((smi_nflx_ts == 1|smi_nflx_ts == -1) & smi_nflx_strat != Lag(smi_nflx_ts), (ret_nflx-0.05)*smi_nflx_strat, ret_nflx*smi_nflx_strat)
smi_nflx_comp <- cbind(smi_nflx_ret, smi_nflx_ret_commission_adj, benchmark_nflx)
colnames(smi_nflx_comp) <- c('SMI','SMI Commission Adj','Netflix Benchmark')
charts.PerformanceSummary(smi_nflx_comp, main = 'Netflix SMI Performance')
smi_nflx_comp_table <- table.AnnualizedReturns(smi_nflx_comp)

# WPR  

# 1. AAPL
wpr_aapl_ret <- ret_aapl*wpr_aapl_strat
wpr_aapl_ret_commission_adj <- ifelse((wpr_aapl_ts == 1|wpr_aapl_ts == -1) & wpr_aapl_strat != Lag(wpr_aapl_ts), (ret_aapl-0.05)*wpr_aapl_strat, ret_aapl*wpr_aapl_strat)
wpr_aapl_comp <- cbind(wpr_aapl_ret, wpr_aapl_ret_commission_adj, benchmark_aapl)
colnames(wpr_aapl_comp) <- c('WPR','WPR Commission Adj','Apple Benchmark')
charts.PerformanceSummary(wpr_aapl_comp, main = 'Apple WPR Performance')
wpr_aapl_comp_table <- table.AnnualizedReturns(wpr_aapl_comp)
# 2. TSLA
wpr_tsla_ret <- ret_tsla*wpr_tsla_strat
wpr_tsla_ret_commission_adj <- ifelse((wpr_tsla_ts == 1|wpr_tsla_ts == -1) & wpr_tsla_strat != Lag(wpr_tsla_ts), (ret_tsla-0.05)*wpr_tsla_strat, ret_tsla*wpr_tsla_strat)
wpr_tsla_comp <- cbind(wpr_tsla_ret, wpr_tsla_ret_commission_adj, benchmark_tsla)
colnames(wpr_tsla_comp) <- c('WPR','WPR Commission Adj','Tesla Benchmark')
charts.PerformanceSummary(wpr_tsla_comp, main = 'Tesla WPR Performance')
wpr_tsla_comp_table <- table.AnnualizedReturns(wpr_tsla_comp)
# 3. NFLX
wpr_nflx_ret <- ret_nflx*wpr_nflx_strat
wpr_nflx_ret_commission_adj <- ifelse((wpr_nflx_ts == 1|wpr_nflx_ts == -1) & wpr_nflx_strat != Lag(wpr_nflx_ts), (ret_nflx-0.05)*wpr_nflx_strat, ret_nflx*wpr_nflx_strat)
wpr_nflx_comp <- cbind(wpr_nflx_ret, wpr_nflx_ret_commission_adj, benchmark_nflx)
colnames(wpr_nflx_comp) <- c('WPR','WPR Commission Adj','Netflix Benchmark')
charts.PerformanceSummary(wpr_nflx_comp, main = 'Netflix WPR Performance')
wpr_nflx_comp_table <- table.AnnualizedReturns(wpr_nflx_comp)