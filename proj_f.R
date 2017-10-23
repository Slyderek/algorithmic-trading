# Bitcoin Algorithmic Trading simulation program
# 22 Oct 2017
# Dongkyu Choi, CFA
# 
#############################################################################################
# OPTIMAL TRADING SIMULATION MODULE
# Based on the df data set, it simulates trading
# 1.  It can be used for both IN sample and OUT sample by giving Isin param (1 or 0)
# 2.  Outputs the trading book record
# 
# 21 Oct 2017

profit_func <- function(df, mu, stdevbig, stdevsmall, Isin) {
  
  bege = 0
  ende = 0
  entry_up   = mu + stdevbig
  entry_down = mu - stdevbig
  exit_up    = mu + stdevsmall
  exit_down  = mu - stdevsmall
  lv = 2
  borrowRateX = 0.01/100 * 24/4 * 252/365
  borrowRateY = 0.01/100 * 24/4 * 252/365
  
  datasize_full = length(df$spread)
  datasize_insample = round(datasize_full * 4.5/5.5)
  
  if (Isin == 0) {  # if Out of sample
    bege = datasize_insample + 1
    ende = datasize_full
  }
  else if (Isin == 1) { # if in sample: 1...in sample
    bege = 1
    ende = datasize_insample
  }
  else {
    print("error, put 1 if you test in-sample, put 0 for out-sample")
  }
  df$equityX[bege] = 500000
  df$equityY[bege] = 500000
  for (i in bege:(ende-1)) {
    if (df$spread[i] >= entry_up && df$on[i] == 0) {  #in! 0->2 (uptrade)
      df$equityX[i+1] = df$equityX[i]*(1-0.0023) # commission+market shock deduction for X
      df$equityY[i+1] = df$equityY[i]*(1-0.0023) # commission+market shock deduction for X
      df$amountX[i] = +lv*df$equityX[i]/df[[i,2]]  # how much did I trade (+ is Long)
      df$amountY[i] = -lv*df$equityY[i]/df[[i,3]]    # how much did I trade (- is Short)
      df$lockinX[i] = df[[i,2]]  # how much price did I trade for X
      df$lockinY[i] = df[[i,3]]  # how much price did I trade for Y
      df$on[i+1] = 2
    }
    else if (df$spread[i] <= entry_down && df$on[i] == 0) { #in! 0->1 (downtrade)
      df$equityX[i+1] = df$equityX[i]*(1-0.0023) # commission+market shock deduction for X
      df$equityY[i+1] = df$equityY[i]*(1-0.0023) # commission+market shock deduction for Y
      df$amountX[i] = -lv*df$equityX[i]/df[[i,2]]  # how much did I trade (+ is Long)
      df$amountY[i] = +lv*df$equityY[i]/df[[i,3]]    # how much did I trade (- is Short)
      df$lockinX[i] = df[[i,2]]  # how much price did I trade for X
      df$lockinY[i] = df[[i,3]]  # how much price did I trade for Y
      df$on[i+1] = 1
    }
    else if (df$spread[i] >= exit_up && df$on[i] == 2) {  #out! 2->0
      df$amountX[i] = df$amountX[i-1]
      df$amountY[i] = df$amountY[i-1]
      df$lockinX[i] = df$lockinX[i-1]
      df$lockinY[i] = df$lockinY[i-1]
      df$profitX[i] = df$amountX[i] * (df[[i,2]] - df[[i-1,2]])  # leveraged amount
      df$profitY[i] = df$amountY[i] * (df[[i,3]] - df[[i-1,3]])
      df$equityX[i+1] = df$equityX[i] + df$profitX[i] # profit is added as a result of averaged trade amount
      df$equityY[i+1] = df$equityY[i] + df$profitY[i] # same
      df$equityX[i+1] = df$equityX[i+1]*(1-0.0023) # commission+market shock deduction for X
      df$equityY[i+1] = df$equityY[i+1]*(1-0.0023) # commission+market shock deduction for Y
      df$on[i+1] = 0
    }
    else if (df$spread[i] >= exit_down && df$on[i] == 1) {  # out! 1->0
      df$amountX[i] = df$amountX[i-1]
      df$amountY[i] = df$amountY[i-1]
      df$lockinX[i] = df$lockinX[i-1]
      df$lockinY[i] = df$lockinY[i-1]
      df$profitX[i] = df$amountX[i] * (df[[i,2]] - df[[i-1,2]])
      df$profitY[i] = df$amountY[i] * (df[[i,3]] - df[[i-1,3]])
      df$equityX[i+1] = df$equityX[i] + df$profitX[i]
      df$equityY[i+1] = df$equityY[i] + df$profitY[i]
      df$equityX[i+1] = df$equityX[i+1]*(1-0.0023) # commission+market shock deduction for X
      df$equityY[i+1] = df$equityY[i+1]*(1-0.0023) # commission+market shock deduction for Y
      df$on[i+1] = 0
    }
    else {  # most of the cases : MTM
      if (df$on[i] == 1 || df$on[i] == 2)
      {  # we are already in the trade
        df$amountX[i] = df$amountX[i-1]
        df$amountY[i] = df$amountY[i-1]
        df$lockinX[i] = df$lockinX[i-1]
        df$lockinY[i] = df$lockinY[i-1]
        df$profitX[i] = df$amountX[i] * (df[[i,2]] - df[[i-1,2]])
        df$profitY[i] = df$amountY[i] * (df[[i,3]] - df[[i-1,3]])
        df$equityX[i+1] = df$equityX[i] + df$profitX[i] - borrowRateX/252*df$lockinX[i]*abs(df$amountX[i])/2 # at the moment, the position's total profit
        df$equityY[i+1] = df$equityY[i] + df$profitY[i] - borrowRateY/252*df$lockinY[i]*abs(df$amountY[i])/2
        df$on[i+1] = df$on[i]
      } else {
        df$equityX[i+1] = df$equityX[i]
        df$equityY[i+1] = df$equityY[i]
        df$on[i+1] = df$on[i]
      }
    }
  }
  return(df[bege:(ende-1),])
}

spreadtrading_profit <- function(df, mu, stdevbig, stdevsmall)
{
  df1 = profit_func(df, mu, stdevbig, stdevsmall, 1)
  df2 = profit_func(df, mu, stdevbig, stdevsmall, 0)
  equityTotal1 = df1[[length(df1$equityX),6]]+df1[[length(df1$equityY),7]]
  equityTotal2 = df2[[length(df2$equityX),6]]+df2[[length(df2$equityY),7]]
  profit_fin <- c(equityTotal1, equityTotal2)
  return(profit_fin)
}

spreadtrading_equity <- function(df, mu, stdevbig, stdevsmall)
{
  df1 = profit_func(df, mu, stdevbig, stdevsmall, 1)
  df2 = profit_func(df, mu, stdevbig, stdevsmall, 0)
  return(df2) # back testing for graph
}


#############################################################################################
# AUTO COMPARISON AND OPTIMIZATION MODULE
# Based on the bdf data set, it attemps iterative trial among the all the exchanges by:
# 1.  Picks a pair of exchange data and generate data frame for it
# 2.  Inputs the dataset to optimize Trade-in / Out multiplier to train in IN-SAMPLE data
#     using OPTIMAL TRADING SIMULATION MODULE
# 3.  Back Testing, and returning summary result as:
#       - Multiplier used in the strategy
#       - Equity
#       - Sharpe Ratio
# Dongkyu Choi, CFA
# 22 Oct 2017
#############################################################################################

CompareOptimization <- function(bdf, exchNo) {
  library(urca)
  EQUITYFIN <- matrix(nrow = exchNo, ncol = exchNo)
  dimnames(EQUITYFIN) <- list(c("Bitstamp", "Kraken", "Itbit", "HitBTC", "BitFinex"),
                              c("Bitstamp", "Kraken", "Itbit", "HitBTC", "BitFinex"))
  SHARPERATIO <- matrix(nrow = exchNo, ncol = exchNo)
  dimnames(SHARPERATIO) <- list(c("Bitstamp", "Kraken", "Itbit", "HitBTC", "BitFinex"),
                                c("Bitstamp", "Kraken", "Itbit", "HitBTC", "BitFinex"))
  TRADEINMUL <- matrix(nrow = exchNo, ncol = exchNo)
  dimnames(TRADEINMUL) <- list(c("Bitstamp", "Kraken", "Itbit", "HitBTC", "BitFinex"),
                               c("Bitstamp", "Kraken", "Itbit", "HitBTC", "BitFinex"))
  TRADEOUTMUL <- matrix(nrow = exchNo, ncol = exchNo)
  dimnames(TRADEOUTMUL) <- list(c("Bitstamp", "Kraken", "Itbit", "HitBTC", "BitFinex"),
                                c("Bitstamp", "Kraken", "Itbit", "HitBTC", "BitFinex"))
  countX = 0
  countY = 0
  for (x in seq(1, exchNo)) # Exch X
  { # exchange is 1:5. we can give variable here
    countX = countX + 1
    countY = 0 # init. before subloop
    for (y in seq(1, exchNo))  # Exch Y
    {
      countY = countY + 1
      cat("\n", "iteration : x = ", x, ", y = ", y, "\n")
      
      if (x == y) {
        print("same exchange, no spread trading.")
      }
      else {
        ## data initialization ##################################################
        df <- bdf[,1:2] #copy ( initialize )
        df <- df[,(1:1), drop=FALSE] # still data frame
        df$ExchX <- bdf[,c(x+1)]  # i
        df$ExchY <- bdf[,c(y+1)]  # j
        df$spread <- df[,c(2)]-df[,c(3)] # Portfolio ExchX-ExchY (assuming beta=1)
        # in above, Don't do df[2]-df[3] as it brings title too, which makes the column type non-number
        df$on <- 0 # trade-in(1) / out(0)
        df$equityX <- 0
        df$equityX[1]=500000  # or, df[6,1]
        df$equityY <- 0
        df$equityY[1]=500000
        df$amountX <- 0
        df$amountY <- 0
        df$lockinX = 0
        df$lockinY = 0
        df$profitX = 0
        df$profitY = 0
        
        ## Parameter generation #################################################
        borrowRateX = 0.01/100 * 24/4 * 252/365
        borrowRateY = 0.01/100 * 24/4 * 252/365
        mu <- mean(df$spread[ 1 : ( length(df$spread) - 252 ) ])
        sd <- sd(df$spread[ 1 : ( length(df$spread) - 252 ) ])
        ADFtest = ur.df(df$spread, type="none", lags=0) # ADF test on the spread
        sumADF = summary(ADFtest)
        sumADF
        
        ## Optimizing ###########################################################
        testSize = length(seq(0, 4, 0.5))  # testSize^2 is matrix size
        ProfitMatrix <- matrix(nrow=testSize, ncol=testSize)
        countRow = 0
        countCol = 0
        for (i in seq(0, 4, 0.5)) {  # row : big
          cat(i, "| ")
          countRow = countRow + 1
          countCol = 0
          for (j in seq(0, 4, 0.5)) {  # col : small
            cat(j, "| ")
            countCol = countCol + 1
            stdevbig = i*sd  # Optimize
            stdevsmall = j*sd
            dffin <- profit_func(df, mu, stdevbig, stdevsmall, 1)
            endet = length(dffin$equityX)
            equity = dffin$equityX+dffin$equityY
            finalequity = dffin$equityX[endet]+dffin$equityY[endet]
            ProfitMatrix[countRow,countCol] <- finalequity
          }
          cat(" :: ")
        }
        param = which(ProfitMatrix == max(ProfitMatrix), arr.ind = TRUE)
        
        ## BackTesting ##########################################################
        bigsdmultiplier = param[1] * 0.5 - 0.5
        smallmultiplier = param[2] * 0.5 - 0.5
        TRADEINMUL[countX, countY] <- bigsdmultiplier
        TRADEOUTMUL[countX, countY] <- smallmultiplier
        cat("\n","IN/OUT multiple for Exchange ", x, " and", y, " are : ", bigsdmultiplier, " ,", smallmultiplier, "\n")
        stdevbig = bigsdmultiplier*sd  # Optimize
        stdevsmall = smallmultiplier*sd
        dffin <- profit_func(df, mu, stdevbig, stdevsmall, 0) # out of sample Back testing!
        endet = length(dffin$equityX)
        eqt = dffin$equityX+dffin$equityY
        
        finalequity = dffin$equityX[endet]+dffin$equityY[endet]
        cat("\n","Equity for Exchange ", x, " and", y, " : ", finalequity, "\n")
        EQUITYFIN[countX, countY] <- finalequity
        
        plot(df$spread, 
             type="l", main = x, sub = y, xlab = "Time", ylab = "Spread")
        
        plot(dffin$equityX+dffin$equityY, 
             type="l", main = x, sub = y, xlab = "Time", ylab = "Equity (USD)", xaxt='n')
        
        axis(1, at=c(0, 20, 40, 60,80, 100, 120,140,160,180,200,220,240 ), 
             lab=c("10/01/2016","11/01/2016","12/01/2016","1/01/17","2/01/2017","3/01/2017",
                   "4/01/2017","5/01/2017","6/01/2017", "7/01/2017", "8/01/2017", "9/01/2017","10/01/2017"))
        
        ## Sharpe Ratio #########################################################
        yield = diff(log(eqt))
        aver = sum(yield-0.01/252)  # Tsy 1y
        stdv = sd(yield)*sqrt(252)
        SR = aver/stdv  # Sharpe ratio, like 1.177264. higher better
        cat("\n","Sharpe ratio for Exchange ", x, " and", y, " : ", SR, "\n")
        SHARPERATIO[countX, countY] <- SR
      }
    }
  }
}
#############################################################################################


#############################################################################################
# Execution
# 

# Set working directory & Read data
setwd("C:/FE/Statistical Arbitrage/Final Project")

bdf <- read.table("Price_five.csv", header=TRUE, sep = ",") # 2005.10.11~2017.10.10

# Find Correlation
exchNo = 5
A = matrix(nrow=exchNo, ncol=exchNo)
dimnames(A) = list(c("Bitstamp", "Kraken", "Itbit", "HitBTC", "BitFinex"),
                   c("Bitstamp", "Kraken", "Itbit", "HitBTC", "BitFinex"))
for (i in seq(2,6)) {
  for (j in seq(2,6)) {
    A[i-1,j-1] <- cor(bdf[i], bdf[j])
  }
}

# Run Robust Regression
#robustReg = rlm(X ~ Y)

# Trading
CompareOptimization(bdf, exchNo)
