
library(quantmod)
library(tidyquant)
library(timetk)
library(lattice)
#Select Portfolio
myStocks0 <- c("BCYC", "WEED.TO", "GILD", "GOOGL", "NVDA", "MU", "ILMN", "ROBO", "BOTZ", "IRBT", "TWOU")

getSymbols(myStocks0, src ="yahoo", from ="2019-01-01", to = "2021-02-01")

#Representative example of asset growth over time.
#candleChart(NVDA, multi.col =TRUE, theme = "black")
chart_Series(NVDA)
chart_Series(ILMN)
chart_Series(GOOGL)
chart_Series(BOTZ)
chart_Series(BCYC)
chart_Series(MU)
chart_Series(IRBT)
chart_Series(TWOU)
chart_Series(WEED.TO)


#Calculating daily return for every asset. The convert to dataframe while keeping the timestamp information(as.xts)
myRetData0 <- data.frame(as.xts(merge(dailyReturn(BCYC), dailyReturn(WEED.TO), dailyReturn(GILD), dailyReturn(BOTZ), dailyReturn(GOOGL),
                                     dailyReturn(ILMN), dailyReturn(IRBT), dailyReturn(MU),dailyReturn(NVDA), dailyReturn(ROBO), dailyReturn(TWOU))))


#Removing the rows that have NA values
head(myRetData0)
sum(is.na(myRetData0))
myRetData1 = na.omit(myRetData0)
myRetData1
sum(is.na(myRetData1))


#Plotting the daily return for different assets. We see that all assets show similar unstable behaviour around April2020
#This may have to do with global factor affecting the economy.
candleChart(dailyReturn(NVDA)*100)
candleChart(dailyReturn(ILMN)*100)
candleChart(dailyReturn(IRBT)*100)
candleChart(dailyReturn(GILD)*100)
candleChart(dailyReturn(BOTZ)*100)
candleChart(dailyReturn(TWOU)*100)
candleChart(dailyReturn(WEED.TO)*100)
candleChart(dailyReturn(MU)*100)

#BOTZ is probably the most stable share with the least daily fluctuation, althougn it is also affected by the April 2020 events.


#Representative example of mean daily returns for every asset.
mean(dailyReturn(NVDA)*100, 4)
mean(dailyReturn(ILMN)*100, 4)
mean(dailyReturn(ROBO)*100, 4)

#Splitting into a mock train and test set. The first set will be used in the GA algorithm and the second for calculating performance.
nrow(myRetData1)
train_set <-myRetData1[1:208, ]
tail(train_set)
test_set <-myRetData1[209:417, ]
tail(test_set)

#Calculating the  mean return. This calculates the mean of daily return for very asset in the train set
mu <- apply(train_set, 2, mean)
mu


#Calculating covariance, of the train set.

sigma <- cov(train_set)
sigma

#assigning weights as many as the number of assets. Normalizing the assets controls the sum of the weights to be equla to1.
x <-  runif(n = length(myStocks0))
x


evalFunc0 <- function(x){
  x <- x /sum(x) # normalizing to keep sum of weights equal to 1
  print(x)
  wreturn <- sum(x*mu) #we calculate the return by multiplying the ga weight with the daily mean and sum up
  wrisk <- sum(x*sigma)# similar multiply the covariance==risk or each asset per the weight and sum up
  return ( 0.5*wreturn +0.5*wrisk) #assign equal proportion to risk and return
}



low_boundary <- c(sample(rep(0,each= 11)))
low_boundary

up_boundary <- c(sample(rep(1,each= 11))) 
up_boundary

GAmodel0 <- ga( maxiter = 1100, type = "real-valued", fitness = evalFunc0, upper = up_boundary, lower = low_boundary, monitor = TRUE, seed =1)

summary(GAmodel0)
plot(GAmodel0)
GAmodel0@solution
best_weights =sum(GAmodel0@solution)
best_weights
best_weights0 = c(GAmodel0@solution/ sum(GAmodel0@solution))
best_weights0
sum(best_weights0)

#GAmodel with Low risk

evalFunc_lowR <- function(x){
  x <- x /sum(x)
  #print(x)
  wreturn <- sum(x*mu)
  wrisk <- sum(x*sigma)
  return ( 0.8*wreturn +0.2*wrisk)
}


GAmodel_lowRisk <- ga( maxiter = 1100, type = "real-valued", fitness = evalFunc_lowR, upper = up_boundary, lower = low_boundary, monitor = TRUE, seed = 1)

summary(GAmodel_lowRisk)
plot(GAmodel_lowRisk)
GAmodel_lowRisk@solution
best_weights_lowRisk =sum(GAmodel_lowRisk@solution)
best_weights_lowRisk 
best_weights_lowRisk = c(GAmodel_lowRisk@solution/ sum(GAmodel_lowRisk@solution)) # normalizing the GA weights  again to correct  and make equal to 1.
best_weights_lowRisk
sum(best_weights_lowRisk)

# We observe that  the asset with the highest weights that has been chosen by the model is NVDA.
#This was also the asset with the highest  persistent growth over time.
# We also observe that asset number 4 and 8 have very low weight. These assets have experienced exponetial growth
# but they have been very unstable from time to time with transient large losses, so the model has correctly assigned low weight on those.
#Surprisingly asset number 2 has been allocated higher percentage although it experienced great losses in price.

# It is also worth noting that the original model produced from GA with 50:50 return:risk
# is remarkably similar to the portfolio produced when we choose low risk. The GA model has kept the proportion of the good assets
# largely the same and reduced the assets that showed transient losses or total losses (assets No 4,8,11)


#GA model with HighReturn
evalFunc_highReturn <- function(x){
  x <- x /sum(x)
  #print(x)
  wreturn <- sum(x*mu)
  wrisk <- sum(x*sigma)
  return ( 0.2*wreturn +0.8*wrisk)
}

GAmodel_highReturn <- ga( maxiter = 1100, type = "real-valued", fitness = evalFunc_highReturn, upper = up_boundary, lower = low_boundary, monitor = TRUE, seed =1)


summary(GAmodel_highReturn)
plot(GAmodel_highReturn)
GAmodel_highReturn@solution
best_weights_highRtrn =sum(GAmodel_highReturn@solution)
best_weights_highRtrn
best_weights_highRtrn = c(GAmodel_highReturn@solution/ sum(GAmodel_highReturn@solution))
best_weights_highRtrn
sum(best_weights_highRtrn)

#Surprisingly enough the asset with the biggest represenation in the high risk portfolio is not NVDA that has the
#largest growth. The asset chosen by the AG is TWOU which a highly unstable asset which experiences losses.

##################################
#Creating equally balanced weights
bweight <- c(sample(rep(1/11,each= 11)))


#Creating  random weights.
random_weights = c(runif(11, min=0, max=1))
random_weights = random_weights/sum(random_weights)
random_weights

#MK custom made prortfolio. Out of curiocity, I took the GA high Risk portfolio and I have swapped
# the weights of TWOU with NVDA. 
MK_weights <- c(0.0661698542, 0.0064127616, 0.0048619752, 0.0049939236, 0.0111138900,
                0.0087575822, 0.0065786823, 0.0008703494, 0.8262104916, 0.0182337848,
                0.0457967050)




#Convert the test set into a time-frame (for plotting later on)
DailyReturn = as.xts(test_set, date_col = 0)
DailyReturn

#Calculating the Means Returns with different weights. First I calculate the mean return for the test set them perfrom matrix multiplication 
#between the vector weights and the mean of daily returns and convert to a percentage.

mu_test <- apply(test_set, 2, mean)
mu_test


MeanReturn_GAweights = c((best_weights0)%*%(mu_test)*100)
MeanReturn_GAweights

MeanReturnGA_lowRisk = c((best_weights_lowRisk) %*%(mu_test)*100)
MeanReturnGA_lowRisk

MeanReturnGA_highReturn = c((best_weights_highRtrn) %*%(mu_test)*100)
MeanReturnGA_highReturn

MeanReturn_equalWeights = c((bweight) %*%(mu_test)*100)
MeanReturn_equalWeights

MeanReturn_randomWeights = c((random_weights) %*% (mu_test)*100)
MeanReturn_randomWeights

MeanReturn_MKcustom = c((MK_weights) %*% (mu_test) * 100)



All_mean_returns = c(MeanReturn_GAweights, MeanReturnGA_lowRisk,MeanReturnGA_highReturn, MeanReturn_equalWeights, MeanReturn_randomWeights, MeanReturn_MKcustom)
#Plot Barplot
barplot(All_mean_returns, main = "All_means_returns", horiz= TRUE, names.arg = c("1:1 Risk:Return", "LowRisk", "HighRisk", "Equal Weights", "Random Weights", "Custom_made"), las=1, xlim = c(0,0.5))

#High Risk outperforms the other combinations. 
#We see that Low risk has almost the same return as 1:1 risk to return, as well as equal weights.
# Surprisingly enough, when I assigned the asset of the largest growth (NVDA ), the largest weight,
# this did not result in higher return. I guess this comes to show the strength of GA modelling.


#Calculating the covariance for the test set
sigma_test <- cov(test_set)
Risk_GAweights = c((best_weights0)%*%(sigma_test)*100)
Risk_GAweights

Risk_lowRisk = c((best_weights_lowRisk) %*%(sigma_test)*100)
Risk_lowRisk

Risk_highReturn = c((best_weights_highRtrn) %*%(sigma_test)*100)
Risk_highReturn

Risk_equalWeights = c((bweight) %*%(sigma_test)*100)
Risk_equalWeights

Risk_randomWeights = c((random_weights) %*% (sigma_test)*100)
Risk_randomWeights

Risk_MKcustom = c((MK_weights) %*% (sigma_test) * 100)
Risk_MKcustom

All_risk_returns = c(Risk_GAweights, Risk_lowRisk, Risk_highReturn, Risk_equalWeights, Risk_randomWeights, Risk_MKcustom)
#Plot Barplot
barplot(t(All_risk_returns), legend = rownames(All_risk_returns), beside =T, col= c("blue", "pink2", "green", "red", "yellow", "turquoise" ))
                                                                                    
                                                                                 
#This was supposed to be a grouped barplot to illustrate the overall risk per asset /per weight condition.
# This has not wokred... funny that! 
#Nevertheless, we can still see that the green colour which is associated with high return has the overall higher risk
# this is something to be expected. We also see that there are two assets (MU, WEEK.TO) that show higher risk.
#This agrees with the daily return of these two assets that seem to be fluctuating a lot.
candleChart(dailyReturn(WEED.TO)*100)
candleChart(dailyReturn(MU)*100)



#Cumulative portfolio return for every condition. The function Return.portfolio, iterates through the daily return and multiplies with the assigned value for the assset.
#Portfolio return with the best weights from GA model (return: risk , 1:1)
portfolio_return_bestw0 = Return.portfolio(DailyReturn, weights = best_weights0)
portfolio_return_bestw0
#Portfolio return with low risk /low return
portfolio_return_lowrisk = Return.portfolio(DailyReturn, weights = best_weights_lowRisk)
portfolio_return_lowrisk
#Portfolio return with high risk/ high return
portfolio_return_highRtrn = Return.portfolio(DailyReturn , weights = best_weights_highRtrn)
portfolio_return_highRtrn
#Portfolio return balanced weights
portfolio_return_bw <- Return.portfolio(DailyReturn,  weights = bweight )
portfolio_return_bw

#Portfolio return random weights
portfolio_return_randomw <- Return.portfolio(DailyReturn ,  weights  =random_weights )

portfolio_return_Custom<- Return.portfolio(DailyReturn ,  weights  =MK_weights)

#Plot Cumulative Return


plot(cumsum(portfolio_return_bestw0),type="l",lwd=5, ylim = c(-0.5, 1.2))
lines(cumsum(portfolio_return_lowrisk), col="yellow")
lines(cumsum(portfolio_return_bw), col= "blue")
lines(cumsum(portfolio_return_randomw), col="red")
lines(cumsum(portfolio_return_highRtrn), col="green")
lines(cumsum(portfolio_return_Custom), col="pink2")
lines(cumsum(DailyReturn), col="bisque3") # Comment that if you do not want to see the assets
legend("topleft",legend=c("1:1 Risk:Return", "Low Risk", "Equal weights", "Random weights", "Hish Risk", "All assets"),
       col = c("black","yellow","blue","red","green","bisque3"),lty=1)

#From Plotting the cummulative daily return we  see that the original GA weights (black) perform quite stably and reliably.
#AS previously observed the weights for the the original GA and the lowReturn were similar and we
# see this is reflected in the overall portfolio performance( overlapping lines).
# The random and equally balanced weights (red, blue) seem to perfrom less well
#The high return(green) portfolio perfrorms better but has higher fluctuations.
# To put things into perspective I have plotted as brown all the assets
#Because half of the assets have actually slower return or even negative return (one asset)
# we see that using any of these portfolios would actually be a safe bet.
#If we were very "lucky" we could have invested only in the  best performing asset but 
#that actually wouldn't increase out return by a lot compared to the high risk portfolio.














