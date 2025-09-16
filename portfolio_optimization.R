# Final Project - R Script file
#
#
# Data for the project are downloaded automatically from Yahoo! and consist of
# closing price data on 5 Vanguard mutual funds:
#
# 1. S&P 500 index (vfinx)
# 2. European stock index (veurx)
# 3. Emerging markets fund (veiex)
# 4. Short term bond index (vbisx)
# 5. Pacific stock index (vpacx)

# Some of the analysis will use the portfolio functions in the file
# portfolio_noshorts.r on Blackboard. Download this file to your computer. You will
# source() in this file later

options(digits=4, width=70)
# load packages
#install.packages('quantmod')
library('quantmod')
library("PerformanceAnalytics")
library("tseries")
library("zoo")
library("boot")
library("quadprog")

# change this to the appropriate path on your computer
source(file="C:/Users/jmper/OneDrive/Documents/Javier/Financial Economics - UCF/portfolio_noshorts.r")

#
# 1. load data from Yahoo!
#

# get monthly adjusted closing price data on Vanguard mutual fund data from Yahoo
# using the tseries function get.hist.quote. Set sample to June 2010 through
# June 2015. 

asset.names = c("vfinx","veurx","veiex","vbisx","vpacx")
start.date = "2019-07-01"
end.date = "2024-07-01"

vfinx.prices = get.hist.quote(instrument="vfinx", start=start.date,
                              end=end.date, quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")    
veurx.prices = get.hist.quote(instrument="veurx", start=start.date,
                              end=end.date, quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")
veiex.prices = get.hist.quote(instrument="veiex", start=start.date,
                              end=end.date, quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")
vbisx.prices = get.hist.quote(instrument="vbisx", start=start.date,
                              end=end.date, quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")
vpacx.prices = get.hist.quote(instrument="vpacx", start=start.date,
                              end=end.date, quote="AdjClose",
                              provider="yahoo", origin="1970-01-01",
                              compression="m", retclass="zoo")

# change time indices to class yearmon, which is most appropriate for monthly data
index(vfinx.prices) = as.yearmon(index(vfinx.prices))
index(veurx.prices) = as.yearmon(index(veurx.prices))
index(veiex.prices) = as.yearmon(index(veiex.prices))
index(vbisx.prices) = as.yearmon(index(vbisx.prices))
index(vpacx.prices) = as.yearmon(index(vpacx.prices))

projectPrices.z = merge(vfinx.prices,veurx.prices,veiex.prices,vbisx.prices,vpacx.prices)
colnames(projectPrices.z) = asset.names

# create data.frame for downloading
projectPrices.df = coredata(projectPrices.z)
rownames(projectPrices.df) = as.character(index(projectPrices.z))

#
# 2. compute cc returns
#

projectReturns.z = diff(log(projectPrices.z))  

# create data.frame for downloading
projectReturns.df = coredata(projectReturns.z)
rownames(projectReturns.df) = as.character(index(projectReturns.z))


#
# 3. plot data
#
my.panel <- function(...) {
  lines(...)
  abline(h=0)
}

plot(projectPrices.z, col="blue", lwd=2, main="Prices-Time Plot")
plot(projectReturns.z, panel=my.panel, col="blue", lwd=2, main="Continuously Compounded Returns of Funds")

# plot growth of $1 over the five years using PerformanceAnalytics function
# chart.CumReturns

projectReturnsSimple.z = exp(projectReturns.z) - 1
chart.CumReturns(projectReturnsSimple.z, wealth.index=TRUE, legend.loc="topleft", 
                 lwd=2, main="Growth of $1") 

#
# 4. Create matrix of return data and compute pairwise scatterplots
#

ret.mat = coredata(projectReturns.z)
pairs(ret.mat, col="blue")
cov(ret.mat)
cor(ret.mat)

# example of four panel plot-vfinx
par(mfrow=c(2,2))
hist(ret.mat[,"vfinx"],main="VFINX Monthly Returns",
     xlab="vfinx", probability=T, col="slateblue1")
boxplot(ret.mat[,"vfinx"],outchar=T,col="slateblue1")
plot(density(ret.mat[,"vfinx"]), main="smoothed histogram", 
     type="l",xlab="monthly return",
     ylab="density estimate")

par(mfrow=c(1,1))

# example of four panel plot-veurx
par(mfrow=c(2,2))
hist(ret.mat[,"veurx"],main="VEURX Monthly Returns",
     xlab="veurx", probability=T, col="slateblue1")
boxplot(ret.mat[,"veurx"],outchar=T,col="slateblue1")
plot(density(ret.mat[,"veurx"]), main="smoothed density", 
     type="l",xlab="monthly return",
     ylab="density estimate")

par(mfrow=c(1,1))

# example of four panel plot-veiex
par(mfrow=c(2,2))
hist(ret.mat[,"veiex"],main="VEIEX Monthly Returns",
     xlab="veiex", probability=T, col="slateblue1")
boxplot(ret.mat[,"veiex"],outchar=T,col="slateblue1")
plot(density(ret.mat[,"veiex"]), main="smoothed density", 
     type="l",xlab="monthly return",
     ylab="density estimate")

par(mfrow=c(1,1))


# example of four panel plot-vbisx
par(mfrow=c(2,2))
hist(ret.mat[,"vbisx"],main="VBISX Monthly Returns",
     xlab="vbisx", probability=T, col="slateblue1")
boxplot(ret.mat[,"vbisx"],outchar=T,col="slateblue1")
plot(density(ret.mat[,"vbisx"]), main="smoothed density", 
     type="l",xlab="monthly return",
     ylab="density estimate")

par(mfrow=c(1,1))

# example of four panel plot-vpacx
par(mfrow=c(2,2))
hist(ret.mat[,"vpacx"],main="VPACX Monthly Returns",
     xlab="vpacx", probability=T, col="slateblue1")
boxplot(ret.mat[,"vpacx"],outchar=T,col="slateblue1")
plot(density(ret.mat[,"vpacx"]), main="smoothed density", 
     type="l",xlab="monthly return",
     ylab="density estimate")

par(mfrow=c(1,1))

#
# compute descriptive statistics
#

muhat.vals = apply(projectReturns.z, 2, mean)
sd.vals = apply(projectReturns.z, 2, sd)
var.vals = sd.vals^2
cov.mat = var(projectReturns.z)
cor.mat = cov2cor(cov.mat)
skew.vals = skewness(projectReturns.z)
kurt.vals = kurtosis(projectReturns.z)
UniDesStat.df = summary(projectReturns.z)

muhat.vals
sd.vals
cov.mat
cor.mat
var.vals
skew.vals
kurt.vals
UniDesStat.df


# Sharpe's Ratio
rf = 0.0004167
sharpe.vals = (muhat.vals-rf)/sd.vals
sharpe.vals

#
# VaR analysis
#

# function to compute normal VaR for a matrix of returns
Value.at.Risk = function(x,p=0.05,w=100000) {
  x = as.matrix(x)
  q = apply(x, 2, mean) + apply(x, 2, sd)*qnorm(p)
  VaR = (exp(q) - 1)*w
  VaR
}

# compute 5% and 1% normal VaR for all assets
VaR.05.df = Value.at.Risk(ret.mat)
VaR.05.df
VaR.01.df = Value.at.Risk(ret.mat, p=0.01)
VaR.01.df

# function to compute annualized normal VaR for a matrix of returns
Value.at.Risk = function(x,p=0.05,w=100000) {
  x = as.matrix(x)
  q = 12*apply(x, 2, mean) + sqrt(12)*apply(x, 2, sd)*qnorm(p)
  VaR = (exp(q) - 1)*w
  VaR
}

# compute 5% and 1% annualized normal VaR for all assets
VaR.05.annual.df = Value.at.Risk(ret.mat)
VaR.05.annual.df
VaR.01.annual.df = Value.at.Risk(ret.mat, p=0.01)
VaR.01.annual.df


## risk free rate
rf = 0.005/12
rf

#
# Portfolio theory allowing for short sales
#

## compute global minimum variance portfolio
gmin.port <- globalMin.portfolio(muhat.vals, cov.mat)
summary(gmin.port, risk.free=rf)
plot(gmin.port)


## compute tangency portfolio
tan.port <- tangency.portfolio(muhat.vals, cov.mat, rf)
tan.port
summary(tan.port, risk.free=rf)
plot(tan.port)


# plot portfolio weights
par(mfrow=c(1,2))
plot(gmin.port)
plot(tan.port)
par(mfrow=c(1,1))



## compute efficient frontier
ef <- efficient.frontier(muhat.vals, cov.mat, alpha.min=-1, 
                         alpha.max=1.5, nport=20)

## plot efficient frontier
plot(ef, plot.assets=T, col="blue", lwd=2)
points(gmin.port$sd, gmin.port$er, col="orange", lwd=2)
points(tan.port$sd, tan.port$er, col="red", lwd=2)
sr.tan = (tan.port$er - rf)/tan.port$sd
abline(a=rf, b=sr.tan, col="green", lwd=2)


