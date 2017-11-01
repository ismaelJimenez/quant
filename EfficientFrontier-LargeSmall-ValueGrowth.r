options(digits=4, width=70)
library(PerformanceAnalytics)
# load portfolio theory functions
# source portfolio functions
source(file="https://raw.githubusercontent.com/ismaelJimenez/quant/master/portfolio.r")

cex.val = 2

################################################################
# Input data
################################################################
asset.names = c("Large Stocks", "Small Stocks", "Value Stocks", "Growth Stocks")
er = c(0.112, 0.19, 0.175, 0.111)
sd = c(0.192, 0.394, 0.334, 0.225)
names(er) = asset.names
names(sd) = asset.names
corrmat =   matrix(c(1.00,	0.69,	0.80,	0.94,
                     0.69,	1.00,	0.84,	0.65,
                     0.80,	0.84,	1.00,	0.70,
                     0.94,	0.65,	0.70,	1.00),
                   nrow=4, ncol=4)
r.free = 0.035
dimnames(corrmat) = list(asset.names, asset.names)
er
corrmat
r.free

################################################################
# Calculate covariance matrix
################################################################
covmat <- sd %*% t(sd) * corrmat
covmat

################################################################
# Compute equally weighted portfolio
################################################################
ew = rep(1,4)/4
equalWeight.portfolio = getPortfolio(er=er,cov.mat=covmat,weights=ew)
print(equalWeight.portfolio)
summary(equalWeight.portfolio)
plot(equalWeight.portfolio, col="blue")

################################################################
# Find the portfolio that maximizes the return subject to having 
# the same risk as the 25, 25, 25, 25 portfolio with no short sales
################################################################
ef.ns <- efficient.portfolio.risk(er, covmat, equalWeight.portfolio$sd, shorts=FALSE)
print(ef.ns)
summary(ef.ns)
plot(ef.ns, col="blue")

################################################################
# Find the portfolio that maximizes the return subject to having 
# the same risk as the 25, 25, 25, 25 portfolio with short sales
################################################################
ef <- efficient.portfolio.risk(er, covmat, equalWeight.portfolio$sd, shorts=TRUE)
print(ef)
summary(ef)
plot(ef, col="blue")

################################################################
# Compute global minimum variance portfolio with no short sales
################################################################
gmin.port.ns = globalMin.portfolio(er, covmat, shorts=FALSE)
print(gmin.port.ns)
summary(gmin.port.ns, risk.free=r.free)
plot(gmin.port.ns, col="blue")

################################################################
# compute tangency portfolio with no short sales
################################################################
tan.port <- tangency.portfolio(er, covmat, r.free, shorts=FALSE)
tan.port
summary(tan.port, risk.free=r.free)
plot(tan.port, col="blue")
