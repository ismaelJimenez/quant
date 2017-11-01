options(digits=4, width=70)
library(PerformanceAnalytics)
# load portfolio theory functions
# source portfolio functions
source(file="/home/ismael/Documents/Investment/Course/portfolio.r")

cex.val = 2

################################################################
# Input data
################################################################
asset.names = c("Large Stocks", "Small Stocks")
er = c(0.08, 0.15)
sd = c(0.25, 0.5)
names(er) = asset.names
names(sd) = asset.names
corrmat =   matrix(c(1.00,	0.40,
                     0.44,	1.00),
                   nrow=2, ncol=2)
dimnames(corrmat) = list(asset.names, asset.names)
er
corrmat

################################################################
# Calculate covariance matrix
################################################################
covmat <- sd %*% t(sd) * corrmat
covmat

################################################################
# Compute global minimum variance portfolio with no short sales
################################################################
gmin.port.ns = globalMin.portfolio(er, covmat, shorts=FALSE)
print(gmin.port.ns)
summary(gmin.port.ns)
plot(gmin.port.ns, col="blue")

################################################################
# Compute efficient portfolio subject to target sd with no short sales
################################################################
target.sd = sd["Large Stocks"]
e.port.large.ns = efficient.portfolio.risk(er, covmat, target.sd, shorts=FALSE)
summary(e.port.large.ns)
plot(e.port.large.ns, col="blue")
