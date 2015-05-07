

# Data Needed for Optimization :

#last 5 years of return
a.m5=a.m[110:170,]
muhat=colMeans(a.m5[,-5])
a=a.m5[,-5]
sd.vals = colSds(a)
mu.vals=colMeans(a)
# Creating Cov Matrix and Cov for last 5 years 
covmat=cov(a.m)[-5,-5]
cov.mat5 = cov(a.m5)[-5,-5]
covhat.vals5 = cov.mat5[lower.tri(cov.mat5)]
covhatvals=covmat[lower.tri(covmat)]

#To get Expected Return, I use the return from last 5-years, Adjusted towards the return and correlation of the 10-years average
# Adjusting Historic Returns to get to Expected Returns 
r.free=0.0011
er=muhat*(2/3)+muhat.vals[-5]*(1/3)
er=er[-5]

#Expected Cov Matrix
ecov=cov.mat1*(2/3)+covmat*(1/3)
ecovmat=ecov[lower.tri(ecov)]
names(covhat.vals5)=names(covhatvals)=names(ecovmat)<- c("sve","lve","lge","lvsv","lgsv","lvlg")

#run the script in potfolio_noshorts now 

# compute equally weighted portfolio
ew = rep(1,4)/4
equalWeight.portfolio = getPortfolio(er=er,cov.mat=ecov,weights=ew)
class(equalWeight.portfolio)
names(equalWeight.portfolio)
equalWeight.portfolio
summary(equalWeight.portfolio)
plot(equalWeight.portfolio, col="blue")

#
# compute global minimum variance portfolio
gmin.port = globalMin.portfolio(er, ecov)
attributes(gmin.port)
print(gmin.port)
summary(gmin.port, risk.free=r.free)
plot(gmin.port, col="blue",main="Global Minimum Weights")

#
# compute global minimum variance portfolio with no short sales
gmin.port.ns = globalMin.portfolio(er, ecov, shorts=FALSE)
attributes(gmin.port.ns)
print(gmin.port.ns)
summary(gmin.port.ns, risk.free=r.free)
plot(gmin.port.ns, col="blue")




# compute efficient portfolio subject to target return equal that of the Small Value Market
target.return = er["svc.z"]
e.port.sv= efficient.portfolio(er,ecov, target.return)
e.port.sv
summary(e.port.sv, risk.free=r.free)
plot(e.port.sv, col="blue")

#
# compute efficient portfolio subject to target return with no short sales
target.return = er["svc.z"]
e.port.sv.ns = efficient.portfolio(er, ecov, target.return, shorts=FALSE)
e.port.sv.ns
summary(e.port.sv.ns, risk.free=r.free)
plot(e.port.sv.ns, col="blue")

#
# compute tangency portfolio
tan.port <- tangency.portfolio(er, ecov, r.free)
tan.port
summary(tan.port, risk.free=r.free)
plot(tan.port, col="blue")

#
# compute tangency portfolio with no short sales
tan.port.ns <- tangency.portfolio(er, ecov, r.free, shorts=FALSE)
tan.port.ns
summary(tan.port.ns, risk.free=r.free)
plot(tan.port.ns, col="blue")

#
# compute portfolio frontier
ef <- efficient.frontier(er, ecov, alpha.min=-2, 
                         alpha.max=1.5, nport=20)
attributes(ef)
ef

plot(ef, plot.assets=TRUE, col="blue", pch=16)
points(gmin.port$sd, gmin.port$er, col="green", pch=16, cex=2)
points(tan.port$sd, tan.port$er, col="red", pch=16, cex=2)
text(gmin.port$sd, gmin.port$er, labels="GLOBAL MIN", pos=2)
text(tan.port$sd, tan.port$er, labels="TANGENCY", pos=2)    
sr.tan = (tan.port$er - r.free)/tan.port$sd
abline(a=r.free, b=sr.tan, col="green", lwd=2)




#
# compute portfolio frontier with no short sales
ef.ns <- efficient.frontier(er, ecov, alpha.min=0, 
                            alpha.max=1, nport=20, shorts=FALSE)
attributes(ef.ns)
ef.ns
summary(ef.ns)

plot(ef.ns, plot.assets=TRUE, col="blue", pch=16)
points(gmin.port.ns$sd, gmin.port.ns$er, col="green", pch=16, cex=2)
points(tan.port.ns$sd, tan.port.ns$er, col="red", pch=16, cex=2)
text(gmin.port.ns$sd, gmin.port.ns$er, labels="GLOBAL MIN", pos=2)
text(tan.port.ns$sd, tan.port.ns$er, labels="TANGENCY", pos=2)    
sr.tan.ns = (tan.port.ns$er - r.free)/tan.port.ns$sd
abline(a=r.free, b=sr.tan.ns, col="green", lwd=2)




#   BOOTSTRAPPING   
# re-sample means and sd values
options(digits=4, width=70)
library(boot) 
library(PerformanceAnalytics)

n.obs=nrow(a)
n.boot = 999
mu.boot = matrix(0, n.boot, ncol(a))
sd.boot = matrix(0, n.boot, ncol(a))
colnames(mu.boot) = colnames(sd.boot) = colnames(a)

set.seed(144)
for (i in 1:n.boot) {
    boot.idx = sample(n.obs, replace=TRUE)
    ret.boot = a[boot.idx, ] 
    mu.boot[i, ] = colMeans(ret.boot)
    sd.boot[i, ] = apply(ret.boot, 2, sd) 
}
# Large Growth Bootstrapped Returns 
plot(sd.boot[, "lgc.z"], mu.boot[, "lgc.z"], col="black", pch=1, cex=0.5,
     ylim=c(-0.03, 0.07), xlim=c(0, 0.20),
     ylab=expression(mu[p]),
     xlab=expression(sigma[p]), cex.lab=2)
points(sd.vals["lgc.z"], mu.vals["lgc.z"], pch=16, col="blue", cex=1)
text(sd.vals["lgc.z"], mu.vals["lgc.z"], labels="Large Growth", pos=4, col="blue", cex = 1)
# Small Value Bootstrapped Returns 
points(sd.boot[, "svc.z"], mu.boot[, "svc.z"], col="yellow", pch=2, cex=0.5)
points(sd.vals["svc.z"], mu.vals["svc.z"], pch=16, col="blue", cex=1)
text(sd.vals["svc.z"], mu.vals["svc.z"], labels="Small Value", col="blue", pos=4, cex = 1)
# Europe Bootstrapped Returns 
points(sd.boot[, "ec.z"], mu.boot[, "ec.z"], col="green", pch=3, cex=0.5)
points(sd.vals["ec.z"], mu.vals["ec.z"], pch=16, col="blue", cex=1)
text(sd.vals["ec.z"], mu.vals["ec.z"], labels="Europe", col="blue", pos=4, cex = 1)
# Large Value Bootstrapped Returns
points(sd.boot[, "lvc.z"], mu.boot[, "lvc.z"], col="red", cex=0.5, pch=5)
points(sd.vals["lvc.z"], mu.vals["lvc.z"], pch=16, col="blue", cex=1)
text(sd.vals["lvc.z"], mu.vals["lvc.z"], labels="Large Value",  col="blue",pos=4, cex = 1)

# show global minimum variance portfolio
gmin.port = globalMin.portfolio(mu.vals, ecov)
gmin.port

# show risk return tradeoffs with global min
plot(sd.vals, mu.vals,  ylim=c(0, 0.04), xlim=c(0, 0.20), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=1, cex.lab=1.75)     
text(sd.vals, mu.vals, labels=names(mu.vals), pos=4, cex = 1)
points(gmin.port$sd, gmin.port$er, pch=16, cex=1, col="green")
text(gmin.port$sd, gmin.port$er, labels="Global Min", pos=2, cex = 0.75)

# bootstrap global min portfolio
mu.gmin.boot = matrix(0, n.boot, 1)
sd.gmin.boot = matrix(0, n.boot, 1)
w.gmin.boot = matrix(0, n.boot, 4)
colnames(mu.gmin.boot) = colnames(sd.gmin.boot) = "global.min"
colnames(w.gmin.boot) = names(mu.vals)

set.seed(144)
for (i in 1:n.boot) {
    boot.idx = sample(n.obs, replace=TRUE)
    ret.boot = a[boot.idx, ] 
    mu.boot = colMeans(ret.boot)
    cov.boot = cov(ret.boot) 
    gmin.boot = globalMin.portfolio(mu.boot, cov.boot)
    mu.gmin.boot[i, ] = gmin.boot$er
    sd.gmin.boot[i, ] = gmin.boot$sd
    w.gmin.boot[i, ] = gmin.boot$weights
}

plot(sd.vals, mu.vals,  ylim=c(-0.01, 0.04), xlim=c(0, 0.20), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=1, cex.lab=1.75)     
abline(h=0, v=0)
text(sd.vals, mu.vals, labels=names(mu.vals), pos=4, cex =1)
points(gmin.port$sd, gmin.port$er, pch=16, cex=1, col="black")
text(gmin.port$sd, gmin.port$er, labels="Global Min", pos=2, cex = 0.75)

# plot bootstrapped global min
points(sd.gmin.boot, mu.gmin.boot, col="green", pch=16, cex = 0.2)

# look at bootstrap distribution
par(mfrow=c(2,2))
hist(mu.gmin.boot, col="slateblue1")
qqnorm(mu.gmin.boot, col="slateblue1", pch=16, cex = 0.75)
qqline(mu.gmin.boot)

hist(sd.gmin.boot, col="slateblue1")
qqnorm(sd.gmin.boot, col="slateblue1", pch=16, cex = 0.75)
qqline(sd.gmin.boot)
par(mfrow=c(1,1))

bias.mu.gmin = mean(mu.gmin.boot) - gmin.port$er
se.mu.gmin = sd(mu.gmin.boot)
ci.mu.gmin.95 = c(gmin.port$er-2*se.mu.gmin, 
                  gmin.port$er+2*se.mu.gmin)
bias.mu.gmin
se.mu.gmin
ci.mu.gmin.95

bias.sd.gmin = mean(sd.gmin.boot) - gmin.port$sd
se.sd.gmin = sd(sd.gmin.boot)
ci.sd.gmin.95 = c(gmin.port$sd-2*se.sd.gmin, 
                  gmin.port$sd+2*se.sd.gmin)
bias.sd.gmin
se.sd.gmin
ci.sd.gmin.95

# min var portfolio weights
par(mfrow=c(2,2))
hist(w.gmin.boot[, "lvc.z"], main="Large Value", xlab="Weight", col="slateblue1")
hist(w.gmin.boot[, "svc.z"], main="Small Value", xlab="Weight", col="slateblue1")
hist(w.gmin.boot[, "lgc.z"], main="Large Growth", xlab="Weight", col="slateblue1")
hist(w.gmin.boot[, "ec.z"], main="Europe", xlab="Weight", col="slateblue1")
par(mfrow=c(1,1))

# compute bias, mse and 95% CI for weights
bias.w.gmin = colMeans(w.gmin.boot) - gmin.port$weights
se.w.gmin = apply(w.gmin.boot, 2, sd)
ci.w.gmin.95 = rbind(gmin.port$weights-2*se.w.gmin, 
                     gmin.port$weights+2*se.w.gmin)
rownames(ci.w.gmin.95) = c("lower", "upper")
bias.w.gmin
se.w.gmin
ci.w.gmin.95


# sort bootstrap values by mean
tmp.w.boot = w.gmin.boot[1:20, ]
tmp.mu.boot = mu.gmin.boot[1:20, ]
tmp.sd.boot = sd.gmin.boot[1:20, ]
sort.idx = order(tmp.mu.boot)

# look at weights in stacked bar charts
chart.StackedBar(tmp.w.boot[sort.idx,], 
                 xaxis.labels=round(tmp.mu.boot[sort.idx],digits=3), 
                 xlab="Portfolio SD", ylab="Weights", cex.lab=1,
                 cex.axis=0.75)

# look at correlation between min var weights
cor(w.gmin.boot)
pairs(w.gmin.boot)

qqnorm(w.gmin.boot[, "svc.z"], col="slateblue1", pch=16, cex = 0.75)
qqline(w.gmin.boot[, "svc.z"])


#
# bootstrap efficient frontier
#

# show efficient frontier from sample estimates
ef = efficient.frontier(mu.vals,ecov)

# plot efficient portfolios
plot(ef$sd, ef$er, type="b", ylim=c(0, 0.04), xlim=c(0, 0.17), 
     pch=16, col="blue", cex = 0.75, ylab=expression(mu[p]), xlab=expression(sigma[p]))

points(sd.vals, mu.vals, pch=16, cex = 1, col="black")
points(gmin.port$sd, gmin.port$er, pch=16, cex = 1, col="green")
text(sd.vals, mu.vals, labels=names(mu.vals), pos=4, cex = 0.75)
text(gmin.port$sd, gmin.port$er, labels="Global min", pos=4, cex = 0.75)

# bootstrap efficient frontier

ef.list = list()
set.seed(123)
for (i in 1:n.boot) {
    boot.idx = sample(n.obs, replace=TRUE)
    ret.boot = a[boot.idx, ] 
    mu.boot = colMeans(ret.boot)
    cov.boot = cov(ret.boot) 
    ef.boot = efficient.frontier(mu.boot, cov.boot)
    ef.list[[i]] = ef.boot
}

# plot sample efficient frontier with first 20 bootstrap 
# efficient frontiers
# plot efficient portfolios
plot(ef$sd, ef$er, type="b", ylim=c(-0.01, 0.03), xlim=c(0.02, 0.07), 
     pch=16, col="blue", cex=0.5, ylab=expression(mu[p]), xlab=expression(sigma[p]))

points(sd.vals, mu.vals, pch=16, cex=0.5, col="black")
text(sd.vals, mu.vals, labels=names(mu.vals), pos=4, cex=0.5)

# plot bootstrap efficient frontiers
for (i in 1:30) {
    points(ef.list[[i]]$sd, ef.list[[i]]$er, type="b",
           pch=16, col=i, cex=0.5)
}

