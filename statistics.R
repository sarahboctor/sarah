
#  Compound Return
ec.z = (CalculateReturns(e.z, method="compound"))[-1]
sgc.z =CalculateReturns(sg.z, method="simple")[-1]
mc.z = CalculateReturns(m.z, method="compound")[-1]
svc.z = CalculateReturns(sv.z, method="compound")[-1]
lvc.z = CalculateReturns(lv.z, method="compound")[-1]
lgc.z = CalculateReturns(lg.z, method="compound")[-1]


chart.TimeSeries(ac.z,lwd=2,main="Returns on Market",legend.loc="bottomright")
chart.CumReturns(ac.z, lwd=2, main="Growth of $1", legend.loc="topleft")

# Exploring the distribution of the returns of the different markets  

# Histogram of Small Value Vs Large Value 
par(mfrow=c(2,1))

hist(sv.m,main="Small Value", col="slateblue1",
     probability=T, ylim=c(0,18))
market.density = density(sv.m[-1])

points(market.density,type="l", col="orange", lwd=2)

hist(lv.m,main="Large Value", col="slateblue1",
     probability=T, ylim=c(0,18))
market.density = density(lv.m[-1])

points(market.density,type="l", col="orange", lwd=2)

# Small Growth vs Large Growth (Small Growth is Nonsensical with a lot of crossover zero)
par(mfrow=c(2,1))

hist(sg.m,main="Small Growth", col="slateblue1",
     probability=T, ylim=c(0,18))
market.density = density(sg.m[-1])
points(market.density,type="l", col="orange", lwd=2)

hist(lg.m,main="Large Growth", col="slateblue1",
     probability=T, ylim=c(0,18))
market.density = density(lg.m[-1])
points(market.density,type="l", col="orange", lwd=2)


# Europe vs. US Market 
par(mfrow=c(2,1))

hist(e.m,main="Europe Equity", col="slateblue1",
     probability=T, ylim=c(0,18))
market.density = density(e.m[-1])
points(market.density,type="l", col="orange", lwd=2)


hist(m.m,main="US Equity", col="slateblue1",
     probability=T, ylim=c(0,18))
market.density = density(m.m[-1])
points(market.density,type="l", col="orange", lwd=2)

#Add Normal with mean 0, and Density Functions to the Histogram
par(mfrow=c(1,1))

chart.Histogram((m.m),methods=c("add.density","add.risk","add.rug","add.qqplot","add.centered"), main="US Equity")
chart.Histogram((lv.m),methods=c("add.density","add.risk","add.rug","add.qqplot","add.centered"),main="Large Value")
chart.Histogram((e.m),methods=c("add.density","add.risk","add.rug","add.qqplot","add.centered"),main="Emerging Markets")
chart.Histogram((sv.m),methods=c("add.density","add.risk","add.rug","add.qqplot","add.centered"),main="Small Value")
chart.Histogram((lg.m),methods=c("add.density","add.risk","add.rug","add.qqplot","add.centered"),main="Large Growth")

# Europe vs US with same Histogram breaks/bins 
m.hist = hist(m.m,plot=F,breaks=10)
class(m.hist)
names(m.hist)      

par(mfrow=c(2,1))
hist(m.m,main="US", col="slateblue1", xlab="returns", breaks=m.hist$breaks)
hist(e.m,main="Europe", col="slateblue1", xlab="returns",breaks=m.hist$breaks)

# With an eybeballing exercise US equity seems to be more negatively skewed than EU equity. Skewness measurment wil confirm:
par(mfrow=c(1,1))

# Descriptive Statistics 
# Comparing Skewness : 
#US 
skewness(m.m)

#Europe
skewness(e.m)

#Skewness of US Distribution is considered substantial. It seems to be the result of many negatively 
# skewed observation, not a few big outliers.



#  Median, IQR, mean, var, sd, skewness and kurtosis 
median(m.m)
quantile(m.m,probs=0.75) - quantile(m.m,probs=0.25)
mean(m.m)
var(m.m)
sd(m.m)
skewness(m.m)
kurtosis(m.m)
summary(m.m)

# n.b: kurtosis function actually computes excess kurtosis
kurtosis(m.m) + 3

#European EQuity 
median(e.m)
quantile(e.m,probs=0.75) - quantile(e.m,probs=0.25)
mean(e.m)
var(e.m)
sd(e.m)
skewness(e.m)
kurtosis(e.m)


# Comparing US Equity and a Gaussian Distribution 
#  create  idd gaussian Distn with same mean and sd as data
set.seed(123)
gwn = rnorm(length(m.m),mean=mean(m.m),sd=sd(m.m))
par(mfrow=c(2,1))
ts.plot(m.m,main="Market Portfolio", lwd=2, col="blue")
abline(h=0)
ts.plot(gwn,main="Gaussian data with same mean and sd as Market Portfolio", lwd=2, col="blue")
abline(h=0)
par(mfrow=c(1,1))

gwn = rnorm(length(m.m))

# compare empirical cdf to standard normal cdf for simulated gaussian data
z1 = scale(gwn)      	# standardize to have mean zero and sd 1
n1 = length(gwn)
F.hat = 1:n1/n1			# empirical cdf
x1 = sort(z1)				# sort from smallest to largest
y1 = pnorm(x1)			# compute standard normal cdf at x

plot(x1,y1,main="Empirical CDF vs. Normal CDF for Gaussian data",
     type="l",lwd=2,xlab="standardized gwn",ylab="CDF")
points(x1,F.hat, type="s", lty=1, lwd=3, col="orange")

legend(x="topleft",legend=c("Normal CDF","Empirical CDF"),
       lty=c(1,1), lwd=2, col=c("black","orange"))

# Quantiles 

quantile(m.m,probs=c(0.01,0.05),na.rm=T)

qnorm(p=c(0.01,0.05), mean=mean(m.m), sd=sd(m.m))

# Compare Gaussian to market quantiles

par(mfrow=c(2,2))  # 4 panel layout: 2 rows and 2 columns
qqnorm(gwn, main="Gaussian data")
qqline(gwn)
qqnorm(m.m, main="US")
qqline(m.m)
qqnorm(e.m, main="Europe")
qqline(e.m)
qqnorm(sv.m, main="Large Growth")
qqline(sv.m)
qqnorm(lg.m, main="Large Growth")
qqline(lg.m)
qqnorm(lv.m, main="Large Growth")
qqline(lv.m)
par(mfrow=c(1,1))

# Boxplot 
boxplot.matrix(a.m,names=c("Euro","SV","LV","LG","US"),outchar=T, col="slateblue1",
               main="Comparison of return distributions",ylab="monthly return")
boxplot(sg.m)

# bivariate scatterplot
plot(m.m,e.m,main="Monthly cc returns on Dev and Em", col="slateblue1")
abline(h=mean(e.m))  # horizontal line at SP500 mean
abline(v=mean(m.m))    # vertical line at MSFT mean

pairs(a.m[-1,-1], col="slateblue1") # most relationships are between both types of value stocks, and between large stocks.The MSCI has poor correlation with all. 
#
var(a.m)
cor(a.m)

#Constant Expected Return Model Calibrated to monthly market return
mu = mean(m.m)
sd.e = sd(m.m)
nobs = 170
set.seed(114)
sim.e = rnorm(nobs, mean=0, sd=sd.e)
sim.ret = mu + sim.e
boxplot(cbind(m.m,sim.ret,gwn), names=c("World","CER","GWN"))

# Random Walk Model UNDERSTAND AND FIX

mu = mean(m.m)
sd.e = sd(m.m)
nobs = 170
set.seed(111)
sim.e = rnorm(nobs, mean=0, sd=sd.e)
sim.p = 1 + mu*seq(nobs) + cumsum(sim.e)
sim.P = exp(sim.p)
par(mfrow=c(2,1),mar=rep(2,4))
ts.plot(sim.p, col="blue",lwd=2,
        ylim=c(-2, 4), ylab="log price")
lines( (1+mu*seq(nobs)), lty="dotted", col="black", lwd=2)
lines(cumsum(sim.e), col="orange", lty="dashed", lwd=2)
abline(h=0)
legend(x="topleft",legend=c("p(t)","E[p(t)]","p(t)-E[p(t)]"),
       lty=c("solid","dotted","dashed"), col=c("blue","black","orange"), 
       lwd=2, cex=c(0.75,0.75,0.75))
ts.plot(sim.P, lwd=2, col="blue", ylab="price")

#compare with the market
par(mfrow=c(1,1))
boxplot(cbind(sim.p,m.m), lwd=2, col=c("green","yellow"), ylim=c(-3,2.5),names=c("random walk","market"),ylab="price")
par(mfrow=c(1,1))
boxplot(m.m)
par(mfrow=c(1,1))

options(digits=4)


# Computing means, variances and covariances 
muhat.vals = colMeans(a.m)
muhat.vals
sigma2hat.vals = colVars(a.m)
sigma2hat.vals
sigmahat.vals = colSds(a.m)
sigmahat.vals
cov.mat = cov(a.m)
cov.mat
cor.mat = cor(a.m)
cor.mat
covhat.vals = cov.mat[lower.tri(cov.mat)]
rhohat.vals = cor.mat[lower.tri(cor.mat)]
names(covhat.vals) <- names(rhohat.vals) <- c("sve","lve","lge","me","lvsv","lgsv","msv","lvlg","lvm","lgm")

# compute estimated standard error for mean
nobs = nrow(a.m)
nobs
se.muhat = sigmahat.vals/sqrt(nobs)
se.muhat # in some cases the se of the mean is even bigger than the mean itself! (in LG)
# compute t-ratios
muhat.vals/se.muhat
# compute exact 95% confidence intervals
t.975 = qt(0.975, df=99)
mu.lower = muhat.vals - t.975*se.muhat
mu.upper = muhat.vals + t.975*se.muhat
mu.width = mu.upper - mu.lower
cbind(mu.lower,mu.upper,mu.width)

se.sigma2hat = sigma2hat.vals/sqrt(nobs/2)
se.sigmahat = sigmahat.vals/sqrt(2*nobs)
se.sigma2hat
se.sigmahat
# compute approx 95% confidence intervals
sigma2.lower = sigma2hat.vals - 2*se.sigma2hat
sigma2.upper = sigma2hat.vals + 2*se.sigma2hat
sigma2.width = sigma2.upper - sigma2.lower
cbind(sigma2.lower,sigma2.upper,sigma2.width)

sigma.lower = sigmahat.vals - 2*se.sigmahat
sigma.upper = sigmahat.vals + 2*se.sigmahat
sigma.width = sigma.upper - sigma.lower
cbind(sigma.lower,sigma.upper,sigma.width)

se.rhohat = (1-rhohat.vals^2)/sqrt(nobs)
se.rhohat
# compute approx 95% confidence intervals
rho.lower = rhohat.vals - 2*se.rhohat
rho.upper = rhohat.vals + 2*se.rhohat
rho.width = rho.upper - rho.lower
cbind(rho.lower,rho.upper,rho.width)


# show risk return tradeoffs

cex.val = 2
plot(sigmahat.vals, muhat.vals,  ylim=c(0, 0.04), xlim=c(0, 0.20), ylab=expression(mu[p]),
     xlab=expression(sigma[p]), pch=16, col="blue", cex=2.5, cex.lab=1.75)     
text(sigmahat.vals, muhat.vals, labels=names(muhat.vals), pos=4, cex = cex.val)

