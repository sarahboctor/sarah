options(digits = 4)
options(width = 75)
options(chmhelp=TRUE)
loadPath = "C:\\Users\\Sarah\\Google Drive\\PROJECT\\"
e.df = read.csv(file=paste(loadPath, "msci eafe.csv", sep=""), 
                    header=TRUE, stringsAsFactors=FALSE)
M.df = read.csv(file=paste(loadPath, "WILL5000INDFC.csv", sep=""), 
                                 header=TRUE, stringsAsFactors=FALSE)
SG.df = read.csv(file=paste(loadPath, "small cap growth 2000.csv", sep=""), 
                    header=TRUE, stringsAsFactors=FALSE)
SV.df = read.csv(file=paste(loadPath, "small cap value 2000.csv", sep=""), 
                    header=TRUE, stringsAsFactors=FALSE)
LG.df = read.csv(file=paste(loadPath, "russell 1000 growth from march 2000.csv", sep=""), 
                    header=TRUE, stringsAsFactors=FALSE)
LV.df = read.csv(file=paste(loadPath, "russell 1000 value from march 2000.csv", sep=""), 
                    header=TRUE, stringsAsFactors=FALSE)
#
lg.df<-na.omit(LG.df)
lv.df<-na.omit(LV.df)
sv.df<-na.omit(SV.df)
sg.df<-na.omit(SG.df)
m.df<-na.omit(M.df)

#to compute returns directly from df 
lg.r<-Return.calculate(lg.df, method="compound")


# to produce zoo objects
rownames(lg.df) = lg.df$Date
rownames(lv.df) = lv.df$Date
rownames(sv.df) = sv.df$Date
rownames(sg.df) = sg.df$Date
rownames(m.df) = m.df$DATE
rownames(e.df) = e.df$Date

m.df = m.df[, "Adj.Close", drop=FALSE]
e.df = e.df[, "Adj.Close", drop=FALSE]
sg.df = sg.df[, "Adj.Close", drop=FALSE]
sv.df = sv.df[, "Adj.Close", drop=FALSE]
lv.df = lv.df[, "Adj.Close", drop=FALSE]
lg.df = lg.df[, "Adj.Close", drop=FALSE]
#
library(PerformanceAnalytics)
library(timeSeries)

dates.e = as.yearmon(rownames(e.df), format="%m/%d/%Y")
dates.sg = as.yearmon(rownames(sg.df), format="%m/%d/%Y")
dates.sv = as.yearmon(rownames(sv.df), format="%m/%d/%Y")
dates.lg = as.yearmon(rownames(lg.df), format="%m/%d/%Y")
dates.lv = as.yearmon(rownames(lv.df), format="%m/%d/%Y")
dates.m = as.yearmon(rownames(m.df), format="%m/%d/%Y")

sg.z = zoo(x=sg.df$Adj.Close, order.by=dates.sg)
sv.z = zoo(x=sv.df$Adj.Close, order.by=dates.sv)
lg.z = zoo(x=lg.df$Adj.Close, order.by=dates.lg)
lv.z = zoo(x=lv.df$Adj.Close, order.by=dates.lv)
m.z = zoo(x=m.df$Adj.Close, order.by=dates.m)
e.z = zoo(x=e.df$Adj.Close, order.by=dates.e)

#
#  Compound Return
ec.z = (CalculateReturns(e.z, method="compound"))[-1]
sgc.z =CalculateReturns(sg.z, method="simple")[-1]
mc.z = CalculateReturns(m.z, method="compound")[-1]
svc.z = CalculateReturns(sv.z, method="compound")[-1]
lvc.z = CalculateReturns(lv.z, method="compound")[-1]
lgc.z = CalculateReturns(lg.z, method="compound")[-1]
ac.z=merge(sgc.z,ec.z,svc.z,lvc.z,lgc.z,mc.z)

head(ac.z)

e.m<-as.matrix(ec.z)
rownames(e.m)<-rownames(e.df)[-1]
sv.m<-as.matrix(svc.z)
rownames(sv.m)<-rownames(sv.df)[-1]
sg.m<-as.matrix(sgc.z)
rownames(sg.m)<-rownames(sg.df)[-1]
lv.m<-as.matrix(lvc.z)
rownames(lv.m)<-rownames(lv.df)[-1]
lg.m<-as.matrix(lgc.z)
rownames(lg.m)<-rownames(lg.df)[-1]
m.m<-m.m<-as.matrix(mc.z)
rownames(m.m)<-rownames(m.df)[-1]
a.m<-as.matrix(ac.z)[,-1]
rownames(a.m)<-rownames(m.df)[-1]

