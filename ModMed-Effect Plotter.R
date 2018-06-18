# Load packages
library(mediation)

#load sample data set
D<-airquality
head(D)
#Remove NAs
D<-D[!is.na(D$Ozone),]

###Run a simple mediation w/ 'mediation' package####
#First run to two OLS models

fit1 <- lm(Wind ~ Solar.R + Temp, data=D)
fit2 <- lm(Ozone ~ Wind*Solar.R+Temp, data=D)
summary(fit1)
summary(fit2)



#create indirect effects table
Table1b<-data.frame(m1=numeric(),e=numeric(),low=numeric(),up=numeric(),stringsAsFactors=FALSE)

#set number of boostrap simulations (set to 100 for demo purposes.. should likely be at least 1000)
sims<-100

#set values of the moderator to caluclate indirect effects for 0 
#in this case every 10 values between 7 and 334
a1<-(seq(7,334, by=10))

#this loops through all of the values of a1 and caluculates indirect effects
#at each level and saves them. Can take a long time depending on number of
#values and also number of simulations
  for (a in a1)
  {
    #a=a+1
    med.out <- mediate(fit1, fit2, treat = "Temp", mediator = "Wind",
                       covariates = list(Solar.R= a), boot = TRUE, sims = sims)
    
    
    
    e<-med.out[1]
    low<-as.numeric(unlist(med.out[3])[1])
    up<-as.numeric(unlist(med.out[3])[2])
    newrow<-list(a,e,low,up)
    Table1b<-rbindlist(list(Table1b,newrow),use.names=F,fill=F)
    
    names(Table1b)<-c("m1","e","low","up")
  }


#fix table formatting
Table1b$e<-as.numeric(sub("^[0]+", "", Table1b$e))
Table1b$e<-unlist(Table1b$e)
Table1b$low<-as.numeric(sub("^[0]+", "", Table1b$low))
Table1b$up<-as.numeric(sub("^[0]+", "", Table1b$up))
Table1b->T1

#This smoothes the CI lines... may need to adjust the spar argument for optimal smoothing
T2 <-predict(smooth.spline(T1$m1, T1$e,spar=1.8))
T2a <-predict(smooth.spline(T1$m1, T1$low,spar=1.8))
T2b <-predict(smooth.spline(T1$m1, T1$up,spar=1.8))


####SAVE PLOT#####

pdf(file="ModMed-Plot.pdf",10,10)

par(oma = c(1,1,1,1))

#likely necceessary to adjust ylim argument depending on you DV
plot(T2, lwd=4,lty=1,type="l",cex.lab=1.4,
     main=NULL,xlab="Solar Radiation",ylab="Point Estimate",axes=FALSE)
axis(side=1,line=NA,cex=.8)
axis(side=2,line=NA,cex=.8)
box(which="plot",col="black")
lines(T2a, lwd=1.7,lty=2,type="l")
lines(T2b, lwd=1.7,lty=2,type="l")
title(main="Indirect Effect of Temperature on Ozone 
Through Wind at Levels of Solar Radiation",cex.main=.9)
abline(0,0,lty=3,lwd=1.7,col="gray60")

dev.off()
