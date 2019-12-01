#Cronbach Alpha (Reliability & Discriminant Validity)

library('foreign')
oi<-read.spss(file="C:/stat/regression2.sav", use.value.label=TRUE, to.data.frame=TRUE)
library('psych')
alpha(oi[, c('Ext1','Ext2','Ext3','Ext4')], na.rm=TRUE)
alpha(oi[, c('Int1','Int2','Int3','Int4')], na.rm=TRUE)
alpha(oi[, c('Per1','Per2','Per3','Per4')], na.rm=TRUE)
alpha(oi[, c('ITC1','ITC2','ITC3','ITC4','ITC5','ITC6')], na.rm=TRUE)

# Factor Analysis (Convergent Validity)
library('GPArotation')
#oi.subset<-oi[complete.cases(oi$Ext1,oi$Ext2,oi$Ext3,oi$Ext4,oi$Int1,oi$Int2,oi$Int3,oi$Int4,
#oi$Per1,oi$Per2,oi$Per3,oi$Per4,oi$ITC1,oi$ITC2,oi$ITC3,oi$ITC4,oi$ITC5,oi$ITC6,
#oi$Byear, oi$EmpN, oi$LnempN),]
oi.subset<-na.omit(oi)
cor(oi.subset)
oif<-princomp(oi.subset[, -(19:21)], cor=TRUE)
summary(oif)
summary(oif)$sdev
sum(summary(oif)$sdev^2)
plot(oif, type='lines', ylim=c(0,5), main = 'Screeplot for PCA(Principal Component Analysis)')
abline(h=1)
oif.no<-principal(oi[, -(19:21)], nfactors = 3, rotate = 'norotate')
oif.vm<-principal(oi[, -(19:21)], nfactors = 3, rotate = 'varimax')
oif.no
oif.vm

#Create Variables
oi.subset$Ext<-(oi.subset$Ext1+oi.subset$Ext2+oi.subset$Ext3+oi.subset$Ext4)/4
oi.subset$Int<-(oi.subset$Int1+oi.subset$Int2+oi.subset$Int3+oi.subset$Int4)/4
oi.subset$Per<-(oi.subset$Per1+oi.subset$Per2+oi.subset$Per3+oi.subset$Per4)/4
oi.subset$ITC<-(oi.subset$ITC1+oi.subset$ITC2+oi.subset$ITC3+oi.subset$ITC4+oi.subset$ITC5+oi.subset$ITC6)/6

#Linearity test

par(mfrow=c(1,3))
plot(oi.subset$Per~oi.subset$Ext, data=oi, col="blue")
plot(oi.subset$Per~oi.subset$Int, data=oi, col="red")
plot(oi.subset$Per~oi.subset$ITC, data=oi, col="orange")

par(mfrow=c(1,3))
plot(jitter(oi.subset$Ext, factor=2), jitter(oi.subset$Per, factor=2), xlab='External-oriented', ylab='Innovation Performance', main = 'External-oriented & Performance')
plot(jitter(oi.subset$Int, factor=2), jitter(oi.subset$Per, factor=2), xlab='Internal-oriented', ylab='Innovation Performance', main = 'Internal-oriented & Performance')
plot(jitter(oi.subset$ITC, factor=2), jitter(oi.subset$Per, factor=2), xlab='IT Capability;', ylab='Innovation Performance', main = 'IT Capability & Performance')

#Regression Analysis
#Step1: Control variables
oi.subset$ByearNew<-2019-oi.subset$Byear
oi.subset1<-lm(oi.subset$Per~oi.subset$Byear+oi.subset$LnempN, data=oi.subset)
summary(oi.subset1)
fitted(oi.subset1)

#Step2: Main variables
oi.subset2<-lm(oi.subset$Per~oi.subset$Byear+oi.subset$LnempN+oi.subset$Ext+oi.subset$Int+oi.subset$ITC, data=oi.subset)
summary(oi.subset2)
anova(oi.subset1, oi.subset2)

#Matching
oi.subset21<-lm(oi.subset$Per~oi.subset$Byear+oi.subset$LnempN+oi.subset$Ext+oi.subset$Int+oi.subset$ITC+oi.subset$Ext*oi.subset$Int, data=oi.subset)
summary(oi.subset21)
anova(oi.subset2, oi.subset21)

#Step3: moderating variables
oi.subset3<-lm(oi.subset$Per~oi.subset$Byear+oi.subset$LnempN+oi.subset$Ext+oi.subset$Int+oi.subset$ITC+oi.subset$Int*oi.subset$ITC, data=oi.subset)
summary(oi.subset3)

anova(oi.subset2, oi.subset3)

#Homoskedascity
library('lmtest')
bptest(oi.subset1)
bptest(oi.subset2)
bptest(oi.subset3)

par(mfrow=c(1,4))
plot(fitted.values(oi.subset3), resid(oi.subset3))
plot(oi.subset$Ext, resid(oi.subset3))
plot(oi.subset$Int, resid(oi.subset3))
plot(oi.subset$ITC, resid(oi.subset3))

#Normality
library('car')
par(mfrow=c(1,1))
qqPlot(oi.subset3)
qqnorm(resid(oi.subset3))
qqline(resid(oi.subset3))

shapiro.test(resid(oi.subset3))

#Independence
round(vif(oi.subset2),2)

#Outlier
cooksd<-cooks.distance(oi.subset3)
cook.threshold<-4/(dim(oi.subset3)[1]-length(oi.subset3$coef))
plot(cooksd)
abline(h=cook.threshold)
outlier<-rep(0, length(cooksd))
outlier[cooksd > cook.threshold]<-1
table(outlier)

#Rsidual Plot
par(mfrow=c(2,2))
plot(oi.subset3)


#Mediating Variables
library('foreign')
oi<-read.spss(file="C:/stat/regression3.sav", use.value.label=TRUE, to.data.frame=TRUE)

#Create Variables
oi$Ext<-(oi$Ext1+oi$Ext2+oi$Ext3+oi$Ext4)/4
oi$Int<-(oi$Int1+oi$Int2+oi$Int3+oi$Int4)/4
oi$Per<-(oi$Per1+oi$Per2+oi$Per3+oi$Per4)/4

#Meadiating Test Step 1
oi.m1<-lm(oi$Fin~oi$Ext+oi$Int, data=oi)
summary(oi.m1)

#Meadiating Test Step 2
oi.m2<-lm(oi$Per~oi$Ext+oi$Int, data=oi)
summary(oi.m2)

#Meadiating Test Step 3
oi.m3<-lm(oi$Fin~oi$Ext+oi$Int+oi$Per, data=oi)
summary(oi.m3)