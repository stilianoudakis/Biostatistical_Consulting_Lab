#######################################################################
# Study ID  		     	  : BCL-2016-R53
# Investigator Name		  : Dr. Kahn
# Purpose			          : To determine if there is a relationship between PEG tube placement and the development of chronic dysphagia 
#
# BIOS Programmer		    : Spiro Stilianoudakis
# Project Start Date		: JAN2017
#	
# Modification Programmer 	: 
# Modification Purpose		: 
#
# Modification Programmer 	: 
# Modification Purpose		: 
#
# Line Numbers:
# Packages                      : 24-48
# Data Management               : 51-128
# Analysis                      : 
#		Question1:   135-163
#		Question2:   175-207
#		Question3:   219-313
#		Question4:   317-340
#		Question5:   351-381
#		Question6:   406-433
#		Question7:   454-500
#		         
# Tables & Figures                    :
#		Question1:   165-167
#		Question2:   209-212
#		Question3:   238-242;269-273;309-313
#		Question4:   342-345
#		Question5:   387-397
#		Question6:   437-446
#		Question7:   503-507
# Demographic Information:   532-697
#######################################################################;


# For logistic reg
library(aod)
library(ggplot2)
library(Rcpp)

#for multinomial logistic reg
library(foreign)
library(nnet)
library(ggplot2)
library(reshape2)

#for ordinal logistic reg
library(foreign)
library(ggplot2)
library(MASS)
library(Hmisc)
library(reshape2)

library(epiR)
library(tables)
library(vcd)

library(scales)
library(gridBase)
library(RColorBrewer)


#importing the data
peg <- read.csv("PEG_data.csv",header=T,na.strings="")
attach(peg)
head(peg)
str(peg)

########################################################################

#renaming variables

dysphagia = Dysphagia..chronic. 
tubeplace = Timing.of.tube.placement 
surgery = Surgical.Procedure 
tumor = Tumor.Site.
tstage = T.Stage  
nstage = N.stage
chemo = Concurrent.Chemotherapy 
wtstart = Weight.at.start.of.XRT
wtend =  Weight.at.end.of.XRT 
hemostart = Hemoglobin.Pre.RT 
hemoend = Hemoglobin.Post.RT
smokers = Pack.Year.Hx
etoh = ETOH 

#############################################################################

#Function to produce frequencies etc 
procfrq <- 
      function(x, digits=4) {
            total <- sum(x)
            rowsum <- apply(x,1,sum)
            colsum <- apply(x,2,sum)
            prop <- x/total
            rowprop <- sweep(x,1,rowsum,"/")
            colprop <- sweep(x,2,colsum,"/")
            expected <- (matrix(rowsum) %*% t(matrix(colsum)))/total
            dimnames(expected) <- dimnames(x)
            resid <- (x-expected)/sqrt(expected)
            adj.resid <- resid/(sqrt((1-matrix(rowsum)/total) %*% t(1-matrix(colsum)/total)))
            df <- prod(dim(x)-1)
            X2 <- sum(resid^2)
            attr(X2,"P-value") <- 1-pchisq(X2,df)
            #to protect from zero frequencies
            tmp <- x*log(x/expected)
            tmp[x==0] <- 0
            G2 <- 2*sum(tmp)
            attr(G2,"P-value") <- 1-pchisq(G2,df)
            list(sample.size=total,
                 row.totals=rowsum,
                 col.totals=colsum,
		     table=x,
                 overall.proportions=prop,
                 row.proportions=rowprop,
                 col.proportions=colprop,
                 expected.freqs=expected,
                 residuals=resid,
                 adjusted.residuals=adj.resid,
                 chi.square=X2,
                 likelihood.ratio.stat=G2,
		     fishersexacttest=fisher.test(x),
                 df=df)
      }


odds.ratio <-
	function(x, pad.zeros=FALSE, conf.level=0.95) {
		if (pad.zeros) {
			if (any(x==0)) x <- x + 0.5
		}
		theta <- x[1,1] * x[2,2] / ( x[2,1] * x[1,2] )
		ASE <- sqrt(sum(1/x))
		CI <- exp(log(theta)
			+ c(-1,1) * qnorm(0.5*(1+conf.level)) *ASE )
		list(estimator=theta,
			ASE=ASE,
			conf.interval=CI,
			conf.level=conf.level)
		}



#############################################################################

#Question 1

dysphagia <- Dysphagia..chronic.[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))]
tube_place <- Timing.of.tube.placement[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))]

dysphagia1 <- as.character(dysphagia)
dysphagia1[which(dysphagia1=="No change from baseline")] = "Not Chronic"
dysphagia1[which(dysphagia1=="Severely altered eating/swallowing; tubefeeding or TPN or hospitalization required")] <- "Chronic"
dysphagia1[which(dysphagia1=="Symptomatic, able to eat regular food")] = "Not Chronic"
dysphagia1[which(dysphagia1=="Symptomatic, altered eating/swallowing")] = "Not Chronic"
dysphagia1[which(dysphagia1=="Life threating consequences")] = "Chronic"
dysphagia1 <- factor(dysphagia1)
dysphagia1

tube_place1 <- as.character(tube_place)
tube_place1[which(tube_place1=="During XRT")] <- "Not Prior"
tube_place1[which(tube_place1=="After XRT")] <- "Not Prior"
tube_place1[which(tube_place1=="Prior to XRT")] <- "Prior"
tube_place1 <- factor(tube_place1, levels=c("Prior","Not Prior"))


q1table <- table(tube_place1,dysphagia1)
q1matrix <- matrix(q1table,byrow=F,ncol=2,dimnames=list("Placement of Tube"=c("Prior","Not Prior"),Dysphagia=c("Chronic","Not Chronic")))

procfrq(q1matrix)		#not significant
fisher.test(q1matrix,alternative="less")
odds.ratio(q1matrix)  	
epi.2by2(q1matrix,method="cohort.count",conf.level=0.95)	#RR 0.42 (0.16, 1.06)	
										#odds ratio .36 (0.12, 1.09)

q1dframe <- cbind.data.frame(tube_place1,dysphagia1)
ggplot(q1dframe) + aes(x=tube_place1,fill=dysphagia1) + geom_bar(position='dodge')
ggplot(q1dframe) + aes(x=tube_place1,fill=dysphagia1) + geom_bar(position='fill')	#proportions

levels(dysphagia1) <- c(1,0)
logit1 <- glm(dysphagia1 ~ tube_place1,data=q1dframe,family=binomial(link="logit"))
summary(logit1)

###########################################################################

#Question 2

surgery <- Surgical.Procedure  #[-c(227,234,245)]
surgery1 <- surgery[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))]
surgery1 <- ifelse(is.na(surgery1)=="TRUE","No Surgery","Surgery")
surgery1 <- factor(surgery1,levels=c("Surgery","No Surgery"))
dysphagia2 <- Dysphagia..chronic.[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))]

dysphagia2 <- as.character(dysphagia2)
dysphagia2[which(dysphagia2=="No change from baseline")] = "Not Chronic"
dysphagia2[which(dysphagia2=="Severely altered eating/swallowing; tubefeeding or TPN or hospitalization required")] <- "Chronic"
dysphagia2[which(dysphagia2=="Symptomatic, able to eat regular food")] = "Not Chronic"
dysphagia2[which(dysphagia2=="Symptomatic, altered eating/swallowing")] = "Not Chronic"
dysphagia2[which(dysphagia2=="Life threating consequences")] = "Chronic"
dysphagia2 <- factor(dysphagia2)
dysphagia2

q2table <- table(surgery1,dysphagia2)
q2matrix <- matrix(q2table,byrow=F,ncol=2,dimnames=list(Surgery=c("Yes","No"),DysphagiaType=c("Chronic","Not Chronic")))

procfrq(q2matrix)		#significant
fisher.test(q2matrix,alternative="greater")
epi.2by2(q2matrix,method="cohort.count",conf.level=0.95)	#RR 2.88 (1.10, 7.53)
										#odds ratio 3.50 (1.05, 11.65)

#Check for confounding of the Surgery variable

q2table2 <- table(tube_place1,dysphagia2,surgery1,dnn=c("Tube Placement","Dyshpagia","Surgery"))
epi.2by2(q2table2,method="cohort.count",conf.level=.95,homogeneity="breslow.day")
mantelhaen.test(q2table2,alternative="less")
epi.2by2(q2table2[,,1])	#significant RR  0.13 (0.02, 0.98)
epi.2by2(q2table2[,,2])	#not significant RR 0.79 (0.22, 2.85)
#surgery is possible effect modifier

q2dframe <- cbind.data.frame(surgery1,dysphagia2)
q2dframe2 <- cbind.data.frame(surgery1,dysphagia2,tube_place1)
#ggplot(q2dframe) + aes(x=surgery1,fill=dysphagia2) + geom_bar(position='dodge')
ggplot(q2dframe) + aes(x=surgery1,fill=dysphagia2) + geom_bar(position='fill')

logit2 <- glm(dysphagia2~tube_place1+surgery1+tube_place1*surgery1,data=q2dframe2,family=binomial(link="logit"))
summary(logit2)

############################################################################

#Question 3

#dysphagia vs tumor site

tumor <- Tumor.Site.[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))]
tumor1 <- factor(tumor)
q3atable <- table(tumor1,dysphagia2)
q3amatrix <- matrix(q3atable,byrow=F,ncol=2,dimnames=list(TumorSite=c("Hypopharynx","Larynx","Nasopharynx","Oral Cavity","Oropharynx"),DysphagiaType=c("Chronic","Not Chronic")))

procfrq(q3amatrix)	#not significant

epi.2by2(table(tube_place1,dysphagia2,tumor1)[,,1])  #OR .158
epi.2by2(table(tube_place1,dysphagia2,tumor1)[,,2])  #OR .75
epi.2by2(table(tube_place1,dysphagia2,tumor1)[,,3])  #OR 1
epi.2by2(table(tube_place1,dysphagia2,tumor1)[,,4])  #OR .039
epi.2by2(table(tube_place1,dysphagia2,tumor1)[,,5])  #OR 1.20



Dysphagia = dysphagia2
q3adframe <- cbind.data.frame(tumor1,Dysphagia)
#ggplot(q3adframe) + aes(x=tumor1,fill=dysphagia2) + geom_bar(position= 'dodge') 
ggplot(q3adframe) + aes(x=tumor1,fill=Dysphagia) + geom_bar(position= 'fill') + xlab("Tumor Site") + ylab("Proportion")
rm(Dysphagia)

#dysphagia vs tstage

tstage <- T.Stage[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))]
tstage1 <- tstage[-38]
dysphagia3 <- dysphagia2[-38]
tstage1 <- factor(tstage1) 
q3btable <- table(tstage1,dysphagia3)
q3bmatrix <- matrix(q3btable,byrow=F,ncol=2,dimnames=list(TStage=c("T1","T2","T3","T4"),DysphagiaType=c("Chronic","Not Chronic")))

procfrq(q3bmatrix)

T2vT1 <- matrix(c(2,37,1,10),byrow=T,ncol=2,dimnames=list(TStage=c("T2","T1"),DysphagiaType=c("Chronic","Not Chronic")))
epi.2by2(T2vT1)

T3vT1 <- matrix(c(5,37,1,10),byrow=T,ncol=2,dimnames=list(TStage=c("T3","T1"),DysphagiaType=c("Chronic","Not Chronic")))
epi.2by2(T3vT1)

T4vT1 <- matrix(c(7,35,1,10),byrow=T,ncol=2,dimnames=list(TStage=c("T4","T1"),DysphagiaType=c("Chronic","Not Chronic")))
epi.2by2(T4vT1)

epi.2by2(table(tube_place1[-38],dysphagia3,tstage1)[,,1]) #OR .882
epi.2by2(table(tube_place1[-38],dysphagia3,tstage1)[,,2]) #OR .68
epi.2by2(table(tube_place1[-38],dysphagia3,tstage1)[,,3]) #OR .18
epi.2by2(table(tube_place1[-38],dysphagia3,tstage1)[,,4])	#significant 0.17 (0.08, 0.94)

Dysphagia=dysphagia3
q3bdframe <- cbind.data.frame(tstage1,Dysphagia)
#ggplot(q3bdframe) + aes(x=tstage1,fill=dysphagia3) + geom_bar(position='dodge')
ggplot(q3bdframe) + aes(x=tstage1,fill=Dysphagia) + geom_bar(position='fill') +xlab("TStage") + ylab("Proportion")
rm(Dysphagia)


#dysphagia vs nstage

nstage <- N.stage[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))]
nstage1 <- as.character(nstage)
nstage1[which(nstage1=="N2a (3-6 cm single node)")] <- "N2"
nstage1[which(nstage1=="N2b (multiple ipsilateral <6cm)")] <- "N2"
nstage1[which(nstage1=="N2c (bilateral <6cm)")] <- "N2"
nstage1[which(nstage1=="N1 (0-3 cm)")] <- "N1"
nstage1[which(nstage1=="N3 (>6cm)")] <- "N3"
which(nstage1=="Not reported")
nstage1 <- nstage1[-38]
nstage1 <- factor(nstage1)
dysphagia3c <- dysphagia2[-38]

q3ctable <- table(nstage1,dysphagia3c)
q3cmatrix <- matrix(q3ctable,byrow=F,ncol=2,dimnames=list(NStage=c("N0","N1","N2","N3"),DysphagiaType=c("Chronic","Not Chronic")))

procfrq(q3cmatrix)

N1vN0 <- matrix(c(2,30,2,25),byrow=T,ncol=2,dimnames=list(NStage=c("N1","N0"),DysphagiaType=c("Chronic","Not Chronic"))) 
epi.2by2(N1vN0)	#OR .83  RR .84

N2vN0 <- matrix(c(11,54,2,25),byrow=T,ncol=2,dimnames=list(NStage=c("N2","N0"),DysphagiaType=c("Chronic","Not Chronic"))) 
epi.2by2(N2vN0)	#OR 2.55  RR 2.28

N3vN0 <- matrix(c(0,10,2,25),byrow=T,ncol=2,dimnames=list(NStage=c("N3","N0"),DysphagiaType=c("Chronic","Not Chronic"))) 
epi.2by2(N3vN0)	#OR .49  RR .51

epi.2by2(table(tube_place1[-38],dysphagia3c,nstage1)[,,1]) #OR .097
epi.2by2(table(tube_place1[-38],dysphagia3c,nstage1)[,,2]) #OR .67
epi.2by2(table(tube_place1[-38],dysphagia3c,nstage1)[,,3]) #OR .26
epi.2by2(table(tube_place1[-38],dysphagia3c,nstage1)[,,4]) #OR .294

Dysphagia = dysphagia3c
q3cdframe <- cbind.data.frame(nstage1,Dysphagia)
#ggplot(q3cdframe) + aes(x=nstage1,fill=Dysphagia) + geom_bar(position='dodge')
ggplot(q3cdframe) + aes(x=nstage1,fill=Dysphagia) + geom_bar(position='fill') + xlab("NStage")+ylab("Proportion")
rm(Dysphagia)

#############################################################################

#Question 4 dysphagia vs concurrent chemotherapy

chemo <- Concurrent.Chemotherapy[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))]
chemo1 <- ifelse(is.na(chemo)=="TRUE","No Chemo","Chemo")
chemo1 <- factor(chemo1)

q4table <- table(chemo1,dysphagia2)
q4matrix <- matrix(q4table,byrow=F,ncol=2,dimnames=list("Concurrent Chemo"=c("Yes","No"),DysphagiaType=c("Chronic","Not Chronic")))

procfrq(q4matrix)
fisher.test(q4matrix,alternative="greater")	#significant .04
epi.2by2(q4matrix,method="cohort.count",conf.level=0.95)	#RR 5.48 (0.75, 40.27)
										#OR 6.24 (0.79, 49.23)

####Checking for confounding of Concurrent Chemotherapy

q4table2 <- table(tube_place1,dysphagia2,chemo1)
epi.2by2(q4table2,method="cohort.count",conf.level=.95,homogeneity="breslow.day")
mantelhaen.test(q4table2,alternative="less")	
#significant common odds ratio 0.3766594
#this is not sign far off from crude: 0.36 (0.12, 1.09)
#so chemo is not a confounder
epi.2by2(q4table2[,,1])
epi.2by2(q4table2[,,2])

q4dframe <- cbind.data.frame(chemo1,dysphagia2)
q4dframe2 <- cbind.data.frame(chemo1,dysphagia2,tube_place1)
ggplot(q4dframe) + aes(x=chemo1,fill=dysphagia2) + geom_bar(position='dodge')
ggplot(q4dframe) + aes(x=chemo1,fill=dysphagia2) + geom_bar(position='fill')

logit4 <- glm(dysphagia2 ~ tube_place1 + chemo1+ tube_place1*chemo1 ,data=q4dframe2,family=binomial(link="logit"))
summary(logit4)
####################################################################

#Question 5 tube placement vs wieght loss

summary(Weight.at.start.of.XRT)
summary(Weight.at.end.of.XRT)
wtstart <- Weight.at.start.of.XRT[-c(which(is.na(Dysphagia..chronic.) | is.na(Timing.of.tube.placement)))]
wtend <- Weight.at.end.of.XRT[-c(which(is.na(Dysphagia..chronic.) | is.na(Timing.of.tube.placement)))]
wtstart2 <- wtstart[-c(110,117,125,133)]
wtend2 <- wtend[-c(110,117,125,133)]
tube_place5 <- tube_place1[-c(110,117,125,133)]
dysphagia5 <- dysphagia2[-c(110,117,125,133)]
wtdiff <- wtstart2 - wtend2

t.test(wtstart2,wtend2,paired=T)	#significant

t.test(wtdiff)

#which(tube_place5=="Prior")
wtstartprior <- wtstart2[c(which(tube_place5=="Prior"))]
wtendprior <- wtend2[c(which(tube_place5=="Prior"))]
t.test(wtstartprior,wtendprior,paired=TRUE)	#significant

#which(tube_place5=="Not Prior")
wtstartafter <- wtstart2[c(which(tube_place5=="Not Prior"))]
wtendafter <- wtend2[c(which(tube_place5=="Not Prior"))]
t.test(wtstartafter,wtendafter,paired=TRUE)	#significant

wtdiffprior <- wtstartprior-wtendprior
wtdiffafter <- wtstartafter-wtendafter
t.test(wtdiffprior,wtdiffafter)	#not significant
t.test(wtdiffprior)
t.test(wtdiffafter)

Placement=tube_place5

data5 <- cbind.data.frame(dysphagia5,Placement,wtdiff)

qplot(wtdiff,geom='blank',fill=Placement,data=data5) +
	geom_line(aes(y=..density..,colour=tube_place5),stat='density') +
  scale_colour_manual(name=c('Density'),values=c('red','blue'))+
	geom_histogram(aes(y=..density..),alpha=.7,position='identity')+
      geom_segment(aes(x = 9.146868, y = 0, xend = 14.502627, yend = 0),size=4,colour='red') +
  #theme(legend.position=c(.85,.85)) +
      geom_segment(aes(x = 5.107884, y = 0, xend = 12.823366, yend = 0),size=2,colour='blue',alpha=.5) +
  #theme(legend.position=c(.85,.85)) +
  labs(title = "Difference in Weight for Patients With PEG Tubes") +
  xlab("Difference in Weight") +
  ylab("Density")


logit5 <- glm(dysphagia5 ~ tube_place5 + wtdiff + wtdiff*tube_place5, data=data5, family = "binomial")
summary(logit5)


#############################################################################

#Question 6 tube placement vs difference in hemoglobin level

summary(Hemoglobin.Pre.RT)
summary(Hemoglobin.Post.RT)
hemostart <- Hemoglobin.Pre.RT[-c(which(is.na(Dysphagia..chronic.) | is.na(Timing.of.tube.placement)))]
hemoend <- Hemoglobin.Post.RT[-c(which(is.na(Dysphagia..chronic.) | is.na(Timing.of.tube.placement)))]
hemostart2 <- hemostart[-c(which(is.na(hemostart)|is.na(hemoend)))]
hemoend2 <- hemoend[-c(which(is.na(hemostart)|is.na(hemoend)))]
tube_place6 <- tube_place1[-c(which(is.na(hemostart)|is.na(hemoend)))]
dysphagia6 <- dysphagia2[-c(which(is.na(hemostart)|is.na(hemoend)))]
hemodiff <- hemostart2 - hemoend2
#which(tube_place5=="Prior")
hemostartprior <- hemostart2[c(which(tube_place6=="Prior"))]
hemoendprior <- hemoend2[c(which(tube_place6=="Prior"))]
t.test(hemostartprior,hemoendprior,paired=TRUE)		#significant

#which(tube_place5=="Not Prior")
hemostartafter <- hemostart2[c(which(tube_place6=="Not Prior"))]
hemoendafter <- hemoend2[c(which(tube_place6=="Not Prior"))]
t.test(hemostartafter,hemoendafter,paired=TRUE)		#significant

Placement=tube_place6    #remove "Placement" from tube_place5 first"

hemodiffprior <- hemostartprior-hemoendprior
hemodiffafter <- hemostartafter-hemoendafter
t.test(hemodiffprior,hemodiffafter)		#not significant
t.test(hemodiffprior)
t.test(hemodiffafter)

data6 <- cbind.data.frame(Placement,dysphagia6,hemodiff)

qplot(hemodiff,geom='blank',fill=Placement,data=data6) +
	geom_line(aes(y=..density..,colour=tube_place6),stat='density') +
  scale_colour_manual(name='Density',values=c('red','blue'))+
	geom_histogram(aes(y=..density..),alpha=.7,position='identity')+
      geom_segment(aes(x = .251, y = 0, xend = 1.16898, yend = 0),colour='red',size=4) +
      geom_segment(aes(x = .1314, y = 0, xend = 1.62722, yend = 0),colour='blue',size=2) +
	#theme(legend.position=c(.85,.85)) +
  labs(title = "Difference in Hemoglobin for Patients With PEG Tubes") +
  xlab("Difference in Hemoglobin") +
  ylab("Density")


logit6 <- glm(dysphagia6 ~ tube_place6  , data=data6, family = "binomial")
summary(logit6)

############################################################################

#Question 7 dysphagia vs smoking

smoking <- Pack.Year.Hx[-c(which(is.na(Dysphagia..chronic.) | is.na(Timing.of.tube.placement)))]
smoking1 <- as.character(smoking)
smoking1[c(which(is.na(smoking1)))] <- "Not Smoking"
smoking1[which(smoking1=="<5 pk/yr hx")] <- "Not Smoking"
smoking1[which(smoking1=="5-10 pk/yr hx")] <- "Not Smoking"
smoking1[which(smoking1=="10-19 pk/yr hx")] <- "Smoking"
smoking1[which(smoking1=="20-30 pk/yr hx")] <- "Smoking"
smoking1[which(smoking1==">30 pk/yr hx")] <- "Smoking"
smoking1 <- factor(smoking1,levels=c("Smoking","Not Smoking"))

q7table <- table(smoking1,dysphagia2)
q7matrix <- matrix(q7table,byrow=F,ncol=2,dimnames=list(Smoking=c("Smokers","Nonsmokers"),DysphagiaType=c("Chronic","Not Chronic")))

procfrq(q7matrix) 	#not significant
epi.2by2(q7matrix)

#checking for confounder of smoking on tube placement vs dysphagia

q7table2 <- table(tube_place1,dysphagia2,smoking1)
epi.2by2(q7table2,method="cohort.count",conf.level=.95,homogeneity="breslow.day")
mantelhaen.test(q7table2)
epi.2by2(q7table2[,,1])
epi.2by2(q7table2[,,2])
#smoking not a confounder or effect modification

summary(glm(dysphagia1 ~ tube_place1 + smoking1,data=cbind.data.frame(dysphagia,tube_place1,smoking1),family=binomial(link="logit")))

###################dysphagia vs etoh

etoh <- ETOH[-c(which(is.na(Dysphagia..chronic.) | is.na(Timing.of.tube.placement)))]
etoh1 <- factor(etoh,levels=c("Yes","No"))

q7btable <- table(etoh1,dysphagia2)
q7bmatrix <- matrix(q7btable,byrow=F,ncol=2,dimnames=list(ETOH=c("Yes","No"),DysphagiaType=c("Chronic","Not Chronic")))

procfrq(q7bmatrix)	#not significant	
epi.2by2(q7bmatrix)

#Checking for confounder of etoh on tube placement vs dysphagia

q7btable2 <- table(tube_place1,dysphagia2,etoh1)
epi.2by2(q7btable2,method="cohort.count",conf.level=.95,homogeneity="breslow.day")
mantelhaen.test(q7btable2)
epi.2by2(q7btable2[,,1])
epi.2by2(q7btable2[,,2])
#etoh is not a confounder or an effect modifier

q67dframe <- cbind.data.frame(smoking1,etoh1,tube_place1,dysphagia2)
ggplot(q67dframe) + aes(smoking1,fill=dysphagia2) + geom_bar(position='dodge')
ggplot(q67dframe) + aes(smoking1,fill=dysphagia2) + geom_bar(position='fill')
ggplot(q67dframe) + aes(etoh1,fill=dysphagia2) + geom_bar(position='dodge')
ggplot(q67dframe) + aes(etoh1,fill=dysphagia2) + geom_bar(position='fill')

summary(glm(dysphagia1 ~ tube_place1 + etoh1,data=cbind.data.frame(dysphagia,tube_place1,etoh1),family=binomial(link="logit")))

##################################################################################################

Variable <- c("PEG Tube Placement (Prior)",
              "Surgical Resection (Yes)",
              "Concurrent Chemotherapy",
              "Smokers (>10 pk/yrs)",
              "EtOH (Yes)")
x <- c("8 (53.3)","5 (33.3)","14 (93.3)","11 (73.3)","14 (93.3)")   #Developed Chronic Dysphagia n=15
y <- c("91 (75.8)","15 (12.5)","83 (69.2)","89 (74.2)","96 (80)")    #Did not develop chronic dysphagia n=120
w <- c(".36 (0.12-1.09)","3.50 (1.05-11.65)","6.24 (0.79-49.23)","0.96 (0.28-3.23)","3.50 (0.44-27.95)")
z <- c(0.116,0.048,0.039,0.661,0.188)      #P-value

comparisons <- data.frame(Variable, 
                          "Developed Chronic Dysphagia (n=15)"=x, 
                          "Did Not Develop Chronic Dysphagia (n=120)"=y,
                          "Odds Ratio"=w,
                          "P-Value"=z, 
                          check.names=F)

comparisons

#####Demographic Information

library(eeptools)
library(lubridate)

dob <- Date.of.Birth
dob <- dob[-c(69,220)]
dob <- mdy(dob)
enddate <- Date.of.last.Follow..Or.date.of.death
enddate <- enddate[-c(69,220)]
enddate <- mdy(enddate)
subset(age,enddate<dob)
which(is.na(dob))

age = function(from, to) {
  from_lt = as.POSIXlt(from)
  to_lt = as.POSIXlt(to)

  age = to_lt$year - from_lt$year

  ifelse(to_lt$mon < from_lt$mon |
         (to_lt$mon == from_lt$mon & to_lt$mday < from_lt$mday),
         age - 1, age)
}

patient.age <- age(dob,enddate)

summary(patient.age)
hist(patient.age)

tube_place.age = Timing.of.tube.placement[-c(69,220)]
dysphagia.age = Dysphagia..chronic.[-c(69,220)]
tube.age = tube_place.age[-c(which(is.na(tube_place.age) | is.na(dysphagia.age) | is.na(patient.age)))]
dys.age = dysphagia.age[-c(which(is.na(tube_place.age) | is.na(dysphagia.age) | is.na(patient.age)))]
pat.age = patient.age[-c(which(is.na(tube_place.age) | is.na(dysphagia.age) | is.na(patient.age)))]

dys.age1 <- as.character(dys.age)
dys.age1[which(dys.age1=="No change from baseline")] = "Not Chronic"
dys.age1[which(dys.age1=="Severely altered eating/swallowing; tubefeeding or TPN or hospitalization required")] <- "Chronic"
dys.age1[which(dys.age1=="Symptomatic, able to eat regular food")] = "Not Chronic"
dys.age1[which(dys.age1=="Symptomatic, altered eating/swallowing")] = "Not Chronic"
dys.age1[which(dys.age1=="Life threating consequences")] = "Chronic"
dys.age1 <- factor(dys.age1)
dys.age1

tube.age1 <- as.character(tube.age)
tube.age1[which(tube.age1=="During XRT")] <- "Not Prior"
tube.age1[which(tube.age1=="After XRT")] <- "Not Prior"
tube.age1[which(tube.age1=="Prior to XRT")] <- "Prior"
tube.age1 <- factor(tube.age1, levels=c("Prior","Not Prior"))
tube.age1

data.age = cbind.data.frame(dys.age1,tube.age1,pat.age)
age.logit = glm(dys.age1 ~ tube.age1 + pat.age+tube.age1*pat.age,
    data=data.age,
    family=binomial(link="logit"))
summary(age.logit)

x = subset(data.age,tube.age1=="Not Prior",select=c(dys.age1,pat.age))$pat.age
min(x);max(x);
y = subset(data.age,tube.age1=="Prior",select=c(dys.age1,pat.age))$pat.age
min(y);max(y);
t.test(x,y)

#######Gender
table(Gender[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))])
gendermat <- as.matrix(table(Gender[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))]))
gendermat/sum(gendermat)

#######Ethnicity
table(Ethnicity[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))])
ethnicitymat <- as.matrix(table(Ethnicity[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))]))
ethnicitymat/sum(ethnicitymat)

#######Smokers
table(Smoking[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))])
smokers <- as.matrix(table(Smoking[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))]))
smokers/sum(smokers)

#######Pack per year
table(Pack.Year.Hx)
pack <- as.matrix(table(Pack.Year.Hx))
pack/sum(pack)
barplot(table(Pack.Year.Hx),beside=T,col=rainbow(4))

#######ETOH use 
table(ETOH)
etoh <- as.matrix(table(ETOH))
etoh/sum(etoh)

#######T Stage
table(T.Stage)
tstage <- as.matrix(table(T.Stage))
tstage/sum(tstage)
barplot(table(T.Stage),beside=T,col=rainbow(6))

#######N Stage
table(N.stage)
nstage <- as.matrix(table(N.stage))
nstage/sum(nstage)
barplot(table(N.stage),beside=T,col=rainbow(7))

#######Stage
table(Stage)
stage <- as.matrix(table(Stage))
stage/sum(stage)
barplot(table(Stage),beside=T,col=rainbow(7))

#######Tumor Site
table(Tumor.Site.)
tumor <- as.matrix(table(Tumor.Site.))
tumor/sum(tumor)
barplot(table(Tumor.Site.),beside=T,col=rainbow(7))

######Salivary Gland
table(Salivary.Gland[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))])
sgland <- as.matrix(table(Salivary.Gland[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))]))
sgland/sum(sgland)
barplot(table(Salivary.Gland[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))]),beside=T,col=rainbow(2))

######CT Scan primary size average
table(CT.scan.Primary.Size[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))])
hist(CT.scan.Primary.Size[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))])
summary(CT.scan.Primary.Size[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))])

######Sugery
table(Surgical.Procedure)
surgproc <- as.matrix(table(Surgical.Procedure))
surgproc/sum(surgproc)
barplot(table(Surgical.Procedure),beside=T)

######Chemotherapy: Neoadjuvant/Concurrant
summary(Concurrent.Chemotherapy )
conchemo <- as.matrix(summary(Concurrent.Chemotherapy ))
conchemo/sum(conchemo)

######Radiation Dose
table(Radiation.Dose.Primary[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))])
dose <- as.matrix(table(Radiation.Dose.Primary[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))]))
dose/sum(dose)
barplot(table(Radiation.Dose.Primary),beside=T)


######Radiation Type
table(Radiation.Treatment.Modality[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))])
trtmt <- as.matrix(table(Radiation.Treatment.Modality[-c(which(is.na(Timing.of.tube.placement) | is.na(Dysphagia..chronic.)))]))
trtmt/sum(trtmt)
barplot(table(Radiation.Treatment.Modality),beside=T)










