#####################################
#data collection and data dictionary#
#####################################

###############################################################
#Loading necessary packages
# For logistic reg
library(aod)
library(ggplot2)
library(Rcpp)

#for multinomial logistic reg
library(foreign)
library(nnet)
library(reshape2)

#for ordinal logistic reg
library(MASS)
library(Hmisc)

library(epiR)
library(tables)
library(vcd)

library(scales)
library(gridBase)
library(RColorBrewer)

library(eeptools)
library(lubridate)
######################################################################

#Data installation

#importing the data
setwd("C:/Users/Spiro Stilianoudakis/Documents/BCL Projects")
#setwd("C:/Users/stilianoudasc/Documents")
peg <- read.csv("HeadAndNeckRadiation_DATA_2017-08-24_0756.csv",header=T,na.strings="")
attach(peg)
dim(peg)
head(peg)
str(peg)
names(peg)

########################################################################

#Data Dictionary
#Gender:
#	Male -> 1
#	Female -> 0
#Ethnicity
#	White -> 1
#	Black -> 2
#	Hispanic -> 3
#	Asian -> 4
#	Other -> 6
#ETOH
#	Yes -> 0
#	No -> 1
#Smoking
#	Never -> 0
#	Quit > 5yrs prior to XRT -> 1
#	Quit > 1yr prior to XRT -> 2
#	Quit > 1month prior to XRT -> 3
#	Still Smoking or quit < 1 month prior to XRT -> 4
#	N/A -> 5
#Pack_yrs_smoking
#	<5 pk/yr -> 0
#	5-10 pk/yr -> 1
#	10-19 pk/yr -> 2
#	20-30 pk/yr -> 3
#	>30 pk/yr -> 4
#tobacco_chewing
#	No -> 1
#	Yes -> 0
#positive_tobacco_chewing
#	Quit > 5yrs prior to XRT -> 0
#	Quit > yr prior to XRT -> 1
#nutritional_tube
#	Yes -> 0
#	No -> 1
#time_nutri_tube
#	Prior to XRT -> 0
#	During XRT -> 1
#	After XRT -> 2
#dysphagia_chronic
#	No change from baseline -> 0
#	symptomatic, able to eat regular food -> 1
#	symptomatic, altered eating/swallowing -> 2
#	severely altered eating/swallowing; tubefeeding or TPN or hospitalization required -> 3
#	life-threatening consequences -> 4
#	death -> 5
#neoadjuvant_chemo_type
#	cisplatin + 5-Fu -> 0 
#	docetaxel + cisplatin + 5-Fu -> 1
#	other -> 2
#concurrent_chemo_type
#	cisplatin -> 0
#	cetuximab -> 1
#	carboplatin -> 2
#	other -> 3
#tumor_site
#	oral cavity -> 0
#	oropharynx -> 1
#	nasopharynx -> 2
#	larynx -> 3
#	hypopharynx -> 4
#	nasal cavity -> 6
#	salivary gland -> 7
#	h&n unknown ->8
#	skin -> 9
#	parotid bed mets -> 10
#oral_cavity
#	oral tongue -> 0
#	floor of mouth -> 1
#	buccal mucosa -> 2
#	hard plate -> 3
#	alveolar ridge -> 4
#	RMT -> 5
#	upper lip -> 6
#	lower lip -> 7
#oropharynx
#	palatine tongue -> 0
#	soft palate -> 1
#	base of tongue -> 2
#	pharyngeal wall -> 3
#	tonsillar pillars -> 4
#larynx 
#	supraglottis -> 0
#	glottis -> 1
#	subglottis -> 2
#supraglottis
#	epiglottis -> 0
#	aryepiglottic fold -> 1
#	arytenoid -> 2
#	false cord -> 3
#	ventricle -> 4
#	not reported -> 5
#glottis
#	right tvc -> 0
#	left tvc -> 1
#	anterior comissure -> 2
#hypopharynx
#	pyriform sinus -> 0
#	post wall -> 1
#paranasal_sinus
#	maxillary -> 0
#	ethmoid -> 1
#salivary_gland
#	parotid -> 0
#	submandibular -> 1
#nasal_cavity
#	cavity -> 1
#stage
#	I -> 0
#	II -> 1
#	III -> 3
#	IVa -> 4
#	IVb -> 5
#	IVc -> 6
#	not reported -> 7
#t_stage
#	T1 -> 0
#	T2 ->1
#	T3 ->2
#	T4 ->3
#	tx -> 4
#	T0 -> 5
#	tis -> 6
#	not reported ->7
#n_stage
#	N0 -> 0
#	N1 -> 1
#	N2a -> 2
#	N2b -> 3
#	N2c -> 4
#	N3 -> 5
#	not reported -> 9
#type_of_surgery
#	wide transoral excision -> 0
#	laryngectomy total -> 1
#	laryngectomy supraglottic -> 2
#	parotidectomy total -> 4
#	parotidectomy -> 5
#	glossectomy total -> 7
#	glossectomy hemi/partial -> 8
#	maxillectomy -> 9
#	mandibulectomy -> 10
#	reconstruction skin graft -> 11
#	reconstruction myocutaneous flap/pectoralis -> 12
#	N/A -> 15
#	other -> 16
#radiation_modality
#	3-D conformal -> 0
#	IMRT -> 1
#	IGRT -> 2
#	arc-therapy -> 3
#radiation_dose
#	<45 gy -> 0
#	45-59.9 -> 1
#	60-63.9 -> 2
#	64-67.9 -> 3
#	68-70 gy -> 4
#	>70 gy -> 5
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

#Fixing the Aged variable for Q1