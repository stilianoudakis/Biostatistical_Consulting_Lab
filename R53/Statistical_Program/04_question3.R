###############################################
#Question 3
#Which tumors by site were more likely 
#to have dysphagia after radiation? and T stage? N stage?
####################################################

#dysphagia vs tumor site

which(colnames(peg)=="tumor_site")
table(peg$tumor_site)

tumor <- tumor_site[complete.cases(peg[,c(11,12,19)])]
length(tumor)
table(tumor)

dysphagia <- dysphagia_chronic[complete.cases(peg[,c(11,12,19)])]
dysphagia3 <- rep(NA,length(dysphagia))
dysphagia3[which(dysphagia <= 2)] <- 0
dysphagia3[which(dysphagia >= 3)] <- 1
dysphagia3 <- factor(dysphagia3)
table(dysphagia3)

tube_place <- time_nutri_tube[complete.cases(peg[,c(11,12,19)])]
tube_place3 <- rep(NA, length(tube_place))
tube_place3[which(tube_place==0)] <- 0
tube_place3[which(tube_place >= 1)] <- 1
tube_place3 <- factor(tube_place3)
table(tube_place3)

tumor3 <- factor(tumor)
levels(tumor3) <- c("oral cavity",
                    "oropharynx",
                    "nasopharynx",
                    "larynx",
                    "hypopharynx",
                    "nasal cavity",
                    "salivary gland",
                    "h&n unknown")
table(tumor3)
q3atable <- table(tumor3,dysphagia3)
q3amatrix <- matrix(c(7,10,0,6,0,0,0,1,35,81,7,35,14,2,1,0),byrow=F,ncol=2,
                    dimnames=list(
                      "Tumor Site"=c("oral cavity",
                                     "oropharynx",
                                     "nasopharynx",
                                     "larynx",
                                     "hypopharynx",
                                     "nasal cavity",
                                     "salivary gland",
                                     "h&n unknown"),
                      "Chronic Dysphagia"=c("Yes",
                                            "No")))

procfrq(q3amatrix)	#not significant

epi.2by2(table(tube_place3,dysphagia3,tumor3)[,,1])
procfrq(table(tube_place3,dysphagia3,tumor3)[,,1])
#p-value = 0.02128
#odds ratio 
#7.917851

epi.2by2(table(tube_place3,dysphagia3,tumor3)[,,2]) 
procfrq(table(tube_place3,dysphagia3,tumor3)[,,2])
#p-value = 1
#odds ratio 
#1.074405

epi.2by2(table(tube_place3,dysphagia3,tumor3)[,,3])
procfrq(table(tube_place3,dysphagia3,tumor3)[,,3])
#p-value = 1
#odds ratio 
#0 

epi.2by2(table(tube_place3,dysphagia3,tumor3)[,,4])
procfrq(table(tube_place3,dysphagia3,tumor3)[,,4])
#p-value = 1
#odds ratio 
#0.7551513 

epi.2by2(table(tube_place3,dysphagia3,tumor3)[,,5])
procfrq(table(tube_place3,dysphagia3,tumor3)[,,5])
#p-value = 1
#odds ratio 
#0

epi.2by2(table(tube_place3,dysphagia3,tumor3)[,,6])
procfrq(table(tube_place3,dysphagia3,tumor3)[,,6])
#p-value = 1
#odds ratio 
#0

epi.2by2(table(tube_place3,dysphagia3,tumor3)[,,7])
procfrq(table(tube_place3,dysphagia3,tumor3)[,,7])
#p-value = 1
#odds ratio 
#0

epi.2by2(table(tube_place3,dysphagia3,tumor3)[,,8])
procfrq(table(tube_place3,dysphagia3,tumor3)[,,8])
#p-value = 1
#odds ratio 
#0 

q3adframe <- cbind.data.frame(tube_place3,dysphagia3,tumor3)

tumorplot <- ggplot(q3adframe, aes(x=dysphagia3,fill=tumor3)) + 
  scale_fill_discrete(name="Tumor Site") + #,
  #                   labels=c("Not Chronic", "Chronic")) +
  scale_x_discrete(labels=c("No","Yes")) +
  geom_bar(
    width=.5,
    position= 'fill') + 
  xlab("Chronic Dysphagia") + 
  ylab("Count") +
  ggtitle("Dysphagia Count by Tumor Site") #+
#theme(axis.text.x=element_text(angle=90,hjust=1))

#ggplot(q3adframe) + aes(x=dysphagia3,fill="tumor type") + geom_bar(position= 'fill') + xlab("Tumor Site") + ylab("Proportion")


#dysphagia vs tstage

tstage <- t_stage[complete.cases(peg[,c(11,12,30)])]

dysphagia <- dysphagia_chronic[complete.cases(peg[,c(11,12,30)])]
dysphagia3b <- rep(NA,length(dysphagia))
dysphagia3b[which(dysphagia <= 2)] <- 0
dysphagia3b[which(dysphagia >= 3)] <- 1
dysphagia3b <- factor(dysphagia3b)
table(dysphagia3b)

tube_place <- time_nutri_tube[complete.cases(peg[,c(11,12,30)])]
tube_place3b <- rep(NA, length(tube_place))
tube_place3b[which(tube_place==0)] <- 0
tube_place3b[which(tube_place >= 1)] <- 1
tube_place3b <- factor(tube_place3b)
table(tube_place3b)

tstage3 <- factor(tstage)
levels(tstage3) <- c("T1",
                     "T2",
                     "T3",
                     "T4",
                     "Tx",
                     "Not Reported")
table(tstage3)

q3btable <- table(tstage3,dysphagia3b)
q3bmatrix <- matrix(c(1,4,7,11,1,0,16,54,51,51,1,2),byrow=F,ncol=2,dimnames=list(TStage=c("T1","T2","T3","T4","Tx","Not Reported"),"Chronic Dysphagia"=c("Yes","No")))

procfrq(q3bmatrix)

T2vT1 <- matrix(c(1,16,4,54),byrow=T,ncol=2,dimnames=list(TStage=c("T1","T2"),"Chronic Dysphagia"=c("Yes","No")))
epi.2by2(T2vT1)

T3vT1 <- matrix(c(1,16,7,51),byrow=T,ncol=2,dimnames=list(TStage=c("T1","T3"),"Chronic Dysphagia"=c("Yes","No")))
epi.2by2(T3vT1)

T4vT1 <- matrix(c(1,16,11,51),byrow=T,ncol=2,dimnames=list(TStage=c("T1","T4"),"Chronic Dysphagia"=c("Yes","No")))
epi.2by2(T4vT1)

q3bdframe <- cbind.data.frame(tube_place3b,dysphagia3b,tstage3)

tstageplot <- ggplot(q3bdframe, aes(x=dysphagia3b,fill=tstage3)) + 
  scale_fill_discrete(name="T Stage") + #,
  #                  labels=c("Not Chronic", "Chronic")) +
  scale_x_discrete(labels=c("No","Yes")) +
  geom_bar(
    width=.5,
    position= 'fill') + 
  xlab("Chronic Dysphagia") + 
  ylab("Count") +
  ggtitle("Dysphagia Count by T Stage") #+
#theme(axis.text.x=element_text(angle=90,hjust=1))


#dysphagia vs nstage

nstage <- n_stage[complete.cases(peg[,c(11,12,31)])]
table(nstage)
nstage[which(nstage >= 2 & nstage <= 4)] <- 2
table(nstage)

dysphagia <- dysphagia_chronic[complete.cases(peg[,c(11,12,31)])]
dysphagia3c <- rep(NA,length(dysphagia))
dysphagia3c[which(dysphagia <= 2)] <- 0
dysphagia3c[which(dysphagia >= 3)] <- 1
dysphagia3c <- factor(dysphagia3c)
table(dysphagia3c)

tube_place <- time_nutri_tube[complete.cases(peg[,c(11,12,31)])]
tube_place3c <- rep(NA, length(tube_place))
tube_place3c[which(tube_place==0)] <- 0
tube_place3c[which(tube_place >= 1)] <- 1
tube_place3c <- factor(tube_place3c)
table(tube_place3c)

nstage3 <- factor(nstage)
levels(nstage3) <- c("N0",
                     "N1",
                     "N2",
                     "N3",
                     "Not Reported")

q3ctable <- table(nstage3,dysphagia3c)
q3cmatrix <- matrix(c(3,5,16,0,0,39,36,84,13,3),byrow=F,ncol=2,dimnames=list(TStage=c("N0","N1","N2","N3","Not Reported"),"Chronic Dysphagia"=c("Yes","No")))

procfrq(q3cmatrix)


N1vN0 <- matrix(c(3,39,5,36),byrow=T,ncol=2,dimnames=list(NStage=c("N0","N1"),"Chronic Dysphagia"=c("Yes","No"))) 
epi.2by2(N1vN0)	

N2vN0 <- matrix(c(3,39,16,84),byrow=T,ncol=2,dimnames=list(NStage=c("N0","N2"),"Chronic Dysphagia"=c("Yes","No"))) 
epi.2by2(N2vN0)	

N3vN0 <- matrix(c(3,39,0,13),byrow=T,ncol=2,dimnames=list(NStage=c("N0","N3"),"Chronic Dysphagia"=c("Yes","No"))) 
epi.2by2(N3vN0)	

q3cdframe <- cbind.data.frame(tube_place3c,dysphagia3c,tstage3)

nstageplot <- ggplot(q3cdframe, aes(x=dysphagia3c,fill=nstage3)) + 
  scale_fill_discrete(name="N Stage") + #,
  #                  labels=c("Not Chronic", "Chronic")) +
  scale_x_discrete(labels=c("No","Yes")) +
  geom_bar(
    width=.5,
    position= 'fill') + 
  xlab("Chronic Dysphagia") + 
  ylab("Count") +
  ggtitle("Dysphagia Count by N Stage") #+
#theme(axis.text.x=element_text(angle=90,hjust=1))


#############################################################################
