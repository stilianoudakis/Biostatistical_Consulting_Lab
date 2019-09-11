#######################################################################
# Study ID  								: 2016-Template
# Investigator Name					: Dr. Adam Sima
# Purpose										: create R template for BCL projects 
#
# BIOS Programmer						: Spiro Stilianoudakis
# Project Start Date				: 30Oct2018
#	
# Modification Programmer 	: 
# Modification Purpose			: 
#
# Modification Programmer 	: 
# Modification Purpose			: 
#
# Line Numbers:
# Data Management           : 
# Analysis                  : 
# Tables                    :
# Figures                   :
#######################################################################;


# Loading Libraries
library(openxlsx)
library(dplyr)
library(ggplot2)
library(epiR)
library(DescTools)
library(grid)
library(vcd)


#setting directory
setwd("Y:/Shares/Biostatistics/Projects/BCL/Research Projects/Open/BCL - 2018 - R161 (Cho - Radiology)/1 - Data")

#reading in data
contrast_data <- readRDS("contrast_data.rds")
contrast_data <- contrast_data[order(contrast_data$date,
                                     contrast_data$id),]

#Analysis of allergic-like reactions
allergic_data <- contrast_data[-which(contrast_data$Reaction=="Physiologic"),]
dim(allergic_data) 
#63279    13
#72776    15
#63282    14

#Making table: Contrast vs Type of Reaction
table(allergic_data$contrast, allergic_data$Severity.of.Rxn)
fisher.test(table(allergic_data$contrast, allergic_data$Severity.of.Rxn)[,-3])


#Making table: Sex & age group vs Contrast agent
summary(allergic_data$age)
##categorizing age: <9, 9-18, >18
allergic_data$age_cat <- NA
allergic_data$age_cat[which(allergic_data$age<9)] <- "<9"
allergic_data$age_cat[which(allergic_data$age>=9 & allergic_data$age<=18)] <- "9-18"
allergic_data$age_cat[which(allergic_data$age>18)] <- ">18"
table(allergic_data$age_cat, exclude = "ifany")
table(allergic_data$sex,allergic_data$contrast)

##conditional tests of association
astab <- table(allergic_data$age_cat, allergic_data$Reaction, allergic_data$sex)
age.gender=margin.table(astab, c(1,2))
oddsratio(age.gender)
exp(apply(astab, 3, function(x){oddsratio(x)$coefficients}))

chisq.test(age.gender)
fisher.test(age.gender)
mantelhaen.test(astab)
woolf_test(astab) 

#Making table: Comparing contrast agents
##gadavist vs magnevist
gvm <- allergic_data[-which(allergic_data$contrast=="Other"),]
gvm$contrast <- factor(gvm$contrast)
table(gvm$contrast, gvm$react_binary)
epiR::epi.2by2(table(gvm$contrast, gvm$react_binary)[,2:1])

gvo <- allergic_data[-which(allergic_data$contrast=="Magnevist"),]
gvo$contrast <- factor(gvo$contrast)
table(gvo$contrast, gvo$react_binary)
epiR::epi.2by2(table(gvo$contrast, gvo$react_binary)[,2:1])

mvo <- allergic_data[-which(allergic_data$contrast=="Gadavist"),]
mvo$contrast <- factor(mvo$contrast)
table(mvo$contrast, mvo$react_binary)
epiR::epi.2by2(table(mvo$contrast, mvo$react_binary)[,2:1])


#Making table: Comparing time periods with gadavist reactions
table(allergic_data$quarter)
length(table(allergic_data$quarter))
##categorizing time period
###2005 Q1 - 2007 Q1 (alt. 2005 Q2 - 2007 Q1): 9 (8)
###2007 Q2 - 2009 Q1: 8
###2009 Q2 - 2011 Q1: 8
###2011 Q2 - 2013 Q1: 8
###2013 Q2 - 2015 Q1: 8
###2015 Q2 - 2017 Q1: 8
###2017 Q2 - 2018 Q1: 4

allergic_data$quarter_cat <- NA
allergic_data$quarter_cat[which(allergic_data$quarter>="2005 Q1" &
                                  allergic_data$quarter<="2007 Q1")] <- "2005 Q2 - 2007 Q1"
allergic_data$quarter_cat[which(allergic_data$quarter>="2007 Q2" &
                                  allergic_data$quarter<="2009 Q1")] <- "2007 Q2 - 2009 Q1"
allergic_data$quarter_cat[which(allergic_data$quarter>="2009 Q2" &
                                  allergic_data$quarter<="2011 Q1")] <- "2009 Q2 - 2011 Q1"
allergic_data$quarter_cat[which(allergic_data$quarter>="2011 Q2" &
                                  allergic_data$quarter<="2013 Q1")] <- "2011 Q2 - 2013 Q1"
allergic_data$quarter_cat[which(allergic_data$quarter>="2013 Q2" &
                                  allergic_data$quarter<="2015 Q1")] <- "2013 Q2 - 2015 Q1"
allergic_data$quarter_cat[which(allergic_data$quarter>="2015 Q2" &
                                  allergic_data$quarter<="2017 Q1")] <- "2015 Q2 - 2017 Q1"
allergic_data$quarter_cat[which(allergic_data$quarter>="2017 Q2" &
                                  allergic_data$quarter<="2018 Q1")] <- "2017 Q2 - 2018 Q1"
table(allergic_data$quarter_cat)
#remove rows with 2005 Q1
allergic_data2 <- allergic_data[-which(allergic_data$quarter=="2005 Q1"),]
table(allergic_data2$quarter_cat)
table(allergic_data2$quarter_cat, allergic_data2$react_binary)


mat <- rbind(table(allergic_data2$quarter_cat, allergic_data2$react_binary)[1,2:1],
               colSums(table(allergic_data2$quarter_cat, allergic_data2$react_binary)[-1,2:1]))
epiR::epi.2by2(mat)  
mat <- rbind(table(allergic_data2$quarter_cat, allergic_data2$react_binary)[2,2:1],
             colSums(table(allergic_data2$quarter_cat, allergic_data2$react_binary)[-2,2:1]))
epiR::epi.2by2(mat) 
mat <- rbind(table(allergic_data2$quarter_cat, allergic_data2$react_binary)[3,2:1],
             colSums(table(allergic_data2$quarter_cat, allergic_data2$react_binary)[-3,2:1]))
epiR::epi.2by2(mat) 
mat <- rbind(table(allergic_data2$quarter_cat, allergic_data2$react_binary)[4,2:1],
             colSums(table(allergic_data2$quarter_cat, allergic_data2$react_binary)[-4,2:1]))
epiR::epi.2by2(mat) 
mat <- rbind(table(allergic_data2$quarter_cat, allergic_data2$react_binary)[5,2:1],
             colSums(table(allergic_data2$quarter_cat, allergic_data2$react_binary)[-5,2:1]))
epiR::epi.2by2(mat) 
mat <- rbind(table(allergic_data2$quarter_cat, allergic_data2$react_binary)[6,2:1],
             colSums(table(allergic_data2$quarter_cat, allergic_data2$react_binary)[-6,2:1]))
epiR::epi.2by2(mat) 
mat <- rbind(table(allergic_data2$quarter_cat, allergic_data2$react_binary)[7,2:1],
             colSums(table(allergic_data2$quarter_cat, allergic_data2$react_binary)[-7,2:1]))
epiR::epi.2by2(mat) 


#Making Figure: Overall injection plot
##creating a variable for 6 month time periods
allergic_data$half <- cut(allergic_data$date, 
                      breaks='6 month')
qcdat <- as.data.frame(cbind(as.matrix(table(allergic_data$half, 
                                             allergic_data$contrast)),
                             as.vector(table(allergic_data$half)),
                             as.matrix(table(allergic_data$half, 
                                             allergic_data$react_binary))[,2]))

qcdat$time <- c("Q1 05",
                "Q2 05",
                "Q1 06",
                "Q2 06",
                "Q1 07",
                "Q2 07",
                "Q1 08",
                "Q2 08",
                "Q1 09",
                "Q2 09",
                "Q1 10",
                "Q2 10",
                "Q1 11",
                "Q2 11",
                "Q1 12",
                "Q2 12",
                "Q1 13",
                "Q2 13",
                "Q1 14",
                "Q2 14",
                "Q1 15",
                "Q2 15",
                "Q1 16",
                "Q2 16",
                "Q1 17",
                "Q2 17",
                "Q1 18"
)

names(qcdat)[c(4,5)] <- c("Total", "Reactions")

qcdat$time <- factor(qcdat$time, levels = c("Q1 05",
                                            "Q2 05",
                                            "Q1 06",
                                            "Q2 06",
                                            "Q1 07",
                                            "Q2 07",
                                            "Q1 08",
                                            "Q2 08",
                                            "Q1 09",
                                            "Q2 09",
                                            "Q1 10",
                                            "Q2 10",
                                            "Q1 11",
                                            "Q2 11",
                                            "Q1 12",
                                            "Q2 12",
                                            "Q1 13",
                                            "Q2 13",
                                            "Q1 14",
                                            "Q2 14",
                                            "Q1 15",
                                            "Q2 15",
                                            "Q1 16",
                                            "Q2 16",
                                            "Q1 17",
                                            "Q2 17",
                                            "Q1 18"
))

qcdatl <- data.frame(Time = rep(qcdat$time,4), 
                     Injections = c(qcdat$Total,
                                    qcdat$Gadavist, 
                                    qcdat$Magnevist,
                                    qcdat$Other),
                     Contrast = c(rep("Total",27),
                                  rep("Gadavist",27),
                                  rep("Magnevist",27),
                                  rep("Other",27)))
qcdatl$Contrast <- factor(qcdatl$Contrast, levels = c("Total",
                                                      "Gadavist",
                                                      "Magnevist",
                                                      "Other"))

table(allergic_data$contrast,allergic_data$half, allergic_data$react_binary)
table(allergic_data$contrast,allergic_data$half, allergic_data$react_binary)[,,2]
reactmat <- t(table(allergic_data$contrast,allergic_data$half, allergic_data$react_binary)[,,2])

qcdatl$Reaction <- c(qcdat$Reactions, 
                     reactmat[,1], 
                     reactmat[,2], 
                     reactmat[,3])
qcdatl$TotInj <- c(rep(colSums(qcdat[,-6])[4],27), 
                   rep(colSums(qcdat[,-6])[1],27), 
                   rep(colSums(qcdat[,-6])[2],27), 
                   rep(colSums(qcdat[,-6])[3],27))
qcdatl$RR <- qcdatl$Reaction/qcdatl$TotInj

p.qc <- ggplot(qcdatl, aes(x=Time, y=Injections, group=Contrast)) +
  geom_line(aes(color=Contrast), size=1.5)+
  geom_point(aes(color=Contrast), size=3)+
  ylab("Number of Injections")+
  xlab("Time")+
  scale_color_manual(name = "Contrast", values = c("black",
                                                   "red",
                                                   "blue",
                                                   "green")) +
  theme_bw() +
  #guides(fill=FALSE)+
  theme(axis.text.x = element_text(angle = 45, margin = ggplot2::margin(t = 15)),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=20),
        legend.position="bottom") 

p.qc


#Making Figure: Reaction Rate plot
qcdatl.rr <- qcdatl[which(
  qcdatl$Contrast=="Gadavist" |
    qcdatl$Contrast=="Magnevist"),]
#qcdatl.rr$RR[which(qcdatl.rr$RR==0)] <- NA


p.rr <- ggplot(qcdatl.rr, aes(x=Time, y=RR, group=Contrast)) +
  geom_line(aes(color=Contrast), size=1.5)+
  geom_point(aes(color=Contrast), size=3)+
  ylab("Reaction Rate")+
  xlab("Time")+
  theme_bw() +
  #guides(fill=FALSE)+
  theme(axis.text.x = element_text(angle = 45, margin = ggplot2::margin(t = 15)),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=20),
        legend.position="bottom") 

p.rr


qcdatl.rr.mag <- qcdatl.rr[which(qcdatl.rr$Contrast=="Magnevist"),]
qcdatl.rr.gad <- qcdatl.rr[which(qcdatl.rr$Contrast=="Gadavist"),]

qcdatl.rr.mag$RR[21:27] <- NA
qcdatl.rr.gad$RR[1:20] <- NA

qcdatl.rr2 <- rbind.data.frame(qcdatl.rr.gad, qcdatl.rr.mag)

p.rr2 <- ggplot(qcdatl.rr2, aes(x=Time, y=RR, group=Contrast)) +
  geom_line(aes(color=Contrast), size=1.5)+
  geom_point(aes(color=Contrast), size=3)+
  ylab("Reaction Rate")+
  xlab("Time")+
  theme_bw() +
  #guides(fill=FALSE)+
  theme(axis.text.x = element_text(angle = 45, margin = ggplot2::margin(t = 15)),
        axis.text.y = element_text(size = 15),
        axis.title.x = element_text(size = 15),
        axis.title.y = element_text(size = 15),
        legend.text=element_text(size=15),
        legend.title=element_text(size=20),
        legend.position="bottom") 

p.rr2
