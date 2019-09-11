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

#setting directory
setwd("Z:/Shares/Biostatistics/Projects/BCL/Research Projects/Open/BCL - 2018 - R161 (Cho - Radiology)/1 - Data")

#reading in data
contrast_data <- readRDS("contrast_data.rds")

#Analysis of allergic-like reactions
allergic_data <- contrast_data[-which(contrast_data$Reaction=="Physiologic"),]
dim(allergic_data) #63279    13

##total number of allergic reactions
table(allergic_data$Reaction)

##table of contrast vs reaction
table(allergic_data$contrast, allergic_data$Severity.of.Rxn)

##table of age vs reaction
###categorizing age
allergic_data$age_cat <- ifelse(allergic_data$age < 9, "<9", 
                                ifelse(allergic_data$age >=9 & allergic_data$age < 18, "6-18",
                                          ifelse(allergic_data$age >= 18, ">18", NA)))

table(allergic_data$age_cat, allergic_data$Severity.of.Rxn)

##table of gender vs reaction
table(allergic_data$sex, allergic_data$Severity.of.Rxn)

#Analysis of physiologic-like reactions
physio_data <- contrast_data[-which(contrast_data$Reaction=="Allergic"),]
dim(physio_data) #63262    13

##table of contrast vs reaction
table(physio_data$contrast, physio_data$Severity.of.Rxn)

##table of age vs reaction
###categorizing age
physio_data$age_cat <- ifelse(physio_data$age < 9, "<9", 
                                ifelse(physio_data$age >=9 & physio_data$age < 18, "6-18",
                                          ifelse(physio_data$age >= 18, ">18", NA)))

table(physio_data$age_cat, physio_data$Severity.of.Rxn)

##table of gender vs reaction
table(physio_data$sex, physio_data$Severity.of.Rxn)


#Analysis of all reactions

##table of contrast vs reaction
table(contrast_data$contrast, contrast_data$Severity.of.Rxn)

##table of age vs reaction
###categorizing age
contrast_data$age_cat <- ifelse(contrast_data$age < 9, "<9", 
                                ifelse(contrast_data$age >=9 & contrast_data$age < 18, "6-18",
                                          ifelse(contrast_data$age >= 18, ">18", NA)))

table(contrast_data$age_cat, contrast_data$Severity.of.Rxn)

##table of gender vs reaction
table(contrast_data$sex, contrast_data$Severity.of.Rxn)

##############################################################

#Comparing reactions between Gadavist and magnavist

##Allergic-like
gadavmagn <- allergic_data[which(allergic_data$contrast=="Gadavist" | allergic_data$contrast=="Magnevist"),]
table(gadavmagn$contrast, gadavmagn$react_binary)

##Physiologic-like
gadavmagn <- physio_data[which(physio_data$contrast=="Gadavist" | physio_data$contrast=="Magnevist"),]
table(gadavmagn$contrast, gadavmagn$react_binary)

##All reactions
gadavmagn <- contrast_data[which(contrast_data$contrast=="Gadavist" | contrast_data$contrast=="Magnevist"),]
table(gadavmagn$contrast, gadavmagn$react_binary)

##############################################################

#Comparing before/after 2015 with reactions

##allergic-like
table(allergic_data$switch.x, allergic_data$react_binary)

##physiologic-like
table(physio_data$switch.x, physio_data$react_binary)

##all reactions
table(contrast_data$switch.x, contrast_data$react_binary)

##############################################################

#comparing reactions within each 2 year period

##allergic-like
allergic_data$two_year <- ifelse(grepl("2005",allergic_data$quarter) | grepl("2006",allergic_data$quarter), "2005-2006",
                                 ifelse(grepl("2007",allergic_data$quarter) | grepl("2008",allergic_data$quarter), "2007-2008",
                                        ifelse(grepl("2009",allergic_data$quarter) | grepl("2010",allergic_data$quarter), "2009-2010",
                                               ifelse(grepl("2011",allergic_data$quarter) | grepl("2012",allergic_data$quarter), "2011-2012",
                                                      ifelse(grepl("2013",allergic_data$quarter) | grepl("2014",allergic_data$quarter), "2013-2014",
                                                             ifelse(grepl("2015",allergic_data$quarter) | grepl("2016",allergic_data$quarter), "2015-2016",
                                                                    ifelse(grepl("2017",allergic_data$quarter) | grepl("2018",allergic_data$quarter), "2017-2018",NA)))))))
table(allergic_data$two_year, exclude = "ifany")
dummies = model.matrix(~two_year+0, data=allergic_data)
dummies <- data.frame(dummies)
allergic_data <- cbind.data.frame(allergic_data, dummies)

table(allergic_data$two_year2005.2006, allergic_data$react_binary)
table(allergic_data$two_year2007.2008, allergic_data$react_binary)
table(allergic_data$two_year2009.2010, allergic_data$react_binary)
table(allergic_data$two_year2011.2012, allergic_data$react_binary)
table(allergic_data$two_year2013.2014, allergic_data$react_binary)
table(allergic_data$two_year2015.2016, allergic_data$react_binary)
table(allergic_data$two_year2017.2018, allergic_data$react_binary)

##physiologic-like
physio_data$two_year <- ifelse(grepl("2005",physio_data$quarter) | grepl("2006",physio_data$quarter), "2005-2006",
                               ifelse(grepl("2007",physio_data$quarter) | grepl("2008",physio_data$quarter), "2007-2008",
                                      ifelse(grepl("2009",physio_data$quarter) | grepl("2010",physio_data$quarter), "2009-2010",
                                             ifelse(grepl("2011",physio_data$quarter) | grepl("2012",physio_data$quarter), "2011-2012",
                                                    ifelse(grepl("2013",physio_data$quarter) | grepl("2014",physio_data$quarter), "2013-2014",
                                                           ifelse(grepl("2015",physio_data$quarter) | grepl("2016",physio_data$quarter), "2015-2016",
                                                                  ifelse(grepl("2017",physio_data$quarter) | grepl("2018",physio_data$quarter), "2017-2018",NA)))))))
table(physio_data$two_year, exclude = "ifany")
dummies = model.matrix(~two_year+0, data=physio_data)
dummies <- data.frame(dummies)
physio_data <- cbind.data.frame(physio_data, dummies)

table(physio_data$two_year2005.2006, physio_data$react_binary)
table(physio_data$two_year2007.2008, physio_data$react_binary)
table(physio_data$two_year2009.2010, physio_data$react_binary)
table(physio_data$two_year2011.2012, physio_data$react_binary)
table(physio_data$two_year2013.2014, physio_data$react_binary)
table(physio_data$two_year2015.2016, physio_data$react_binary)
table(physio_data$two_year2017.2018, physio_data$react_binary)


##all reactions
contrast_data$two_year <- ifelse(grepl("2005",contrast_data$quarter) | grepl("2006",contrast_data$quarter), "2005-2006",
                                 ifelse(grepl("2007",contrast_data$quarter) | grepl("2008",contrast_data$quarter), "2007-2008",
                                        ifelse(grepl("2009",contrast_data$quarter) | grepl("2010",contrast_data$quarter), "2009-2010",
                                               ifelse(grepl("2011",contrast_data$quarter) | grepl("2012",contrast_data$quarter), "2011-2012",
                                                      ifelse(grepl("2013",contrast_data$quarter) | grepl("2014",contrast_data$quarter), "2013-2014",
                                                             ifelse(grepl("2015",contrast_data$quarter) | grepl("2016",contrast_data$quarter), "2015-2016",
                                                                    ifelse(grepl("2017",contrast_data$quarter) | grepl("2018",contrast_data$quarter), "2017-2018",NA)))))))
table(contrast_data$two_year, exclude = "ifany")
dummies = model.matrix(~two_year+0, data=contrast_data)
dummies <- data.frame(dummies)
contrast_data <- cbind.data.frame(contrast_data, dummies)

table(contrast_data$two_year2005.2006, contrast_data$react_binary)
table(contrast_data$two_year2007.2008, contrast_data$react_binary)
table(contrast_data$two_year2009.2010, contrast_data$react_binary)
table(contrast_data$two_year2011.2012, contrast_data$react_binary)
table(contrast_data$two_year2013.2014, contrast_data$react_binary)
table(contrast_data$two_year2015.2016, contrast_data$react_binary)
table(contrast_data$two_year2017.2018, contrast_data$react_binary)

##################################################################

#reaction rate plots

##allergic-like
qcdat <- as.data.frame(cbind(as.matrix(table(allergic_data$half, allergic_data$contrast)),as.vector(table(allergic_data$half)),as.matrix(table(allergic_data$half, allergic_data$react_binary))[,2]))

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

names(qcdat)[c(8,9)] <- c("Total", "Reactions")

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

qcdatl <- data.frame(Time = rep(qcdat$time,8), 
                     Injections = c(qcdat$Total,
                                    qcdat$Gadavist, 
                                    qcdat$Magnevist,
                                    qcdat$ABlavar,
                                    qcdat$Dotarem,
                                    qcdat$Eovist,
                                    qcdat$Prohance,
                                    qcdat$Unknown),
                     Contrast = c(rep("Total",27),
                                  rep("Gadavist",27),
                                  rep("Magnavist",27),
                                  rep("Ablavar",27),
                                  rep("Dotarem",27),
                                  rep("Eovist",27),
                                  rep("Prohance",27),
                                  rep("Unknown",27)))
qcdatl$Contrast <- factor(qcdatl$Contrast, levels = c("Total",
                                                      "Gadavist",
                                                      "Magnavist",
                                                      "Ablavar",
                                                      "Dotarem",
                                                      "Eovist",
                                                      "Prohance",
                                                      "Unknown"))

table(allergic_data$contrast,allergic_data$half, allergic_data$react_binary)
table(allergic_data$contrast,allergic_data$half, allergic_data$react_binary)[,,2]
reactmat <- t(table(allergic_data$contrast,allergic_data$half, allergic_data$react_binary)[,,2])

qcdatl$Reaction <- c(qcdat$Reactions, 
                     reactmat[,4], 
                     reactmat[,5], 
                     reactmat[,1],
                     reactmat[,2],
                     reactmat[,3],
                     reactmat[,6],
                     reactmat[,7])
qcdatl$TotInj <- c(rep(colSums(qcdat[,-10])[8],27), 
                   rep(colSums(qcdat[,-10])[4],27), 
                   rep(colSums(qcdat[,-10])[5],27), 
                   rep(colSums(qcdat[,-10])[1],27),
                   rep(colSums(qcdat[,-10])[2],27),
                   rep(colSums(qcdat[,-10])[3],27),
                   rep(colSums(qcdat[,-10])[6],27),
                   rep(colSums(qcdat[,-10])[7],27))
qcdatl$RR <- qcdatl$Reaction/qcdatl$TotInj

p.qc <- ggplot(qcdatl, aes(x=Time, y=Injections, group=Contrast)) +
  geom_line(aes(color=Contrast))+
  geom_point(aes(color=Contrast))+
  ylab("Number of Injections")+
  xlab("Time")+
  scale_color_manual(name = "Contrast", values = c("black",
                                                   "red",
                                                   "blue",
                                                   "pink",
                                                   "purple",
                                                   "yellow",
                                                   "orange",
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

qcdatl.rr <- qcdatl[which(
                            qcdatl$Contrast=="Gadavist" |
                            qcdatl$Contrast=="Magnavist"),]
#qcdatl.rr$RR[which(qcdatl.rr$RR==0)] <- NA

p.rr <- p.qc <- ggplot(qcdatl.rr, aes(x=Time, y=RR, group=Contrast)) +
  geom_line(aes(color=Contrast))+
  geom_point(aes(color=Contrast))+
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

##########################

#Physiologic-like
qcdat <- as.data.frame(cbind(as.matrix(table(physio_data$half, physio_data$contrast)),as.vector(table(physio_data$half)),as.matrix(table(physio_data$half, physio_data$react_binary))[,2]))

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

names(qcdat)[c(8,9)] <- c("Total", "Reactions")

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

qcdatl <- data.frame(Time = rep(qcdat$time,8), 
                     Injections = c(qcdat$Total,
                                    qcdat$Gadavist, 
                                    qcdat$Magnevist,
                                    qcdat$ABlavar,
                                    qcdat$Dotarem,
                                    qcdat$Eovist,
                                    qcdat$Prohance,
                                    qcdat$Unknown),
                     Contrast = c(rep("Total",27),
                                  rep("Gadavist",27),
                                  rep("Magnavist",27),
                                  rep("Ablavar",27),
                                  rep("Dotarem",27),
                                  rep("Eovist",27),
                                  rep("Prohance",27),
                                  rep("Unknown",27)))
qcdatl$Contrast <- factor(qcdatl$Contrast, levels = c("Total",
                                                      "Gadavist",
                                                      "Magnavist",
                                                      "Ablavar",
                                                      "Dotarem",
                                                      "Eovist",
                                                      "Prohance",
                                                      "Unknown"))

table(physio_data$contrast,physio_data$half, physio_data$react_binary)
table(physio_data$contrast,physio_data$half, physio_data$react_binary)[,,2]
reactmat <- t(table(physio_data$contrast,physio_data$half, physio_data$react_binary)[,,2])

qcdatl$Reaction <- c(qcdat$Reactions, 
                     reactmat[,4], 
                     reactmat[,5], 
                     reactmat[,1],
                     reactmat[,2],
                     reactmat[,3],
                     reactmat[,6],
                     reactmat[,7])
qcdatl$TotInj <- c(rep(colSums(qcdat[,-10])[8],27), 
                   rep(colSums(qcdat[,-10])[4],27), 
                   rep(colSums(qcdat[,-10])[5],27), 
                   rep(colSums(qcdat[,-10])[1],27),
                   rep(colSums(qcdat[,-10])[2],27),
                   rep(colSums(qcdat[,-10])[3],27),
                   rep(colSums(qcdat[,-10])[6],27),
                   rep(colSums(qcdat[,-10])[7],27))
qcdatl$RR <- qcdatl$Reaction/qcdatl$TotInj

p.qc <- ggplot(qcdatl, aes(x=Time, y=Injections, group=Contrast)) +
  geom_line(aes(color=Contrast))+
  geom_point(aes(color=Contrast))+
  ylab("Number of Injections")+
  xlab("Time")+
  scale_color_manual(name = "Contrast", values = c("black",
                                                   "red",
                                                   "blue",
                                                   "pink",
                                                   "purple",
                                                   "yellow",
                                                   "orange",
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

qcdatl.rr <- qcdatl[which(
  qcdatl$Contrast=="Gadavist" |
    qcdatl$Contrast=="Magnavist"),]
#qcdatl.rr$RR[which(qcdatl.rr$RR==0)] <- NA

p.rr <- p.qc <- ggplot(qcdatl.rr, aes(x=Time, y=RR, group=Contrast)) +
  geom_line(aes(color=Contrast))+
  geom_point(aes(color=Contrast))+
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

###############################

#All reactions
qcdat <- as.data.frame(cbind(as.matrix(table(contrast_data$half, contrast_data$contrast)),as.vector(table(contrast_data$half)),as.matrix(table(contrast_data$half, contrast_data$react_binary))[,2]))

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

names(qcdat)[c(8,9)] <- c("Total", "Reactions")

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

qcdatl <- data.frame(Time = rep(qcdat$time,8), 
                     Injections = c(qcdat$Total,
                                    qcdat$Gadavist, 
                                    qcdat$Magnevist,
                                    qcdat$ABlavar,
                                    qcdat$Dotarem,
                                    qcdat$Eovist,
                                    qcdat$Prohance,
                                    qcdat$Unknown),
                     Contrast = c(rep("Total",27),
                                  rep("Gadavist",27),
                                  rep("Magnavist",27),
                                  rep("Ablavar",27),
                                  rep("Dotarem",27),
                                  rep("Eovist",27),
                                  rep("Prohance",27),
                                  rep("Unknown",27)))
qcdatl$Contrast <- factor(qcdatl$Contrast, levels = c("Total",
                                                      "Gadavist",
                                                      "Magnavist",
                                                      "Ablavar",
                                                      "Dotarem",
                                                      "Eovist",
                                                      "Prohance",
                                                      "Unknown"))

table(contrast_data$contrast,contrast_data$half, contrast_data$react_binary)
table(contrast_data$contrast,contrast_data$half, contrast_data$react_binary)[,,2]
reactmat <- t(table(contrast_data$contrast,contrast_data$half, contrast_data$react_binary)[,,2])

qcdatl$Reaction <- c(qcdat$Reactions, 
                     reactmat[,4], 
                     reactmat[,5], 
                     reactmat[,1],
                     reactmat[,2],
                     reactmat[,3],
                     reactmat[,6],
                     reactmat[,7])
qcdatl$TotInj <- c(rep(colSums(qcdat[,-10])[8],27), 
                   rep(colSums(qcdat[,-10])[4],27), 
                   rep(colSums(qcdat[,-10])[5],27), 
                   rep(colSums(qcdat[,-10])[1],27),
                   rep(colSums(qcdat[,-10])[2],27),
                   rep(colSums(qcdat[,-10])[3],27),
                   rep(colSums(qcdat[,-10])[6],27),
                   rep(colSums(qcdat[,-10])[7],27))
qcdatl$RR <- qcdatl$Reaction/qcdatl$TotInj

p.qc <- ggplot(qcdatl, aes(x=Time, y=Injections, group=Contrast)) +
  geom_line(aes(color=Contrast))+
  geom_point(aes(color=Contrast))+
  ylab("Number of Injections")+
  xlab("Time")+
  scale_color_manual(name = "Contrast", values = c("black",
                                                   "red",
                                                   "blue",
                                                   "pink",
                                                   "purple",
                                                   "yellow",
                                                   "orange",
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

qcdatl.rr <- qcdatl[which(
  qcdatl$Contrast=="Gadavist" |
    qcdatl$Contrast=="Magnavist"),]
#qcdatl.rr$RR[which(qcdatl.rr$RR==0)] <- NA

p.rr <- p.qc <- ggplot(qcdatl.rr, aes(x=Time, y=RR, group=Contrast)) +
  geom_line(aes(color=Contrast))+
  geom_point(aes(color=Contrast))+
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