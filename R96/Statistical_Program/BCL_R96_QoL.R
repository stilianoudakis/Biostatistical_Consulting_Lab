#######################################################################
# Study ID  								: 96
# Investigator Name					: Yasamin Sharifzadeh
#
# BIOS Programmer						: Spiro Stilianoudakis
# Project Start Date				: 13Sept2017
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
#######################################################################

### Loading necessary packages ###

library(openxlsx)
library(scales)
library(lattice)
library(nlme)
library(lme4)
library(lattice)
library(multcomp)
library(lmtest)


### DATA MANAGEMENT ###

#Importing the data

setwd("Z:/Shares/Biostatistics/Projects/BCL/Research Projects/Open/BCL - 2017 - R96 (Sharifzadeh - Internal Medicine)/1 - Data")
setwd("T:/Biostatistics/Projects/BCL/Research Projects/Open/BCL - 2017 - R96 (Sharifzadeh - Internal Medicine)/1 - Data")
list.files()
med_hist <- read.xlsx("Jh QoL all 7-19-17 FINAL cleaned DEID.xlsx",
                    sheet = 1,
                    startRow = 1,
                    colNames = TRUE,
                    na.strings = c("","n/a","N/a"),
                    detectDates = TRUE)
quest <- read.xlsx("NeuroQoL_FINAL_YS_DEID.xlsx",
                           sheet = 1,
                           startRow = 1,
                           colNames = TRUE)
View(med_hist)
attach(med_hist)
names(med_hist)
dim(med_hist)
head(med_hist)

View(quest)
attach(quest)
names(quest)
dim(quest)
head(quest)

#Removing rows where timing of the survey was after 3 months

colnames(quest)[3] <- "timing_of_survey"

quest2 <- quest[quest[,3] <= 3,]
dim(quest2)

#Removing all but the QoL column, the 4 uncertainty related questions, the timing, and location

quest3 <- quest2[,which(colnames(quest2)=="Data.Base.ID" |
                                          colnames(quest2)=="timing_of_survey" |
                                          colnames(quest2)=="Location.(0.office,.1.home)" |
                                          colnames(quest2)=="Overall.quality.of.life" |
                                          colnames(quest2)=="Feel.uncertain.about.future?" |
                                          colnames(quest2)=="Setbacks.in.condition?" |
                                          colnames(quest2)=="Concerned.about.disrupting.family.life?" |
                                          colnames(quest2)=="Outlook.on.future.worsen?")]
dim(quest3)
names(quest3) <- c("ID", "timing_of_survey", "location_of_survey", "QoL", "uncertainty1", "uncertainty2", "uncertainty3","uncertainty4")

#Removing rows with either 999 or NA in them or others

quest4 <- quest3[complete.cases(quest3), ]
quest5 <- subset(quest4, !quest4$ID %in% quest4$ID[quest4$QoL==999 |
                                                                   quest4$uncertainty1==999 |
                                                                   quest4$uncertainty2==999 |
                                                                   quest4$uncertainty3==999 |
                                                                   quest4$uncertainty4==999])
num <- c("1","2","3","4")
grep(paste(num, collapse = "|"), quest5$uncertainty2,invert = TRUE)
quest5 <- quest5[-grep(paste(num, collapse = "|"), quest5$uncertainty2,invert = TRUE),]
dim(quest5)
head(quest5)
str(quest5)

quest5$uncertainty2 <- as.numeric(quest5$uncertainty2)

#Transforming QoL 

QoL_calc <- function(x){
  ((x-1)/(max(quest5$QoL)-min(quest5$QoL)))*100
}
quest5$QoL_score <- QoL_calc(quest5$QoL)

#Adding scores for uncertainty

quest5$raw_score <- rowMeans(quest5[,5:8])

#Transforming uncertainty

uncertain_calc <- function(x){
  ((x-1)/(max(quest5$raw_score)-min(quest5$raw_score)))*100
}
quest5$uncertain_score <- uncertain_calc(quest5$raw_score)

#removing QoL, uncertainty1-4
quest6 <- quest5[,c(1,2,3,9,11)]

#Determining which patients (IDs) have resposes for t=1 or 3 but no baseline
#t3vt1 <- setdiff(quest6$ID[which(quest6$timing_of_survey==3)],quest6$ID[which(quest6$timing_of_survey==1)])
#t3vbase <- setdiff(quest6$ID[which(quest6$timing_of_survey==3)],quest6$ID[which(quest6$timing_of_survey==0)])
#t1vbase <- setdiff(quest6$ID[which(quest6$timing_of_survey==1)],quest6$ID[which(quest6$timing_of_survey==0)])
#union(union(t3vt1,t3vbase),t1vbase)

#Removing patients that have response for t1 or t3 but not baseline
#quest7 <- quest6[-which(quest6$ID %in% union(union(t3vt1,t3vbase),t1vbase)),]

quest7 <- quest6

#Seperating by timing of survey
baseline <- subset(quest7,quest7$timing_of_survey==0)
t1 <- subset(quest7,quest7$timing_of_survey==1)
t3 <- subset(quest7,quest7$timing_of_survey==3)

#Adding covariates from med_hist data to the questionaiire data
vars <- c(1,4,6,7,10,13,17,19,21,9,18,26,32)
med_hist2 <- med_hist[,vars]
names(med_hist2) <- c("ID", "Gender", "Age_cont", "Age_Group", "prior_brain_radiation", "radiation_type",
                      "prior_resection", "tumor_histology", "histology", "KPS", 
                      "concurrent_chemo", "tumor_location", "cancer_still_affecting" )

#which patients have a baseline,t1,t3 response but do not appear in medical history data
setdiff(baseline$ID, med_hist2$ID)
setdiff(t1$ID, med_hist2$ID)
setdiff(t3$ID, med_hist2$ID)

#remove these from questionaire data
baseline <- baseline[-which(baseline$ID==69 | baseline$ID==149),]
t1 <- t1[-which(t1$ID==69 | t1$ID==83 | t1$ID==100),]

#check that all baseline,t1,t3 responders are in medical data
setdiff(baseline$ID, med_hist2$ID)
setdiff(t1$ID, med_hist2$ID)
setdiff(t3$ID, med_hist2$ID)

#removing patients (IDs) from medical history data that do not appear in the final questionaire data

med_hist3 <- med_hist2[which(med_hist2$ID %in% baseline$ID),]
all.equal(med_hist3$ID,baseline$ID)

#Removing patients with radiation type=4 or 5
which(med_hist3$radiation_type==4 | med_hist3$radiation_type==5)
med_hist4 <- med_hist3[-which(med_hist3$radiation_type==4 | med_hist3$radiation_type==5),]

med_hist3$ID[which(med_hist3$radiation_type==4 | med_hist3$radiation_type==5)]
baseline <- baseline[-which(baseline$ID %in% med_hist3$ID[which(med_hist3$radiation_type==4 | med_hist3$radiation_type==5)]),]
t1 <- t1[-which(t1$ID %in% med_hist3$ID[which(med_hist3$radiation_type==4 | med_hist3$radiation_type==5)])]
#no patients match in t3 data

#fixing the variable classes
med_hist4$Gender <- ifelse(med_hist4$Gender==1,0,1)
med_hist4$Gender <- factor(med_hist4$Gender)

med_hist4$Age_Group <- ifelse(med_hist4$Age_Group==1,0,1)
med_hist4$Age_Group <- factor(med_hist4$Age_Group)

med_hist4$prior_brain_radiation <- ifelse(grepl("o",med_hist4$prior_brain_radiation),0,1)
med_hist4$prior_brain_radiation <- factor(med_hist4$prior_brain_radiation)

table(med_hist4$radiation_type)
med_hist4$radiation_type <- factor(med_hist4$radiation_type)

med_hist4$prior_resection <- ifelse(grepl("o",med_hist4$prior_resection),0,1)
med_hist4$prior_resection <- factor(med_hist4$prior_resection)

med_hist4$histology <- ifelse(med_hist4$histology==5,0,1)
med_hist4$histology <- factor(med_hist4$histology)

med_hist4$concurrent_chemo <- ifelse(grepl("o",med_hist4$concurrent_chemo),0,1)
med_hist4$concurrent_chemo <- factor(med_hist4$concurrent_chemo)

med_hist4$cancer_still_affecting <- ifelse(grepl("o",med_hist4$cancer_still_affecting),0,1)
med_hist4$cancer_still_affecting <- factor(med_hist4$cancer_still_affecting)

#dropping tumor_histology and tumor_location because there are too many subgroups
names(med_hist4)
med_hist4 <- med_hist4[,-c(8,12)]

str(med_hist4)

#Adding medical history to each subset of the questionaire data

base_med <- merge(baseline,med_hist4, by="ID")
t1_med <- merge(t1,med_hist4, by="ID")
t3_med <- merge(t3,med_hist4, by="ID")

#seeing if any of the data have missing values
which(rowSums(is.na(base_med))!=0)
which(rowSums(is.na(t1_med))!=0)
which(rowSums(is.na(t3_med))!=0)

#remove missing values from base_med data
base_med <- base_med[-which(rowSums(is.na(base_med))!=0),]

###################################################################

#Dataset for complete cases
base_med_comp <- base_med[which(base_med$ID %in% intersect(intersect(base_med$ID,t1$ID),t3$ID)),]
t1_med_comp <- t1_med[which(t1_med$ID %in%intersect(intersect(base_med$ID,t1$ID),t3$ID)),]

completed <- rbind.data.frame(base_med_comp, t1_med_comp, t3_med)
completed <- completed[order(completed$ID),]
dim(completed)
head(completed)

saveRDS(completed, "completed.RDS")
##################################################################

#Dataset for patients that only have baseline and 1 month response
base_med_2time <- base_med[which(base_med$ID %in% setdiff(t1_med$ID,t3_med$ID)),]
t1_med_2time <- t1_med[which(t1_med$ID %in% setdiff(t1_med$ID,t3_med$ID)),]
two_time_pts <- rbind.data.frame(base_med_2time,t1_med_2time)

two_time_pts <- two_time_pts[order(two_time_pts$ID),]
dim(two_time_pts)
head(two_time_pts)

saveRDS(two_time_pts, "two_time_pts.RDS")

#################################################################

#Dataset for patients that only have baseline
intersect(setdiff(base_med$ID,t1$ID),setdiff(base_med$ID,t3$ID))
baseline_pt <- base_med[which(base_med$ID %in% intersect(setdiff(base_med$ID,t1$ID),setdiff(base_med$ID,t3$ID))),]
dim(baseline_pt)
head(baseline_pt)

saveRDS(baseline_pt, "baseline_pt.RDS")









