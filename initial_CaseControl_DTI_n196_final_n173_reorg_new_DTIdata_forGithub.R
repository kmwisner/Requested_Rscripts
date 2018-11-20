##### This script describes analyses completed for a paper submitted to Brain Behavior Imaging by Krista Wisner
##### Script deposited on GitHub per request by the reviewer

##### Script analyzes behavioral variables and DTI FA values and their relationships 
##### Behavioral variables include those resulting from stress tasks, affect assessment before and after tasks (panas), symptoms, demos

## stress tests are PASAT (math test) and MTPT (star tracing) - order of completion is randomized - completed on the same day
## see publication for more details

###########################################################################################################
###########################################################################################################
###########################################################################################################

dat <- read.csv(file.choose(),header=T)
datt <- dat[1:196,] ## remove extra rows
dattt <- datt[1:140] ## remove extra columns
dat <- dattt
remove(datt)
remove(dattt)

## for both stress tasks, not using first set of practice data
## for math(pasat) task this is set "1" ... using sets: "2" "2x" "3"
## for star(mtpt) task this is set "easy" ... using sets: "medium" "hard" "last"
## for each task - last set participants can quit : for math: "3" for star "last" ... used to assign quit/noquit

## using entire sample

## making MATH (pasat) variable

dat$PasatTotalErrors2 <- dat$Pasatwrong2+dat$PasatNA2
dat$PasatTotalAll2 <- dat$PasatScore2+dat$Pasatwrong2+dat$PasatNA2
dat$PasatPerctErrors2 <- dat$PasatTotalErrors2/dat$PasatTotalAll2

dat$PasatTotalErrors2x <- dat$Pasatwrong2x+dat$PasatNA2x
dat$PasatTotalAll2x <- dat$PasatScore2x+dat$Pasatwrong2x+dat$PasatNA2x
dat$PasatPerctErrors2x <- dat$PasatTotalErrors2x/dat$PasatTotalAll2x

dat$PasatTotalErrors3 <- dat$Pasatwrong3+dat$PasatNA3
dat$PasatTotalAll3 <- dat$PasatScore3+dat$Pasatwrong3+dat$PasatNA3
dat$PasatPerctErrors3 <- dat$PasatTotalErrors3/dat$PasatTotalAll3
dat$PasatErrorPerSecLast <- dat$PasatTotalErrors3/dat$PasatQuittime ## new

dat$PasatAcross3setsTotalErrors <- dat$PasatTotalErrors2+dat$PasatTotalErrors2x+dat$PasatTotalErrors3
dat$PasatAcross3setsTotalAll <- dat$PasatTotalAll2+dat$PasatTotalAll2x+dat$PasatTotalAll3
dat$PasatAcross3setsPerctErrors <- dat$PasatAcross3setsTotalErrors/dat$PasatAcross3setsTotalAll

dat$PasatPersistPerctTime <- dat$PasatQuittime/420

## making STAR (mtpt) variable

dat$MtptTotalErrorsMed <- dat$TErrorMedium
dat$MtptTotalAllMed <- dat$TDistanceMedium+dat$TErrorMedium
dat$MtptPerctErrorsMed <- dat$MtptTotalErrorsMed/dat$MtptTotalAllMed
dat$MtptErrorsPerSecMed <- dat$TErrorMedium/(dat$TTimeMedium/1000)

dat$MtptTotalErrorsHard <- dat$TErrorHard
dat$MtptTotalAllHard <- dat$TDistanceHard+dat$TErrorHard
dat$MtptPerctErrorsHard <- dat$MtptTotalErrorsHard/dat$MtptTotalAllHard
dat$MtptErrorsPerSecHard <- dat$TErrorHard/(dat$TTimeHard/1000)

dat$MtptTotalErrorsLast <- dat$TErrorLast
dat$MtptTotalAllLast <- dat$TDistanceLast+dat$TErrorLast
dat$MtptPerctErrorsLast <- dat$MtptTotalErrorsLast/dat$MtptTotalAllLast
dat$MtptErrorsPerSecLast <- dat$TErrorLast/(dat$TTimeLast/1000)

dat$MtptAcross3setsTotalErrors <- dat$MtptTotalErrorsMed+dat$MtptTotalErrorsHard+dat$MtptTotalErrorsLast
dat$MtptAcross3setsTotalAll <- dat$MtptTotalAllMed+dat$MtptTotalAllHard+dat$MtptTotalAllLast
dat$MtptAcross3setsPerctErrors <- dat$MtptAcross3setsTotalErrors/dat$MtptAcross3setsTotalAll

dat$TTimeLastSec <- dat$TTimeLast/1000
dat$MtptPersistPerctTime <- dat$TTimeLastSec/420

# combining across tasks - last section (when can quit) variables
dat$Avg2tasksErrorsPerSecLast <- (dat$MtptErrorsPerSecLast + dat$PasatErrorPerSecLast)/2
dat$Avg2tasksQuittime <- ((dat$TTimeLast/1000) + dat$PasatQuittime)/2
# these are the two main variables for performance aside from quit vs. noquit categorization below

######################################################################################################
### continue here - to add in other data

### reduce the number of variables in stress datasheet - to only those necessary
dat_1 <- data.frame(dat$C_subjID,dat$NonSmoker,dat$Age,dat$Gender,dat$CaseControlCode,dat$pt_type_all,dat$Str_subjID,dat$MathQuit2,dat$MathQuitTimeFinal,dat$PasatPersistPerctTime,dat$PasatAcross3setsPerctErrors,dat$PasatErrorPerSecLast,dat$StarQuit2,dat$StarQuitTimeFinal,dat$MtptPersistPerctTime,dat$MtptAcross3setsPerctErrors,dat$MtptErrorsPerSecLast,dat$QuitSum,dat$QuitTimeSum,dat$Avg2tasksErrorsPerSecLast,dat$Avg2tasksQuittime)
dat_1names <- c("C_subjID","NonSmoker","Age","Gender","CaseControlCode","pt_type_all","ST_subjID","MathQuit2","MathQuitTime","MathPersistPerctTime","MathAcross3setsPerctErrors","MathErrorsPerSecLast","StarQuit2","StarQuitTime","StarPersistPerctTime","StarAcross3setsPerctErrors","StarErrorsPerSecLast","QuitSum","QuitTimeSum","Avg2tasksErrorsPerSecLast","Avg2tasksQuittime")
colnames(dat_1) <- dat_1names

## isolate panas data for later
dat_panas <- dat[61:121]

# "QuitSum" is variable made in spreadsheet, 0,1,2, indicating number of tasks quit early 
## make binary quit variable ... 0 vs. quit at least 1x (1 or 2)
dat_1$binaryQuit1 <- ifelse(dat_1$QuitSum>0,1,0)

cog <- read.csv(file.choose(),header=T)
cog <- cog[1:4]

## merge variables sheets
stresscog <- merge(dat_1,cog,by.x="C_subjID",by.y="subjIDcog") 

###################################################################################################
## adding DTI - to combine stress and cog data with DTI to make final dataset

#####  DTI FA DATA

dti <- read.csv(file.choose(),header=T,na.strings = "NA")
class(dti$AverageFA) # confirm data read in as numeric now
mean(dti$AverageFA)

## isolate FA values from subject IDs and scale all FA values by dividing them by 10000 to scale
dti_temp <- dti[4:17]
dti_temp2 <- dti_temp/10000
dti_scaled <- data.frame(dti[1:3],dti_temp2)
remove(dti_temp)
remove(dti_temp2)

dat_5b <- merge(stresscog,dti_scaled,by.x="C_subjID",by.y="SID")
dat_5 <- dat_5b
remove(dat_5b)

#############################################################################################################################
############## clean data based on behav

## data from "dat_5" was exported and assessed for the following
# inappropriate star quit (was doing well)
# inappropriate math quit (was doing well)
# missing required variables for analysis

# using row deletion, removed subjs missing required data or showing invalid performance
# resulting file was "dat_6_cleanboth"
# this was now the final dataset - confirmed SZ HV groups still matched for age sex smoking status - yes they were

###################################################################################################
############## continue with this finalized dataset

dat_6_cleanboth_backup <- dat_6_cleanboth
## so have copy of orig in case need to reload/remerge


########### add in spreadsheet with more demos meds and functioning

demmedfunc <- read.csv(file.choose(),header=T)

dat_6_cleanboth_more <- merge(dat_6_cleanboth,demmedfunc,by.x="C_subjID",by.y="subjID")

## also just link ID columns with demfuncmeds so can look at medication stuff more closely
n173_ids <- dat_6_cleanboth[1:6]
medsonly <- data.frame(demmedfunc[1],demmedfunc[15:22])
meds_more <- merge(n173_ids,medsonly,by.x="C_subjID",by.y="subjID")
meds_more_pts <- meds_more[meds_more$CaseControlCode==1,]
meds_more_hvs <- meds_more[meds_more$CaseControlCode==2,]
table(meds_more_pts$antipsychotic_type.) 


## so rename dat_6_cleanboth_more as dat_6_cleanboth so can use it in previous code
## now that have dat_6_cleanboth_backup of original just in case
dat_6_cleanboth <- dat_6_cleanboth_more
remove(dat_6_cleanboth_more)


# make cog factor for dataset
dat_6_cleanboth$DigitSequenRaw_z <- scale(dat_6_cleanboth$Digit_Sequencing_Raw)
dat_6_cleanboth$DigitSymbolCodRaw_z <- scale(dat_6_cleanboth$Digit_Symbol_Coding_Raw)
dat_6_cleanboth$CogCombo_z <- (dat_6_cleanboth$DigitSequenRaw_z + dat_6_cleanboth$DigitSymbolCodRaw_z)/2
mean(dat_6_cleanboth$CogCombo_z)
sd(dat_6_cleanboth$CogCombo_z)


################## adding PANAS variables back in - dataset now # dat_6_cleanboth_p3

# dat_panas ## 

dat_6_cleanboth_p3 <- merge(dat_6_cleanboth,dat_panas,by.x="C_subjID",by.y="R_subjID")

## make an average of the negative affect variables 

dat_6_cleanboth_p3$TotalNegAffPre <- dat_6_cleanboth_p3$AfraidPre+dat_6_cleanboth_p3$ScaredPre+dat_6_cleanboth_p3$NervousPre+dat_6_cleanboth_p3$JitteryPre+dat_6_cleanboth_p3$IrritablePre+dat_6_cleanboth_p3$HostilePre+dat_6_cleanboth_p3$GuiltyPre+dat_6_cleanboth_p3$AshamedPre+dat_6_cleanboth_p3$UpsetPre+dat_6_cleanboth_p3$DistressedPre
dat_6_cleanboth_p3$TotalNegAffFinal <- dat_6_cleanboth_p3$AfraidFinal+dat_6_cleanboth_p3$ScaredFinal+dat_6_cleanboth_p3$NervousFinal+dat_6_cleanboth_p3$JitteryFinal+dat_6_cleanboth_p3$IrritableFinal+dat_6_cleanboth_p3$HostileFinal+dat_6_cleanboth_p3$GuiltyFinal+dat_6_cleanboth_p3$AshamedFinal+dat_6_cleanboth_p3$UpsetFinal+dat_6_cleanboth_p3$DistressedFinal

#### add in numeric/factor version of binary covariates before split
dat_6_cleanboth_p3$Gender1 <- ifelse(dat_6_cleanboth_p3$Gender=="Male",1,0)
dat_6_cleanboth_p3$NonSmoker1 <- ifelse(dat_6_cleanboth_p3$NonSmoker=="Yes",1,0)
class(dat_6_cleanboth_p3$Gender1)
class(dat_6_cleanboth_p3$NonSmoker1)


########
# remove datasheets dont need at this point
remove(cog)
remove(dat)
remove(dat_1)
remove(dat_panas)
remove(stresscog)
remove(demmedfunc)
remove(dti)
remove(dti_scaled)
remove(dat_6_cleanboth)

######################################################################################################
################## dont split the data yet, do TRANSFORMS first
### check distributions at this point - try to make all in normal distribution with transforms - or specify to keep raw

## different transforms to improve variable distributions tested, and best used here for each

dat_6_cleanboth_p3_T <- dat_6_cleanboth_p3

dat_6_cleanboth_p3_T$Avg2tasksErrorsPerSecLast_T <- dat_6_cleanboth_p3_T$Avg2tasksErrorsPerSecLast^(1/4)
hist(dat_6_cleanboth_p3_T$Avg2tasksErrorsPerSecLast)
hist(dat_6_cleanboth_p3_T$Avg2tasksErrorsPerSecLast_T)
shapiro.test(dat_6_cleanboth_p3_T$Avg2tasksErrorsPerSecLast)
shapiro.test(dat_6_cleanboth_p3_T$Avg2tasksErrorsPerSecLast_T)

dat_6_cleanboth_p3_T$Avg2tasksQuittime_T <- dat_6_cleanboth_p3_T$Avg2tasksQuittime^(2)
hist(dat_6_cleanboth_p3_T$Avg2tasksQuittime)
hist(dat_6_cleanboth_p3_T$Avg2tasksQuittime_T)
shapiro.test(dat_6_cleanboth_p3_T$Avg2tasksQuittime)
shapiro.test(dat_6_cleanboth_p3_T$Avg2tasksQuittime_T)

dat_6_cleanboth_p3_T$CGC_T <- dat_6_cleanboth_p3_T$CGC^(3)
hist(dat_6_cleanboth_p3_T$CGC)
hist(dat_6_cleanboth_p3_T$CGC_T)
shapiro.test(dat_6_cleanboth_p3_T$CGC)
shapiro.test(dat_6_cleanboth_p3_T$CGC_T)

dat_6_cleanboth_p3_T$CGC_L_T <- dat_6_cleanboth_p3_T$CGC_L^(3)
hist(dat_6_cleanboth_p3_T$CGC_L)
hist(dat_6_cleanboth_p3_T$CGC_L_T)
shapiro.test(dat_6_cleanboth_p3_T$CGC_L)
shapiro.test(dat_6_cleanboth_p3_T$CGC_L_T)

dat_6_cleanboth_p3_T$CGC_R_T <- dat_6_cleanboth_p3_T$CGC_R^(3)
hist(dat_6_cleanboth_p3_T$CGC_R)
hist(dat_6_cleanboth_p3_T$CGC_R_T)
shapiro.test(dat_6_cleanboth_p3_T$CGC_R)
shapiro.test(dat_6_cleanboth_p3_T$CGC_R_T)

dat_6_cleanboth_p3_T$CGH_T <- dat_6_cleanboth_p3_T$CGH^(3)
hist(dat_6_cleanboth_p3_T$CGH)
hist(dat_6_cleanboth_p3_T$CGH_T)
shapiro.test(dat_6_cleanboth_p3_T$CGH)
shapiro.test(dat_6_cleanboth_p3_T$CGH_T)

dat_6_cleanboth_p3_T$CGH_L_T <- dat_6_cleanboth_p3_T$CGH_L^(3)
hist(dat_6_cleanboth_p3_T$CGH_L)
hist(dat_6_cleanboth_p3_T$CGH_L_T)
shapiro.test(dat_6_cleanboth_p3_T$CGH_L)
shapiro.test(dat_6_cleanboth_p3_T$CGH_L_T)

dat_6_cleanboth_p3_T$CGH_R_T <- dat_6_cleanboth_p3_T$CGH_R^(3)
hist(dat_6_cleanboth_p3_T$CGH_R)
hist(dat_6_cleanboth_p3_T$CGH_R_T)
shapiro.test(dat_6_cleanboth_p3_T$CGH_R)
shapiro.test(dat_6_cleanboth_p3_T$CGH_R_T)

dat_6_cleanboth_p3_T$FX 
shapiro.test(dat_6_cleanboth_p3_T$FX) ## raw is good

dat_6_cleanboth_p3_T$UNC 
shapiro.test(dat_6_cleanboth_p3_T$UNC)  ## raw is good
shapiro.test(dat_6_cleanboth_p3_T$UNC_L) ## raw is good
shapiro.test(dat_6_cleanboth_p3_T$UNC_R) ## raw is good

dat_6_cleanboth_p3_T$AverageFA_T <- dat_6_cleanboth_p3_T$AverageFA^(4)
hist(dat_6_cleanboth_p3_T$AverageFA)
hist(dat_6_cleanboth_p3_T$AverageFA_T)
shapiro.test(dat_6_cleanboth_p3_T$AverageFA)
shapiro.test(dat_6_cleanboth_p3_T$AverageFA_T)

dat_6_cleanboth_p3_T$Age 
shapiro.test(dat_6_cleanboth_p3_T$Age) ## raw is good

dat_6_cleanboth_p3_T$CogCombo_z 
shapiro.test(dat_6_cleanboth_p3_T$CogCombo_z) ## raw is good

dat_6_cleanboth_p3_T$UPSA_scaled_Total_mod_T <- dat_6_cleanboth_p3_T$UPSA_scaled_Total_mod^(4)
hist(dat_6_cleanboth_p3_T$UPSA_scaled_Total_mod)
hist(dat_6_cleanboth_p3_T$UPSA_scaled_Total_mod_T)
shapiro.test(dat_6_cleanboth_p3_T$UPSA_scaled_Total_mod)
shapiro.test(dat_6_cleanboth_p3_T$UPSA_scaled_Total_mod_T)


# transform wont work for affect variables
# used non-parametric analyses on these

# transform wont work for symptom/CPZ variables
# used non-parametric analyses on these


#### check other variables that are factors
table(dat_6_cleanboth_p3_T$NonSmoker)  # yes,no >> factor
table(dat_6_cleanboth_p3_T$NonSmoker1) # 0,1: 1 = non-smoker >> numeric

table(dat_6_cleanboth_p3_T$Gender)  # female, male >> factor
table(dat_6_cleanboth_p3_T$Gender1) # 0,1: 1 = male >> numeric

table(dat_6_cleanboth_p3_T$binaryQuit1) # 0,1: 1 = quit one or both >> numeric

table(dat_6_cleanboth_p3_T$CaseControlCode) # 1,2: 1 = sz >> integer

## need to make only 1 and 0
dat_6_cleanboth_p3_T$CaseControlCode1 <- ifelse(dat_6_cleanboth_p3_T$CaseControlCode==1,1,0)
table(dat_6_cleanboth_p3_T$CaseControlCode1) # 0,1: 1 = sz >> numeric

### when want a factor, just use as.factor() 

##### now have all transformed variables needed



### parse the data into the required subgroups
pts_p3dat6_cb_T <- dat_6_cleanboth_p3_T[dat_6_cleanboth_p3_T$CaseControlCode==1,] 
hvs_p3dat6_cb_T <- dat_6_cleanboth_p3_T[dat_6_cleanboth_p3_T$CaseControlCode==2,] 

## make cog factor for SZ and HV separately

pts_p3dat6_cb_T$DigitSequenRaw_z_subgroup <- scale(pts_p3dat6_cb_T$Digit_Sequencing_Raw)
pts_p3dat6_cb_T$DigitSymbolCodRaw_z_subgroup <- scale(pts_p3dat6_cb_T$Digit_Symbol_Coding_Raw)
pts_p3dat6_cb_T$CogCombo_z_subgroup <- (pts_p3dat6_cb_T$DigitSequenRaw_z_subgroup + pts_p3dat6_cb_T$DigitSymbolCodRaw_z_subgroup)/2
mean(pts_p3dat6_cb_T$CogCombo_z_subgroup)
sd(pts_p3dat6_cb_T$CogCombo_z_subgroup)

hvs_p3dat6_cb_T$DigitSequenRaw_z_subgroup <- scale(hvs_p3dat6_cb_T$Digit_Sequencing_Raw)
hvs_p3dat6_cb_T$DigitSymbolCodRaw_z_subgroup <- scale(hvs_p3dat6_cb_T$Digit_Symbol_Coding_Raw)
hvs_p3dat6_cb_T$CogCombo_z_subgroup <- (hvs_p3dat6_cb_T$DigitSequenRaw_z_subgroup + hvs_p3dat6_cb_T$DigitSymbolCodRaw_z_subgroup)/2
mean(hvs_p3dat6_cb_T$CogCombo_z_subgroup)
sd(hvs_p3dat6_cb_T$CogCombo_z_subgroup)


### making subgroups based on binary quit
pts6p3_cb_T_bQ1_0 <- pts_p3dat6_cb_T[pts_p3dat6_cb_T$binaryQuit1==0,]
pts6p3_cb_T_bQ1_1 <- pts_p3dat6_cb_T[pts_p3dat6_cb_T$binaryQuit1==1,]
hvs6p3_cb_T_bQ1_0 <- hvs_p3dat6_cb_T[hvs_p3dat6_cb_T$binaryQuit1==0,]
hvs6p3_cb_T_bQ1_1 <- hvs_p3dat6_cb_T[hvs_p3dat6_cb_T$binaryQuit1==1,]


###################################################################################################
######  packages may be used in script
library(car)
library(ggplot2)
library(effsize)
library(ppcor)
library(lmtest) 
library(caret)
library(MASS)
library(plyr)
library(orddom) 


###################################################################################################

################## demo analyses - did not transform age or cog
##################

## quit chi-sq tables and analyses
table(dat_6_cleanboth_p3_T$CaseControlCode,dat_6_cleanboth_p3_T$binaryQuit1)
binaryquit1_2x2 <- matrix(c(17,61,42,53), nrow = 2, ncol = 2, byrow = TRUE)
chisq.test(binaryquit1_2x2) 

## gender - pts vs. hvs
table(dat_6_cleanboth_p3_T$CaseControlCode,dat_6_cleanboth_p3_T$Gender)
gender_2x2 <- matrix(c(22,56,40,55), nrow = 2, ncol = 2, byrow = TRUE)
chisq.test(gender_2x2) 
## gender - in pts - quit diff
table(pts_p3dat6_cb_T$binaryQuit1,pts_p3dat6_cb_T$Gender)
gender_2x2_pts <- matrix(c(8,9,14,47), nrow = 2, ncol = 2, byrow = TRUE)
chisq.test(gender_2x2_pts) 
## gender - in hvs - quit diff
table(hvs_p3dat6_cb_T$binaryQuit1,hvs_p3dat6_cb_T$Gender)
gender_2x2_hvs <- matrix(c(23,19,17,36), nrow = 2, ncol = 2, byrow = TRUE)
chisq.test(gender_2x2_hvs)

## smoking - pts vs. hvs
table(dat_6_cleanboth_p3_T$CaseControlCode,dat_6_cleanboth_p3_T$NonSmoker)
smoking_2x2 <- matrix(c(35,43,28,67), nrow = 2, ncol = 2, byrow = TRUE)
chisq.test(smoking_2x2) 
## smoking - in pts - quit diff
table(pts_p3dat6_cb_T$binaryQuit1,pts_p3dat6_cb_T$NonSmoker)
smoking_2x2_pts <- matrix(c(4,13,31,30), nrow = 2, ncol = 2, byrow = TRUE)
chisq.test(smoking_2x2_pts) 
## smoking - in hvs - quit diff
table(hvs_p3dat6_cb_T$binaryQuit1,hvs_p3dat6_cb_T$NonSmoker)
smoking_2x2_hvs <- matrix(c(11,31,17,36), nrow = 2, ncol = 2, byrow = TRUE)
chisq.test(smoking_2x2_hvs) 

## age
t.test(pts_p3dat6_cb_T$Age,hvs_p3dat6_cb_T$Age,var.equal=T) 
mean(pts_p3dat6_cb_T$Age)
sd(pts_p3dat6_cb_T$Age)
mean(hvs_p3dat6_cb_T$Age)
sd(hvs_p3dat6_cb_T$Age)
# sz - quit diff
t.test(pts6p3_cb_T_bQ1_0$Age,pts6p3_cb_T_bQ1_1$Age,var.equal=T) 
# hv - quit diff
t.test(hvs6p3_cb_T_bQ1_0$Age,hvs6p3_cb_T_bQ1_1$Age,var.equal=T) 

## other tests for demo table

## cog 
t.test(pts_p3dat6_cb_T$CogCombo_z,hvs_p3dat6_cb_T$CogCombo_z,var.equal=T) 
# sz - quit diff
t.test(pts6p3_cb_T_bQ1_0$CogCombo_z,pts6p3_cb_T_bQ1_1$CogCombo_z,var.equal=T)
# hv - quit diff
t.test(hvs6p3_cb_T_bQ1_0$CogCombo_z,hvs6p3_cb_T_bQ1_1$CogCombo_z,var.equal=T) 

## UPSA - pts vs. hvs - use UPSA_scaled_Total_mod 
t.test(pts_p3dat6_cb_T$UPSA_scaled_Total_mod_T,hvs_p3dat6_cb_T$UPSA_scaled_Total_mod_T,var.equal=T) 
# sz - quit diff
t.test(pts6p3_cb_T_bQ1_0$UPSA_scaled_Total_mod_T,pts6p3_cb_T_bQ1_1$UPSA_scaled_Total_mod_T,var.equal=T) 
# hv - quit diff
t.test(hvs6p3_cb_T_bQ1_0$UPSA_scaled_Total_mod_T,hvs6p3_cb_T_bQ1_1$UPSA_scaled_Total_mod_T,var.equal=T) 

### CPZ - in pts only - quit diff
wilcox.test(pts6p3_cb_T_bQ1_0$CPZ,pts6p3_cb_T_bQ1_1$CPZ) 


### see symptoms area down below for test of symptom diffs across quit groups in SZ for demos tables



################## basic affect analyses
##################

## note additional analyses in here that are not reported ... completed to clarify

## total negative affect ... whole sample
mean(dat_6_cleanboth_p3_T$TotalNegAffFinal)
sd(dat_6_cleanboth_p3_T$TotalNegAffFinal)
mean(dat_6_cleanboth_p3_T$TotalNegAffPre)
sd(dat_6_cleanboth_p3_T$TotalNegAffPre)
## whole sample - paired to assess change
wilcox.test(dat_6_cleanboth_p3_T$TotalNegAffFinal,dat_6_cleanboth_p3_T$TotalNegAffPre,paired=T) 

##### bargraph for visualizing ... get means and sd for each item ... whole sample
## for now just put in excel to make initial graph of affect variables
# means list
panas_neg_means <- c(mean(dat_6_cleanboth_p3_T$AfraidFinal),mean(dat_6_cleanboth_p3_T$ScaredFinal),mean(dat_6_cleanboth_p3_T$NervousFinal),mean(dat_6_cleanboth_p3_T$JitteryFinal),mean(dat_6_cleanboth_p3_T$IrritableFinal),mean(dat_6_cleanboth_p3_T$HostileFinal),mean(dat_6_cleanboth_p3_T$GuiltyFinal),mean(dat_6_cleanboth_p3_T$AshamedFinal),mean(dat_6_cleanboth_p3_T$UpsetFinal),mean(dat_6_cleanboth_p3_T$DistressedFinal))
panas_neg_names <- c("AfraidFinal_M","ScaredFinal_M","NervousFinal_M","JitteryFinal_M","IrritableFinal_M","HostileFinal_M","GuiltyFinal_M","AshamedFinal_M","UpsetFinal_M","DistressedFinal_M")
panas_neg_means_pre <- c(mean(dat_6_cleanboth_p3_T$AfraidPre),mean(dat_6_cleanboth_p3_T$ScaredPre),mean(dat_6_cleanboth_p3_T$NervousPre),mean(dat_6_cleanboth_p3_T$JitteryPre),mean(dat_6_cleanboth_p3_T$IrritablePre),mean(dat_6_cleanboth_p3_T$HostilePre),mean(dat_6_cleanboth_p3_T$GuiltyPre),mean(dat_6_cleanboth_p3_T$AshamedPre),mean(dat_6_cleanboth_p3_T$UpsetPre),mean(dat_6_cleanboth_p3_T$DistressedPre))
panas_neg_names_pre <- c("AfraidPre_M","ScaredPre_M","NervousPre_M","JitteryPre_M","IrritablePre_M","HostilePre_M","GuiltyPre_M","AshamedPre_M","UpsetPre_M","DistressedPre_M")
panas_neg_means_2 <- cbind(panas_neg_names,panas_neg_means,panas_neg_names_pre,panas_neg_means_pre)
colnames(panas_neg_means_2) <- c("Measure","Value","Measure","Value")
panas_neg_means_2 <- data.frame(panas_neg_means_2)
# std errors list
panas_neg_stder <- c(sd(dat_6_cleanboth_p3_T$AfraidFinal)/sqrt(length(dat_6_cleanboth_p3_T$AfraidFinal)),sd(dat_6_cleanboth_p3_T$ScaredFinal)/sqrt(length(dat_6_cleanboth_p3_T$ScaredFinal)),sd(dat_6_cleanboth_p3_T$NervousFinal)/sqrt(length(dat_6_cleanboth_p3_T$NervousFinal)),sd(dat_6_cleanboth_p3_T$JitteryFinal)/sqrt(length(dat_6_cleanboth_p3_T$JitteryFinal)),sd(dat_6_cleanboth_p3_T$IrritableFinal)/sqrt(length(dat_6_cleanboth_p3_T$IrritableFinal)),sd(dat_6_cleanboth_p3_T$HostileFinal)/sqrt(length(dat_6_cleanboth_p3_T$HostileFinal)),sd(dat_6_cleanboth_p3_T$GuiltyFinal)/sqrt(length(dat_6_cleanboth_p3_T$GuiltyFinal)),sd(dat_6_cleanboth_p3_T$AshamedFinal)/sqrt(length(dat_6_cleanboth_p3_T$AshamedFinal)),sd(dat_6_cleanboth_p3_T$UpsetFinal)/sqrt(length(dat_6_cleanboth_p3_T$UpsetFinal)),sd(dat_6_cleanboth_p3_T$DistressedFinal)/sqrt(length(dat_6_cleanboth_p3_T$DistressedFinal)))
panas_neg_names <- c("AfraidFinal_stderr","ScaredFinal_stderr","NervousFinal_stderr","JitteryFinal_stderr","IrritableFinal_stderr","HostileFinal_stderr","GuiltyFinal_stderr","AshamedFinal_stderr","Upset_Final_stderr","Distressed_Final_stderr")
panas_neg_stder_pre <- c(sd(dat_6_cleanboth_p3_T$AfraidPre)/sqrt(length(dat_6_cleanboth_p3_T$AfraidPre)),sd(dat_6_cleanboth_p3_T$ScaredPre)/sqrt(length(dat_6_cleanboth_p3_T$ScaredPre)),sd(dat_6_cleanboth_p3_T$NervousPre)/sqrt(length(dat_6_cleanboth_p3_T$NervousPre)),sd(dat_6_cleanboth_p3_T$JitteryPre)/sqrt(length(dat_6_cleanboth_p3_T$JitteryPre)),sd(dat_6_cleanboth_p3_T$IrritablePre)/sqrt(length(dat_6_cleanboth_p3_T$IrritablePre)),sd(dat_6_cleanboth_p3_T$HostilePre)/sqrt(length(dat_6_cleanboth_p3_T$HostilePre)),sd(dat_6_cleanboth_p3_T$GuiltyPre)/sqrt(length(dat_6_cleanboth_p3_T$GuiltyPre)),sd(dat_6_cleanboth_p3_T$AshamedPre)/sqrt(length(dat_6_cleanboth_p3_T$AshamedPre)),sd(dat_6_cleanboth_p3_T$UpsetPre)/sqrt(length(dat_6_cleanboth_p3_T$UpsetPre)),sd(dat_6_cleanboth_p3_T$DistressedPre)/sqrt(length(dat_6_cleanboth_p3_T$DistressedPre)))
panas_neg_names_pre <- c("AfraidPre_stderr","ScaredPre_stderr","NervousPre_stderr","JitteryPre_stderr","IrritablePre_stderr","HostilePre_stderr","GuiltyPre_stderr","AshamedPre_stderr","UpsetPre_stderr","DistressedPre_stderr")
panas_neg_stder_2 <- cbind(panas_neg_names,panas_neg_stder,panas_neg_names_pre,panas_neg_stder_pre)
colnames(panas_neg_stder_2) <- c("Measure","Value","Measure","Value")
panas_neg_stder_2 <- data.frame(panas_neg_stder_2)
######

## whole sample - pre vs. final - individual neg affect items
## using wilcox paired tests - have final first to have positive T
wilcox.test(dat_6_cleanboth_p3_T$NervousFinal,dat_6_cleanboth_p3_T$NervousPre,paired=T) 
wilcox.test(dat_6_cleanboth_p3_T$GuiltyFinal,dat_6_cleanboth_p3_T$GuiltyPre,paired=T) 
wilcox.test(dat_6_cleanboth_p3_T$ScaredFinal,dat_6_cleanboth_p3_T$ScaredPre,paired=T) 
wilcox.test(dat_6_cleanboth_p3_T$AfraidFinal,dat_6_cleanboth_p3_T$AfraidPre,paired=T) 
wilcox.test(dat_6_cleanboth_p3_T$JitteryFinal,dat_6_cleanboth_p3_T$JitteryPre,paired=T) 
wilcox.test(dat_6_cleanboth_p3_T$AshamedFinal,dat_6_cleanboth_p3_T$AshamedPre,paired=T) 
wilcox.test(dat_6_cleanboth_p3_T$HostileFinal,dat_6_cleanboth_p3_T$HostilePre,paired=T) 
wilcox.test(dat_6_cleanboth_p3_T$DistressedFinal,dat_6_cleanboth_p3_T$DistressedPre,paired=T)
wilcox.test(dat_6_cleanboth_p3_T$UpsetFinal,dat_6_cleanboth_p3_T$UpsetPre,paired=T) 
wilcox.test(dat_6_cleanboth_p3_T$IrritableFinal,dat_6_cleanboth_p3_T$IrritablePre,paired=T) 
## 10 tests, 0.05/10 = 0.005 ... several of these pass that

## look at effect sizes instead to help narrow - whole sample
## CANNOT use cohen D for effect size to do normality of distribution violation >>> use Cliffs Delta 
## + or - depends only on which vector you entered first into the calculations - have pre first for postive effect size
## For Cliff's Delta (absolute value) small effect size  around .147, a medium effect size around .33, and a large effect size around .474
## use $dw value from dmes() output for within paired comparisons
dmes(dat_6_cleanboth_p3_T$NervousPre,dat_6_cleanboth_p3_T$NervousFinal) 
dmes(dat_6_cleanboth_p3_T$GuiltyPre,dat_6_cleanboth_p3_T$GuiltyFinal) 
dmes(dat_6_cleanboth_p3_T$ScaredPre,dat_6_cleanboth_p3_T$ScaredFinal) 
dmes(dat_6_cleanboth_p3_T$AfraidPre,dat_6_cleanboth_p3_T$AfraidFinal) 
dmes(dat_6_cleanboth_p3_T$JitteryPre,dat_6_cleanboth_p3_T$JitteryFinal) 
dmes(dat_6_cleanboth_p3_T$AshamedPre,dat_6_cleanboth_p3_T$AshamedFinal) 
dmes(dat_6_cleanboth_p3_T$HostilePre,dat_6_cleanboth_p3_T$HostileFinal) 
dmes(dat_6_cleanboth_p3_T$DistressedPre,dat_6_cleanboth_p3_T$DistressedFinal) 
dmes(dat_6_cleanboth_p3_T$UpsetPre,dat_6_cleanboth_p3_T$UpsetFinal) 
dmes(dat_6_cleanboth_p3_T$IrritablePre,dat_6_cleanboth_p3_T$IrritableFinal) 

## pre
## look at differences subgroup within each -use these for subgroup irritability figure
mean(pts6p3_cb_T_bQ1_1$IrritablePre) 
sd(pts6p3_cb_T_bQ1_1$IrritablePre)/sqrt(length(pts6p3_cb_T_bQ1_1$IrritablePre)) 
mean(pts6p3_cb_T_bQ1_0$IrritablePre) 
sd(pts6p3_cb_T_bQ1_0$IrritablePre)/sqrt(length(pts6p3_cb_T_bQ1_0$IrritablePre)) 
mean(hvs6p3_cb_T_bQ1_1$IrritablePre) 
sd(hvs6p3_cb_T_bQ1_1$IrritablePre)/sqrt(length(hvs6p3_cb_T_bQ1_1$IrritablePre)) 
mean(hvs6p3_cb_T_bQ1_0$IrritablePre) 
sd(hvs6p3_cb_T_bQ1_0$IrritablePre)/sqrt(length(hvs6p3_cb_T_bQ1_0$IrritablePre))
## final
## look at differences subgroup within each -use these for subgroup irritability figuremean(pts6p3_cb_T_bQ1_1$IrritableFinal) 
sd(pts6p3_cb_T_bQ1_1$IrritableFinal)/sqrt(length(pts6p3_cb_T_bQ1_1$IrritableFinal))
mean(pts6p3_cb_T_bQ1_0$IrritableFinal)
sd(pts6p3_cb_T_bQ1_0$IrritableFinal)/sqrt(length(pts6p3_cb_T_bQ1_0$IrritableFinal)) 
mean(hvs6p3_cb_T_bQ1_1$IrritableFinal) 
sd(hvs6p3_cb_T_bQ1_1$IrritableFinal)/sqrt(length(hvs6p3_cb_T_bQ1_1$IrritableFinal)) 
mean(hvs6p3_cb_T_bQ1_0$IrritableFinal) 
sd(hvs6p3_cb_T_bQ1_0$IrritableFinal)/sqrt(length(hvs6p3_cb_T_bQ1_0$IrritableFinal)) 

## pts - pre-final tests
wilcox.test(pts6p3_cb_T_bQ1_1$IrritableFinal,pts6p3_cb_T_bQ1_1$IrritablePre,paired=T) 
wilcox.test(pts6p3_cb_T_bQ1_0$IrritableFinal,pts6p3_cb_T_bQ1_0$IrritablePre,paired=T) 
## hvs - pre-final tests
wilcox.test(hvs6p3_cb_T_bQ1_1$IrritableFinal,hvs6p3_cb_T_bQ1_1$IrritablePre,paired=T) 
wilcox.test(hvs6p3_cb_T_bQ1_0$IrritableFinal,hvs6p3_cb_T_bQ1_0$IrritablePre,paired=T) 


###### irritability change score calculation for each group/subgroup for further analysis
dat_6_cleanboth_p3_T$IrritableX2 <- dat_6_cleanboth_p3_T$IrritableFinal - dat_6_cleanboth_p3_T$IrritablePre
pts_p3dat6_cb_T$IrritableX2 <- pts_p3dat6_cb_T$IrritableFinal - pts_p3dat6_cb_T$IrritablePre
hvs_p3dat6_cb_T$IrritableX2 <- hvs_p3dat6_cb_T$IrritableFinal - hvs_p3dat6_cb_T$IrritablePre
pts6p3_cb_T_bQ1_0$IrritableX2 <- pts6p3_cb_T_bQ1_0$IrritableFinal - pts6p3_cb_T_bQ1_0$IrritablePre
pts6p3_cb_T_bQ1_1$IrritableX2 <- pts6p3_cb_T_bQ1_1$IrritableFinal - pts6p3_cb_T_bQ1_1$IrritablePre
hvs6p3_cb_T_bQ1_0$IrritableX2 <- hvs6p3_cb_T_bQ1_0$IrritableFinal - hvs6p3_cb_T_bQ1_0$IrritablePre
hvs6p3_cb_T_bQ1_1$IrritableX2 <- hvs6p3_cb_T_bQ1_1$IrritableFinal - hvs6p3_cb_T_bQ1_1$IrritablePre

######## ordinal logistic regression as alternative to ANCOVA for irritability change scores
## whole samples
dat_6_cleanboth_p3_T$IrritableX2_fullfactor <- as.factor(dat_6_cleanboth_p3_T$IrritableX2)
levels(dat_6_cleanboth_p3_T$IrritableX2_fullfactor)
class(dat_6_cleanboth_p3_T$IrritableX2_fullfactor)
changeirrit <- polr((IrritableX2_fullfactor)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1)*as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
#changeirrit <- polr((IrritableX2_fullfactor)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+as.factor(binaryQuit1)*as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
summary(changeirrit)
ctable <- coef(summary(changeirrit))
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
ctable <- cbind(ctable, "p value" = p)
ctable

## pts
pts_p3dat6_cb_T$IrritableX2_fullfactor <- as.factor(pts_p3dat6_cb_T$IrritableX2)
levels(pts_p3dat6_cb_T$IrritableX2_fullfactor)
class(pts_p3dat6_cb_T$IrritableX2_fullfactor)
changeirrit_pts <- polr((IrritableX2_fullfactor)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1),data=pts_p3dat6_cb_T)
#changeirrit_pts <- polr((IrritableX2_fullfactor)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+as.factor(binaryQuit1),data=pts_p3dat6_cb_T)
summary(changeirrit_pts)
ctable_pts <- coef(summary(changeirrit_pts))
p_pts <- pnorm(abs(ctable_pts[, "t value"]), lower.tail = FALSE) * 2
ctable_pts <- cbind(ctable_pts, "p value" = p_pts)
ctable_pts

## hvs
hvs_p3dat6_cb_T$IrritableX2_fullfactor <- as.factor(hvs_p3dat6_cb_T$IrritableX2)
levels(hvs_p3dat6_cb_T$IrritableX2_fullfactor)
class(hvs_p3dat6_cb_T$IrritableX2_fullfactor)
changeirrit_hvs <- polr((IrritableX2_fullfactor)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1),data=hvs_p3dat6_cb_T)
#changeirrit_hvs <- polr((IrritableX2_fullfactor)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+as.factor(binaryQuit1),data=hvs_p3dat6_cb_T)
summary(changeirrit_hvs)
ctable_hvs <- coef(summary(changeirrit_hvs))
p_hvs <- pnorm(abs(ctable_hvs[, "t value"]), lower.tail = FALSE) * 2
ctable_hvs <- cbind(ctable_hvs, "p value" = p_hvs)
ctable_hvs

### these are the W tests to be used as post-hoc tests
wilcox.test(pts6p3_cb_T_bQ1_1$IrritableX2,hvs6p3_cb_T_bQ1_1$IrritableX2) 
wilcox.test(pts6p3_cb_T_bQ1_0$IrritableX2,hvs6p3_cb_T_bQ1_0$IrritableX2) 



################## basic performance analyses
##################


## avg errors per second 
## note - this is now only the last block - experimental block - see first part of script where making Avg2tasks variables
## across the full sample 
mean(dat_6_cleanboth_p3_T$Avg2tasksErrorsPerSecLast)

## all
errors_anova <- lm(scale(Avg2tasksErrorsPerSecLast_T)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1)*as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
#errors_anova <- lm(scale(Avg2tasksErrorsPerSecLast_T)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+as.factor(binaryQuit1)*as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
Anova(errors_anova, type="II") 
summary(errors_anova)
shapiro.test(residuals(errors_anova)) 
par(mfrow=c(2,2)) 
plot(errors_anova) 
bptest(errors_anova) 
ncvTest(errors_anova) 

## patients
errors_anova_pts <- lm(scale(Avg2tasksErrorsPerSecLast_T)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1),data=pts_p3dat6_cb_T)
#errors_anova_pts <- lm(scale(Avg2tasksErrorsPerSecLast_T)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+as.factor(binaryQuit1),data=pts_p3dat6_cb_T)
Anova(errors_anova_pts, type="II") 
summary(errors_anova_pts)
shapiro.test(residuals(errors_anova_pts)) 
par(mfrow=c(2,2)) 
plot(errors_anova_pts) 
bptest(errors_anova_pts) 
ncvTest(errors_anova_pts) 

## controls
errors_anova_hvs <- lm(scale(Avg2tasksErrorsPerSecLast_T)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1),data=hvs_p3dat6_cb_T)
#errors_anova_hvs <- lm(scale(Avg2tasksErrorsPerSecLast_T)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+as.factor(binaryQuit1),data=hvs_p3dat6_cb_T)
Anova(errors_anova_hvs, type="II") 
summary(errors_anova_hvs)
shapiro.test(residuals(errors_anova_hvs)) 
par(mfrow=c(2,2)) 
plot(errors_anova_hvs) 
bptest(errors_anova_hvs) 
ncvTest(errors_anova_hvs) 

## avg quit time 
# look only in people who quit - compare patients and controls
quitters <- rbind(pts6p3_cb_T_bQ1_1,hvs6p3_cb_T_bQ1_1) 
mean(pts6p3_cb_T_bQ1_1$Avg2tasksQuittime)
sd(pts6p3_cb_T_bQ1_1$Avg2tasksQuittime)
mean(hvs6p3_cb_T_bQ1_1$Avg2tasksQuittime)
sd(hvs6p3_cb_T_bQ1_1$Avg2tasksQuittime)
t.test(pts6p3_cb_T_bQ1_1$Avg2tasksQuittime_T,hvs6p3_cb_T_bQ1_1$Avg2tasksQuittime_T,var.eq=T)



################## DTI analyses
##################


###### average (whole brain) FA

## whole sample
a_all <- lm(scale(AverageFA_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
#a_all <- lm(scale(AverageFA_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
Anova(a_all, type="II") 
summary(a_all)
shapiro.test(residuals(a_all)) 
par(mfrow=c(2,2)) 
plot(a_all) 
bptest(a_all) 
ncvTest(a_all) 


###### cingulum CG FA (dorsal/anterior)

## whole sample
a_all <- lm(scale(CGC_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
#a_all <- lm(scale(CGC_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
Anova(a_all, type="II") 
summary(a_all)
shapiro.test(residuals(a_all)) 
par(mfrow=c(2,2)) 
plot(a_all)
bptest(a_all) 
ncvTest(a_all) 

## patients
a_pts <- lm(scale(CGC_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+ as.factor(binaryQuit1),data=pts_p3dat6_cb_T)
#a_pts <- lm(scale(CGC_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+ as.factor(binaryQuit1),data=pts_p3dat6_cb_T)
Anova(a_pts, type="II") 
summary(a_pts)
shapiro.test(residuals(a_pts)) 
par(mfrow=c(2,2)) 
plot(a_pts) 
bptest(a_pts) 
ncvTest(a_pts) 

## controls
a_hvs <- lm(scale(CGC_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+ as.factor(binaryQuit1),data=hvs_p3dat6_cb_T)
#a_hvs <- lm(scale(CGC_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+ as.factor(binaryQuit1),data=hvs_p3dat6_cb_T)
Anova(a_hvs, type="II") 
summary(a_hvs) 
shapiro.test(residuals(a_hvs)) 
par(mfrow=c(2,2)) 
plot(a_hvs) 
bptest(a_hvs) 
ncvTest(a_hvs) 

## post-hoc t.tests 
t.test(pts6p3_cb_T_bQ1_0$CGC_T,hvs6p3_cb_T_bQ1_0$CGC_T,var.equal=T) 
t.test(pts6p3_cb_T_bQ1_1$CGC_T,hvs6p3_cb_T_bQ1_1$CGC_T,var.equal=T) 

## test left/right cingulum CGC for specificity of the finding

## test left
## whole sample
a_all <- lm(scale(CGC_L_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
#a_all <- lm(scale(CGC_L_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
Anova(a_all, type="II") 
shapiro.test(residuals(a_all)) 
bptest(a_all) 
ncvTest(a_all)

## test right
## whole sample
a_all <- lm(scale(CGC_R_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
#a_all <- lm(scale(CGC_R_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
Anova(a_all, type="II")
shapiro.test(residuals(a_all)) 
bptest(a_all)
ncvTest(a_all) 



##### cingulum HP FA (ventral/posterior)

## whole sample
a_all <- lm(scale(CGH_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
#a_all <- lm(scale(CGH_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
Anova(a_all, type="II") 
summary(a_all) 
shapiro.test(residuals(a_all)) 
par(mfrow=c(2,2)) 
plot(a_all) 
bptest(a_all) 
ncvTest(a_all) 

# left
a_all <- lm(scale(CGH_L_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
Anova(a_all, type="II") 
# right
a_all <- lm(scale(CGH_R_T) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
Anova(a_all, type="II") 



###### fornix FA 
## whole sample
a_all <- lm(scale(FX) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
#a_all <- lm(scale(FX) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
Anova(a_all, type="II") 
summary(a_all)
shapiro.test(residuals(a_all)) 
par(mfrow=c(2,2)) 
plot(a_all) 
bptest(a_all) 
ncvTest(a_all) 

## dont need to test left/right for FX



####### uncinate fasciculus FA ***
## whole sample
a_all <- lm(scale(UNC) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
#a_all <- lm(scale(UNC) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
Anova(a_all, type="II") 
summary(a_all)
shapiro.test(residuals(a_all)) 
par(mfrow=c(2,2)) 
plot(a_all) 
bptest(a_all) 
ncvTest(a_all) 

# left
a_all <- lm(scale(UNC_L) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
Anova(a_all, type="II") 
# right
a_all <- lm(scale(UNC_R) ~ scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+as.factor(binaryQuit1) * as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T)
Anova(a_all, type="II") 




################# plots for ANOVAs 
################ plots use raw (not transformed) values


##### graphing irritable change - using binaryQuit1 
SZbQ1_0.N <- length(pts6p3_cb_T_bQ1_0$IrritableX2) # = 17
IrrX2.SZbQ1_0.mean <- mean(pts6p3_cb_T_bQ1_0$IrritableX2)
IrrX2.SZbQ1_0.sd <- sd(pts6p3_cb_T_bQ1_0$IrritableX2)
IrrX2.SZbQ1_0.stder <- IrrX2.SZbQ1_0.sd/sqrt(SZbQ1_0.N)

SZbQ1_1.N <- length(pts6p3_cb_T_bQ1_1$IrritableX2) # = 61
IrrX2.SZbQ1_1.mean <- mean(pts6p3_cb_T_bQ1_1$IrritableX2)
IrrX2.SZbQ1_1.sd <- sd(pts6p3_cb_T_bQ1_1$IrritableX2)
IrrX2.SZbQ1_1.stder <- IrrX2.SZbQ1_1.sd/sqrt(SZbQ1_1.N)

HVbQ1_0.N <- length(hvs6p3_cb_T_bQ1_0$IrritableX2) # = 42
IrrX2.HVbQ1_0.mean <- mean(hvs6p3_cb_T_bQ1_0$IrritableX2)
IrrX2.HVbQ1_0.sd <- sd(hvs6p3_cb_T_bQ1_0$IrritableX2)
IrrX2.HVbQ1_0.stder <- IrrX2.HVbQ1_0.sd/sqrt(HVbQ1_0.N)

HVbQ1_1.N <- length(hvs6p3_cb_T_bQ1_1$IrritableX2) # = 53
IrrX2.HVbQ1_1.mean <- mean(hvs6p3_cb_T_bQ1_1$IrritableX2)
IrrX2.HVbQ1_1.sd <- sd(hvs6p3_cb_T_bQ1_1$IrritableX2)
IrrX2.HVbQ1_1.stder <- IrrX2.HVbQ1_1.sd/sqrt(HVbQ1_1.N)

dat_n173_IrrX2_summary <- data.frame(c("SZ_bQ1_0","SZ_bQ1_1","HV_bQ1_0","HV_bQ1_1"),c("1SZ","1SZ","2HV","2HV"),c("bQ1_0","bQ1_1","bQ1_0","bQ1_1"),c(SZbQ1_0.N,SZbQ1_1.N,HVbQ1_0.N,HVbQ1_1.N),c(IrrX2.SZbQ1_0.mean,IrrX2.SZbQ1_1.mean,IrrX2.HVbQ1_0.mean,IrrX2.HVbQ1_1.mean),c(IrrX2.SZbQ1_0.sd,IrrX2.SZbQ1_1.sd,IrrX2.HVbQ1_0.sd,IrrX2.HVbQ1_1.sd),c(IrrX2.SZbQ1_0.stder,IrrX2.SZbQ1_1.stder,IrrX2.HVbQ1_0.stder,IrrX2.HVbQ1_1.stder))
colnames(dat_n173_IrrX2_summary) <- c("Group","CaseCode","QuitCode","Size","IrritableX2.Mean","FA.StandardDev","FA.StandardError")
dat_n173_IrrX2_summary

stderrlimits <- aes(ymax=IrritableX2.Mean+FA.StandardError, ymin=IrritableX2.Mean-FA.StandardError)
dodge <- position_dodge(width=0.1)
p <- ggplot(dat_n173_IrrX2_summary, aes(colour=CaseCode, y=IrritableX2.Mean, x=QuitCode))
p + geom_line(aes(group=CaseCode),position=dodge,size=1) + geom_errorbar(stderrlimits, width=0.3, position=dodge,size=1) + ylim(0,2.0) + scale_color_manual(values=c("red", "blue")) + theme_bw()


##### graphing errors per second (avg across last block) - using binaryQuit1 
SZbQ1_0.N <- length(pts6p3_cb_T_bQ1_0$Avg2tasksErrorsPerSecLast) # = 17
avgEPSL.SZbQ1_0.mean <- mean(pts6p3_cb_T_bQ1_0$Avg2tasksErrorsPerSecLast)
avgEPSL.SZbQ1_0.sd <- sd(pts6p3_cb_T_bQ1_0$Avg2tasksErrorsPerSecLast)
avgEPSL.SZbQ1_0.stder <- avgEPSL.SZbQ1_0.sd/sqrt(SZbQ1_0.N)

SZbQ1_1.N <- length(pts6p3_cb_T_bQ1_1$Avg2tasksErrorsPerSecLast) # = 61
avgEPSL.SZbQ1_1.mean <- mean(pts6p3_cb_T_bQ1_1$Avg2tasksErrorsPerSecLast)
avgEPSL.SZbQ1_1.sd <- sd(pts6p3_cb_T_bQ1_1$Avg2tasksErrorsPerSecLast)
avgEPSL.SZbQ1_1.stder <- avgEPSL.SZbQ1_1.sd/sqrt(SZbQ1_1.N)

HVbQ1_0.N <- length(hvs6p3_cb_T_bQ1_0$Avg2tasksErrorsPerSecLast) # = 42
avgEPSL.HVbQ1_0.mean <- mean(hvs6p3_cb_T_bQ1_0$Avg2tasksErrorsPerSecLast)
avgEPSL.HVbQ1_0.sd <- sd(hvs6p3_cb_T_bQ1_0$Avg2tasksErrorsPerSecLast)
avgEPSL.HVbQ1_0.stder <- avgEPSL.HVbQ1_0.sd/sqrt(HVbQ1_0.N)

HVbQ1_1.N <- length(hvs6p3_cb_T_bQ1_1$Avg2tasksErrorsPerSecLast) # = 53
avgEPSL.HVbQ1_1.mean <- mean(hvs6p3_cb_T_bQ1_1$Avg2tasksErrorsPerSecLast)
avgEPSL.HVbQ1_1.sd <- sd(hvs6p3_cb_T_bQ1_1$Avg2tasksErrorsPerSecLast)
avgEPSL.HVbQ1_1.stder <- avgEPSL.HVbQ1_1.sd/sqrt(HVbQ1_1.N)

dat_n173_avgEPSL_summary <- data.frame(c("SZ_bQ1_0","SZ_bQ1_1","HV_bQ1_0","HV_bQ1_1"),c("1SZ","1SZ","2HV","2HV"),c("bQ1_0","bQ1_1","bQ1_0","bQ1_1"),c(SZbQ1_0.N,SZbQ1_1.N,HVbQ1_0.N,HVbQ1_1.N),c(avgEPSL.SZbQ1_0.mean,avgEPSL.SZbQ1_1.mean,avgEPSL.HVbQ1_0.mean,avgEPSL.HVbQ1_1.mean),c(avgEPSL.SZbQ1_0.sd,avgEPSL.SZbQ1_1.sd,avgEPSL.HVbQ1_0.sd,avgEPSL.HVbQ1_1.sd),c(avgEPSL.SZbQ1_0.stder,avgEPSL.SZbQ1_1.stder,avgEPSL.HVbQ1_0.stder,avgEPSL.HVbQ1_1.stder))
colnames(dat_n173_avgEPSL_summary) <- c("Group","CaseCode","QuitCode","Size","Avg2tasksErrorsPerSecLast.Mean","FA.StandardDev","FA.StandardError")
dat_n173_avgEPSL_summary

stderrlimits <- aes(ymax=Avg2tasksErrorsPerSecLast.Mean+FA.StandardError, ymin=Avg2tasksErrorsPerSecLast.Mean-FA.StandardError)
dodge <- position_dodge(width=0.1)
p <- ggplot(dat_n173_avgEPSL_summary, aes(colour=CaseCode, y=Avg2tasksErrorsPerSecLast.Mean, x=QuitCode))
p + geom_line(aes(group=CaseCode),position=dodge,size=1) + geom_errorbar(stderrlimits, width=0.3, position=dodge,size=1) + ylim(0.20,1.0) + scale_color_manual(values=c("red", "blue")) + theme_bw()


##### graphing whole brain average  - using binaryQuit1 
SZbQ1_0.N <- length(pts6p3_cb_T_bQ1_0$AverageFA) # = 17
FAwb.SZbQ1_0.mean <- mean(pts6p3_cb_T_bQ1_0$AverageFA)
FAwb.SZbQ1_0.sd <- sd(pts6p3_cb_T_bQ1_0$AverageFA)
FAwb.SZbQ1_0.stder <- FAwb.SZbQ1_0.sd/sqrt(SZbQ1_0.N)

SZbQ1_1.N <- length(pts6p3_cb_T_bQ1_1$AverageFA) # = 61
FAwb.SZbQ1_1.mean <- mean(pts6p3_cb_T_bQ1_1$AverageFA)
FAwb.SZbQ1_1.sd <- sd(pts6p3_cb_T_bQ1_1$AverageFA)
FAwb.SZbQ1_1.stder <- FAwb.SZbQ1_1.sd/sqrt(SZbQ1_1.N)

HVbQ1_0.N <- length(hvs6p3_cb_T_bQ1_0$AverageFA) # = 42
FAwb.HVbQ1_0.mean <- mean(hvs6p3_cb_T_bQ1_0$AverageFA)
FAwb.HVbQ1_0.sd <- sd(hvs6p3_cb_T_bQ1_0$AverageFA)
FAwb.HVbQ1_0.stder <- FAwb.HVbQ1_0.sd/sqrt(HVbQ1_0.N)

HVbQ1_1.N <- length(hvs6p3_cb_T_bQ1_1$AverageFA) # = 53
FAwb.HVbQ1_1.mean <- mean(hvs6p3_cb_T_bQ1_1$AverageFA)
FAwb.HVbQ1_1.sd <- sd(hvs6p3_cb_T_bQ1_1$AverageFA)
FAwb.HVbQ1_1.stder <- FAwb.HVbQ1_1.sd/sqrt(HVbQ1_1.N)

dat_n173_FAwb_summary <- data.frame(c("SZ_bQ1_0","SZ_bQ1_1","HV_bQ1_0","HV_bQ1_1"),c("1SZ","1SZ","2HV","2HV"),c("bQ1_0","bQ1_1","bQ1_0","bQ1_1"),c(SZbQ1_0.N,SZbQ1_1.N,HVbQ1_0.N,HVbQ1_1.N),c(FAwb.SZbQ1_0.mean,FAwb.SZbQ1_1.mean,FAwb.HVbQ1_0.mean,FAwb.HVbQ1_1.mean),c(FAwb.SZbQ1_0.sd,FAwb.SZbQ1_1.sd,FAwb.HVbQ1_0.sd,FAwb.HVbQ1_1.sd),c(FAwb.SZbQ1_0.stder,FAwb.SZbQ1_1.stder,FAwb.HVbQ1_0.stder,FAwb.HVbQ1_1.stder))
colnames(dat_n173_FAwb_summary) <- c("Group","CaseCode","QuitCode","Size","AverageFA.Mean","FA.StandardDev","FA.StandardError")
dat_n173_FAwb_summary

stderrlimits <- aes(ymax=AverageFA.Mean+FA.StandardError, ymin=AverageFA.Mean-FA.StandardError)
dodge <- position_dodge(width=0.1)
p <- ggplot(dat_n173_FAwb_summary, aes(colour=CaseCode, y=AverageFA.Mean, x=QuitCode))
p + geom_line(aes(group=CaseCode),position=dodge,size=1) + geom_errorbar(stderrlimits, width=0.3, position=dodge,size=1) + ylim(0.30,0.40) + scale_color_manual(values=c("red", "blue")) + theme_bw()


##### graphing cingulum - CG  - using binaryQuit1 
SZbQ1_0.N <- length(pts6p3_cb_T_bQ1_0$CGC) # = 17
FAcgc.SZbQ1_0.mean <- mean(pts6p3_cb_T_bQ1_0$CGC)
FAcgc.SZbQ1_0.sd <- sd(pts6p3_cb_T_bQ1_0$CGC)
FAcgc.SZbQ1_0.stder <- FAcgc.SZbQ1_0.sd/sqrt(SZbQ1_0.N)

SZbQ1_1.N <- length(pts6p3_cb_T_bQ1_1$CGC) # = 61
FAcgc.SZbQ1_1.mean <- mean(pts6p3_cb_T_bQ1_1$CGC)
FAcgc.SZbQ1_1.sd <- sd(pts6p3_cb_T_bQ1_1$CGC)
FAcgc.SZbQ1_1.stder <- FAcgc.SZbQ1_1.sd/sqrt(SZbQ1_1.N)

HVbQ1_0.N <- length(hvs6p3_cb_T_bQ1_0$CGC) # = 42
FAcgc.HVbQ1_0.mean <- mean(hvs6p3_cb_T_bQ1_0$CGC)
FAcgc.HVbQ1_0.sd <- sd(hvs6p3_cb_T_bQ1_0$CGC)
FAcgc.HVbQ1_0.stder <- FAcgc.HVbQ1_0.sd/sqrt(HVbQ1_0.N)

HVbQ1_1.N <- length(hvs6p3_cb_T_bQ1_1$CGC) # = 53
FAcgc.HVbQ1_1.mean <- mean(hvs6p3_cb_T_bQ1_1$CGC)
FAcgc.HVbQ1_1.sd <- sd(hvs6p3_cb_T_bQ1_1$CGC)
FAcgc.HVbQ1_1.stder <- FAcgc.HVbQ1_1.sd/sqrt(HVbQ1_1.N)

dat_n173_FAcgc_summary <- data.frame(c("SZ_bQ1_0","SZ_bQ1_1","HV_bQ1_0","HV_bQ1_1"),c("1SZ","1SZ","2HV","2HV"),c("bQ1_0","bQ1_1","bQ1_0","bQ1_1"),c(SZbQ1_0.N,SZbQ1_1.N,HVbQ1_0.N,HVbQ1_1.N),c(FAcgc.SZbQ1_0.mean,FAcgc.SZbQ1_1.mean,FAcgc.HVbQ1_0.mean,FAcgc.HVbQ1_1.mean),c(FAcgc.SZbQ1_0.sd,FAcgc.SZbQ1_1.sd,FAcgc.HVbQ1_0.sd,FAcgc.HVbQ1_1.sd),c(FAcgc.SZbQ1_0.stder,FAcgc.SZbQ1_1.stder,FAcgc.HVbQ1_0.stder,FAcgc.HVbQ1_1.stder))
colnames(dat_n173_FAcgc_summary) <- c("Group","CaseCode","QuitCode","Size","FACingulumCG.Mean","FA.StandardDev","FA.StandardError")
dat_n173_FAcgc_summary

stderrlimits <- aes(ymax=FACingulumCG.Mean+FA.StandardError, ymin=FACingulumCG.Mean-FA.StandardError)
dodge <- position_dodge(width=0.1)
p <- ggplot(dat_n173_FAcgc_summary, aes(colour=CaseCode, y=FACingulumCG.Mean, x=QuitCode))
p + geom_line(aes(group=CaseCode),position=dodge,size=1) + geom_errorbar(stderrlimits, width=0.3, position=dodge,size=1) + ylim(0.55,0.65) + scale_color_manual(values=c("red", "blue")) + theme_bw()



##### graphing cingulum - HP  - using binaryQuit1 
SZbQ1_0.N <- length(pts6p3_cb_T_bQ1_0$CGH) # = 17
FAcgH.SZbQ1_0.mean <- mean(pts6p3_cb_T_bQ1_0$CGH)
FAcgH.SZbQ1_0.sd <- sd(pts6p3_cb_T_bQ1_0$CGH)
FAcgH.SZbQ1_0.stder <- FAcgH.SZbQ1_0.sd/sqrt(SZbQ1_0.N)

SZbQ1_1.N <- length(pts6p3_cb_T_bQ1_1$CGH) # = 61
FAcgH.SZbQ1_1.mean <- mean(pts6p3_cb_T_bQ1_1$CGH)
FAcgH.SZbQ1_1.sd <- sd(pts6p3_cb_T_bQ1_1$CGH)
FAcgH.SZbQ1_1.stder <- FAcgH.SZbQ1_1.sd/sqrt(SZbQ1_1.N)

HVbQ1_0.N <- length(hvs6p3_cb_T_bQ1_0$CGH) # = 42
FAcgH.HVbQ1_0.mean <- mean(hvs6p3_cb_T_bQ1_0$CGH)
FAcgH.HVbQ1_0.sd <- sd(hvs6p3_cb_T_bQ1_0$CGH)
FAcgH.HVbQ1_0.stder <- FAcgH.HVbQ1_0.sd/sqrt(HVbQ1_0.N)

HVbQ1_1.N <- length(hvs6p3_cb_T_bQ1_1$CGH) # = 53
FAcgH.HVbQ1_1.mean <- mean(hvs6p3_cb_T_bQ1_1$CGH)
FAcgH.HVbQ1_1.sd <- sd(hvs6p3_cb_T_bQ1_1$CGH)
FAcgH.HVbQ1_1.stder <- FAcgH.HVbQ1_1.sd/sqrt(HVbQ1_1.N)

dat_n173_FAcgH_summary <- data.frame(c("SZ_bQ1_0","SZ_bQ1_1","HV_bQ1_0","HV_bQ1_1"),c("1SZ","1SZ","2HV","2HV"),c("bQ1_0","bQ1_1","bQ1_0","bQ1_1"),c(SZbQ1_0.N,SZbQ1_1.N,HVbQ1_0.N,HVbQ1_1.N),c(FAcgH.SZbQ1_0.mean,FAcgH.SZbQ1_1.mean,FAcgH.HVbQ1_0.mean,FAcgH.HVbQ1_1.mean),c(FAcgH.SZbQ1_0.sd,FAcgH.SZbQ1_1.sd,FAcgH.HVbQ1_0.sd,FAcgH.HVbQ1_1.sd),c(FAcgH.SZbQ1_0.stder,FAcgH.SZbQ1_1.stder,FAcgH.HVbQ1_0.stder,FAcgH.HVbQ1_1.stder))
colnames(dat_n173_FAcgH_summary) <- c("Group","CaseCode","QuitCode","Size","FAcgH.Mean","FA.StandardDev","FA.StandardError")
dat_n173_FAcgH_summary

stderrlimits <- aes(ymax=FAcgH.Mean+FA.StandardError, ymin=FAcgH.Mean-FA.StandardError)
dodge <- position_dodge(width=0.1)
p <- ggplot(dat_n173_FAcgH_summary, aes(colour=CaseCode, y=FAcgH.Mean, x=QuitCode))
p + geom_line(aes(group=CaseCode),position=dodge,size=1) + geom_errorbar(stderrlimits, width=0.3, position=dodge,size=1) + ylim(0.45,0.55) + scale_color_manual(values=c("red", "blue")) + theme_bw()


##### graphing fornix  - using binaryQuit1 
SZbQ1_0.N <- length(pts6p3_cb_T_bQ1_0$FX) # = 17
FAfornix.SZbQ1_0.mean <- mean(pts6p3_cb_T_bQ1_0$FX)
FAfornix.SZbQ1_0.sd <- sd(pts6p3_cb_T_bQ1_0$FX)
FAfornix.SZbQ1_0.stder <- FAfornix.SZbQ1_0.sd/sqrt(SZbQ1_0.N)

SZbQ1_1.N <- length(pts6p3_cb_T_bQ1_1$FX) # = 61
FAfornix.SZbQ1_1.mean <- mean(pts6p3_cb_T_bQ1_1$FX)
FAfornix.SZbQ1_1.sd <- sd(pts6p3_cb_T_bQ1_1$FX)
FAfornix.SZbQ1_1.stder <- FAfornix.SZbQ1_1.sd/sqrt(SZbQ1_1.N)

HVbQ1_0.N <- length(hvs6p3_cb_T_bQ1_0$FX) # = 42
FAfornix.HVbQ1_0.mean <- mean(hvs6p3_cb_T_bQ1_0$FX)
FAfornix.HVbQ1_0.sd <- sd(hvs6p3_cb_T_bQ1_0$FX)
FAfornix.HVbQ1_0.stder <- FAfornix.HVbQ1_0.sd/sqrt(HVbQ1_0.N)

HVbQ1_1.N <- length(hvs6p3_cb_T_bQ1_1$FX) # = 53
FAfornix.HVbQ1_1.mean <- mean(hvs6p3_cb_T_bQ1_1$FX)
FAfornix.HVbQ1_1.sd <- sd(hvs6p3_cb_T_bQ1_1$FX)
FAfornix.HVbQ1_1.stder <- FAfornix.HVbQ1_1.sd/sqrt(HVbQ1_1.N)

dat_n173_FAfornix_summary <- data.frame(c("SZ_bQ1_0","SZ_bQ1_1","HV_bQ1_0","HV_bQ1_1"),c("1SZ","1SZ","2HV","2HV"),c("bQ1_0","bQ1_1","bQ1_0","bQ1_1"),c(SZbQ1_0.N,SZbQ1_1.N,HVbQ1_0.N,HVbQ1_1.N),c(FAfornix.SZbQ1_0.mean,FAfornix.SZbQ1_1.mean,FAfornix.HVbQ1_0.mean,FAfornix.HVbQ1_1.mean),c(FAfornix.SZbQ1_0.sd,FAfornix.SZbQ1_1.sd,FAfornix.HVbQ1_0.sd,FAfornix.HVbQ1_1.sd),c(FAfornix.SZbQ1_0.stder,FAfornix.SZbQ1_1.stder,FAfornix.HVbQ1_0.stder,FAfornix.HVbQ1_1.stder))
colnames(dat_n173_FAfornix_summary) <- c("Group","CaseCode","QuitCode","Size","FAFornix.Mean","FA.StandardDev","FA.StandardError")
dat_n173_FAfornix_summary

stderrlimits <- aes(ymax=FAFornix.Mean+FA.StandardError, ymin=FAFornix.Mean-FA.StandardError)
dodge <- position_dodge(width=0.1)
p <- ggplot(dat_n173_FAfornix_summary, aes(colour=CaseCode, y=FAFornix.Mean, x=QuitCode))
p + geom_line(aes(group=CaseCode),position=dodge,size=1) + geom_errorbar(stderrlimits, width=0.3, position=dodge,size=1) + ylim(0.40,0.50) + scale_color_manual(values=c("red", "blue")) + theme_bw()


##### graphing uncinate fasiculus  - using binaryQuit1  ***
SZbQ1_0.N <- length(pts6p3_cb_T_bQ1_0$UNC) # = 17
FAunc.SZbQ1_0.mean <- mean(pts6p3_cb_T_bQ1_0$UNC)
FAunc.SZbQ1_0.sd <- sd(pts6p3_cb_T_bQ1_0$UNC)
FAunc.SZbQ1_0.stder <- FAunc.SZbQ1_0.sd/sqrt(SZbQ1_0.N)

SZbQ1_1.N <- length(pts6p3_cb_T_bQ1_1$UNC) # = 61
FAunc.SZbQ1_1.mean <- mean(pts6p3_cb_T_bQ1_1$UNC)
FAunc.SZbQ1_1.sd <- sd(pts6p3_cb_T_bQ1_1$UNC)
FAunc.SZbQ1_1.stder <- FAunc.SZbQ1_1.sd/sqrt(SZbQ1_1.N)

HVbQ1_0.N <- length(hvs6p3_cb_T_bQ1_0$UNC) # = 42
FAunc.HVbQ1_0.mean <- mean(hvs6p3_cb_T_bQ1_0$UNC)
FAunc.HVbQ1_0.sd <- sd(hvs6p3_cb_T_bQ1_0$UNC)
FAunc.HVbQ1_0.stder <- FAunc.HVbQ1_0.sd/sqrt(HVbQ1_0.N)

HVbQ1_1.N <- length(hvs6p3_cb_T_bQ1_1$UNC) # = 53
FAunc.HVbQ1_1.mean <- mean(hvs6p3_cb_T_bQ1_1$UNC)
FAunc.HVbQ1_1.sd <- sd(hvs6p3_cb_T_bQ1_1$UNC)
FAunc.HVbQ1_1.stder <- FAunc.HVbQ1_1.sd/sqrt(HVbQ1_1.N)

dat_n173_FAunc_summary <- data.frame(c("SZ_bQ1_0","SZ_bQ1_1","HV_bQ1_0","HV_bQ1_1"),c("1SZ","1SZ","2HV","2HV"),c("bQ1_0","bQ1_1","bQ1_0","bQ1_1"),c(SZbQ1_0.N,SZbQ1_1.N,HVbQ1_0.N,HVbQ1_1.N),c(FAunc.SZbQ1_0.mean,FAunc.SZbQ1_1.mean,FAunc.HVbQ1_0.mean,FAunc.HVbQ1_1.mean),c(FAunc.SZbQ1_0.sd,FAunc.SZbQ1_1.sd,FAunc.HVbQ1_0.sd,FAunc.HVbQ1_1.sd),c(FAunc.SZbQ1_0.stder,FAunc.SZbQ1_1.stder,FAunc.HVbQ1_0.stder,FAunc.HVbQ1_1.stder))
colnames(dat_n173_FAunc_summary) <- c("Group","CaseCode","QuitCode","Size","FAunc.Mean","FA.StandardDev","FA.StandardError")
dat_n173_FAunc_summary

stderrlimits <- aes(ymax=FAunc.Mean+FA.StandardError, ymin=FAunc.Mean-FA.StandardError)
dodge <- position_dodge(width=0.1)
p <- ggplot(dat_n173_FAunc_summary, aes(colour=CaseCode, y=FAunc.Mean, x=QuitCode))
p + geom_line(aes(group=CaseCode),position=dodge,size=1) + geom_errorbar(stderrlimits, width=0.3, position=dodge,size=1) + ylim(0.50,0.60) + scale_color_manual(values=c("red", "blue")) + theme_bw()



################## logistic models pulling things together to see which components actually predict quit
##################

### trying to predict decision to quit - using logistic model
### coding for quit: 1 = quit - so predicting quit behavior
# 1-pchisq(res-deviance,res-DOF) - to test fit of model with data (can be over-fit) 
# can calculate % deviance as:  changeinresidualdeviace/nulldeviance

###### can use null models (intercept only) to compare to active models
null <- glm(as.factor(binaryQuit1)~1,data=dat_6_cleanboth_p3_T,family="binomial")
summary(null)
null_pts <- glm(as.factor(binaryQuit1)~1,data=pts_p3dat6_cb_T,family="binomial")
summary(null_pts)
null_hvs <- glm(as.factor(binaryQuit1)~1,data=hvs_p3dat6_cb_T,family="binomial")
summary(null_hvs)

test1 <- glm(as.factor(binaryQuit1)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(Avg2tasksErrorsPerSecLast_T)+scale(IrritableX2)+scale(CGC_T)*as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T,family="binomial")
#test1 <- glm(as.factor(binaryQuit1)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+scale(Avg2tasksErrorsPerSecLast_T)+scale(IrritableX2)+scale(FACingulumCG_T)*as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T,family="binomial")
summary(test1)
1-pchisq(167.80,164) 
## additional metrics
(222.04-167.80)/222.04 
anova(null,test1,test="Chisq") 

test_pts1 <- glm(as.factor(binaryQuit1)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(Avg2tasksErrorsPerSecLast_T)+scale(IrritableX2)+scale(CGC_T),data=pts_p3dat6_cb_T,family="binomial")
#test_pts1 <- glm(as.factor(binaryQuit1)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+scale(Avg2tasksErrorsPerSecLast_T)+scale(IrritableX2)+scale(FACingulumCG_T),data=pts_p3dat6_cb_T,family="binomial")
summary(test_pts1)  
1-pchisq(65.069,71) 
## additional metrics
(81.791-65.069)/81.791 
anova(null_pts,test_pts1,test="Chisq") 

test_hvs1 <- glm(as.factor(binaryQuit1)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(Avg2tasksErrorsPerSecLast_T)+scale(IrritableX2)+scale(CGC_T),data=hvs_p3dat6_cb_T,family="binomial")
#test_hvs1 <- glm(as.factor(binaryQuit1)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+scale(Avg2tasksErrorsPerSecLast_T)+scale(IrritableX2)+scale(FACingulumCG_T),data=hvs_p3dat6_cb_T,family="binomial")
summary(test_hvs1)  
1-pchisq(99.888,88) 
## additional metrics
(130.421-99.888)/130.421 
anova(null_hvs,test_hvs1,test="Chisq") 


### changing interaction term so check impact ... test if interaction with dx still sig

## 2-way interaction irritability x group 
test1tet <- glm(as.factor(binaryQuit1)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(Avg2tasksErrorsPerSecLast_T)+scale(CGC_T)+scale(IrritableX2)*as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T,family="binomial")
#test1tet <- glm(as.factor(binaryQuit1)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+scale(Avg2tasksErrorsPerSecLast_T)+scale(FACingulumCG_T)+scale(IrritableX2)*as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T,family="binomial")
summary(test1tet) 
1-pchisq(175.37,164) 
## additional metrics
(222.04-175.37)/222.04 
anova(null,test1tet,test="Chisq") 

## 2-way interaction errors x group
test1tet <- glm(as.factor(binaryQuit1)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CGC_T)+scale(IrritableX2)+scale(Avg2tasksErrorsPerSecLast_T)*as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T,family="binomial")
#test1tet <- glm(as.factor(binaryQuit1)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+scale(FACingulumCG_T)+scale(IrritableX2)+scale(Avg2tasksErrorsPerSecLast_T)*as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T,family="binomial")
summary(test1tet)  
1-pchisq(176.77,164) 
## additional metrics
(222.04-176.77)/222.04
anova(null,test1tet,test="Chisq") 


### now testing how important cingulum is in model 

## cingulum interaction removed only (model 2a) 
test2a <- glm(as.factor(binaryQuit1)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(Avg2tasksErrorsPerSecLast_T)+scale(IrritableX2)+scale(CGC_T)+as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T,family="binomial")
#test2a <- glm(as.factor(binaryQuit1)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+scale(Avg2tasksErrorsPerSecLast_T)+scale(IrritableX2)+scale(FACingulumCG_T)+as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T,family="binomial")
summary(test2a)  
1-pchisq(176.79,165) 

## cingulum removed completely (model 2b)
test2b <- glm(as.factor(binaryQuit1)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(Avg2tasksErrorsPerSecLast_T)+scale(IrritableX2)+as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T,family="binomial")
#test2b <- glm(as.factor(binaryQuit1)~scale(Age)+as.factor(Gender1)+as.factor(NonSmoker1)+scale(CogCombo_z)+scale(Avg2tasksErrorsPerSecLast_T)+scale(IrritableX2)+as.factor(CaseControlCode1),data=dat_6_cleanboth_p3_T,family="binomial")
summary(test2b)   
1-pchisq(177.12,166) 

## compare the glm with and without cingulum (need to be nested models) ## original model is test 1
anova(test1,test2a,test="Chisq") # model 2 # significantly worse when remove interaction with cingulum
anova(test1,test2b,test="Chisq") # model 3 # significantly worse remove cingulum removed completely


############################
############################

### looking at relationships between variables 

# pts 
cor.test(pts_p3dat6_cb_T$IrritableX2,pts_p3dat6_cb_T$Avg2tasksErrorsPerSecLast_T,method="spearman") 
cor.test(pts_p3dat6_cb_T$IrritableX2,pts_p3dat6_cb_T$CGC_T,method="spearman")
cor.test(pts_p3dat6_cb_T$Avg2tasksErrorsPerSecLast_T,pts_p3dat6_cb_T$CGC_T,method="pearson")

cor.test(pts6p3_cb_T_bQ1_1$IrritableX2,pts6p3_cb_T_bQ1_1$Avg2tasksQuittime_T,method="spearman")
cor.test(pts6p3_cb_T_bQ1_1$Avg2tasksErrorsPerSecLast_T,pts6p3_cb_T_bQ1_1$Avg2tasksQuittime_T,method="pearson")
cor.test(pts6p3_cb_T_bQ1_1$CGC_T,pts6p3_cb_T_bQ1_1$Avg2tasksQuittime_T,method="pearson") 

# hvs 
cor.test(hvs_p3dat6_cb_T$IrritableX2,hvs_p3dat6_cb_T$Avg2tasksErrorsPerSecLast_T,method="spearman") 
cor.test(hvs_p3dat6_cb_T$IrritableX2,hvs_p3dat6_cb_T$CGC_T,method="spearman") 
cor.test(hvs_p3dat6_cb_T$Avg2tasksErrorsPerSecLast_T,hvs_p3dat6_cb_T$CGC_T,method="pearson") 

cor.test(hvs6p3_cb_T_bQ1_1$IrritableX2,hvs6p3_cb_T_bQ1_1$Avg2tasksQuittime_T,method="spearman")
cor.test(hvs6p3_cb_T_bQ1_1$Avg2tasksErrorsPerSecLast_T,hvs6p3_cb_T_bQ1_1$Avg2tasksQuittime_T,method="pearson")
cor.test(hvs6p3_cb_T_bQ1_1$CGC_T,hvs6p3_cb_T_bQ1_1$Avg2tasksQuittime_T,method="pearson")




##################################################
##################################################

### add in symptoms!!! 

dat_6_cleanboth_p3_T_backup <- dat_6_cleanboth_p3_T

### load in symptom files 

bprs <- read.csv(file.choose(),header=T)
bnss <- read.csv(file.choose(),header=T) 

clin_scales <- merge(bprs,bnss,by.x="subjID",by.y="subjID")

dat_6_cleanboth_p3_T_sx <- merge(dat_6_cleanboth_p3_T,clin_scales,by.x="C_subjID",by.y="subjID")

## parse the data into the required subgroups
pts_sxdat6_cb_T <- dat_6_cleanboth_p3_T_sx[dat_6_cleanboth_p3_T_sx$CaseControlCode==1,] 
hvs_sxdat6_cb_T <- dat_6_cleanboth_p3_T_sx[dat_6_cleanboth_p3_T_sx$CaseControlCode==2,] 

## making subgroups based on binary quit 
pts6sx_cb_bQ1_0_T <- pts_sxdat6_cb_T[pts_sxdat6_cb_T$binaryQuit1==0,]
pts6sx_cb_bQ1_1_T <- pts_sxdat6_cb_T[pts_sxdat6_cb_T$binaryQuit1==1,]
hvs6sx_cb_bQ1_0_T <- hvs_sxdat6_cb_T[hvs_sxdat6_cb_T$binaryQuit1==0,]
hvs6sx_cb_bQ1_1_T <- hvs_sxdat6_cb_T[hvs_sxdat6_cb_T$binaryQuit1==1,]


########### only doing test for symptoms and CPZ for patients
########### 

####### within patient group diff tests for demo table

### needs to do CPZ using initial sheet
### this was also done above where cog was tested for subgroup differences 
mean(pts6sx_cb_bQ1_0_T$CPZ,na.rm=T)
sd(pts6sx_cb_bQ1_0_T$CPZ,na.rm=T)
mean(pts6sx_cb_bQ1_1_T$CPZ,na.rm=T)
sd(pts6sx_cb_bQ1_1_T$CPZ,na.rm=T)
wilcox.test(pts6p3_cb_T_bQ1_0$CPZ,pts6p3_cb_T_bQ1_1$CPZ) 

#### bprs_tot_adj
mean(pts6sx_cb_bQ1_0_T$bprs_tot_adj,na.rm=T)
sd(pts6sx_cb_bQ1_0_T$bprs_tot_adj,na.rm=T)
mean(pts6sx_cb_bQ1_1_T$bprs_tot_adj,na.rm=T)
sd(pts6sx_cb_bQ1_1_T$bprs_tot_adj,na.rm=T)
wilcox.test(pts6sx_cb_bQ1_0_T$bprs_tot_adj,pts6sx_cb_bQ1_1_T$bprs_tot_adj) 

#### ### bprs_psychosis_adj (subscale)
mean(pts6sx_cb_bQ1_0_T$bprs_psychosis_adj,na.rm=T)
sd(pts6sx_cb_bQ1_0_T$bprs_psychosis_adj,na.rm=T)
mean(pts6sx_cb_bQ1_1_T$bprs_psychosis_adj,na.rm=T)
sd(pts6sx_cb_bQ1_1_T$bprs_psychosis_adj,na.rm=T)
wilcox.test(pts6sx_cb_bQ1_0_T$bprs_psychosis_adj,pts6sx_cb_bQ1_1_T$bprs_psychosis_adj) 

#### bnss_total_adj
mean(pts6sx_cb_bQ1_0_T$bnss_total_adj,na.rm=T)
sd(pts6sx_cb_bQ1_0_T$bnss_total_adj,na.rm=T)
mean(pts6sx_cb_bQ1_1_T$bnss_total_adj,na.rm=T)
sd(pts6sx_cb_bQ1_1_T$bnss_total_adj,na.rm=T)
wilcox.test(pts6sx_cb_bQ1_0_T$bnss_total_adj,pts6sx_cb_bQ1_1_T$bnss_total_adj) 

####### correlations with variables

### bprs_tot_adj
cor.test(pts_sxdat6_cb_T$bprs_tot_adj,pts_sxdat6_cb_T$Avg2tasksErrorsPerSecLast_T,method="spearman") 
cor.test(pts_sxdat6_cb_T$bprs_tot_adj,pts_sxdat6_cb_T$CGC_T,method="spearman") 
cor.test(pts_sxdat6_cb_T$bprs_tot_adj,pts_sxdat6_cb_T$IrritableX2,method="spearman")

### bprs_psychosis_adj (subscale)
cor.test(pts_sxdat6_cb_T$bprs_psychosis_adj,pts_sxdat6_cb_T$Avg2tasksErrorsPerSecLast_T,method="spearman") 
cor.test(pts_sxdat6_cb_T$bprs_psychosis_adj,pts_sxdat6_cb_T$CGC_T,method="spearman")
cor.test(pts_sxdat6_cb_T$bprs_psychosis_adj,pts_sxdat6_cb_T$IrritableX2,method="spearman")

### bnss_total_adj 
cor.test(pts_sxdat6_cb_T$bnss_total_adj,pts_sxdat6_cb_T$Avg2tasksErrorsPerSecLast_T,method="spearman") 
cor.test(pts_sxdat6_cb_T$bnss_total_adj,pts_sxdat6_cb_T$CGC_T,method="spearman") 
cor.test(pts_sxdat6_cb_T$bnss_total_adj,pts_sxdat6_cb_T$IrritableX2,method="spearman") 

### CPZ - need to use: pts_p3dat6_cb_T
cor.test(pts_p3dat6_cb_T$CPZ,pts_p3dat6_cb_T$Avg2tasksErrorsPerSecLast_T,method="spearman") 
cor.test(pts_p3dat6_cb_T$CPZ,pts_p3dat6_cb_T$CGC_T,method="spearman")
cor.test(pts_p3dat6_cb_T$CPZ,pts_p3dat6_cb_T$IrritableX2,method="spearman") 

