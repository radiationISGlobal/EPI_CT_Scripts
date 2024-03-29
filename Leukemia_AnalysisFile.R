
#############################################################################################################################################
## 29/04/2019
## This script generates the LEUKEMIA INTERNATIONAL ANALYSIS FILE
## This file is based ct's and doses sent by IARC
## This new version is due to a complete change in the outcomes (mails from elisabeth in 26-29 April 2019)
##
##  //// INPUT
##      - IARC_PatientList20180323: (dowloaded from IARC 04/04/2018). same considerations as before  (FilesDescription_2017October31.docx):
##               -	 not fulfilling the age eligibility criterion
##               -	 with inconsistency in data that cannot be solved
##               -	 With follow-up less than 1 year
##               -	 Diagnosed with cancer before the first CT or within 1 year after first CT. 
##      - IARC_ExaminationList20180323: (dowloaded from IARC 04/04/2018)
##      - Doses_means_3001_3200_s1_20180503: table with the summary stats for 200 realisations of each ct sent by IARC from 03/05/2018, 
##        includes imputed doses to arms.
##      - OutComeDef: table with leukemia/lymphoma definitions (new table, data from excel file from Elisabeth "OutComeDef_WithAusraFile_20190426.xlsx")
##
##  ////  OUTPUT
##      - txt file with the detail of subjects and cts dropped
##      - Leukemia_AnalysisFile_Date.rds
##      with all the ct's + demographic vars + cancer data + doses (median and mean in ActiveMarrow)
##
## 23/11/2017 - Code much simpler with final data. Numbers checked with Michael's and we get the same number of CTs and subjects at the end
## 20/12/2017 - Categorise SES (added sesorig2) in data from France and Netherlands
## 15/01/2018 - Outcome definition updated and added to file
## 09/04/2018 - Upate in Patient, Exams and Doses tables
## 11/06/2018 - Upate in Doses tables
## 03/09/2018 - We add start of cancer registry to calc the entry time of each subject.
##            - Change in outcome def (9670 is not CLL anymore)
## 16/10/2018 - Outcomes "Excluded as debate as to whether lymphoma" no taken into account
##            - Change in "All acute Leukemia" outcomes
## 06/11/2018 - Some changes in outcome definitions
## 29/04/2019 - New outcomes defitinions, all outcomes are changed
## 04/06/2019 - Correction in morpho code 9735 (Plasmablastic lymphoma), now marked as Lymphoid and NHL
## 13/01/2020 - Modification of "outdef" including two new outcomes -> "leuk with MDS and MPS" and "leukemia with MDS"
##              with new outcome definition in -> outdef1
## 29/11/2022 - New outcomes defined in "Numbers of cases and Definition of outcomes - haematological malignancies final classifiation 20221114d_updated29112022.xlsx"
#############################################################################################################################################

library(RODBC)
library(dplyr)
library(sqldf)
library(lubridate)

rm(list=ls())

# ......................................
wd_default <- "Y:/EPI CT Analysis/R Leucemia/"    # working path
wd_FileAnalysis <- "Y:/EPI CT Analysis/R Leucemia/Files"    # path for analysis files
DateVersion <- format(Sys.Date(),"%Y%m%d")
# ......................................

setwd("Y:/EPI CT Analysis/R Leucemia")

# ......................................
# load functions
source("Y:/EPI CT Analysis/R Leucemia/Rfunctions_procs/SES_Categories.R")
# ......................................

# ......................................
# Variables with INPUT and OUTPUTS file names
# INPUT TABLES FROM SQL-SERVER, put correct values
patienttbl <- "IARC_PatientList20180323"
examstbl <- "IARC_ExaminationList20180323"
dosestbl <- "Doses_means_3001_3200_s1_20180605" 
outcomedeftbl <- "OutComeDef"
# ......................................

# ......................................
# OUTCOME FILES
outputrds <- paste("Leukemia_AnalysisFile_", DateVersion, ".rds", sep="")
# ......................................

# ......................................
# Load data (patient, exams and outcome definition) from SQL-Server
channel <- odbcConnect("EpiCT_INT", uid="", pwd="")
patient <- sqlQuery(channel, paste("select * from ", patienttbl))
#outdef <- sqlQuery(channel, paste("select * from ", outcomedeftbl)) # arxiu que modifiquem a Newvariables_OutComeDefinition_2020013.r
exams <- sqlQuery(channel, paste("select * from ", examstbl, sep="")) 
odbcClose(channel)
# ......................................
# New variable outcomes
outdef1 <- read_xlsx("Y:/EPI CT Analysis/2022_new_grouping/Numbers of cases and Definition of outcomes - haematological malignancies final classifiation 20221114d_updated29112022.xlsx")
names(outdef1)


# ......................................
# SES categories
# we add here variable sesorig2, 5 categories, from 1 (lower SES) to 5 (higher SES)
patient <- SES_Cat(patient)
# ......................................

# ......................................
# conversion some variables to date
patient$birth <- as.Date(patient$birth)
patient$ct1st <- as.Date(patient$ct1st)
patient$endfu <- as.Date(patient$endfu)
patient$diagdate <- as.Date(patient$diagdate)
exams$doe <- as.Date(exams$doe)
# ......................................

# ......................................
# JOIN patient and outcome data
# outdef$group <- as.character(outdef$group)
# outdef <- outdef[outdef$group !="Excluded as debate as to whether lymphoma",] 
# outdef <- outdef[outdef$group !="H/L malignancy classified as Myelodysplastic syndrome",] 
# outdef <- outdef[outdef$group !="H/L malignancy classified as Myelodysplastic/Myeloproliferative diseases",] 
# outdef <- outdef[outdef$group !="H/L malignancy classified as Myeloproliferative disease",] 
# outdef <- outdef[outdef$group !="H/L malignancy linked to treatment or syndrome",] 
# outdef <- outdef[outdef$group !="Other",] 
#outdef2 <- select(outdef, -c(icdo3mb, designation_ICDO3, NewTerminology, all, therapy_down, mds_mps, mm))
#outdef1 <- outdef1[outdef1$Major_grouping!="HISTIOCYTIC/DENDRITIC",]
#outdef1 <- outdef1[outdef1$morphology!=9898,] # down syndrome related 
#outdef2 <- select(outdef1, -c(IdTbl, OLD_preApril2019_classification, icdo3mb, designation_ICDO3, New_terminology, Major_grouping, Subtype,
#                             Stage, HISTIOCYTIC_DENDRITIC, mds_mpn))
#patient <- left_join(patient, outdef2, by=c('morphology'))

# all_excl_therap_syndrel
all <- outdef1$...1[which(outdef1$`New classification` == 1)]
all_excl_therap_syndrel <- ifelse(patient$morphology %in% all, 1,0)
patient <- cbind(patient, all_excl_therap_syndrel)

# Lymphoid
lymp <- outdef1$...1[which(outdef1$...10 == 1)]
Lymphoid <- ifelse(patient$morphology %in% lymp, 1,0)
table(is.na(Lymphoid))
patient <- cbind(patient, Lymphoid)

# HL
hl_m <- outdef1$...1[which(outdef1$...11 == 1)]
HL <- ifelse(patient$morphology %in% hl_m, 1,0)
table(is.na(HL))
patient <- cbind(patient, HL)

# NHL
nhl_m <- outdef1$...1[which(outdef1$...12 == 1)]
NHL <- ifelse(patient$morphology %in% nhl_m, 1,0)
table(is.na(NHL))
patient <- cbind(patient, NHL)

# NHL mature Bcell_lymph
nhl_bcell_m <- outdef1$...1[which(outdef1$...13 == 1)]
NHL_Bcell <- ifelse(patient$morphology %in% nhl_bcell_m, 1,0)
table(is.na(NHL_Bcell))
patient <- cbind(patient, NHL_Bcell)

# NHLmature  Tcell_and_NK_cell
nhl_Tcell_m <- outdef1$...1[which(outdef1$...14 == 1)]
NHL_Tcell <- ifelse(patient$morphology %in% nhl_Tcell_m, 1,0)
table(is.na(NHL_Tcell))
patient <- cbind(patient, NHL_Tcell)

# NHL Precursor cell
nhl_Precursorcell_m <- outdef1$...1[which(outdef1$...15 == 1)]
NHL_Precursor_cell <- ifelse(patient$morphology %in% nhl_Precursorcell_m, 1,0)
table(is.na(NHL_Precursor_cell))
patient <- cbind(patient, NHL_Precursor_cell)

# Myeloid (+ Mixed phenotype)
Myeloid_m <- outdef1$...1[which(outdef1$...17 == 1)]
Myeloid <- ifelse(patient$morphology %in% Myeloid_m, 1,0)
table(is.na(Myeloid))
patient <- cbind(patient, Myeloid)

#Myeloid genetic
Myeld_gen_m <- outdef1$...1[which(outdef1$...18 == 1)]
Myeloid_genetic <- ifelse(patient$morphology %in% Myeld_gen_m, 1,0)
table(is.na(Myeloid_genetic))
patient <- cbind(patient, Myeloid_genetic)

#AML and related precursor neoplasms + ALMP/ALAL
AML_prec_ALMP_ALAL_m <- outdef1$...1[which(outdef1$...19 == 1)]
AML_prec_ALMP_ALAL <- ifelse(patient$morphology %in% AML_prec_ALMP_ALAL_m, 1,0)
table(is.na(AML_prec_ALMP_ALAL))
patient <- cbind(patient, AML_prec_ALMP_ALAL)

#AML+AL+ALMP/AL excluding genetic
AML_AL_ALMP_AL_excl_gen_m <- outdef1$...1[which(outdef1$...20 == 1)]
AML_AL_ALMP_AL_excl_gen <- ifelse(patient$morphology %in% AML_AL_ALMP_AL_excl_gen_m, 1,0)
table(is.na(AML_AL_ALMP_AL_excl_gen))
patient <- cbind(patient, AML_AL_ALMP_AL_excl_gen)

#MPN + MDS/MPN + MDS
MPN_MDSMPN_MDS_m <- outdef1$...1[which(outdef1$...21 == 1)]
MPN_MDSMPN_MDS <- ifelse(patient$morphology %in% MPN_MDSMPN_MDS_m, 1,0)
table(is.na(MPN_MDSMPN_MDS))
patient <- cbind(patient, MPN_MDSMPN_MDS)

#MPN 
MPN_m <- outdef1$...1[which(outdef1$...22 == 1)]
MPN <- ifelse(patient$morphology %in% MPN_m, 1,0)
table(is.na(MPN))
patient <- cbind(patient, MPN)

# HISTIOCYTIC/DENDRITIC
HISTIOCYTIC_DENDRITIC_m <- outdef1$...1[which(outdef1$...25 == 1)]
HISTIOCYTIC_DENDRITIC <- ifelse(patient$morphology %in% HISTIOCYTIC_DENDRITIC_m, 1,0)
table(is.na(HISTIOCYTIC_DENDRITIC))
patient <- cbind(patient, HISTIOCYTIC_DENDRITIC)

# UNSP
UNSP_m <- outdef1$...1[which(outdef1$...30 == 1)]
UNSP <- ifelse(patient$morphology %in% UNSP_m, 1,0)
table(is.na(UNSP))
patient <- cbind(patient, UNSP)

# leuk_noCLL_AK
leuk_noCll_AK_m <- outdef1$...1[which(outdef1$...8 == 1)]
leuk_noCll_AK <- ifelse(patient$morphology %in% leuk_noCll_AK_m, 1,0)
table(is.na(leuk_noCll_AK))
patient <- cbind(patient, leuk_noCll_AK)


# ......................................

# ......................................
# JOIN PATIENT AND EXAMINATION DATA
#df <- sqldf("select t1.*, t2.incn, t2.doe, t2.hospid, t2.age, t2.examcode, t2.series, t2.ctafterdiag
df <- sqldf("select t1.*, t2.incn, t2.doe, t2.hospid, t2.age, t2.examcode, t2.series
             from patient t1 left join exams t2 on t1.patientids = t2.patientids")
# ......................................

# ......................................
# calc start od coverage and end of coverage
# start coverage
df$startregy <- NA
df[df$country == 11, c("startregy")] <- 2004
df[df$country == 12, c("startregy")] <- 1943
df[df$country == 13, c("startregy")] <- 2000
df[df$country == 14, c("startregy")] <- 1980
df[df$country == 15, c("startregy")] <- 1989
df[df$country == 16, c("startregy")] <- 1953
df[df$region == "Girona-2012", c("startregy")] <- 1994
df[df$region == "Madrid-2013", c("startregy")] <- 2005
df[df$region == "Murcia-2009", c("startregy")] <- 1983
df[df$region == "Navarra-2010", c("startregy")] <- 1982
df[df$region == "PaisBasc-2013", c("startregy")] <- 1986
df[df$region == "Tarragona-2011", c("startregy")] <- 1980
df[df$region=="Valencia-2011" & year(df$birth) < 1969, c("startregy")] <- 2006
df[df$region=="Valencia-2011" & year(df$birth) >= 1969, c("startregy")] <- 1983
df[df$country == 18, c("startregy")] <- 1958
df[df$country == 19, c("startregy")] <- 1985
df$startreg <- as.Date(paste0(df$startregy, "-01-01"))

# stop coverage
df$stopregy <- NA
df[df$country != 17, c("stopregy")] <- 2015
df[df$region == "Girona-2012", c("stopregy")] <- 2013
df[df$region == "Madrid-2013", c("stopregy")] <- 2014
df[df$region == "Murcia-2009", c("stopregy")] <- 2010
df[df$region == "Navarra-2010", c("stopregy")] <- 2011
df[df$region == "PaisBasc-2013", c("stopregy")] <- 2014
df[df$region == "Tarragona-2011", c("stopregy")] <- 2012

df[df$region == "Valencia-2011" & year(df$birth) >= 1969 & year(df$birth) < 1992, c("stopreg")] <- 
  df[df$region == "Valencia-2011" & year(df$birth) >= 1969 & year(df$birth) < 1992, c("birth")] + years(14)
df$stopreg <- as.Date(df$stopreg, origin="1970-01-01")
# there is one subject born in 1988-02-29, if we sum 14 years --> 2002-02-29 does not exit, therefor we sum days: 365.25 * 14
df[df$region == "Valencia-2011" & year(df$birth) >= 1969 & year(df$birth) < 1992 & is.na(df$stopreg), c("stopreg")] <- 
  df[df$region == "Valencia-2011" & year(df$birth) >= 1969 & year(df$birth) < 1992  & is.na(df$stopreg), c("birth")] + (365.25 * 14)
df[df$region == "Valencia-2011" & is.na(df$stopreg), c("stopregy")] <- 2012

df[is.na(df$stopreg), c("stopreg")] <- paste0(df[is.na(df$stopreg), c("stopregy")],"-01-01")
# ......................................

# ......................................
# calc exit & entry
df <- transform(df, exit = pmin(diagdate, endfu, stopreg, na.rm=TRUE))
table(is.na(df$exit))
df <- transform(df, entry = pmax(df$ct1st + (365.25 * 2), startreg))  # I don't add N years, and use days instead, to avoid problems with leap years.
table(is.na(df$entry))
# ......................................

# ......................................
# remove cts where doe >= exit
df <- df[df$doe < df$exit,]
# ......................................

# ......................................
# load doses
#load(paste("C:/Users/jfiguerola.ISGLOBAL/Documents/servidor/Epi-CT/Dosimetria/IARC/", dosesfile, sep=""))
channel <- odbcConnect("EpiCT_INT", uid="", pwd="")
dosesActMar <- sqlQuery(channel, paste("select sampid, patientids, incn, mean_activemarrow_3001_3200 as am, p50_activemarrow_3001_3200 as med
                                      from ", dosestbl, sep="")) 
odbcClose(channel)
# ......................................

# ......................................
# link data + doses
df2 <- sqldf("select t1.*, t2.sampid, t2.am as ActMar_mean, t2.med as ActMar_med 
             from df t1 inner join dosesActMar t2 on t1.patientids=t2.patientids and t1.incn=t2.incn")
rm("dosesActMar")
gc()
# check there is no missing dose, dt1 should be empty
#table(is.na(df2$ActMar_med))
# ......................................

# ......................................
# selection of variables and correct order to use Frances R code
# drop some unused columns and put in thei place new variables, Number of vars at the end will be 56 and in same order as older files
df2 <- select(df2, -topography2, -tdesign2, -morphology2, -icdo3mb2, -mdesign2)
#df2 <- df2[, c(1:23, 45:48, 50:50, 24:25, 49:49, 26:44, 51:52)]
#df2 <- df2[, c(1:23, 43:46, 48:48, 24:25, 47:47, 26:42, 49:50)]
df2 <- df2[, c(1:23, 42:45, 47:47, 24:25, 46:46, 26:41, 48:56)]
names(df2)
# ......................................

# ......................................
# export data
setwd(wd_FileAnalysis)
saveRDS(df2, file = outputrds)
#save.dta13(df2, file = outputdta)
#write.csv(df2, outputcsv, sep="\t", row.names=FALSE)
#write.dta(df2, outputdta)
# ......................................


#/////////////////////////////////////////////////////////////////////////////////////
#NOT TO EXECUTE!! just to check numbers of subjects that remain in leukemia analysis 
# keep CTs with any follow-up
df3 <- df2[df2$entry <= df2$exit,]
length(unique(df3$patientids))


# apply lag time
df3 <- df3[df3$exit - df3$doe > (365.25 * 2),]
length(unique(df3$patientids))
#/////////////////////////////////////////////////////////////////////////////////////
