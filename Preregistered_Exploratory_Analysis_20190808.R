# Script by E.S. Poppelaars (2019).
# All packages up to date as of 02-07-2019.


# Setup ----------------------------------------------------------------
# Set seed
set.seed(123)

## Load files

## Packages
# Install packages if necessary
#install.packages("tidyverse", dependencies = TRUE)
#install.packages("magrittr", dependencies = TRUE)
#install.packages("xlsx", dependencies = TRUE)

# Load packages
library(tidyverse)
library(magrittr)
library(xlsx)


## Load files
# Load CFC data
setwd("D:\\Publications\\4. Social-evaluative threat oscillations\\Analysis\\Matlab")
CFC <- read.xlsx("SET_CFC_MatlabOutput.xlsx", 1)
# Select only variables to be analyzed
CFC_brief <- CFC %>% select(Subject, contains("Avg_dPAC_Z"), contains("Avg_AAC_R"))

# Remove participants with missing data in CFC variables
CFC_brief <- na.omit(CFC_brief)

# Load earlier data (behavioral, physiology)
setwd("D:\\Publications\\4. Social-evaluative threat oscillations\\Analysis\\R\\CFC")
load("SET.RData")

# Merge dataframes
SET_CFC <- merge.data.frame(SET, CFC_brief, by.x = "Subject", by.y = "Subject")

# Save new dataset
save(SET_CFC, file = "SET_CFC.RData")

# Remove old variables
remove(SET)
remove(CFC)
remove(CFC_brief)

# Individual reactivity (preregistered) ----------------------------------------------------
#install.packages("car", dependencies = TRUE)
library(car) # for densityPlot

## Frontal dPAC
# Get reactivity increase (max - baseline)
SET_CFC$react_Frontal_Avg_dPAC_Z <- I(apply(SET_CFC[, c('EarlyAnticip_Frontal_Avg_dPAC_Z', 
                                                        'LateAnticip_Frontal_Avg_dPAC_Z')
                                                    ], 1, max)) - SET_CFC$RS_Frontal_Avg_dPAC_Z
# Density plot
densityPlot(SET_CFC$react_Frontal_Avg_dPAC_Z, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$react_Frontal_Avg_dPAC_Z)
# Get peak time
SET_CFC$react.time_Frontal_Avg_dPAC_Z <- apply(SET_CFC[, c('EarlyAnticip_Frontal_Avg_dPAC_Z', 
                                                       'LateAnticip_Frontal_Avg_dPAC_Z')
                                                       ], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.max(x)} }) 
SET_CFC$react.time_Frontal_Avg_dPAC_Z[SET_CFC$react.time_Frontal_Avg_dPAC_Z==1] <- 5 # Recode into actual time after start SET
SET_CFC$react.time_Frontal_Avg_dPAC_Z[SET_CFC$react.time_Frontal_Avg_dPAC_Z==2] <- 7.5 # Recode into actual time after start SET
# Frequency table of the peak times
table(SET_CFC$react.time_Frontal_Avg_dPAC_Z)


## Parietal dPAC
# Get reactivity increase (max - baseline)
SET_CFC$react_Parietal_Avg_dPAC_Z <- I(apply(SET_CFC[, c('EarlyAnticip_Parietal_Avg_dPAC_Z', 
                                                        'LateAnticip_Parietal_Avg_dPAC_Z')
                                                    ], 1, max)) - SET_CFC$RS_Parietal_Avg_dPAC_Z
# Density plot
densityPlot(SET_CFC$react_Parietal_Avg_dPAC_Z, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$react_Parietal_Avg_dPAC_Z)
# Get peak time
SET_CFC$react.time_Parietal_Avg_dPAC_Z <- apply(SET_CFC[, c('EarlyAnticip_Parietal_Avg_dPAC_Z', 
                                                           'LateAnticip_Parietal_Avg_dPAC_Z')
                                                       ], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.max(x)} }) 
SET_CFC$react.time_Parietal_Avg_dPAC_Z[SET_CFC$react.time_Parietal_Avg_dPAC_Z==1] <- 5 # Recode into actual time after start SET
SET_CFC$react.time_Parietal_Avg_dPAC_Z[SET_CFC$react.time_Parietal_Avg_dPAC_Z==2] <- 7.5 # Recode into actual time after start SET
# Frequency table of the peak times
table(SET_CFC$react.time_Parietal_Avg_dPAC_Z)


## Frontal AAC
# Get reactivity increase (max - baseline)
SET_CFC$react_Frontal_Avg_AAC_R <- I(apply(SET_CFC[, c('EarlyAnticip_Frontal_Avg_AAC_R', 
                                                        'LateAnticip_Frontal_Avg_AAC_R')
                                                    ], 1, max)) - SET_CFC$RS_Frontal_Avg_AAC_R
# Density plot
densityPlot(SET_CFC$react_Frontal_Avg_AAC_R, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$react_Frontal_Avg_AAC_R)
# Get peak time
SET_CFC$react.time_Frontal_Avg_AAC_R <- apply(SET_CFC[, c('EarlyAnticip_Frontal_Avg_AAC_R', 
                                                           'LateAnticip_Frontal_Avg_AAC_R')
                                                       ], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.max(x)} }) 
SET_CFC$react.time_Frontal_Avg_AAC_R[SET_CFC$react.time_Frontal_Avg_AAC_R==1] <- 5 # Recode into actual time after start SET
SET_CFC$react.time_Frontal_Avg_AAC_R[SET_CFC$react.time_Frontal_Avg_AAC_R==2] <- 7.5 # Recode into actual time after start SET
# Frequency table of the peak times
table(SET_CFC$react.time_Frontal_Avg_AAC_R)


## Parietal AAC
# Get reactivity increase (max - baseline)
SET_CFC$react_Parietal_Avg_AAC_R <- I(apply(SET_CFC[, c('EarlyAnticip_Parietal_Avg_AAC_R', 
                                                       'LateAnticip_Parietal_Avg_AAC_R')
                                                   ], 1, max)) - SET_CFC$RS_Parietal_Avg_AAC_R
# Density plot
densityPlot(SET_CFC$react_Parietal_Avg_AAC_R, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$react_Parietal_Avg_AAC_R)
# Get peak time
SET_CFC$react.time_Parietal_Avg_AAC_R <- apply(SET_CFC[, c('EarlyAnticip_Parietal_Avg_AAC_R', 
                                                          'LateAnticip_Parietal_Avg_AAC_R')
                                                      ], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.max(x)} }) 
SET_CFC$react.time_Parietal_Avg_AAC_R[SET_CFC$react.time_Parietal_Avg_AAC_R==1] <- 5 # Recode into actual time after start SET
SET_CFC$react.time_Parietal_Avg_AAC_R[SET_CFC$react.time_Parietal_Avg_AAC_R==2] <- 7.5 # Recode into actual time after start SET
SET_CFC$react.time_Parietal_Avg_AAC_R[SET_CFC$react.time_Parietal_Avg_AAC_R==1] <- 24 # Recode into actual time after start SET
# Frequency table of the peak times
table(SET_CFC$react.time_Parietal_Avg_AAC_R)

# Save new variables in the dataset
save(SET_CFC, file = "SET_CFC.RData")

# Check whether there are no missing values
any(is.na(SET_CFC))


# Individual recovery (exploratory) ---------------------------------------------------

# Packages
library(magrittr) # for Piping
library(car) # for densityPlot

# Load data
load("SET_CFC.RData")

## Frontal dPAC
# Get recovery decrease (min - max)
SET_CFC$recov_Frontal_Avg_dPAC_Z <- I(apply(SET_CFC[, c('EarlyRecov_Frontal_Avg_dPAC_Z', 'LateRecov_Frontal_Avg_dPAC_Z')], 1, min)) - 
  I(apply(SET_CFC[, c('EarlyAnticip_Frontal_Avg_dPAC_Z', 'LateAnticip_Frontal_Avg_dPAC_Z')], 1, max))
# Density plot
densityPlot(SET_CFC$recov_Frontal_Avg_dPAC_Z, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$recov_Frontal_Avg_dPAC_Z)
# Get min time
SET_CFC$recov.time_Frontal_Avg_dPAC_Z <- apply(SET_CFC[, c('EarlyRecov_Frontal_Avg_dPAC_Z',
                                                           'LateRecov_Frontal_Avg_dPAC_Z')
                                                       ], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.min(x)} })
SET_CFC$recov.time_Frontal_Avg_dPAC_Z[SET_CFC$recov.time_Frontal_Avg_dPAC_Z==1] <- 24 # Recode into actual time after start SET
SET_CFC$recov.time_Frontal_Avg_dPAC_Z[SET_CFC$recov.time_Frontal_Avg_dPAC_Z==2] <- 54 # Recode into actual time after start SET

# Frequency table of the peak times
table(SET_CFC$recov.time_Frontal_Avg_dPAC_Z)

## Parietal dPAC
# Get recovery decrease (min - max)
SET_CFC$recov_Parietal_Avg_dPAC_Z <- I(apply(SET_CFC[, c('EarlyRecov_Parietal_Avg_dPAC_Z', 'LateRecov_Parietal_Avg_dPAC_Z')], 1, min)) - 
  I(apply(SET_CFC[, c('EarlyAnticip_Parietal_Avg_dPAC_Z', 'LateAnticip_Parietal_Avg_dPAC_Z')], 1, max))
# Density plot
densityPlot(SET_CFC$recov_Parietal_Avg_dPAC_Z, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$recov_Parietal_Avg_dPAC_Z)
# Get min time
SET_CFC$recov.time_Parietal_Avg_dPAC_Z <- apply(SET_CFC[, c('EarlyRecov_Parietal_Avg_dPAC_Z',
                                                            'LateRecov_Parietal_Avg_dPAC_Z')
                                                        ], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.min(x)} })
SET_CFC$recov.time_Parietal_Avg_dPAC_Z[SET_CFC$recov.time_Parietal_Avg_dPAC_Z==1] <- 24 # Recode into actual time after start SET
SET_CFC$recov.time_Parietal_Avg_dPAC_Z[SET_CFC$recov.time_Parietal_Avg_dPAC_Z==2] <- 54 # Recode into actual time after start SET

# Frequency table of the peak times
table(SET_CFC$recov.time_Parietal_Avg_dPAC_Z)

## Frontal AAC
# Get recovery decrease (min - max)
SET_CFC$recov_Frontal_Avg_AAC_R <- I(apply(SET_CFC[, c('EarlyRecov_Frontal_Avg_AAC_R', 'LateRecov_Frontal_Avg_AAC_R')], 1, min)) - 
  I(apply(SET_CFC[, c('EarlyAnticip_Frontal_Avg_AAC_R', 'LateAnticip_Frontal_Avg_AAC_R')], 1, max))
# Density plot
densityPlot(SET_CFC$recov_Frontal_Avg_AAC_R, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$recov_Frontal_Avg_AAC_R)
# Get min time
SET_CFC$recov.time_Frontal_Avg_AAC_R <- apply(SET_CFC[, c('EarlyRecov_Frontal_Avg_AAC_R',
                                                          'LateRecov_Frontal_Avg_AAC_R')
                                                      ], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.min(x)} })
SET_CFC$recov.time_Frontal_Avg_AAC_R[SET_CFC$recov.time_Frontal_Avg_AAC_R==1] <- 24 # Recode into actual time after start SET
SET_CFC$recov.time_Frontal_Avg_AAC_R[SET_CFC$recov.time_Frontal_Avg_AAC_R==2] <- 54 # Recode into actual time after start SET

# Frequency table of the peak times
table(SET_CFC$recov.time_Frontal_Avg_AAC_R)

## Parietal AAC
# Get recovery decrease (last measure - max)
SET_CFC$recov_Parietal_Avg_AAC_R <- I(apply(SET_CFC[, c('EarlyRecov_Parietal_Avg_AAC_R', 'LateRecov_Parietal_Avg_AAC_R')], 1, min)) - 
  I(apply(SET_CFC[, c('EarlyAnticip_Parietal_Avg_AAC_R', 'LateAnticip_Parietal_Avg_AAC_R')], 1, max))
# Density plot
densityPlot(SET_CFC$recov_Parietal_Avg_AAC_R, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$recov_Frontal_Avg_AAC_R)
# Get min time
SET_CFC$recov.time_Parietal_Avg_AAC_R <- apply(SET_CFC[, c('EarlyRecov_Parietal_Avg_AAC_R', 
                                                           'LateRecov_Parietal_Avg_AAC_R')
                                                       ], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.min(x)} })
SET_CFC$recov.time_Parietal_Avg_AAC_R[SET_CFC$recov.time_Parietal_Avg_AAC_R==1] <- 24 # Recode into actual time after start SET
SET_CFC$recov.time_Parietal_Avg_AAC_R[SET_CFC$recov.time_Parietal_Avg_AAC_R==2] <- 54 # Recode into actual time after start SET

# Frequency table of the peak times
table(SET_CFC$recov.time_Parietal_Avg_AAC_R)


## Anxiety
# Get recovery decrease (min - max)
SET_CFC$anx.recov <- I(apply(SET_CFC[, c('Anx.3', 'Anx.4')], 1, min)) - 
  I(apply(SET_CFC[, c('Anx.2', 'Anx.3')], 1, max))
# Get peak time
SET_CFC$anx.recov.time <- apply(SET_CFC[, c('Anx.3', 'Anx.4')], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.min(x)} }) 
SET_CFC$anx.recov.time[SET_CFC$anx.recov.time==1] <- 19 # Recode into actual time after start SET_CFC
SET_CFC$anx.recov.time[SET_CFC$anx.recov.time==2] <- 30 # Recode into actual time after start SET_CFC

# Density plot
densityPlot(SET_CFC$anx.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$anx.recov)
# Frequency table of the peak times
table(SET_CFC$anx.recov.time)

## Approach motivation
# Get recovery decrease (min - max)
SET_CFC$appr.recov <- I(apply(SET_CFC[, c('Appr.3', 'Appr.4')], 1, max)) - 
  I(apply(SET_CFC[, c('Appr.2', 'Appr.3')], 1, min))
# Get peak time
SET_CFC$appr.recov.time <- apply(SET_CFC[, c('Appr.3', 'Appr.4')], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.max(x)} }) 
SET_CFC$appr.recov.time[SET_CFC$appr.recov.time==1] <- 19 # Recode into actual time after start SET_CFC
SET_CFC$appr.recov.time[SET_CFC$appr.recov.time==2] <- 30 # Recode into actual time after start SET_CFC

# Density plot
densityPlot(SET_CFC$appr.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$appr.recov)
# Frequency table of the peak times
table(SET_CFC$appr.recov.time)

## BP
# Get recovery decrease (min - max)
SET_CFC$bp.recov <- I(apply(SET_CFC[, c('BP.5', 'BP.6', 'BP.7', 'BP.8')], 1, min)) - 
  I(apply(SET_CFC[, c('BP.3', 'BP.4', 'BP.5', 'BP.6')], 1, max))
# Get peak time
SET_CFC$bp.recov.time <- apply(SET_CFC[, c('BP.5', 'BP.6', 'BP.7', 'BP.8')], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.min(x)} }) 
SET_CFC$bp.recov.time[SET_CFC$bp.recov.time==1] <- 23 # Recode into actual time after start SET_CFC
SET_CFC$bp.recov.time[SET_CFC$bp.recov.time==2] <- 29 # Recode into actual time after start SET_CFC
SET_CFC$bp.recov.time[SET_CFC$bp.recov.time==3] <- 53 # Recode into actual time after start SET_CFC
SET_CFC$bp.recov.time[SET_CFC$bp.recov.time==4] <- 59 # Recode into actual time after start SET_CFC

# Density plot
densityPlot(SET_CFC$bp.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$bp.recov)
# Frequency table of the peak times
table(SET_CFC$bp.recov.time)

## Heart rate
# Get recovery
SET_CFC$hr.recov <- I(apply(SET_CFC[, c('HR.7', 'HR.8', 'HR.9', 'HR.10')], 1, min)) - 
  I(apply(SET_CFC[, c('HR.3', 'HR.4', 'HR.7', 'HR.8')], 1, max))
# Get peak time
SET_CFC$hr.recov.time <- apply(SET_CFC[, c('HR.7', 'HR.8', 'HR.9', 'HR.10')], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.min(x)} }) 
SET_CFC$hr.recov.time[SET_CFC$hr.recov.time==1] <- 24 # Recode into actual time after start SET_CFC
SET_CFC$hr.recov.time[SET_CFC$hr.recov.time==2] <- 26.5  # Recode into actual time after start SET_CFC
SET_CFC$hr.recov.time[SET_CFC$hr.recov.time==3] <- 54 # Recode into actual time after start SET_CFC
SET_CFC$hr.recov.time[SET_CFC$hr.recov.time==4] <- 56.5 # Recode into actual time after start SET_CFC

# Density plot
densityPlot(SET_CFC$hr.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$hr.recov)
# Frequency table of the peak times
table(SET_CFC$hr.recov.time)

## PEP
# Get recovery
SET_CFC$pep.recov <- I(apply(SET_CFC[, c('PEP.7', 'PEP.8', 'PEP.9', 'PEP.10')], 1, max)) - 
  I(apply(SET_CFC[, c('PEP.3', 'PEP.4', 'PEP.7', 'PEP.8')], 1, min))
# Get peak time
SET_CFC$pep.recov.time <- apply(SET_CFC[, c('PEP.7', 'PEP.8', 'PEP.9', 'PEP.10')], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.max(x)} }) 
SET_CFC$pep.recov.time[SET_CFC$pep.recov.time==1] <- 24 # Recode into actual time after start SET_CFC
SET_CFC$pep.recov.time[SET_CFC$pep.recov.time==2] <- 26.5 # Recode into actual time after start SET_CFC
SET_CFC$pep.recov.time[SET_CFC$pep.recov.time==3] <- 54 # Recode into actual time after start SET_CFC
SET_CFC$pep.recov.time[SET_CFC$pep.recov.time==4] <- 56.5 # Recode into actual time after start SET_CFC

# Density plot
densityPlot(SET_CFC$pep.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$pep.recov)
# Frequency table of the peak times
table(SET_CFC$pep.recov.time)

## Cardiac output
# Get recovery
SET_CFC$co.recov <- I(apply(SET_CFC[, c('CO.7', 'CO.8', 'CO.9', 'CO.10')], 1, min)) - 
  I(apply(SET_CFC[, c('CO.3', 'CO.4', 'CO.7', 'CO.8')], 1, max))
# Get peak time
SET_CFC$co.recov.time <- apply(SET_CFC[, c('CO.7', 'CO.8', 'CO.9', 'CO.10')], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.min(x)} }) 
SET_CFC$co.recov.time[SET_CFC$co.recov.time==1] <- 24 # Recode into actual time after start SET_CFC
SET_CFC$co.recov.time[SET_CFC$co.recov.time==2] <- 26.5 # Recode into actual time after start SET_CFC
SET_CFC$co.recov.time[SET_CFC$co.recov.time==3] <- 54 # Recode into actual time after start SET_CFC
SET_CFC$co.recov.time[SET_CFC$co.recov.time==4] <- 56.5 # Recode into actual time after start SET_CFC

# Density plot
densityPlot(SET_CFC$co.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$co.recov)
# Frequency table of the peak times
table(SET_CFC$co.recov.time)

## TPR
# Get recovery
SET_CFC$tpr.recov <- I(apply(SET_CFC[, c('TPR.5', 'TPR.6', 'TPR.7', 'TPR.8')], 1, max)) - 
  I(apply(SET_CFC[, c('TPR.3', 'TPR.4', 'TPR.5', 'TPR.6')], 1, min))
# Get peak time
SET_CFC$tpr.recov.time <- apply(SET_CFC[, c('TPR.5', 'TPR.6', 'TPR.7', 'TPR.8')], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.max(x)} }) 
SET_CFC$tpr.recov.time[SET_CFC$tpr.recov.time==1] <- 24 # Recode into actual time after start SET_CFC
SET_CFC$tpr.recov.time[SET_CFC$tpr.recov.time==2] <- 26.5 # Recode into actual time after start SET_CFC
SET_CFC$tpr.recov.time[SET_CFC$tpr.recov.time==3] <- 54 # Recode into actual time after start SET_CFC
SET_CFC$tpr.recov.time[SET_CFC$tpr.recov.time==4] <- 56.5 # Recode into actual time after start SET_CFC

# Density plot
densityPlot(SET_CFC$tpr.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$tpr.recov)
# Frequency table of the peak times
table(SET_CFC$tpr.recov.time)

## TCI
# Get recovery
SET_CFC$tci.recov <- I(apply(SET_CFC[, c('TCI.5', 'TCI.6', 'TCI.7', 'TCI.8')], 1, max)) - 
  I(apply(SET_CFC[, c('TCI.3', 'TCI.4', 'TCI.5', 'TCI.6')], 1, min))
# Get peak time
SET_CFC$tci.recov.time <- apply(SET_CFC[, c('TCI.5', 'TCI.6', 'TCI.7', 'TCI.8')], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.max(x)} }) 
SET_CFC$tci.recov.time[SET_CFC$tci.recov.time==1] <- 24 # Recode into actual time after start SET_CFC
SET_CFC$tci.recov.time[SET_CFC$tci.recov.time==2] <- 26.5 # Recode into actual time after start SET_CFC
SET_CFC$tci.recov.time[SET_CFC$tci.recov.time==3] <- 54 # Recode into actual time after start SET_CFC
SET_CFC$tci.recov.time[SET_CFC$tci.recov.time==4] <- 56.5 # Recode into actual time after start SET_CFC

# Density plot
densityPlot(SET_CFC$tci.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$tci.recov)
# Frequency table of the peak times
table(SET_CFC$tpr.recov.time)

## RSA
# Get recovery
SET_CFC$rsa.recov <- I(apply(SET_CFC[, c('RSA.7', 'RSA.8', 'RSA.9', 'RSA.10')], 1, max)) - 
  I(apply(SET_CFC[, c('RSA.3', 'RSA.4', 'RSA.7', 'RSA.8')], 1, min))
# Get peak time
SET_CFC$rsa.recov.time <- apply(SET_CFC[, c('RSA.7', 'RSA.8', 'RSA.9', 'RSA.10')], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.max(x)} }) 
SET_CFC$rsa.recov.time[SET_CFC$rsa.recov.time==1] <- 24 # Recode into actual time after start SET_CFC
SET_CFC$rsa.recov.time[SET_CFC$rsa.recov.time==2] <- 26.5 # Recode into actual time after start SET_CFC
SET_CFC$rsa.recov.time[SET_CFC$rsa.recov.time==3] <- 54 # Recode into actual time after start SET_CFC
SET_CFC$rsa.recov.time[SET_CFC$rsa.recov.time==4] <- 56.5 # Recode into actual time after start SET_CFC

# Density plot
densityPlot(SET_CFC$rsa.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$rsa.recov)
# Frequency table of the peak times
table(SET_CFC$tci.recov.time)

## RR
# Get recovery
SET_CFC$rr.recov <- I(apply(SET_CFC[, c('RR.7', 'RR.8', 'RR.9', 'RR.10')], 1, min)) - 
  I(apply(SET_CFC[, c('RR.3', 'RR.4', 'RR.7', 'RR.8')], 1, max))
# Get peak time
SET_CFC$rr.recov.time <- apply(SET_CFC[, c('RR.7', 'RR.8', 'RR.9', 'RR.10')], 1, function(x) {if (all(anyNA(x))) {NA}  else {which.min(x)} }) 
SET_CFC$rr.recov.time[SET_CFC$rr.recov.time==1] <- 24 # Recode into actual time after start SET_CFC
SET_CFC$rr.recov.time[SET_CFC$rr.recov.time==2] <- 26.5 # Recode into actual time after start SET_CFC
SET_CFC$rr.recov.time[SET_CFC$rr.recov.time==3] <- 54 # Recode into actual time after start SET_CFC
SET_CFC$rr.recov.time[SET_CFC$rr.recov.time==4] <- 56.5 # Recode into actual time after start SET_CFC

# Density plot
densityPlot(SET_CFC$rr.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$rr.recov)
# Frequency table of the peak times
table(SET_CFC$rr.recov.time)

## Cortisol
# Get recovery
SET_CFC$cort.recov <- I(apply(SET_CFC[, c('Cortisol.4', 'Cortisol.5',
                                          'Cortisol.6', 'Cortisol.7')], 1, min)) - 
  I(apply(SET_CFC[, c('Cortisol.2', 'Cortisol.3', 'Cortisol.4', 
                      'Cortisol.5', 'Cortisol.6', 'Cortisol.7')], 1, max))
# Get peak time
SET_CFC$cort.recov.time <- apply(SET_CFC[, c('Cortisol.4', 'Cortisol.5', 
                                             'Cortisol.6', 'Cortisol.7')],
                                 1, function(x) {if (all(anyNA(x))) {NA}  else {which.min(x)} }) 
SET_CFC$cort.recov.time[SET_CFC$cort.recov.time==1] <- 36 # Recode into actual time after start SET_CFC
SET_CFC$cort.recov.time[SET_CFC$cort.recov.time==2] <- 41 # Recode into actual time after start SET_CFC
SET_CFC$cort.recov.time[SET_CFC$cort.recov.time==3] <- 46 # Recode into actual time after start SET_CFC
SET_CFC$cort.recov.time[SET_CFC$cort.recov.time==4] <- 51 # Recode into actual time after start SET_CFC

# Density plot
densityPlot(SET_CFC$cort.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$cort.recov)
# Frequency table of the peak times
table(SET_CFC$cort.recov.time)


## Testosterone
# Get recovery
SET_CFC$testo.recov <- I(apply(SET_CFC[, c('Testosterone.4', 'Testosterone.5',
                                           'Testosterone.6', 'Testosterone.7')], 1, max)) - 
  I(apply(SET_CFC[, c('Testosterone.2', 'Testosterone.3', 'Testosterone.4', 
                      'Testosterone.5', 'Testosterone.6', 'Testosterone.7')], 1, min))
# Get peak time
SET_CFC$testo.recov.time <- apply(SET_CFC[, c('Testosterone.4', 'Testosterone.5', 
                                              'Testosterone.6', 'Testosterone.7')],
                                  1, function(x) {if (all(anyNA(x))) {NA}  else {which.max(x)} }) 
SET_CFC$testo.recov.time[SET_CFC$testo.recov.time==1] <- 36 # Recode into actual time after start SET_CFC
SET_CFC$testo.recov.time[SET_CFC$testo.recov.time==2] <- 41 # Recode into actual time after start SET_CFC
SET_CFC$testo.recov.time[SET_CFC$testo.recov.time==3] <- 46 # Recode into actual time after start SET_CFC
SET_CFC$testo.recov.time[SET_CFC$testo.recov.time==4] <- 51 # Recode into actual time after start SET_CFC
SET_CFC$testo.recov.time <- as.factor(SET_CFC$testo.recov.time) # Set as facto
# Density plot
densityPlot(SET_CFC$testo.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$testo.recov)
# Frequency table of the peak times
table(SET_CFC$testo.recov.time)


## Estradiol
# Get recovery
SET_CFC$estra.recov <- I(apply(SET_CFC[, c('Estradiol.4', 'Estradiol.5',
                                           'Estradiol.6', 'Estradiol.7')], 1, min)) - 
  I(apply(SET_CFC[, c('Estradiol.2', 'Estradiol.3', 'Estradiol.4', 
                      'Estradiol.5', 'Estradiol.6', 'Estradiol.7')], 1, max))
# Get peak time
SET_CFC$estra.recov.time <- apply(SET_CFC[, c('Estradiol.4', 'Estradiol.5', 
                                              'Estradiol.6', 'Estradiol.7')],
                                  1, function(x) {if (all(anyNA(x))) {NA}  else {which.min(x)} }) 
SET_CFC$estra.recov.time[SET_CFC$estra.recov.time==1] <- 36 # Recode into actual time after start SET_CFC
SET_CFC$estra.recov.time[SET_CFC$estra.recov.time==2] <- 41 # Recode into actual time after start SET_CFC
SET_CFC$estra.recov.time[SET_CFC$estra.recov.time==3] <- 46 # Recode into actual time after start SET_CFC
SET_CFC$estra.recov.time[SET_CFC$estra.recov.time==4] <- 51 # Recode into actual time after start SET_CFC
SET_CFC$estra.recov.time <- as.factor(SET_CFC$estra.recov.time) # Set as facto
# Density plot
densityPlot(SET_CFC$estra.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$estra.recov)
# Frequency table of the peak times
table(SET_CFC$estra.recov.time)


# Make sure all the recovery variables are of class numeric.
sapply(SET_CFC, class) # Check class.
library(dplyr)
SET_CFC <- SET_CFC %>% mutate_if(funs(class(.) == "AsIs"), as.numeric)
sapply(SET_CFC, class) # Check class again.

# Save dataSET_CFC
save(SET_CFC, file = "SET_CFC.RData")


# Save individual reactivity and recovery ----------------------------------------------------
## Packages
library(dplyr) # Data manipulation
library(magrittr) # Piping
library(xlsx) # Exporting to Excel

# Load data
load("SET_CFC.RData")

## Get all baselines
SET_CFC_MinMax <- SET_CFC %>% select(Sex, LSAS_Split,
                                     RS_Frontal_Avg_dPAC_Z, RS_Frontal_Avg_AAC_R,
                                     RS_Parietal_Avg_dPAC_Z, RS_Parietal_Avg_AAC_R,
                                     Anx.1, Cortisol.1)

## Calculate individual max (reactivity) and mix (recovery)
# Frontal dPAC
SET_CFC_MinMax$Frontal_dPAC_Z_max <- I(apply(SET_CFC[, c('EarlyAnticip_Frontal_Avg_dPAC_Z', 'LateAnticip_Frontal_Avg_dPAC_Z')], 1, max))
SET_CFC_MinMax$Frontal_dPAC_Z_min <- I(apply(SET_CFC[, c('EarlyRecov_Frontal_Avg_dPAC_Z', 'LateRecov_Frontal_Avg_dPAC_Z')], 1, min))

# Parietal dPAC
SET_CFC_MinMax$Parietal_dPAC_Z_max <- I(apply(SET_CFC[, c('EarlyAnticip_Parietal_Avg_dPAC_Z', 'LateAnticip_Parietal_Avg_dPAC_Z')], 1, max))
SET_CFC_MinMax$Parietal_dPAC_Z_min <- I(apply(SET_CFC[, c('EarlyRecov_Parietal_Avg_dPAC_Z', 'LateRecov_Parietal_Avg_dPAC_Z')], 1, min))

# Frontal AAC
SET_CFC_MinMax$Frontal_AAC_R_max <- I(apply(SET_CFC[, c('EarlyAnticip_Frontal_Avg_AAC_R', 'LateAnticip_Frontal_Avg_AAC_R')], 1, max))
SET_CFC_MinMax$Frontal_AAC_R_min <- I(apply(SET_CFC[, c('EarlyRecov_Frontal_Avg_AAC_R', 'LateRecov_Frontal_Avg_AAC_R')], 1, min))

# Parietal AAC
SET_CFC_MinMax$Parietal_AAC_R_max <- I(apply(SET_CFC[, c('EarlyAnticip_Parietal_Avg_AAC_R', 'LateAnticip_Parietal_Avg_AAC_R')], 1, max))
SET_CFC_MinMax$Parietal_AAC_R_min <- I(apply(SET_CFC[, c('EarlyRecov_Parietal_Avg_AAC_R', 'LateRecov_Parietal_Avg_AAC_R')], 1, min))

# Anxiety
SET_CFC_MinMax$Anx.23_max <- I(apply(SET_CFC[, c('Anx.2', 'Anx.3')], 1, max))
SET_CFC_MinMax$Anx.34_min <- I(apply(SET_CFC[, c('Anx.3', 'Anx.4')], 1, min))

# Cortisol
SET_CFC_MinMax$Cortisol.234567_max <- I(apply(SET_CFC[, c('Cortisol.2', 'Cortisol.3', 'Cortisol.4', 
                                                          'Cortisol.5', 'Cortisol.6', 'Cortisol.7')], 1, max))
SET_CFC_MinMax$Cortisol.4567_min <- I(apply(SET_CFC[, c('Cortisol.4', 'Cortisol.5',
                                                        'Cortisol.6', 'Cortisol.7')], 1, min))

# Make sure all the recovery variables are of class numeric.
sapply(SET_CFC_MinMax, class) # Check class.
SET_CFC_MinMax <- SET_CFC_MinMax %>% mutate_if(funs(class(.) == "AsIs"), as.numeric)
sapply(SET_CFC_MinMax, class) # Check class again.

# Save dataSET_CFC
save(SET_CFC_MinMax, file = "SET_CFC_MinMax.RData")



# Area under the curve ----------------------------------------------------

## Packages
library(dplyr)
library(magrittr)

### Select frontal PAC data
data <- SET_CFC %>% select(RS_Frontal_Avg_dPAC_Z,
                           EarlyAnticip_Frontal_Avg_dPAC_Z,
                           LateAnticip_Frontal_Avg_dPAC_Z,
                           EarlyRecov_Frontal_Avg_dPAC_Z,
                           LateRecov_Frontal_Avg_dPAC_Z)
## Calculate AUC
# Initialize variables
n.obs <- function(x) {sum(complete.cases(na.omit(x)))} # Custom function to calculate the number of observations
obs <- n.obs(data) # Number of observations
t <- as.vector(c(0, 30, 32.5, 49, 79)) # Time points of each measurement
auc <- array(NA, dim = c(obs, length(t)-1))
AUCg <- matrix(NA, nrow = obs, ncol = 1)
AUC1 <- matrix(NA, nrow = obs, ncol = 1)
# First part of formula for calculating AUCg
for (n in 1:obs) {
  for (i in 1:I(length(t)-1)) {
    auc[n, i] <- I(data[n, i+1] + data[n, i]) * I(t[i+1]-t[i]) / 2
  }
}
# Second part of formula for calculating AUCg
AUCg[, 1] <- rowSums(auc[, ])
# Subtract the ground of AUCg to get AUC1
for (n in 1:obs) {
  AUC1[n, 1] <- AUCg[n, 1] - I(data[n, 1] * t[length(t)])
}
# Add new variable to dataframe
SET_CFC$frontal_PAC.auc <- AUC1


### Select parietal PAC data
data <- SET_CFC %>% select(RS_Parietal_Avg_dPAC_Z,
                           EarlyAnticip_Parietal_Avg_dPAC_Z,
                           LateAnticip_Parietal_Avg_dPAC_Z,
                           EarlyRecov_Parietal_Avg_dPAC_Z,
                           LateRecov_Parietal_Avg_dPAC_Z)
## Calculate AUC
# Initialize variables
n.obs <- function(x) {sum(complete.cases(na.omit(x)))} # Custom function to calculate the number of observations
obs <- n.obs(data) # Number of observations
t <- as.vector(c(0, 30, 32.5, 49, 79)) # Time points of each measurement
auc <- array(NA, dim = c(obs, length(t)-1))
AUCg <- matrix(NA, nrow = obs, ncol = 1)
AUC1 <- matrix(NA, nrow = obs, ncol = 1)
# First part of formula for calculating AUCg
for (n in 1:obs) {
  for (i in 1:I(length(t)-1)) {
    auc[n, i] <- I(data[n, i+1] + data[n, i]) * I(t[i+1]-t[i]) / 2
  }
}
# Second part of formula for calculating AUCg
AUCg[, 1] <- rowSums(auc[, ])
# Subtract the ground of AUCg to get AUC1
for (n in 1:obs) {
  AUC1[n, 1] <- AUCg[n, 1] - I(data[n, 1] * t[length(t)])
}
# Add new variable to dataframe
SET_CFC$parietal_PAC.auc <- AUC1


### Select frontal AAC data
data <- SET_CFC %>% select(RS_Frontal_Avg_AAC_R,
                           EarlyAnticip_Frontal_Avg_AAC_R,
                           LateAnticip_Frontal_Avg_AAC_R,
                           EarlyRecov_Frontal_Avg_AAC_R,
                           LateRecov_Frontal_Avg_AAC_R)
## Calculate AUC
# Initialize variables
n.obs <- function(x) {sum(complete.cases(na.omit(x)))} # Custom function to calculate the number of observations
obs <- n.obs(data) # Number of observations
t <- as.vector(c(0, 30, 32.5, 49, 79)) # Time points of each measurement
auc <- array(NA, dim = c(obs, length(t)-1))
AUCg <- matrix(NA, nrow = obs, ncol = 1)
AUC1 <- matrix(NA, nrow = obs, ncol = 1)
# First part of formula for calculating AUCg
for (n in 1:obs) {
  for (i in 1:I(length(t)-1)) {
    auc[n, i] <- I(data[n, i+1] + data[n, i]) * I(t[i+1]-t[i]) / 2
  }
}
# Second part of formula for calculating AUCg
AUCg[, 1] <- rowSums(auc[, ])
# Subtract the ground of AUCg to get AUC1
for (n in 1:obs) {
  AUC1[n, 1] <- AUCg[n, 1] - I(data[n, 1] * t[length(t)])
}
# Add new variable to dataframe
SET_CFC$frontal_AAC.auc <- AUC1


### Select parietal AAC data
data <- SET_CFC %>% select(RS_Parietal_Avg_AAC_R,
                           EarlyAnticip_Parietal_Avg_AAC_R,
                           LateAnticip_Parietal_Avg_AAC_R,
                           EarlyRecov_Parietal_Avg_AAC_R,
                           LateRecov_Parietal_Avg_AAC_R)
## Calculate AUC
# Initialize variables
n.obs <- function(x) {sum(complete.cases(na.omit(x)))} # Custom function to calculate the number of observations
obs <- n.obs(data) # Number of observations
t <- as.vector(c(0, 30, 32.5, 49, 79)) # Time points of each measurement
auc <- array(NA, dim = c(obs, length(t)-1))
AUCg <- matrix(NA, nrow = obs, ncol = 1)
AUC1 <- matrix(NA, nrow = obs, ncol = 1)
# First part of formula for calculating AUCg
for (n in 1:obs) {
  for (i in 1:I(length(t)-1)) {
    auc[n, i] <- I(data[n, i+1] + data[n, i]) * I(t[i+1]-t[i]) / 2
  }
}
# Second part of formula for calculating AUCg
AUCg[, 1] <- rowSums(auc[, ])
# Subtract the ground of AUCg to get AUC1
for (n in 1:obs) {
  AUC1[n, 1] <- AUCg[n, 1] - I(data[n, 1] * t[length(t)])
}
# Add new variable to dataframe
SET_CFC$parietal_AAC.auc <- AUC1


### Save dataframe
save(SET_CFC, file = "SET_CFC.RData")

### Remove unnecessary variables
remove(auc, AUC1, AUCg, data, i, n, obs, t, n.obs)

# Grubbs outlier calculation (preregistered) ----------------------------------------------

# Packages
library(outliers) # Grubbs test
library(magrittr) # Piping
library(xlsx) # Exporting to Excel

# Load data
load("SET_CFC_brief.RData")
data <- SET_CFC_brief

# Check outliers
grub <- list(NA)
grub2 <- list(NA)
for (i in 1:ncol(data)) {
  if (is.numeric(data[, i])) {
    # Only complete cases
    sub <- subset(data[, c(1, i)], complete.cases(data[, i]))
    # Lower tail
    grub[[i]] <- grubbs.test(na.omit(data[, i]), type = 10, opposite = TRUE)
    grub[[i]][["data.name"]] <- colnames(data)[i]
    grub[[i]][["outlier.subject.number"]] <- sub[which.min(sub[, 2]), "Subject"]
    # Upper tail
    grub2[[i]] <- grubbs.test(na.omit(data[, i]), type = 10, opposite = FALSE)
    grub2[[i]][["data.name"]] <- colnames(data)[i]
    grub2[[i]][["outlier.subject.number"]] <- sub[which.max(sub[, 2]), "Subject"]
  } else {
    grub[[i]] <- NA
    grub2[[i]] <- NA
  }
}
# Get indices of NA in grub
factors <- which(is.na(grub))

## Put the t-test results in a table
# Extract results from list
t.table1 <- sapply(grub[-c(factors)], function(x) {
  c(test = x$method,
    var = x$data.name,
    test.stat = x$statistic[["G"]],
    subject.number = x$outlier.subject.number,
    p.value = x$p.value)
})

t.table2 <- sapply(grub2[-c(factors)], function(x) {
  c(test = x$method,
    var = x$data.name,
    test.stat = x$statistic[["G"]],
    subject.number = x$outlier.subject.number,
    p.value = x$p.value)
})

# Save the lower tail results as a dataframe
t.table1 <- as.data.frame(t.table1)
t.table1 <- t(t.table1) %>% as.data.frame(stringsAsFactors = FALSE) # Transpose the dataframe
t.table1[, 3:5] <- lapply(t.table1[, 3:5], function(x) as.numeric(as.character(x))) %>% 
  lapply(function(x) round(x, 5)) # Set the numeric columns to numeric and round to 5 decimals
t.table1[, "tail"] <- "lower" # Save for which tail the outlier was
# Save the upper tail results as a dataframe
t.table2 <- as.data.frame(t.table2)
t.table2 <- t(t.table2) %>% as.data.frame(stringsAsFactors = FALSE) # Transpose the dataframe
t.table2[, 3:5] <- lapply(t.table2[, 3:5], function(x) as.numeric(as.character(x))) %>% 
  lapply(function(x) round(x, 5)) # Set the numeric columns to numeric and round to 5 decimals
t.table2[, "tail"] <- "upper" # Save for which tail the outlier was
# Bind together
t.table <- rbind.data.frame(t.table1[-c(1:2), ], t.table2[-c(1:2), ])
rownames(t.table) <- NULL # Reset rownames

## Adjust p-values with bonferroni-correction
p.adj <- p.adjust(t.table[, "p.value"], method = "bonferroni", n = length(t.table[, "p.value"])) # Do fdr-correction
t.table[, "p.value.adj"] <- p.adj # Add adjusted p-values into the dataframe with all results

# Save new table with only significant variables
t.table.sig <- t.table[t.table$p.value.adj <= .05, ]
rownames(t.table.sig) <- NULL
write.xlsx(t.table.sig, "outliers_SET_CFC_brief.xlsx")

# Count the number of unique subject numbers
num.outl <- t.table.sig$subject.number %>% unique() %>% length()

## Remove temporary variables
remove(grub)
remove(grub2)
remove(factors)
remove(t.table1)
remove(t.table2)
remove(t.table)
remove(p.adj)
remove(sub)
remove(data)
remove(t.table.sig)
remove(i)
remove(num.outl)

# Normality tests (preregistered) ---------------------------------------------------------
# Load packages
library(dplyr) # Data manipulation
library(magrittr) # Piping
library(xlsx) # Exporting to Excel

# Custom function to check the significance of p.values
p.value.sig <- function(x) {
  if (x < 0.001) {
    y <- "***"
  } else if (x < 0.01) {
    y <- "**"
  } else if (x < 0.05) {
    y <- "*"
  } else if (x < 0.1) {
    y <- "+"
  } else {
    y <- "not significant"
  } 
}

# Select variables
data <- SET_CFC_brief %>% select(-Subject, -Sex)

## Shapiro-Wilk test of normality
# Initialize results list
normTest <- list()
# Test all columns in the data
for (i in 1:ncol(data)) {
  normTest[[i]] <- shapiro.test(data[, i])
  normTest[[i]][["data.name"]] <- colnames(data)[i]
}
# Convert results to a dataframe
normTest_df <- do.call(rbind.data.frame, normTest)
# Do FDR-correction on p-values
p.val <- normTest_df$p.value
p.adj <-  p.adjust(p.val, method = "fdr", n = length(p.val)) # Do fdr-correction
normTest_df$p.adj <- p.adj
normTest_df$p.adj.sig <- sapply(normTest_df$p.adj, function(x) p.value.sig(x)) # Add column with significance

# Save lm results
write.xlsx(normTest_df, "normTest.SET_CFC_brief.xlsx")

## Remove temporary variables
remove(normTest_df)
remove(normTest)
remove(i)
remove(data)
remove(p.val)
remove(p.adj)
remove(p.value.sig)


# Median split trait social anxiety (exploratory) ------------------------------------------------------------

# Median split the LSAS variable
SET_CFC$LSAS_Split <- factor(
  SET_CFC$LSAS <= median(SET_CFC$LSAS), 
  levels = c(TRUE, FALSE), 
  labels = c("Low", "High"))
# Check class
class(SET_CFC$LSAS_Split)
# Check length of each group
length(SET_CFC$LSAS_Split[SET_CFC$LSAS_Split == "Low"])
length(SET_CFC$LSAS_Split[SET_CFC$LSAS_Split == "High"])

# Save new dataset
setwd("D:\\Publications\\4. Social-evaluative threat oscillations\\Analysis\\R\\CFC")
save(SET_CFC, file = "SET_CFC.RData")


# Exporting descriptives (preregistered) --------------------------------------------------

## Packages
library(dplyr) # Data manipulation
library(magrittr) # Piping
library(xlsx) # Exporting to Excel

# Load data
load("SET_CFC.RData")
data <- SET_CFC %>% select(c("Subject", "Sex", "LSAS_Split", "LSAS", "EnglishCompetence", I(which(colnames(SET_CFC)=="RS_Frontal_Avg_dPAC_Z"):which(colnames(SET_CFC)=="recov_Parietal_Avg_AAC_R")),
                             "recov.time_Frontal_Avg_dPAC_Z", "recov.time_Parietal_Avg_dPAC_Z",
                             "recov.time_Frontal_Avg_AAC_R", "recov.time_Parietal_Avg_AAC_R",
                             seq(which(colnames(SET_CFC)=="Cortisol.1"), I(which(colnames(SET_CFC)=="Cortisol.1")+6)),
                             seq(which(colnames(SET_CFC)=="Anx.1"), I(which(colnames(SET_CFC)=="Anx.1")+7)),
                             "anx.react", "anx.recov", "anx.react.time", "anx.recov.time",
                             "cort.react", "cort.recov", "cort.react.time", "cort.recov.time" ))

## Descriptive statistics
# Custom standard error function
se <- function(x) {sqrt(var(x)/length(x))}
# Custom function to get descriptive statistics per column
descr <- function(x) {
  mean <- round(mean(na.omit(x)), 3)
  sd <- round(sd(na.omit(x)), 3)
  se <- round(se(na.omit(x)),3)
  n <- round(sum(complete.cases(na.omit(x))), 3)
  return(c(mean, sd, se, n))
}
# Custom function to get descriptive statistics per column for male and female separately
descr.group <- function(x, groupvar, group1, group2) {
  x.group1 <- subset(x, groupvar == group1)
  mean.group1 <- round(mean(na.omit(x.group1)), 3)
  sd.group1 <- round(sd(na.omit(x.group1)), 3)
  se.group1 <- round(se(na.omit(x.group1)),3)
  n.group1 <- round(sum(complete.cases(na.omit(x.group1))), 3)
  x.group2 <- subset(x, groupvar == group2)
  mean.group2 <- round(mean(na.omit(x.group2)), 3)
  sd.group2 <- round(sd(na.omit(x.group2)), 3)
  se.group2 <- round(se(na.omit(x.group2)),3)
  n.group2 <- round(sum(complete.cases(na.omit(x.group2))), 3)
  return(c(mean.group1, sd.group1, se.group1, n.group1, mean.group2, sd.group2, se.group2, n.group2))
}

# Apply descrive statistics on all numeric columns for complete cases only (skipping the Subject number variable)
SET_CFC_summary <- data[, -c(which(colnames(data)=="Subject"))] %>% select_if(is.numeric) %>% sapply(function(x) descr(x)) %>% as.data.frame()
# Set the rownames correctly
rownames(SET_CFC_summary) <- c("Mean", "Standard deviation", "Standard error of the mean", "Number of observations")
write.xlsx(SET_CFC_summary, "summary_SET_CFC.xlsx")
save(SET_CFC_summary, file = "SET_CFC_summary.RData")

# Apply descriptive statistics on all numeric columns for complete cases separately for male and female (skipping the Subject number variable)
SET_CFC_summary_sex <- data[, -c(which(colnames(data)=="Subject"))] %>% select_if(is.numeric) %>% sapply(function(x) descr.group(x, data$Sex, "Male", "Female")) %>% as.data.frame()
rownames(SET_CFC_summary_sex) <- c("Mean: Male", "Standard deviation: Male", "Standard error of the mean: Male", "Number of observations: Male", 
                                        "Mean: Female", "Standard deviation: Female", "Standard error of the mean: Female", "Number of observations: Female")
write.xlsx(SET_CFC_summary_sex, "summary.sex_SET_CFC.xlsx")
save(SET_CFC_summary_sex, file = "SET_CFC_summary_sex.RData")

# Apply descriptive statistics on all numeric columns for complete cases separately for low and high LSAS (skipping the Subject number variable)
SET_CFC_summary_group <- data[, -c(which(colnames(data)=="Subject"))] %>% select_if(is.numeric) %>% sapply(function(x) descr.group(x, data$LSAS_Split, "Low", "High")) %>% as.data.frame()
rownames(SET_CFC_summary_group) <- c("Mean: Low LSAS", "Standard deviation: Low LSAS", "Standard error of the mean: Low LSAS", "Number of observations: Low LSAS", 
                                     "Mean: High LSAS", "Standard deviation: High LSAS", "Standard error of the mean: High LSAS", "Number of observations: High LSAS")
write.xlsx(SET_CFC_summary_group, "summary.LSAS_SET_CFC.xlsx")
save(SET_CFC_summary_group, file = "SET_CFC_summary_LSAS_Split.RData")

## Remove temporary variables
remove(se)
remove(descr)
remove(descr.group)
remove(SET_CFC_summary)
remove(data)
remove(SET_CFC_summary_sex)
remove(SET_CFC_summary_group)


# Exporting descriptives of individual reactivity and recovery (exploratory) --------------------------------------------------
## Packages
library(dplyr) # Data manipulation
library(magrittr) # Piping
library(xlsx) # Exporting to Excel

# Load data
load("SET_CFC_MinMax.RData")

# Custom standard error function
se <- function(x) {sqrt(var(x)/length(x))}
# Custom function to get descriptive statistics per column
descr <- function(x) {
  mean <- round(mean(na.omit(x)), 3)
  sd <- round(sd(na.omit(x)), 3)
  se <- round(se(na.omit(x)),3)
  n <- round(sum(complete.cases(na.omit(x))), 3)
  return(c(mean, sd, se, n))
}
# Custom function to get descriptive statistics per column for male and female separately
descr.group <- function(x, groupvar, group1, group2) {
  x.group1 <- subset(x, groupvar == group1)
  mean.group1 <- round(mean(na.omit(x.group1)), 3)
  sd.group1 <- round(sd(na.omit(x.group1)), 3)
  se.group1 <- round(se(na.omit(x.group1)),3)
  n.group1 <- round(sum(complete.cases(na.omit(x.group1))), 3)
  x.group2 <- subset(x, groupvar == group2)
  mean.group2 <- round(mean(na.omit(x.group2)), 3)
  sd.group2 <- round(sd(na.omit(x.group2)), 3)
  se.group2 <- round(se(na.omit(x.group2)),3)
  n.group2 <- round(sum(complete.cases(na.omit(x.group2))), 3)
  return(c(mean.group1, sd.group1, se.group1, n.group1, mean.group2, sd.group2, se.group2, n.group2))
}

# Apply descrive statistics on all numeric columns for complete cases only
SET_CFC_MinMax_summary <- SET_CFC_MinMax %>% select_if(is.numeric) %>% sapply(function(x) descr(x)) %>% as.data.frame()
# Set the rownames correctly
rownames(SET_CFC_MinMax_summary) <- c("Mean", "Standard deviation", "Standard error of the mean", "Number of observations")
write.xlsx(SET_CFC_MinMax_summary, "summary_SET_CFC_MinMax.xlsx")

# Apply descriptive statistics on all numeric columns for complete cases separately for male and female
SET_CFC_MinMax_summary_sex <- SET_CFC_MinMax %>% select_if(is.numeric) %>% sapply(function(x) descr.group(x, SET_CFC_MinMax$Sex, "Male", "Female")) %>% as.data.frame()
rownames(SET_CFC_MinMax_summary_sex) <- c("Mean: Male", "Standard deviation: Male", "Standard error of the mean: Male", "Number of observations: Male", 
                                          "Mean: Female", "Standard deviation: Female", "Standard error of the mean: Female", "Number of observations: Female")
write.xlsx(SET_CFC_MinMax_summary_sex, "summary.sex_SET_CFC_MinMax.xlsx")
save(SET_CFC_MinMax_summary_sex, file = "SET_CFC_MinMax_summary_sex.RData")

# Apply descriptive statistics on all numeric columns for complete cases separately for low and high LSAS
SET_CFC_MinMax_summary_LSAS_Split <- SET_CFC_MinMax %>% select_if(is.numeric) %>% sapply(function(x) descr.group(x, SET_CFC_MinMax$LSAS_Split, "Low", "High")) %>% as.data.frame()
rownames(SET_CFC_MinMax_summary_LSAS_Split) <- c("Mean: Low LSAS", "Standard deviation: Low LSAS", "Standard error of the mean: Low LSAS", "Number of observations: Low LSAS", 
                                                 "Mean: High LSAS", "Standard deviation: High LSAS", "Standard error of the mean: High LSAS", "Number of observations: High LSAS")
write.xlsx(SET_CFC_MinMax_summary_LSAS_Split, "summary.LSAS_SET_CFC_MinMax.xlsx")
save(SET_CFC_MinMax_summary_LSAS_Split, file = "SET_CFC_MinMax_summary_LSAS_Split.RData")

## Remove temporary variables
remove(se)
remove(descr)
remove(descr.group)
remove(SET_CFC_MinMax_summary)
remove(SET_CFC_MinMax_summary_sex)
remove(SET_CFC_MinMax_summary_LSAS_Split)


# Line / bar plots (preregistered) -------------------------------------------------------------------

## Packages
library(grid) # To organize multiple subplots in one large plot
library(gridBase) # To organize multiple subplots in one large plot
library(gridExtra) # To organize multiple subplots in one large plot
library(ggplot2) # Plotting packages (used for LSAS density plots)
library(reshape2) # To reshape into long format to use with ggplot
library(magrittr) # For piping
library(dplyr) # For data manipulation

# Load data
load("SET_CFC_summary.RData")

### Make plots with base R (using "SET_CFC_summary_group")
# Save default parameters
restore <- par(no.readonly = TRUE)


## Figure 1: Cross-frequency coupling and psychological/endocrinological states and traits: divided by LSAS group
tiff("FIG1.tiff", width = 40, height = 25, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(2, 2), oma = c(0, 0, 4, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left


## A) Frontal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.60, 1.00), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.60,
     xright = 13,
     ytop = 1.00,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.60,
     xright = 18,
     ytop = 1.00,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 1.0, x1 = 60, y1 = 1.0, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.90, x1 = 60, y1 = 0.90, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.80, x1 = 60, y1 = 0.80, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.70, x1 = 60, y1 = 0.70, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.60, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.60, 1.00, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary["Mean", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary["Mean", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] + SET_CFC_summary["Standard error of the mean", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary["Mean", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] - SET_CFC_summary["Standard error of the mean", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2.5) # Move inwards
# Add y-axis label
title(ylab = "Z-score", # Y label
      cex.lab = 2.2, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Phase-amplitude coupling", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("a)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left



## B) Frontal AAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.0, 0.03), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.0,
     xright = 13,
     ytop = 0.03,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.0,
     xright = 18,
     ytop = 0.03,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 0.03, x1 = 60, y1 = 0.03, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.02, x1 = 60, y1 = 0.02, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.01, x1 = 60, y1 = 0.01, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.0, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.0, 0.03, 0.01)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary["Mean", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary["Mean", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] + SET_CFC_summary["Standard error of the mean", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary["Mean", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] - SET_CFC_summary["Standard error of the mean", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Correlation coefficient", # Y label
      cex.lab = 2.2, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Amplitude-amplitude coupling", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## C) State anxiety
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(5, 65), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later (more inwards)
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 5,
     xright = 13,
     ytop = 65,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 5,
     xright = 18,
     ytop = 65,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 65, x1 = 60, y1 = 65, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 45, x1 = 60, y1 = 45, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 25, x1 = 60, y1 = 25, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 5, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(5, 65, 20)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add data
lines(x = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary["Mean", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add error bars
arrows(x0 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary["Mean", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] + SET_CFC_summary["Standard error of the mean", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means plus SE
       x1 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary["Mean", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] - SET_CFC_summary["Standard error of the mean", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Visual analogue scale (0 - 100)", # Y label
      cex.lab = 2.2, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "State anxiety", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("c)")), side = 3, adj = 0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)


## D) Cortisol
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(3, 8), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 3,
     xright = 13,
     ytop = 8,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 3,
     xright = 18,
     ytop = 8,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 8, x1 = 60, y1 = 8, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 7, x1 = 60, y1 = 7, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 6, x1 = 60, y1 = 6, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 5, x1 = 60, y1 = 5, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 4, x1 = 60, y1 = 4, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 3, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(3, 8, 1)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add data
lines(x = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary["Mean", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add error bars
arrows(x0 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary["Mean", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] + SET_CFC_summary["Standard error of the mean", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means plus SE
       x1 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary["Mean", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] - SET_CFC_summary["Standard error of the mean", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Free salivary cortisol (ng/ml)", # Y label
      cex.lab = 2.2, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "Cortisol", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("d)")), side = 3, adj = 0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)


## Plot all subfigures of Figure 1 simultaneously
# Main title
mtext("Frontal delta-beta coupling and stress responses",
      outer = TRUE, # Add to outer margins
      cex = 3.5) # Make bigger
# Print Figure
dev.off()
# Reset parameters
par(restore)



## Figure 2: Parietal cross-frequency coupling: divided by LSAS group
tiff("FIG2.tiff", width = 40, height = 12.5, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(1, 2), oma = c(0, 0, 4, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left


## B) Parietal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.80, 1.10), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.80,
     xright = 13,
     ytop = 1.10,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.80,
     xright = 18,
     ytop = 1.10,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 1.10, x1 = 60, y1 = 1.10, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.0, x1 = 60, y1 = 1.0, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.90, x1 = 60, y1 = 0.90, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.80, cex.axis = 1.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 1.5, las = 1, at = seq(0.80, 1.10, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary["Mean", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary["Mean", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] + SET_CFC_summary["Standard error of the mean", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary["Mean", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] - SET_CFC_summary["Standard error of the mean", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 1.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Z-score", # Y label
      cex.lab = 1.5, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Phase-amplitude coupling", # Title of plot
      cex.main = 2, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## B) Parietal AAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.01, 0.04), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.01,
     xright = 13,
     ytop = 0.04,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.01,
     xright = 18,
     ytop = 0.04,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 0.04, x1 = 60, y1 = 0.04, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.03, x1 = 60, y1 = 0.03, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.02, x1 = 60, y1 = 0.02, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.01, cex.axis = 1.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 1.5, las = 1, at = seq(0.01, 0.04, 0.01)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary["Mean", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary["Mean", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] + SET_CFC_summary["Standard error of the mean", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary["Mean", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] - SET_CFC_summary["Standard error of the mean", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 1.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Correlation coefficient", # Y label
      cex.lab = 1.5, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Amplitude-amplitude correlation", # Title of plot
      cex.main = 2, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## Plot all subfigures of Figure 2 simultaneously
# Main title
mtext("Parietal delta-beta coupling",
      outer = TRUE, # Add to outer margins
      cex = 3.5, # Make bigger
      line = 1) # Move the text slightly upwards
# Print Figure
dev.off()
# Reset parameters
par(restore)


# Remove temporary variables
remove(SET_CFC_summary)
remove(restore)


# Line / bar plots men-women (preregistered) -------------------------------------------------------------------

## Packages
library(grid) # To organize multiple subplots in one large plot
library(gridBase) # To organize multiple subplots in one large plot
library(gridExtra) # To organize multiple subplots in one large plot
library(ggplot2) # Plotting packages (used for LSAS density plots)
library(reshape2) # To reshape into long format to use with ggplot

# Load data
load("SET_CFC_summary_sex.RData")

### Make plots with base R (using "SET_CFC_summary_sex")
# Save default parameters
restore <- par(no.readonly = TRUE)

## Figure 1: Cross-frequency coupling and psychological/endocrinological states and traits: divided by men and women
tiff("FIG1_sex.tiff", width = 40, height = 37.50, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(3, 2), oma = c(0, 0, 4, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left


## A) Trait social anxiety
data <- SET_CFC %>% select(Sex, LSAS) # Select relevant variables
data <- melt(data) # Make into long format
# Main plot
a <- ggplot(data, aes(x = value, fill = Sex)) + # Use sex as a grouping factor
  geom_density(alpha = .5) + # Make two transparent density plots
  xlim(0, 100) + # Show the full range of LSAS scores
  ylim(0, .03) + # Show the full range of density scores
  scale_fill_manual(values=c("black", "grey80")) + # Fill density with manual colours
  labs(x = "Liebowitz social anxiety scale (0-144)", # Set the x-axis label
       title = "Trait social anxiety", # Set the title
       subtitle = "a)") + # Set the subtitle
  theme(plot.title = element_text(size = 32, face = "bold", family = "serif", hjust = 0.5, # Make the title larger, bold, and centered
                                  margin = margin(b = -27, unit = "pt")), # Lower the title
        plot.subtitle = element_text(size = 32, face = "bold", family = "serif"), # Make the subtitle larger and bold
        legend.position = "none", # Remove legend
        legend.title = element_blank(), # Remove legend
        legend.text = element_blank(), # Remove legend
        axis.title = element_text(size = 22, family = "serif", colour = "black"), # Make the axis title larger and black
        axis.text = element_text(size = 22, family = "serif", colour = "black"), # Make the axis text larger and black
        panel.grid.major = element_line(size = 0.5, colour = "grey50"), # Make the major grid lines grey and thinner
        panel.grid.major.x = element_blank(), # Remove major grid lines from the x-axis
        panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.background = element_blank(), # Remove background
        axis.line = element_line(size = 0.5, colour = "black") # Add axis lines in thin black
  ) 

# Save space for the LSAS plot to be printed here later
plot.new()

# Plot another empty space at the top-right
plot.new()


## B) Frontal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.50, 1.10), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.50,
     xright = 13,
     ytop = 1.10,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.50,
     xright = 18,
     ytop = 1.10,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 1.10, x1 = 60, y1 = 1.10, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.0, x1 = 60, y1 = 1.0, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.90, x1 = 60, y1 = 0.90, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.80, x1 = 60, y1 = 0.80, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.70, x1 = 60, y1 = 0.70, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.60, x1 = 60, y1 = 0.60, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.50, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.50, 1.10, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_sex["Mean: Male", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_sex["Mean: Male", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] + SET_CFC_summary_sex["Standard error of the mean: Male", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_sex["Mean: Male", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] - SET_CFC_summary_sex["Standard error of the mean: Male", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_sex["Mean: Female", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_sex["Mean: Female", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] + SET_CFC_summary_sex["Standard error of the mean: Female", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_sex["Mean: Female", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] - SET_CFC_summary_sex["Standard error of the mean: Female", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2.5) # Move inwards
# Add y-axis label
title(ylab = "Z-score", # Y label
      cex.lab = 2.2, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Phase-amplitude coupling", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## C) Frontal AAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(-0.01, 0.03), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = -0.01,
     xright = 13,
     ytop = 0.03,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = -0.01,
     xright = 18,
     ytop = 0.03,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 0.03, x1 = 60, y1 = 0.03, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.02, x1 = 60, y1 = 0.02, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.01, x1 = 60, y1 = 0.01, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.00, x1 = 60, y1 = 0.00, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = -0.01, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(-0.01, 0.03, 0.01)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_sex["Mean: Male", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_sex["Mean: Male", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] + SET_CFC_summary_sex["Standard error of the mean: Male", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_sex["Mean: Male", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] - SET_CFC_summary_sex["Standard error of the mean: Male", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_sex["Mean: Female", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_sex["Mean: Female", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] + SET_CFC_summary_sex["Standard error of the mean: Female", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_sex["Mean: Female", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] - SET_CFC_summary_sex["Standard error of the mean: Female", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Correlation coefficient", # Y label
      cex.lab = 2.2, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Amplitude-amplitude coupling", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("c)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## D) State anxiety
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(5, 65), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later (more inwards)
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 5,
     xright = 13,
     ytop = 65,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 5,
     xright = 18,
     ytop = 65,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 65, x1 = 60, y1 = 65, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 45, x1 = 60, y1 = 45, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 25, x1 = 60, y1 = 25, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 5, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(5, 65, 20)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_sex["Mean: Male", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_sex["Mean: Male", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] + SET_CFC_summary_sex["Standard error of the mean: Male", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means plus SE
       x1 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_sex["Mean: Male", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] - SET_CFC_summary_sex["Standard error of the mean: Male", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_sex["Mean: Female", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_sex["Mean: Female", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] + SET_CFC_summary_sex["Standard error of the mean: Female", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means plus SE
       x1 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_sex["Mean: Female", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] - SET_CFC_summary_sex["Standard error of the mean: Female", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Visual analogue scale (0 - 100)", # Y label
      cex.lab = 2.2, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "State anxiety", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("d)")), side = 3, adj = 0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)


## E) Cortisol
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(3, 8), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 3,
     xright = 13,
     ytop = 8,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 3,
     xright = 18,
     ytop = 8,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 8, x1 = 60, y1 = 8, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 7, x1 = 60, y1 = 7, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 6, x1 = 60, y1 = 6, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 5, x1 = 60, y1 = 5, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 4, x1 = 60, y1 = 4, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 3, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(3, 8, 1)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_sex["Mean: Male", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_sex["Mean: Male", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] + SET_CFC_summary_sex["Standard error of the mean: Male", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means plus SE
       x1 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_sex["Mean: Male", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] - SET_CFC_summary_sex["Standard error of the mean: Male", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_sex["Mean: Female", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_sex["Mean: Female", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] + SET_CFC_summary_sex["Standard error of the mean: Female", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means plus SE
       x1 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_sex["Mean: Female", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] - SET_CFC_summary_sex["Standard error of the mean: Female", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2.5, # Make line width slightly thicker
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Free salivary cortisol (ng/ml)", # Y label
      cex.lab = 2.2, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "Cortisol", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("e)")), side = 3, adj = 0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)


## Trait social anxiety (again)
# Create a viewport to plot it in the topleft
vp <- viewport(height = unit(0.3,"npc"), width=unit(0.46, "npc"), 
               just = c("left","top"), y = 0.93, x = 0.007)
print(a, vp = vp)


## Plot all subfigures of Figure 1 simultaneously
# Main title
mtext("Frontal delta-beta coupling and stress responses",
      outer = TRUE, # Add to outer margins
      cex = 3.5) # Make bigger
# Overlay clear plot (in order to use legend location "top)
reset <- function() {
  par(mfrow = c(1, 1), oma = c(0, 0, 2, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(x = 0:1, y = 0:1, type = "n", xlab = "", ylab = "", axes = FALSE)
}
reset()
# Add legend
legend(x = "topright", # Location of legend
       legend = c("Men", "Women"), # Text of legend
       col = c("grey10", "grey40"), # Colours of lines
       lwd = c(5, 5), # Line width
       lty = c(1, 5), # Solid and dashed (long) lines
       bty = "n", # Don't add box / border
       ncol = 2, # Horizontal legend
       cex = 2) # Make text slightly bigger
# Print Figure
dev.off()
# Reset parameters
par(restore)


## Figure 2: Parietal cross-frequency coupling: divided by men and women
tiff("FIG2_sex.tiff", width = 40, height = 12.5, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(1, 2), oma = c(0, 0, 4, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left


## A) Parietal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.70, 1.20), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.70,
     xright = 13,
     ytop = 1.20,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.70,
     xright = 18,
     ytop = 1.20,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 1.20, x1 = 60, y1 = 1.20, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.10, x1 = 60, y1 = 1.10, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.0, x1 = 60, y1 = 1.0, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.90, x1 = 60, y1 = 0.90, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.80, x1 = 60, y1 = 0.80, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.70, cex.axis = 1.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 1.5, las = 1, at = seq(0.70, 1.20, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_sex["Mean: Male", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_sex["Mean: Male", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] + SET_CFC_summary_sex["Standard error of the mean: Male", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_sex["Mean: Male", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] - SET_CFC_summary_sex["Standard error of the mean: Male", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_sex["Mean: Female", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_sex["Mean: Female", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] + SET_CFC_summary_sex["Standard error of the mean: Female", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_sex["Mean: Female", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] - SET_CFC_summary_sex["Standard error of the mean: Female", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 1.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Z-score", # Y label
      cex.lab = 1.5, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Phase-amplitude coupling", # Title of plot
      cex.main = 2, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("a)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## B) Parietal AAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.0, 0.05), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.0,
     xright = 13,
     ytop = 0.05,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.0,
     xright = 18,
     ytop = 0.05,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 0.05, x1 = 60, y1 = 0.05, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.04, x1 = 60, y1 = 0.04, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.03, x1 = 60, y1 = 0.03, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.02, x1 = 60, y1 = 0.02, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.01, x1 = 60, y1 = 0.01, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.0, cex.axis = 1.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 1.5, las = 1, at = seq(0.0, 0.05, 0.01)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_sex["Mean: Male", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_sex["Mean: Male", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] + SET_CFC_summary_sex["Standard error of the mean: Male", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_sex["Mean: Male", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] - SET_CFC_summary_sex["Standard error of the mean: Male", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_sex["Mean: Female", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_sex["Mean: Female", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] + SET_CFC_summary_sex["Standard error of the mean: Female", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_sex["Mean: Female", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] - SET_CFC_summary_sex["Standard error of the mean: Female", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 1.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Correlation coefficient", # Y label
      cex.lab = 1.5, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Amplitude-amplitude coupling", # Title of plot
      cex.main = 2, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## Plot all subfigures of Figure 2 simultaneously
# Main title
mtext("Parietal delta-beta coupling",
      outer = TRUE, # Add to outer margins
      cex = 3.5, # Make bigger
      line = 1) # Move the text slightly upwards
# Overlay clear plot (in order to use legend location "top)
reset <- function() {
  par(mfrow = c(1, 1), oma = c(0, 0, 3, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(x = 0:1, y = 0:1, type = "n", xlab = "", ylab = "", axes = FALSE)
}
reset()
# Add legend
legend(x = "topright", # Location of legend
       legend = c("Men", "Women"), # Text of legend
       col = c("grey10", "grey40"), # Colours of lines
       lwd = c(5, 5), # Line width
       lty = c(1, 5), # Solid and dashed (long) lines
       bty = "n", # Don't add box / border
       ncol = 2, # Horizontal legend
       cex = 2) # Make text slightly bigger
# Print Figure
dev.off()
# Reset parameters
par(restore)



# Remove temporary variables
remove(SET_CFC_summary_sex)
remove(reset)
remove(restore)
remove(data)
remove(a)
remove(vp)


# Line / bar plots individual reactivity and recovery men-women (exploratory) -------------------------------------------------------------------

## Packages
library(grid) # To organize multiple subplots in one large plot
library(gridBase)
library(gridExtra) # To organize multiple subplots in one large plot
library(ggplot2) # Plotting packages (used for LSAS density plots)
library(reshape2) # To reshape into long format to use with ggplot
library(magrittr) # For piping
library(dplyr) # For data manipulation

## Load data
load("SET_CFC_MinMax_summary_sex.RData")
SET_CFC_summary_group <- SET_CFC_MinMax_summary_sex

### Make plots with base R (using "SET_CFC_summary_group")
# Save default parameters
restore <- par(no.readonly = TRUE)

## Figure 1: Cross-frequency coupling min-max and psychological/endocrinological states and traits: divided by men and women
tiff("FIG1_MinMax_sex.tiff", width = 40, height = 37.50, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(3, 2), oma = c(0, 0, 4, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left


## A) Trait social anxiety
data <- SET_CFC %>% select(Sex, LSAS) # Select relevant variables
data <- melt(data) # Make into long format
# Main plot
a <- ggplot(data, aes(x = value, fill = Sex)) + # Use sex as a grouping factor
  geom_density(alpha = .5) + # Make two transparent density plots
  xlim(0, 100) + # Show the full range of LSAS scores
  ylim(0, .03) + # Show the full range of density scores
  scale_fill_manual(values=c("black", "grey80")) + # Fill density with manual colours
  labs(x = "Liebowitz social anxiety scale (0-144)", # Set the x-axis label
       title = "Trait social anxiety", # Set the title
       subtitle = "a)") + # Set the subtitle
  theme(plot.title = element_text(size = 32, face = "bold", family = "serif", hjust = 0.5, # Make the title larger, bold, and centered
                                  margin = margin(b = -27, unit = "pt")), # Lower the title
        plot.subtitle = element_text(size = 32, face = "bold", family = "serif"), # Make the subtitle larger and bold
        legend.position = "none", # Remove legend
        legend.title = element_blank(), # Remove legend
        legend.text = element_blank(), # Remove legend
        axis.title = element_text(size = 22, family = "serif", colour = "black"), # Make the axis title larger and black
        axis.text = element_text(size = 22, family = "serif", colour = "black"), # Make the axis text larger and black
        panel.grid.major = element_line(size = 0.5, colour = "grey50"), # Make the major grid lines grey and thinner
        panel.grid.major.x = element_blank(), # Remove major grid lines from the x-axis
        panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.background = element_blank(), # Remove background
        axis.line = element_line(size = 0.5, colour = "black") # Add axis lines in thin black
  ) 

# Save space for the LSAS plot to be printed here later
plot.new()

# Plot another empty space at the top-right
plot.new()


## B) Frontal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.50, 1.30), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.50,
     xright = 13,
     ytop = 1.30,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.50,
     xright = 18,
     ytop = 1.30,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 1.30, x1 = 60, y1 = 1.30, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.20, x1 = 60, y1 = 1.20, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.10, x1 = 60, y1 = 1.10, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.0, x1 = 60, y1 = 1.0, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.90, x1 = 60, y1 = 0.90, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.80, x1 = 60, y1 = 0.80, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.70, x1 = 60, y1 = 0.70, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.60, x1 = 60, y1 = 0.60, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.50, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.50, 1.30, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Male data
lines(x = c(-25, 6.39, 33.17), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Male", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add Male error bars
arrows(x0 = c(-25, 6.39, 33.17), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Male", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")] + SET_CFC_summary_group["Standard error of the mean: Male", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")]), # Means plus SE
       x1 = c(-25, 6.39, 33.17), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Male", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")] - SET_CFC_summary_group["Standard error of the mean: Male", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add Female data
lines(x = c(-25, 6.48, 39.56), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Female", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add Female error bars
arrows(x0 = c(-25, 6.48, 39.56), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Female", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")] + SET_CFC_summary_group["Standard error of the mean: Female", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")]), # Means plus SE
       x1 = c(-25, 6.48, 39.56), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Female", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")] - SET_CFC_summary_group["Standard error of the mean: Female", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2.5) # Move inwards
# Add y-axis label
title(ylab = "Z-score", # Y label
      cex.lab = 2.2, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Phase-amplitude coupling", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## C) Frontal AAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(-0.02, 0.05), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = -0.02,
     xright = 13,
     ytop = 0.05,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = -0.02,
     xright = 18,
     ytop = 0.05,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 0.05, x1 = 60, y1 = 0.05, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.04, x1 = 60, y1 = 0.04, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.03, x1 = 60, y1 = 0.03, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.02, x1 = 60, y1 = 0.02, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.01, x1 = 60, y1 = 0.01, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.00, x1 = 60, y1 = 0.00, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = -0.01, x1 = 60, y1 = -0.01, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = -0.02, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(-0.02, 0.05, 0.01)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Male data
lines(x = c(-25, 6.25, 37.33), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Male", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add Male error bars
arrows(x0 = c(-25, 6.25, 37.33), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Male", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")] + SET_CFC_summary_group["Standard error of the mean: Male", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")]), # Means plus SE
       x1 = c(-25, 6.25, 37.33), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Male", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")] - SET_CFC_summary_group["Standard error of the mean: Male", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add Female data
lines(x = c(-25, 6.30, 40.67), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Female", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add Female error bars
arrows(x0 = c(-25, 6.30, 40.67), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Female", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")] + SET_CFC_summary_group["Standard error of the mean: Female", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")]), # Means plus SE
       x1 = c(-25, 6.30, 40.67), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Female", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")] - SET_CFC_summary_group["Standard error of the mean: Female", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Correlation coefficient", # Y label
      cex.lab = 2.2, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Amplitude-amplitude correlation", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("c)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## D) State anxiety
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(10, 70), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later (more inwards)
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 10,
     xright = 13,
     ytop = 70,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 10,
     xright = 18,
     ytop = 70,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 70, x1 = 60, y1 = 70, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 60, x1 = 60, y1 = 60, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 50, x1 = 60, y1 = 50, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 40, x1 = 60, y1 = 40, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 30, x1 = 60, y1 = 30, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 20, x1 = 60, y1 = 20, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 10, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(10, 70, 10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Male data
lines(x = c(-19, 13.67, 27.25), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Male", c("Anx.1", "Anx.23_max", "Anx.34_min")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add Male error bars
arrows(x0 = c(-19, 13.67, 27.25), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Male", c("Anx.1", "Anx.23_max", "Anx.34_min")] + SET_CFC_summary_group["Standard error of the mean: Male", c("Anx.1", "Anx.23_max", "Anx.34_min")]), # Means plus SE
       x1 = c(-19, 13.67, 27.25), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Male", c("Anx.1", "Anx.23_max", "Anx.34_min")] - SET_CFC_summary_group["Standard error of the mean: Male", c("Anx.1", "Anx.23_max", "Anx.34_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add Female data
lines(x = c(-19, 12.78, 25.11), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Female", c("Anx.1", "Anx.23_max", "Anx.34_min")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add Female error bars
arrows(x0 = c(-19, 12.78, 25.11), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Female", c("Anx.1", "Anx.23_max", "Anx.34_min")] + SET_CFC_summary_group["Standard error of the mean: Female", c("Anx.1", "Anx.23_max", "Anx.34_min")]), # Means plus SE
       x1 = c(-19, 12.78, 25.11), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Female", c("Anx.1", "Anx.23_max", "Anx.34_min")] - SET_CFC_summary_group["Standard error of the mean: Female", c("Anx.1", "Anx.23_max", "Anx.34_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Visual analogue scale (0 - 100)", # Y label
      cex.lab = 2.2, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "State anxiety", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("d)")), side = 3, adj = 0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)


## E) Cortisol
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(3, 9), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 3,
     xright = 13,
     ytop = 9,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 3,
     xright = 18,
     ytop = 9,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 9, x1 = 60, y1 = 9, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 8, x1 = 60, y1 = 8, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 7, x1 = 60, y1 = 7, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 6, x1 = 60, y1 = 6, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 5, x1 = 60, y1 = 5, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 4, x1 = 60, y1 = 4, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 3, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(3, 9, 1)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Male data
lines(x = c(-18, 33.67, 47.67), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Male", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add Male error bars
arrows(x0 = c(-18, 33.67, 47.67), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Male", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")] + SET_CFC_summary_group["Standard error of the mean: Male", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")]), # Means plus SE
       x1 = c(-18, 33.67, 47.67), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Male", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")] - SET_CFC_summary_group["Standard error of the mean: Male", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add Female data
lines(x = c(-18, 30.48, 47.67), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Female", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add Female error bars
arrows(x0 = c(-18, 30.48, 47.67), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Female", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")] + SET_CFC_summary_group["Standard error of the mean: Female", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")]), # Means plus SE
       x1 = c(-18, 30.48, 47.67), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Female", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")] - SET_CFC_summary_group["Standard error of the mean: Female", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2.5, # Make line width slightly thicker
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Free salivary cortisol (ng/ml)", # Y label
      cex.lab = 2.2, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "Cortisol", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("e)")), side = 3, adj = 0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)



## Trait social anxiety (again)
# Create a viewport to plot it in the topleft
vp <- viewport(height = unit(0.3,"npc"), width=unit(0.46, "npc"), 
               just = c("left","top"), y = 0.93, x = 0.007)
print(a, vp = vp)


## Plot all subfigures of Figure 1 simultaneously
# Main title
mtext("Individual frontal delta-beta coupling and stress responses",
      outer = TRUE, # Add to outer margins
      cex = 3.5) # Make bigger
# Overlay clear plot (in order to use legend location "top)
reset <- function() {
  par(mfrow = c(1, 1), oma = c(0, 0, 2, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(x = 0:1, y = 0:1, type = "n", xlab = "", ylab = "", axes = FALSE)
}
reset()
# Add legend
legend(x = "topright", # Location of legend
       legend = c("Men", "Women"), # Text of legend
       col = c("grey10", "grey40"), # Colours of lines
       lwd = c(5, 5), # Line width
       lty = c(1, 5), # Solid and dashed (long) lines
       bty = "n", # Don't add box / border
       ncol = 2, # Horizontal legend
       cex = 2) # Make text slightly bigger
# Print Figure
dev.off()
# Reset parameters
par(restore)



## Figure 2: Parietal cross-frequency coupling min-max: divided by men and women
tiff("FIG2_MinMax_sex.tiff", width = 40, height = 12.5, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(1, 2), oma = c(0, 0, 4, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left


## A) Parietal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.60, 1.30), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.60,
     xright = 13,
     ytop = 1.30,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.60,
     xright = 18,
     ytop = 1.30,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 1.30, x1 = 60, y1 = 1.30, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.20, x1 = 60, y1 = 1.20, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.10, x1 = 60, y1 = 1.10, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.0, x1 = 60, y1 = 1.0, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.90, x1 = 60, y1 = 0.90, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.80, x1 = 60, y1 = 0.80, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.70, x1 = 60, y1 = 0.70, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.60, cex.axis = 1.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 1.5, las = 1, at = seq(0.60, 1.30, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Male data
lines(x = c(-25, 6.39, 32.33), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Male", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add Male error bars
arrows(x0 = c(-25, 6.39, 32.33), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Male", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")] + SET_CFC_summary_group["Standard error of the mean: Male", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")]), # Means plus SE
       x1 = c(-25, 6.39, 32.33), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Male", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")] - SET_CFC_summary_group["Standard error of the mean: Male", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add Female data
lines(x = c(-25, 6.30, 38.44), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Female", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add Female error bars
arrows(x0 = c(-25, 6.30, 38.44), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Female", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")] + SET_CFC_summary_group["Standard error of the mean: Female", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")]), # Means plus SE
       x1 = c(-25, 6.30, 38.44), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Female", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")] - SET_CFC_summary_group["Standard error of the mean: Female", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 1.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Z-score", # Y label
      cex.lab = 1.5, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Phase-amplitude coupling", # Title of plot
      cex.main = 2, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("a)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## B) Parietal AAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(-0.01, 0.06), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = -0.01,
     xright = 13,
     ytop = 0.06,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = -0.01,
     xright = 18,
     ytop = 0.06,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 0.06, x1 = 60, y1 = 0.06, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.05, x1 = 60, y1 = 0.05, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.04, x1 = 60, y1 = 0.04, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.03, x1 = 60, y1 = 0.03, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.02, x1 = 60, y1 = 0.02, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.01, x1 = 60, y1 = 0.01, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.00, x1 = 60, y1 = 0.00, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = -0.01, cex.axis = 1.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 1.5, las = 1, at = seq(-0.01, 0.06, 0.01)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Male data
lines(x = c(-25, 6.32, 39.83), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Male", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add Male error bars
arrows(x0 = c(-25, 6.32, 39.83), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Male", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")] + SET_CFC_summary_group["Standard error of the mean: Male", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")]), # Means plus SE
       x1 = c(-25, 6.32, 39.83), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Male", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")] - SET_CFC_summary_group["Standard error of the mean: Male", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add Female data
lines(x = c(-25, 6.48, 41.78), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Female", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add Female error bars
arrows(x0 = c(-25, 6.48, 41.78), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Female", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")] + SET_CFC_summary_group["Standard error of the mean: Female", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")]), # Means plus SE
       x1 = c(-25, 6.48, 41.78), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Female", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")] - SET_CFC_summary_group["Standard error of the mean: Female", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 1.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Correlation coefficient", # Y label
      cex.lab = 1.5, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Amplitude-amplitude correlation", # Title of plot
      cex.main = 2, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## Plot all subfigures of Figure 2 simultaneously
# Main title
mtext("Individual parietal delta-beta coupling",
      outer = TRUE, # Add to outer margins
      cex = 3.5, # Make bigger
      line = 1) # Move the text slightly upwards
# Overlay clear plot (in order to use legend location "top)
reset <- function() {
  par(mfrow = c(1, 1), oma = c(0, 0, 3, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(x = 0:1, y = 0:1, type = "n", xlab = "", ylab = "", axes = FALSE)
}
reset()
# Add legend
legend(x = "topright", # Location of legend
       legend = c("Men", "Women"), # Text of legend
       col = c("grey10", "grey40"), # Colours of lines
       lwd = c(5, 5), # Line width
       lty = c(1, 5), # Solid and dashed (long) lines
       bty = "n", # Don't add box / border
       ncol = 2, # Horizontal legend
       cex = 2) # Make text slightly bigger
# Print Figure
dev.off()
# Reset parameters
par(restore)


# Remove temporary variables
remove(SET_CFC_summary_group)
remove(SET_CFC_MinMax_summary_sex)
remove(reset)
remove(restore)
remove(data)
remove(a)
remove(vp)


# Line / bar plots high-low LSAS (exploratory) -------------------------------------------------------------------

## Packages
library(grid) # To organize multiple subplots in one large plot
library(gridBase)
library(gridExtra) # To organize multiple subplots in one large plot
library(ggplot2) # Plotting packages (used for LSAS density plots)
library(reshape2) # To reshape into long format to use with ggplot
library(magrittr) # For piping
library(dplyr) # For data manipulation

# Load data
load("SET_CFC_summary_LSAS_Split.RData")

### Make plots with base R (using "SET_CFC_summary_group")
# Save default parameters
restore <- par(no.readonly = TRUE)


## Figure 1: Cross-frequency coupling and psychological/endocrinological states and traits: divided by LSAS group
tiff("FIG1_LSAS.tiff", width = 40, height = 37.50, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(3, 2), oma = c(0, 0, 4, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left


## A) Trait social anxiety
data <- SET_CFC %>% select(LSAS_Split, LSAS) # Select relevant variables
data <- melt(data) # Make into long format
# Main plot
a <- ggplot(data, aes(x = value, fill = LSAS_Split)) + # Use sex as a grouping factor
  geom_histogram(alpha = .75, bins = 55, color="black") + # Make two transparent histograms
  xlim(0, 100) + # Show the full range of LSAS scores
  ylim(0, 5) + # Show the full range of count scores
  scale_fill_manual(values=c("black", "grey80")) + # Fill histogram with manual colours
  labs(x = "Liebowitz social anxiety scale (0-144)", # Set the x-axis label
       title = "Trait social anxiety", # Set the title
       subtitle = "a)") + # Set the subtitle
  #geom_vline(aes(xintercept = median(SET_CFC$LSAS)), col = 'black', size = 1) + # Add a line for the median
  theme(plot.title = element_text(size = 32, face = "bold", family = "serif", hjust = 0.5, # Make the title larger, bold, and centered
                                  margin = margin(b = -27, unit = "pt")), # Lower the title
        plot.subtitle = element_text(size = 32, face = "bold", family = "serif"), # Make the subtitle larger and bold
        legend.position = "none", # Remove legend
        legend.title = element_blank(), # Remove legend
        legend.text = element_blank(), # Remove legend
        axis.title = element_text(size = 22, family = "serif", colour = "black"), # Make the axis title larger and black
        axis.text = element_text(size = 22, family = "serif", colour = "black"), # Make the axis text larger and black
        panel.grid.major = element_line(size = 0.5, colour = "grey50"), # Make the major grid lines grey and thinner
        panel.grid.major.x = element_blank(), # Remove major grid lines from the x-axis
        panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.background = element_blank(), # Remove background
        axis.line = element_line(size = 0.5, colour = "black") # Add axis lines in thin black
  ) 

# Save space for the LSAS plot to be printed here later
plot.new()

# Plot another empty space at the top-right
plot.new()


## B) Frontal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.50, 1.10), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.50,
     xright = 13,
     ytop = 1.10,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.50,
     xright = 18,
     ytop = 1.10,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 1.10, x1 = 60, y1 = 1.10, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.0, x1 = 60, y1 = 1.0, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.90, x1 = 60, y1 = 0.90, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.80, x1 = 60, y1 = 0.80, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.70, x1 = 60, y1 = 0.70, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.60, x1 = 60, y1 = 0.60, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.50, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.50, 1.10, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Low LSAS data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Low LSAS", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add Low LSAS error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] + SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] - SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add High LSAS data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: High LSAS", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add High LSAS error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] + SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] - SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Frontal_Avg_dPAC_Z", "EarlyAnticip_Frontal_Avg_dPAC_Z", "LateAnticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2.5) # Move inwards
# Add y-axis label
title(ylab = "Z-score", # Y label
      cex.lab = 2.2, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Phase-amplitude coupling", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left



## C) Frontal AAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.0, 0.03), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.0,
     xright = 13,
     ytop = 0.03,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.0,
     xright = 18,
     ytop = 0.03,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 0.03, x1 = 60, y1 = 0.03, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.02, x1 = 60, y1 = 0.02, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.01, x1 = 60, y1 = 0.01, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.0, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.0, 0.03, 0.01)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Low LSAS data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Low LSAS", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add Low LSAS error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] + SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] - SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add High LSAS data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: High LSAS", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add High LSAS error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] + SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] - SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Frontal_Avg_AAC_R", "EarlyAnticip_Frontal_Avg_AAC_R", "LateAnticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Correlation coefficient", # Y label
      cex.lab = 2.2, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Amplitude-amplitude coupling", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("c)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## D) State anxiety
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(5, 65), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later (more inwards)
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 5,
     xright = 13,
     ytop = 65,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 5,
     xright = 18,
     ytop = 65,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 65, x1 = 60, y1 = 65, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 45, x1 = 60, y1 = 45, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 25, x1 = 60, y1 = 25, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 5, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(5, 65, 20)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Low LSAS data
lines(x = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Low LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add Low LSAS error bars
arrows(x0 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] + SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means plus SE
       x1 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] - SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add High LSAS data
lines(x = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: High LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add High LSAS error bars
arrows(x0 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] + SET_CFC_summary_group["Standard error of the mean: High LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means plus SE
       x1 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] - SET_CFC_summary_group["Standard error of the mean: High LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Visual analogue scale (0 - 100)", # Y label
      cex.lab = 2.2, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "State anxiety", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("d)")), side = 3, adj = 0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)


## E) Cortisol
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(3, 8), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 3,
     xright = 13,
     ytop = 8,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 3,
     xright = 18,
     ytop = 8,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 8, x1 = 60, y1 = 8, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 7, x1 = 60, y1 = 7, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 6, x1 = 60, y1 = 6, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 5, x1 = 60, y1 = 5, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 4, x1 = 60, y1 = 4, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 3, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(3, 8, 1)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Low LSAS data
lines(x = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Low LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add Low LSAS error bars
arrows(x0 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] + SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means plus SE
       x1 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] - SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add High LSAS data
lines(x = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: High LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add High LSAS error bars
arrows(x0 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] + SET_CFC_summary_group["Standard error of the mean: High LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means plus SE
       x1 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] - SET_CFC_summary_group["Standard error of the mean: High LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2.5, # Make line width slightly thicker
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Free salivary cortisol (ng/ml)", # Y label
      cex.lab = 2.2, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "Cortisol", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("e")), side = 3, adj = 0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)


## Trait social anxiety (again)
# Create a viewport to plot it in the topleft
vp <- viewport(height = unit(0.3,"npc"), width=unit(0.46, "npc"), 
               just = c("left","top"), y = 0.93, x = 0.007)
print(a, vp = vp)


## Plot all subfigures of Figure 1 simultaneously
# Main title
mtext("Frontal delta-beta coupling and stress responses",
      outer = TRUE, # Add to outer margins
      cex = 3.5) # Make bigger
# Overlay clear plot (in order to use legend location "top)
reset <- function() {
  par(mfrow = c(1, 1), oma = c(0, 0, 2, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(x = 0:1, y = 0:1, type = "n", xlab = "", ylab = "", axes = FALSE)
}
reset()
# Add legend
legend(x = "topright", # Location of legend
       legend = c("Low trait social anxiety", "High trait social anxiety"), # Text of legend
       col = c("grey10", "grey40"), # Colours of lines
       lwd = c(5, 5), # Line width
       lty = c(1, 5), # Solid and dashed (long) lines
       bty = "n", # Don't add box / border
       ncol = 2, # Horizontal legend
       cex = 2) # Make text slightly bigger
# Print Figure
dev.off()
# Reset parameters
par(restore)



## Figure 2: Parietal cross-frequency coupling: divided by LSAS group
tiff("FIG2_LSAS.tiff", width = 40, height = 12.5, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(1, 2), oma = c(0, 0, 4, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left


## B) Parietal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.70, 1.20), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.70,
     xright = 13,
     ytop = 1.20,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.70,
     xright = 18,
     ytop = 1.20,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 1.20, x1 = 60, y1 = 1.20, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.10, x1 = 60, y1 = 1.10, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.0, x1 = 60, y1 = 1.0, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.90, x1 = 60, y1 = 0.90, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.80, x1 = 60, y1 = 0.80, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.70, cex.axis = 1.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 1.5, las = 1, at = seq(0.70, 1.20, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Low LSAS data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Low LSAS", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add Low LSAS error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] + SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] - SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add High LSAS data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: High LSAS", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add High LSAS error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] + SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] - SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Parietal_Avg_dPAC_Z", "EarlyAnticip_Parietal_Avg_dPAC_Z", "LateAnticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 1.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Z-score", # Y label
      cex.lab = 1.5, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Phase-amplitude coupling", # Title of plot
      cex.main = 2, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## B) Parietal AAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.0, 0.05), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.0,
     xright = 13,
     ytop = 0.05,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.0,
     xright = 18,
     ytop = 0.05,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 0.05, x1 = 60, y1 = 0.05, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.04, x1 = 60, y1 = 0.04, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.03, x1 = 60, y1 = 0.03, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.02, x1 = 60, y1 = 0.02, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.01, x1 = 60, y1 = 0.01, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.0, cex.axis = 1.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 1.5, las = 1, at = seq(0.0, 0.05, 0.01)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Low LSAS data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Low LSAS", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add Low LSAS error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] + SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] - SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add High LSAS data
lines(x = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: High LSAS", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add High LSAS error bars
arrows(x0 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] + SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 7.5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] - SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Parietal_Avg_AAC_R", "EarlyAnticip_Parietal_Avg_AAC_R", "LateAnticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 1.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Correlation coefficient", # Y label
      cex.lab = 1.5, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Amplitude-amplitude correlation", # Title of plot
      cex.main = 2, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## Plot all subfigures of Figure 2 simultaneously
# Main title
mtext("Parietal delta-beta coupling",
      outer = TRUE, # Add to outer margins
      cex = 3.5, # Make bigger
      line = 1) # Move the text slightly upwards
# Overlay clear plot (in order to use legend location "top)
reset <- function() {
  par(mfrow = c(1, 1), oma = c(0, 0, 3, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(x = 0:1, y = 0:1, type = "n", xlab = "", ylab = "", axes = FALSE)
}
reset()
# Add legend
legend(x = "topright", # Location of legend
       legend = c("Low trait social anxiety", "High trait social anxiety"), # Text of legend
       col = c("grey10", "grey40"), # Colours of lines
       lwd = c(5, 5), # Line width
       lty = c(1, 5), # Solid and dashed (long) lines
       bty = "n", # Don't add box / border
       ncol = 2, # Horizontal legend
       cex = 2) # Make text slightly bigger
# Print Figure
dev.off()
# Reset parameters
par(restore)


# Remove temporary variables
remove(SET_CFC_summary_group)
remove(reset)
remove(restore)
remove(data)
remove(a)
remove(vp)


# Line / bar plots individual reactivity and recovery high-low LSAS (exploratory) -------------------------------------------------------------------

## Packages
library(grid) # To organize multiple subplots in one large plot
library(gridBase)
library(gridExtra) # To organize multiple subplots in one large plot
library(ggplot2) # Plotting packages (used for LSAS density plots)
library(reshape2) # To reshape into long format to use with ggplot
library(magrittr) # For piping
library(dplyr) # For data manipulation

# Load data: LSAS
load("SET_CFC_MinMax_summary_LSAS_Split.RData")
SET_CFC_summary_group <- SET_CFC_MinMax_summary_LSAS_Split

### Make plots with base R (using "SET_CFC_summary_group")
# Save default parameters
restore <- par(no.readonly = TRUE)


## Figure 1: Cross-frequency coupling min-max and psychological/endocrinological states and traits: divided by LSAS group
tiff("FIG1_MinMax_LSAS.tiff", width = 40, height = 37.50, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(3, 2), oma = c(0, 0, 4, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left


## A) Trait social anxiety
data <- SET_CFC %>% select(LSAS_Split, LSAS) # Select relevant variables
data <- melt(data) # Make into long format
# Main plot
a <- ggplot(data, aes(x = value, fill = LSAS_Split)) + # Use sex as a grouping factor
  geom_histogram(alpha = .75, bins = 55, color="black") + # Make two transparent histograms
  xlim(0, 100) + # Show the full range of LSAS scores
  ylim(0, 5) + # Show the full range of count scores
  scale_fill_manual(values=c("black", "grey80")) + # Fill histogram with manual colours
  labs(x = "Liebowitz social anxiety scale (0-144)", # Set the x-axis label
       title = "Trait social anxiety", # Set the title
       subtitle = "a)") + # Set the subtitle
  #geom_vline(aes(xintercept = median(SET_CFC$LSAS)), col = 'black', size = 1) + # Add a line for the median
  theme(plot.title = element_text(size = 32, face = "bold", family = "serif", hjust = 0.5, # Make the title larger, bold, and centered
                                  margin = margin(b = -27, unit = "pt")), # Lower the title
        plot.subtitle = element_text(size = 32, face = "bold", family = "serif"), # Make the subtitle larger and bold
        legend.position = "none", # Remove legend
        legend.title = element_blank(), # Remove legend
        legend.text = element_blank(), # Remove legend
        axis.title = element_text(size = 22, family = "serif", colour = "black"), # Make the axis title larger and black
        axis.text = element_text(size = 22, family = "serif", colour = "black"), # Make the axis text larger and black
        panel.grid.major = element_line(size = 0.5, colour = "grey50"), # Make the major grid lines grey and thinner
        panel.grid.major.x = element_blank(), # Remove major grid lines from the x-axis
        panel.grid.minor = element_blank(), # Remove minor grid lines
        panel.background = element_blank(), # Remove background
        axis.line = element_line(size = 0.5, colour = "black") # Add axis lines in thin black
  ) 

# Save space for the LSAS plot to be printed here later
plot.new()

# Plot another empty space at the top-right
plot.new()


## B) Frontal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.40, 1.20), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.40,
     xright = 13,
     ytop = 1.20,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.40,
     xright = 18,
     ytop = 1.20,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 1.20, x1 = 60, y1 = 1.20, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.10, x1 = 60, y1 = 1.10, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.0, x1 = 60, y1 = 1.0, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.90, x1 = 60, y1 = 0.90, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.80, x1 = 60, y1 = 0.80, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.70, x1 = 60, y1 = 0.70, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.60, x1 = 60, y1 = 0.60, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.50, x1 = 60, y1 = 0.50, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.40, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.40, 1.20, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Low LSAS data
lines(x = c(-25, 6.41, 36.19), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Low LSAS", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add Low LSAS error bars
arrows(x0 = c(-25, 6.41, 36.19), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")] + SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")]), # Means plus SE
       x1 = c(-25, 6.41, 36.19), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")] - SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add High LSAS data
lines(x = c(-25, 6.45, 35.61), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: High LSAS", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add High LSAS error bars
arrows(x0 = c(-25, 6.45, 35.61), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")] + SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")]), # Means plus SE
       x1 = c(-25, 6.45, 35.61), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")] - SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Frontal_Avg_dPAC_Z", "Frontal_dPAC_Z_max", "Frontal_dPAC_Z_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2.5) # Move inwards
# Add y-axis label
title(ylab = "Z-score", # Y label
      cex.lab = 2.2, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Phase-amplitude coupling", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## C) Frontal AAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(-0.01, 0.05), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = -0.01,
     xright = 13,
     ytop = 0.05,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = -0.01,
     xright = 18,
     ytop = 0.05,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 0.05, x1 = 60, y1 = 0.05, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.04, x1 = 60, y1 = 0.04, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.03, x1 = 60, y1 = 0.03, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.02, x1 = 60, y1 = 0.02, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.01, x1 = 60, y1 = 0.01, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.00, x1 = 60, y1 = 0.00, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = -0.01, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(-0.01, 0.05, 0.01)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Low LSAS data
lines(x = c(-25, 6.02, 40.88), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Low LSAS", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add Low LSAS error bars
arrows(x0 = c(-25, 6.02, 40.88), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")] + SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")]), # Means plus SE
       x1 = c(-25, 6.02, 40.88), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")] - SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add High LSAS data
lines(x = c(-25, 6.53, 36.58), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: High LSAS", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add High LSAS error bars
arrows(x0 = c(-25, 6.53, 36.58), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")] + SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")]), # Means plus SE
       x1 = c(-25, 6.53, 36.58), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")] - SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Frontal_Avg_AAC_R", "Frontal_AAC_R_max", "Frontal_AAC_R_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Correlation coefficient", # Y label
      cex.lab = 2.2, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Amplitude-amplitude correlation", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("c)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## D) State anxiety
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(10, 70), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later (more inwards)
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 10,
     xright = 13,
     ytop = 70,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 10,
     xright = 18,
     ytop = 70,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 70, x1 = 60, y1 = 70, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 60, x1 = 60, y1 = 60, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 50, x1 = 60, y1 = 50, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 40, x1 = 60, y1 = 40, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 30, x1 = 60, y1 = 30, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 20, x1 = 60, y1 = 20, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 10, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(10, 70, 10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Low LSAS data
lines(x = c(-19, 13.50, 25.88), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Low LSAS", c("Anx.1", "Anx.23_max", "Anx.34_min")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add Low LSAS error bars
arrows(x0 = c(-19, 13.50, 25.88), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("Anx.1", "Anx.23_max", "Anx.34_min")] + SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("Anx.1", "Anx.23_max", "Anx.34_min")]), # Means plus SE
       x1 = c(-19, 13.50, 25.88), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("Anx.1", "Anx.23_max", "Anx.34_min")] - SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("Anx.1", "Anx.23_max", "Anx.34_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add High LSAS data
lines(x = c(-19, 13.07, 26.81), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: High LSAS", c("Anx.1", "Anx.23_max", "Anx.34_min")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add High LSAS error bars
arrows(x0 = c(-19, 13.07, 26.81), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("Anx.1", "Anx.23_max", "Anx.34_min")] + SET_CFC_summary_group["Standard error of the mean: High LSAS", c("Anx.1", "Anx.23_max", "Anx.34_min")]), # Means plus SE
       x1 = c(-19, 13.07, 26.81), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("Anx.1", "Anx.23_max", "Anx.34_min")] - SET_CFC_summary_group["Standard error of the mean: High LSAS", c("Anx.1", "Anx.23_max", "Anx.34_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Visual analogue scale (0 - 100)", # Y label
      cex.lab = 2.2, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "State anxiety", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("d)")), side = 3, adj = 0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)


## E) Cortisol
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(3, 9), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 3,
     xright = 13,
     ytop = 9,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 3,
     xright = 18,
     ytop = 9,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 9, x1 = 60, y1 = 9, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 8, x1 = 60, y1 = 8, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 7, x1 = 60, y1 = 7, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 6, x1 = 60, y1 = 6, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 5, x1 = 60, y1 = 5, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 4, x1 = 60, y1 = 4, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 3, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(3, 9, 1)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Low LSAS data
lines(x = c(-18, 33.19, 46.94), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Low LSAS", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add Low LSAS error bars
arrows(x0 = c(-18, 33.19, 46.94), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")] + SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")]), # Means plus SE
       x1 = c(-18, 33.19, 46.94), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")] - SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add High LSAS data
lines(x = c(-18, 31.39, 48.42), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: High LSAS", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add High LSAS error bars
arrows(x0 = c(-18, 31.39, 48.42), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")] + SET_CFC_summary_group["Standard error of the mean: High LSAS", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")]), # Means plus SE
       x1 = c(-18, 31.39, 48.42), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")] - SET_CFC_summary_group["Standard error of the mean: High LSAS", c("Cortisol.1", "Cortisol.234567_max", "Cortisol.4567_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2.5, # Make line width slightly thicker
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Free salivary cortisol (ng/ml)", # Y label
      cex.lab = 2.2, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "Cortisol", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("e)")), side = 3, adj = 0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)



## Trait social anxiety (again)
# Create a viewport to plot it in the topleft
vp <- viewport(height = unit(0.3,"npc"), width=unit(0.46, "npc"), 
               just = c("left","top"), y = 0.93, x = 0.007)
print(a, vp = vp)


## Plot all subfigures of Figure 1 simultaneously
# Main title
mtext("Individual frontal delta-beta coupling and stress responses",
      outer = TRUE, # Add to outer margins
      cex = 3.5) # Make bigger
# Overlay clear plot (in order to use legend location "top)
reset <- function() {
  par(mfrow = c(1, 1), oma = c(0, 0, 2, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(x = 0:1, y = 0:1, type = "n", xlab = "", ylab = "", axes = FALSE)
}
reset()
# Add legend
legend(x = "topright", # Location of legend
       legend = c("Low trait social anxiety", "High trait social anxiety"), # Text of legend
       col = c("grey10", "grey40"), # Colours of lines
       lwd = c(5, 5), # Line width
       lty = c(1, 5), # Solid and dashed (long) lines
       bty = "n", # Don't add box / border
       ncol = 2, # Horizontal legend
       cex = 2) # Make text slightly bigger
# Print Figure
dev.off()
# Reset parameters
par(restore)


## Figure 2: Parietal cross-frequency coupling min-max: divided by LSAS group
tiff("FIG2_MinMax_LSAS.tiff", width = 40, height = 12.5, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(1, 2), oma = c(0, 0, 4, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left

## A) Parietal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.60, 1.30), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.60,
     xright = 13,
     ytop = 1.30,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.60,
     xright = 18,
     ytop = 1.30,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 1.30, x1 = 60, y1 = 1.30, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.20, x1 = 60, y1 = 1.20, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.10, x1 = 60, y1 = 1.10, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.0, x1 = 60, y1 = 1.0, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.90, x1 = 60, y1 = 0.90, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.80, x1 = 60, y1 = 0.80, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.70, x1 = 60, y1 = 0.70, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.60, cex.axis = 1.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 1.5, las = 1, at = seq(0.60, 1.30, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Low LSAS data
lines(x = c(-25, 6.41, 38.06), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Low LSAS", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add Low LSAS error bars
arrows(x0 = c(-25, 6.41, 38.06), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")] + SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")]), # Means plus SE
       x1 = c(-25, 6.41, 38.06), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")] - SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add High LSAS data
lines(x = c(-25, 6.29, 31.74), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: High LSAS", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add High LSAS error bars
arrows(x0 = c(-25, 6.29, 31.74), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")] + SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")]), # Means plus SE
       x1 = c(-25, 6.29, 31.74), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")] - SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Parietal_Avg_dPAC_Z", "Parietal_dPAC_Z_max", "Parietal_dPAC_Z_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 1.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Z-score", # Y label
      cex.lab = 1.5, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Phase-amplitude coupling", # Title of plot
      cex.main = 2, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("a)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left




## B) Parietal AAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(-0.01, 0.06), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = -0.01,
     xright = 13,
     ytop = 0.06,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = -0.01,
     xright = 18,
     ytop = 0.06,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 0.06, x1 = 60, y1 = 0.06, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.05, x1 = 60, y1 = 0.05, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.04, x1 = 60, y1 = 0.04, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.03, x1 = 60, y1 = 0.03, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.02, x1 = 60, y1 = 0.02, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.01, x1 = 60, y1 = 0.01, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.00, x1 = 60, y1 = 0.00, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = -0.01, cex.axis = 1.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 1.5, las = 1, at = seq(-0.01, 0.06, 0.01)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add Low LSAS data
lines(x = c(-25, 6.33, 42.75), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: Low LSAS", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add Low LSAS error bars
arrows(x0 = c(-25, 6.33, 42.75), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")] + SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")]), # Means plus SE
       x1 = c(-25, 6.33, 42.75), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: Low LSAS", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")] - SET_CFC_summary_group["Standard error of the mean: Low LSAS", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add High LSAS data
lines(x = c(-25, 6.45, 38.52), # Time points of measurements, relative to start SET manipulation
      y = SET_CFC_summary_group["Mean: High LSAS", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add High LSAS error bars
arrows(x0 = c(-25, 6.45, 38.52), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")] + SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")]), # Means plus SE
       x1 = c(-25, 6.45, 38.52), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(SET_CFC_summary_group["Mean: High LSAS", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")] - SET_CFC_summary_group["Standard error of the mean: High LSAS", c("RS_Parietal_Avg_AAC_R", "Parietal_AAC_R_max", "Parietal_AAC_R_min")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 1.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Correlation coefficient", # Y label
      cex.lab = 1.5, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Amplitude-amplitude correlation", # Title of plot
      cex.main = 2, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("B)")), side = 3, adj = 0, cex = 2.5) # Add the number of the subfigure top left


## Plot all subfigures of Figure 2 simultaneously
# Main title
mtext("Individual parietal delta-beta coupling",
      outer = TRUE, # Add to outer margins
      cex = 3.5, # Make bigger
      line = 1) # Move the text slightly upwards
# Overlay clear plot (in order to use legend location "top)
reset <- function() {
  par(mfrow = c(1, 1), oma = c(0, 0, 3, 0), mar = c(0, 0, 0, 0), new = TRUE)
  plot(x = 0:1, y = 0:1, type = "n", xlab = "", ylab = "", axes = FALSE)
}
reset()
# Add legend
legend(x = "topright", # Location of legend
       legend = c("Low trait social anxiety", "High trait social anxiety"), # Text of legend
       col = c("grey10", "grey40"), # Colours of lines
       lwd = c(5, 5), # Line width
       lty = c(1, 5), # Solid and dashed (long) lines
       bty = "n", # Don't add box / border
       ncol = 2, # Horizontal legend
       cex = 2) # Make text slightly bigger
# Print Figure
dev.off()
# Reset parameters
par(restore)


## Remove unnecessary variables
remove(a)
remove(restore)
remove(data)
remove(vp)
remove(SET_CFC_summary_group)
remove(SET_CFC_MinMax_summary_LSAS_Split)
remove(reset)


# Correlations with EnglishCompetence ------------------------------------------------------------
# Check for influence from confounding variable

# Load packages
library(dplyr) # For data manipulation
library(reshape2) # For melt function
library(Hmisc) # For rcorr function
library(psych) # For converting rho to Cohen's d
library(xlsx) # For exporting Excel results
library(BayesFactor) # For Bayesian statistics
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors

## Create correlation matrix
data <- SET_CFC %>% select("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                           "recov_Frontal_Avg_dPAC_Z",
                           "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                           "recov_Frontal_Avg_AAC_R",
                           "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                           "recov_Parietal_Avg_dPAC_Z",
                           "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                           "recov_Parietal_Avg_AAC_R",
                           "LSAS", "anx.react", "anx.recov",
                           "Cortisol.1", "cort.react", "cort.recov", "EnglishCompetence")
## Correlation calculation
cormat <- rcorr(as.matrix(data), type = "spearman")

## Make correlation coefficients into dataframe
cormat_r <- cormat$r %>% round(6)
# Remove everything under the diagonal
cormat_r[lower.tri(cormat_r, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat_r <- melt(cormat_r, na.rm = TRUE)
# Rename 'value' variable appropriately
colnames(melted_cormat_r)[3] <- "corr"

## Make p-values into dataframe
cormat_p <- cormat$P %>% round(6)
# Remove everything under the diagonal
cormat_p[lower.tri(cormat_p, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat_p <- melt(cormat_p, na.rm = TRUE)
# Rename 'value' variable appropriately
colnames(melted_cormat_p)[3] <- "p.value"

## Put r and p together
corr_test_df <- cbind(melted_cormat_r, melted_cormat_p[, "p.value"])
# rename stubborn 'p-value' variable again
colnames(corr_test_df)[4] <- "p.value"

# Remove factor
corr_test_df[, "Var1"] <- corr_test_df[, "Var1"] %>% as.character()
corr_test_df[, "Var2"] <- corr_test_df[, "Var2"] %>% as.character()

# Reset rownames
rownames(corr_test_df) <- NULL

# Remove everything but correlations involving EnglishCompetence
corr_test_df <- corr_test_df[grep("EnglishCompetence", c(corr_test_df$Var2, corr_test_df$Var1)), ]

# Add Cohen's d
rho <- corr_test_df[, "corr"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
corr_test_df$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
corr_test_df$cohen.d.mag <- sapply(corr_test_df$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

# Do FDR-correction on p-values
p.val <- corr_test_df$p.value
p.adj <-  p.adjust(p.val, method = "fdr", n = length(p.val)) # Do fdr-correction
corr_test_df$p.adj <- p.adj
corr_test_df$p.adj.sig <- sapply(corr_test_df$p.adj, function(x) p.value.sig(x)) # Add column with significance
rownames(corr_test_df) <- NULL # Reset rownames

## Bayesian statistics
corrBF <- list(NA)
for (i in 1:nrow(corr_test_df)) {
  BF <- correlationBF(x = data[, corr_test_df[i, "Var1"] ], y = data[, corr_test_df[i, "Var2"] ] )
  corrBF[[i]] <- extractBF(BF, onlybf = TRUE)
}

# Add bayes factors to dataframe
corr_test_df[, "BF"] <- corrBF %>% unlist()
# Add column with Bayes factor interpretation
corr_test_df$BF.evidence <- sapply(corr_test_df$BF, function(x) BF.evidence(x)) # Add column with interpretation

# Save lm results
write.xlsx(corr_test_df, "Correlations_EnglishCompetence.xlsx")

# Remove variables
remove(data)
remove(corr_test_df)
remove(p.val)
remove(p.adj)
remove(p.value.sig)
remove(BF)
remove(corrBF)
remove(i)
remove(cormat)
remove(cormat_p)
remove(cormat_r)
remove(melted_cormat_p)
remove(melted_cormat_r)
remove(BF.evidence)
remove(d, rho, cohen.d.magnitude)

# Sex differences in frontal CFC (preregistered) ---------------------------------------------------------
# Load packages
library(data.table) # For function 'rbindlist'
library(magrittr) # For piping
library(xlsx) # For exporting to Excel
library(BayesFactor) # For Bayesian statistics with uninformed priors
library(dplyr) # For data manipulation
library(psych) # For converting rho to Cohen's d
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors

# Select relevant variables
data <- SET_CFC[, c("RS_Frontal_Avg_dPAC_Z",
                    "RS_Frontal_Avg_AAC_R",
                    "react_Frontal_Avg_dPAC_Z",
                    "react_Frontal_Avg_AAC_R",
                    "LSAS",
                    "anx.react",
                    "Cortisol.1",
                    "cort.react",
                    "recov_Frontal_Avg_dPAC_Z",
                    "recov_Frontal_Avg_AAC_R",
                    "anx.recov",
                    "cort.recov",
                    "EnglishCompetence"
                    )]
# Separate CFC data according to sex
data_male <- data[SET_CFC$Sex == "Male", ]
data_female <- data[SET_CFC$Sex == "Female", ]

# independent 2-group Wilcoxon signed rank test
MWU_test <- list() # Initialize results list
for (i in 1:ncol(data_male)) {
  MWU_test[[i]] <- wilcox.test(data_male[, i],data_female[, i], exact = FALSE)
  MWU_test[[i]][["data.name"]] <- colnames(data_male)[i] # Add the apropriate variable name
}
# Convert results to a dataframe
MWU_test_df <- rbindlist(MWU_test)
MWU_test_df <- MWU_test_df[ , -c("parameter", "null.value")] # Remove unnecessary columns
# Calculate rank correlation
MWU_test_df[, "rank.corr"] <- I(nrow(data_male)+nrow(data_female)) / MWU_test_df[, "statistic"]
# Replace infinite correlations with 1
MWU_test_df[which(MWU_test_df[, "rank.corr"] == Inf), "rank.corr"] <- 1
# Rename the 'statistic' result appropriately
colnames(MWU_test_df)[which(colnames(MWU_test_df) == "statistic")] <- "W"

# Add Cohen's d
rho <- MWU_test_df[, "rank.corr"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
MWU_test_df$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
MWU_test_df$cohen.d.mag <- sapply(MWU_test_df$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

# Do FDR-correction on p-values
p.val <- MWU_test_df$p.value
p.adj <-  p.adjust(p.val, method = "fdr", n = length(p.val)) # Do fdr-correction
MWU_test_df$p.adj <- p.adj
MWU_test_df$p.adj.sig <- sapply(MWU_test_df$p.adj, function(x) p.value.sig(x)) # Add column with significance

## Bayesian statistics
BF_test <- list() # Initialize results list
# Calculate Bayes factor
for (i in 1:ncol(data_male)) {
  BF <- ttestBF(data_male[, i], data_female[, i])
  BF_test[[i]] <- extractBF(BF, onlybf = TRUE)
}

# Add bayes factors to dataframe
MWU_test_df[, "BF"] <- BF_test %>% unlist()
# Add column with Bayes factor interpretation
MWU_test_df$BF.evidence <- sapply(MWU_test_df$BF, function(x) BF.evidence(x)) # Add column with interpretation

# Save lm results
write.xlsx(MWU_test_df, "SexDifferences_Frontal.SET_CFC.xlsx")

# Remove variables
remove(MWU_test)
remove(MWU_test_df)
remove(data)
remove(data_female)
remove(data_male)
remove(i)
remove(p.val)
remove(p.adj)
remove(p.value.sig)
remove(BF)
remove(BF_test)
remove(BF.evidence)
remove(d, rho, cohen.d.magnitude)

# Sex differences in parietal CFC (preregistered) ---------------------------------------------------------
# Load packages
library(data.table) # For function 'rbindlist'
library(magrittr) # For piping
library(xlsx) # For exporting to Excel
library(brms) # For Bayesian statistics
library(dplyr) # For data manipulation
library(psych) # For converting rho to Cohen's d
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors

# Separate CFC data according to sex
data <- SET_CFC[, c("RS_Parietal_Avg_dPAC_Z",
                    "RS_Parietal_Avg_AAC_R",
                    "react_Parietal_Avg_dPAC_Z",
                    "react_Parietal_Avg_AAC_R",
                    "recov_Parietal_Avg_dPAC_Z",
                    "recov_Parietal_Avg_AAC_R")]
data_male <- data[SET_CFC$Sex == "Male", ]
data_female <- data[SET_CFC$Sex == "Female", ]

# independent 2-group Wilcoxon signed rank test
MWU_test <- list() # Initialize results list
for (i in 1:ncol(data_male)) {
  MWU_test[[i]] <- wilcox.test(data_male[, i],data_female[, i], exact = FALSE)
  MWU_test[[i]][["data.name"]] <- colnames(data_male)[i] # Add the apropriate variable name
}
# Convert results to a dataframe
MWU_test_df <- rbindlist(MWU_test)
MWU_test_df <- MWU_test_df[ , -c("parameter", "null.value")] # Remove unnecessary columns
# Calculate rank correlation
MWU_test_df[, "rank.corr"] <- I(nrow(data_male)+nrow(data_female)) / MWU_test_df[, "statistic"]
# Replace infinite correlations with 1
MWU_test_df[which(MWU_test_df[, "rank.corr"] == Inf), "rank.corr"] <- 1
# Rename the 'statistic' result appropriately
colnames(MWU_test_df)[which(colnames(MWU_test_df) == "statistic")] <- "W"

# Add Cohen's d
rho <- MWU_test_df[, "rank.corr"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
MWU_test_df$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
MWU_test_df$cohen.d.mag <- sapply(MWU_test_df$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

# Do FDR-correction on p-values
p.val <- MWU_test_df$p.value
p.adj <-  p.adjust(p.val, method = "fdr", n = length(p.val)) # Do fdr-correction
MWU_test_df$p.adj <- p.adj
MWU_test_df$p.adj.sig <- sapply(MWU_test_df$p.adj, function(x) p.value.sig(x)) # Add column with significance

## Bayesian statistics
BF_test <- list() # Initialize results list
# Calculate Bayes factor
for (i in 1:ncol(data_male)) {
  BF <- ttestBF(data_male[, i], data_female[, i])
  BF_test[[i]] <- extractBF(BF, onlybf = TRUE)
}

# Add bayes factors to dataframe
MWU_test_df[, "BF"] <- BF_test %>% unlist()
# Add column with Bayes factor interpretation
MWU_test_df$BF.evidence <- sapply(MWU_test_df$BF, function(x) BF.evidence(x)) # Add column with interpretation

# Save lm results
write.xlsx(MWU_test_df, "SexDifferences_Parietal.SET_CFC.xlsx")

# Remove variables
remove(MWU_test)
remove(MWU_test_df)
remove(data_female)
remove(data_male)
remove(i)
remove(p.val)
remove(p.adj)
remove(p.value.sig)
remove(BF)
remove(BF_test)
remove(data)
remove(BF.evidence)
remove(d, rho, cohen.d.magnitude)

# Sig frontal CFC reactivity/recovery (preregistered/exploratory) ----------------------------------------------------------

#Load packages
library(dplyr) # For data manipulation
library(magrittr) # For piping
library(xlsx) # For exporting to Excel
library(data.table) # For function rbindlist
library(psych) # For converting rho to Cohen's d
library(BayesFactor) # For bayesian statistics with uninformed priors
source("Aladins package\\BF_t.R") # For bayesian statistics with informed priors: "Aladins Bayes Factor in R" (downloaded from https://doi.org/10.17045/sthlmuni.4981154.v3)
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors

options(scipen = 999) # Convert scientific notation into decimals

# Load prior data
load("LSA_HSA.RData")

# Select CFC reactivity data
data_obtained <- select(SET_CFC, c("react_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_AAC_R", 
                                   "recov_Frontal_Avg_dPAC_Z", "recov_Frontal_Avg_AAC_R", 
                                   "anx.react", "cort.react",
                                   "anx.recov", "cort.recov"))
# Rename variable names to something plottable
colnames(data_obtained) <- c("Frontal PAC reactivity", "Frontal AAC reactivity",
                             "Frontal PAC recovery", "Frontal AAC recovery", 
                             "State anxiety reactivity", "Cortisol reactivity",
                             "State anxiety recovery", "Cortisol recovery")
# Select CFC reactivity data from previous study
data_theory <- select(LSA_HSA, c("react_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_AAC_R",
                                 "recov_Frontal_Avg_dPAC_Z", "recov_Frontal_Avg_AAC_R", 
                                 "anx.react", "anx.recov"))
# Rename variable names to something plottable
colnames(data_theory) <- c("Frontal PAC reactivity", "Frontal AAC reactivity",
                           "Frontal PAC recovery", "Frontal AAC recovery",
                           "State anxiety reactivity", "State anxiety recovery")


# One-sample Wilcoxon signed rank Test
MWU_test <- list() # Initialize results list
for (i in 1:ncol(data_obtained)) {
  MWU_test[[i]] <- wilcox.test(data_obtained[, i], mu = 0)
  MWU_test[[i]][["data.name"]] <- colnames(data_obtained)[i] # Add the apropriate variable name
}
# Convert results to a dataframe
MWU_test_df <- rbindlist(MWU_test)
MWU_test_df <- MWU_test_df[ , -c("parameter", "null.value")] # Remove unnecessary columns
# Calculate rank correlation
MWU_test_df[, "rank.corr"] <- nrow(data_obtained) / MWU_test_df[, "statistic"]
# Replace infinite correlations with 1
MWU_test_df[which(MWU_test_df[, "rank.corr"] == Inf), "rank.corr"] <- 1
# Rename the 'statistic' result appropriately
colnames(MWU_test_df)[which(colnames(MWU_test_df) == "statistic")] <- "W"

# Add Cohen's d
rho <- MWU_test_df[, "rank.corr"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
MWU_test_df$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
MWU_test_df$cohen.d.mag <- sapply(MWU_test_df$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

## Do FDR-correction on p-values
p.val <- MWU_test_df$p.value
p.adj <-  p.adjust(p.val, method = "fdr", n = length(p.val)) # Do fdr-correction
MWU_test_df$p.adj <- p.adj
MWU_test_df$p.adj.sig <- sapply(MWU_test_df$p.adj, function(x) p.value.sig(x)) # Add column with significance

### Bayes factors with informed or uninformed priors, depending on availability
# BF10 = likelihood of the data given H1, relative to the likehood of the data given H0
# E.g., BF10 = 4 means that there is four times more evidence for H1 than for H0.

# Standard error of the mean (SEM) function
se <- function(x) {sqrt(var(x)/length(x))}

# Bayesian statistics
BF_test <- list() # Initialize results list
# Calculate Bayes factor
for (i in 1:ncol(data_obtained)) {
  # Check whether there are priors available for this variable
  if (colnames(data_obtained)[[i]] %in% colnames(data_theory)) {
    ## If yes, use informed prior
    MWU_test_df[i, "Prior"] <- "Informed prior" # Save as uninformed prior
    # Calculate obtained data
    meanobtained <- data_obtained[, i] %>% mean() %>% round(3) # Calculate mean reactivity
    semobtained <- data_obtained[, i] %>% se() %>% round(3) # Calculate SEM
    dfobtained <- data_obtained[, i] %>% complete.cases() %>% sum() %>% round(3) %>% -1 # Calculate df (sample size - 1)
    # Calculate prior data
    meantheory <- data_theory[, colnames(data_obtained)[[i]] ] %>% mean() %>% round(3) # Calculate mean reactivity
    sdtheory <- data_theory[, colnames(data_obtained)[[i]] ] %>% se() %>% round(3) # Calculate SEM
    dftheory <- data_theory[, colnames(data_obtained)[[i]] ] %>% complete.cases() %>% sum() %>% round(3) %>% -1 # Calculate df (sample size - 1)
    # Calculate Bayes factor
    tiff(paste0("Prior_", i, "_BF.tiff", sep = ""), res = 300, width = 16 * 0.7, height = 9 * 0.7, units = "in") # Save high-resolution TIFF file
    BF_test[[i]] <- BF_t(meantheory, sdtheory, dftheory, meanobtained, semobtained, dfobtained, colnames(data_obtained)[[i]])
    dev.off() # Save plot
  } else {
    ## If no, use uninformed prior
    MWU_test_df[i, "Prior"] <- "Uninformed prior" # Save as uninformed prior
    BF <- ttestBF(data_obtained[, i])
    BF_test[[i]] <- extractBF(BF, onlybf = TRUE)
  }
}

# Add bayes factors to dataframe
MWU_test_df[, "BF"] <- BF_test %>% unlist()
# Add column with Bayes factor interpretation
MWU_test_df$BF.evidence <- sapply(MWU_test_df$BF, function(x) BF.evidence(x)) # Add column with interpretation

# Save results
write.xlsx(MWU_test_df, "Frontal-reactivity-recovery.SET_CFC.xlsx")

# Remove variables
remove(MWU_test)
remove(MWU_test_df)
remove(data_obtained)
remove(data_theory)
remove(i)
remove(p.val)
remove(p.adj)
remove(p.value.sig)
remove(BF_t)
remove(BF_test)
remove(meantheory)
remove(sdtheory)
remove(dftheory)
remove(meanobtained)
remove(semobtained)
remove(dfobtained)
remove(se)
remove(BF)
remove(LSA_HSA)
remove(BF.evidence)
remove(d, rho, cohen.d.magnitude)


# Sig parietal CFC reactivity/recovery (preregistered/exploratory) ----------------------------------------------------------

#Load packages
library(dplyr) # For data manipulation
library(magrittr) # For piping
library(xlsx) # For exporting to Excel
library(data.table) # For function rbindlist
library(BayesFactor) # For bayesian statistics with uninformed priors
library(psych) # For converting rho to Cohen's d
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors

# Select CFC reactivity data
data_obtained <- select(SET_CFC, c("react_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_AAC_R",
                                   "recov_Parietal_Avg_dPAC_Z", "recov_Parietal_Avg_AAC_R"))
# Rename variable names to something plottable
colnames(data_obtained) <- c("Parietal PAC reactivity", "Parietal AAC reactivity",
                             "Parietal PAC recovery", "Parietal AAC recovery")

# One-sample Wilcoxon signed rank Test
MWU_test <- list() # Initialize results list
for (i in 1:ncol(data_obtained)) {
  MWU_test[[i]] <- wilcox.test(data_obtained[, i], mu = 0)
  MWU_test[[i]][["data.name"]] <- colnames(data_obtained)[i] # Add the apropriate variable name
}
# Convert results to a dataframe
MWU_test_df <- rbindlist(MWU_test)
MWU_test_df <- MWU_test_df[ , -c("parameter", "null.value")] # Remove unnecessary columns
# Calculate rank correlation
MWU_test_df[, "rank.corr"] <- nrow(data_obtained) / MWU_test_df[, "statistic"]
# Replace infinite correlations with 1
MWU_test_df[which(MWU_test_df[, "rank.corr"] == Inf), "rank.corr"] <- 1
# Rename the 'statistic' result appropriately
colnames(MWU_test_df)[which(colnames(MWU_test_df) == "statistic")] <- "W"

# Add Cohen's d
rho <- MWU_test_df[, "rank.corr"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
MWU_test_df$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
MWU_test_df$cohen.d.mag <- sapply(MWU_test_df$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

## Do FDR-correction on p-values
p.val <- MWU_test_df$p.value
p.adj <-  p.adjust(p.val, method = "fdr", n = length(p.val)) # Do fdr-correction
MWU_test_df$p.adj <- p.adj
MWU_test_df$p.adj.sig <- sapply(MWU_test_df$p.adj, function(x) p.value.sig(x)) # Add column with significance

### Bayes factors with uninformed priors

# Bayesian statistics
BF_test <- list() # Initialize results list
# Calculate Bayes factor
for (i in 1:ncol(data_obtained)) {
    ## Use uninformed prior
    MWU_test_df[i, "Prior"] <- "Uninformed prior" # Save as uninformed prior
    BF <- ttestBF(data_obtained[, i])
    BF_test[[i]] <- extractBF(BF, onlybf = TRUE)
}

# Add bayes factors to dataframe
MWU_test_df[, "BF"] <- BF_test %>% unlist()
# Add column with Bayes factor interpretation
MWU_test_df$BF.evidence <- sapply(MWU_test_df$BF, function(x) BF.evidence(x)) # Add column with interpretation

# Save results
write.xlsx(MWU_test_df, "Parietal-reactivity-recovery.SET_CFC.xlsx")

# Remove variables
remove(MWU_test)
remove(MWU_test_df)
remove(data_obtained)
remove(i)
remove(p.val)
remove(p.adj)
remove(p.value.sig)
remove(BF_test)
remove(BF)
remove(BF.evidence)
remove(d, rho, cohen.d.magnitude)

# Sig area under the curve (exploratory) ----------------------------------------------------------

#Load packages
library(dplyr) # For data manipulation
library(magrittr) # For piping
library(xlsx) # For exporting to Excel
library(data.table) # For function rbindlist
library(psych) # For converting rho to Cohen's d
library(BayesFactor) # For bayesian statistics with uninformed priors
source("Aladins package\\BF_t.R") # For bayesian statistics with informed priors: "Aladins Bayes Factor in R" (downloaded from https://doi.org/10.17045/sthlmuni.4981154.v3)
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors

options(scipen = 999) # Convert scientific notation into decimals

# Load prior data
load("LSA_HSA.RData")

# Select CFC data
data_obtained <- select(SET_CFC, c(frontal_PAC.auc, parietal_PAC.auc, 
                                   frontal_AAC.auc, parietal_AAC.auc))
# Rename variable names to something plottable
colnames(data_obtained) <- c("Frontal PAC AUC", "Parietal PAC AUC",
                             "Frontal AAC AUC", "Parietal AAC AUC")
# Select CFC data from previous study
data_theory <- select(LSA_HSA, c(frontal_PAC.auc, frontal_AAC.auc))
# Rename variable names to something plottable
colnames(data_theory) <- c("Frontal PAC AUC", "Frontal AAC AUC")

# One-sample Wilcoxon signed rank Test
MWU_test <- list() # Initialize results list
for (i in 1:ncol(data_obtained)) {
  MWU_test[[i]] <- wilcox.test(data_obtained[, i], mu = 0)
  MWU_test[[i]][["data.name"]] <- colnames(data_obtained)[i] # Add the apropriate variable name
}
# Convert results to a dataframe
MWU_test_df <- rbindlist(MWU_test)
MWU_test_df <- MWU_test_df[ , -c("parameter", "null.value")] # Remove unnecessary columns
# Calculate rank correlation
MWU_test_df[, "rank.corr"] <- nrow(data_obtained) / MWU_test_df[, "statistic"]
# Replace infinite correlations with 1
MWU_test_df[which(MWU_test_df[, "rank.corr"] == Inf), "rank.corr"] <- 1
# Rename the 'statistic' result appropriately
colnames(MWU_test_df)[which(colnames(MWU_test_df) == "statistic")] <- "W"

# Add Cohen's d
rho <- MWU_test_df[, "rank.corr"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
MWU_test_df$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
MWU_test_df$cohen.d.mag <- sapply(MWU_test_df$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

## Do FDR-correction on p-values
p.val <- MWU_test_df$p.value
p.adj <-  p.adjust(p.val, method = "fdr", n = length(p.val)) # Do fdr-correction
MWU_test_df$p.adj <- p.adj
MWU_test_df$p.adj.sig <- sapply(MWU_test_df$p.adj, function(x) p.value.sig(x)) # Add column with significance

### Bayes factors with informed or uninformed priors, depending on availability
# BF10 = likelihood of the data given H1, relative to the likehood of the data given H0
# E.g., BF10 = 4 means that there is four times more evidence for H1 than for H0.

# Standard error of the mean (SEM) function
se <- function(x) {sqrt(var(x)/length(x))}

# Bayesian statistics
BF_test <- list() # Initialize results list
# Calculate Bayes factor
for (i in 1:ncol(data_obtained)) {
  # Check whether there are priors available for this variable
  if (colnames(data_obtained)[[i]] %in% colnames(data_theory)) {
    ## If yes, use informed prior
    MWU_test_df[i, "Prior"] <- "Informed prior" # Save as uninformed prior
    # Calculate obtained data
    meanobtained <- data_obtained[, i] %>% mean() %>% round(3) # Calculate mean reactivity
    semobtained <- data_obtained[, i] %>% se() %>% round(3) # Calculate SEM
    dfobtained <- data_obtained[, i] %>% complete.cases() %>% sum() %>% round(3) %>% -1 # Calculate df (sample size - 1)
    # Calculate prior data
    meantheory <- data_theory[, colnames(data_obtained)[[i]] ] %>% mean() %>% round(3) # Calculate mean reactivity
    sdtheory <- data_theory[, colnames(data_obtained)[[i]] ] %>% se() %>% round(3) # Calculate SEM
    dftheory <- data_theory[, colnames(data_obtained)[[i]] ] %>% complete.cases() %>% sum() %>% round(3) %>% -1 # Calculate df (sample size - 1)
    # Calculate Bayes factor
    tiff(paste0("Prior_AUC_", i, "_BF.tiff", sep = ""), res = 300, width = 16 * 0.7, height = 9 * 0.7, units = "in") # Save high-resolution TIFF file
    BF_test[[i]] <- BF_t(meantheory, sdtheory, dftheory, meanobtained, semobtained, dfobtained, colnames(data_obtained)[[i]])
    dev.off() # Save plot
  } else {
    ## If no, use uninformed prior
    MWU_test_df[i, "Prior"] <- "Uninformed prior" # Save as uninformed prior
    BF <- ttestBF(data_obtained[, i])
    BF_test[[i]] <- extractBF(BF, onlybf = TRUE)
  }
}

# Add bayes factors to dataframe
MWU_test_df[, "BF"] <- BF_test %>% unlist()
# Add column with Bayes factor interpretation
MWU_test_df$BF.evidence <- sapply(MWU_test_df$BF, function(x) BF.evidence(x)) # Add column with interpretation

# Save results
write.xlsx(MWU_test_df, "AUC.SET_CFC.xlsx")

# Remove variables
remove(MWU_test)
remove(MWU_test_df)
remove(data_obtained)
remove(data_theory)
remove(i)
remove(p.val)
remove(p.adj)
remove(p.value.sig)
remove(BF_t)
remove(BF_test)
remove(meantheory)
remove(sdtheory)
remove(dftheory)
remove(meanobtained)
remove(semobtained)
remove(dfobtained)
remove(se)
remove(BF)
remove(LSA_HSA)
remove(BF.evidence)
remove(d, rho, cohen.d.magnitude)


# Frontal CFC correlations (preregistered) ------------------------------------------------------------
# Load packages
library(dplyr) # For data manipulation
library(reshape2) # For melt function
library(Hmisc) # For rcorr function
library(psych) # For converting rho to Cohen's d
library(xlsx) # For exporting Excel results
library(BayesFactor) # For Bayesian statistics
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors

## Create correlation matrix
data <- SET_CFC %>% select("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                           "recov_Frontal_Avg_dPAC_Z",
                           "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                           "recov_Frontal_Avg_AAC_R",
                           "LSAS", "anx.react", "anx.recov",
                           "Cortisol.1", "cort.react", "cort.recov")
## Correlation calculation
cormat <- rcorr(as.matrix(data), type = "spearman")

## Make correlation coefficients into dataframe
cormat_r <- cormat$r %>% round(6)
# Remove everything under the diagonal
cormat_r[lower.tri(cormat_r, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat_r <- melt(cormat_r, na.rm = TRUE)
# Rename 'value' variable appropriately
colnames(melted_cormat_r)[3] <- "corr"

## Make p-values into dataframe
cormat_p <- cormat$P %>% round(6)
# Remove everything under the diagonal
cormat_p[lower.tri(cormat_p, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat_p <- melt(cormat_p, na.rm = TRUE)
# Rename 'value' variable appropriately
colnames(melted_cormat_p)[3] <- "p.value"

## Put r and p together
corr_test_df <- cbind(melted_cormat_r, melted_cormat_p[, "p.value"])
# rename stubborn 'p-value' variable again
colnames(corr_test_df)[4] <- "p.value"

# Remove factor
corr_test_df[, "Var1"] <- corr_test_df[, "Var1"] %>% as.character()
corr_test_df[, "Var2"] <- corr_test_df[, "Var2"] %>% as.character()

# Add Cohen's d
rho <- corr_test_df[, "corr"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
corr_test_df$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
corr_test_df$cohen.d.mag <- sapply(corr_test_df$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

# Do FDR-correction on p-values
p.val <- corr_test_df$p.value
p.adj <-  p.adjust(p.val, method = "fdr", n = length(p.val)) # Do fdr-correction
corr_test_df$p.adj <- p.adj
corr_test_df$p.adj.sig <- sapply(corr_test_df$p.adj, function(x) p.value.sig(x)) # Add column with significance
rownames(corr_test_df) <- NULL # Reset rownames

## Bayesian statistics
corrBF <- list(NA)
for (i in 1:nrow(corr_test_df)) {
  BF <- correlationBF(x = data[, corr_test_df[i, "Var1"] ], y = data[, corr_test_df[i, "Var2"] ] )
  corrBF[[i]] <- extractBF(BF, onlybf = TRUE)
}

# Add bayes factors to dataframe
corr_test_df[, "BF"] <- corrBF %>% unlist()
# Add column with Bayes factor interpretation
corr_test_df$BF.evidence <- sapply(corr_test_df$BF, function(x) BF.evidence(x)) # Add column with interpretation

# Save lm results
write.xlsx(corr_test_df, "Correlations_Frontal.SET_CFC.xlsx")

# Remove variables
remove(data)
remove(corr_test_df)
remove(p.val)
remove(p.adj)
remove(p.value.sig)
remove(BF)
remove(corrBF)
remove(i)
remove(cormat)
remove(cormat_p)
remove(cormat_r)
remove(melted_cormat_p)
remove(melted_cormat_r)
remove(BF.evidence)
remove(d, rho, cohen.d.magnitude)


# Frontal CFC correlations per sex (exploratory) ------------------------------------------------------------
# Load packages
library(dplyr) # For data manipulation
library(reshape2) # For melt function
library(Hmisc) # For rcorr function
library(psych) # For converting rho to Cohen's d
library(xlsx) # For exporting Excel results
library(BayesFactor) # For Bayesian statistics
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors

## Create correlation matrix
data <- SET_CFC %>% select("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                           "recov_Frontal_Avg_dPAC_Z",
                           "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                           "recov_Frontal_Avg_AAC_R",
                           "LSAS", "anx.react", "anx.recov",
                           "Cortisol.1", "cort.react", "cort.recov")
# Select only men
data_male <- data[SET_CFC$Sex == "Male", ]
# Select only women
data_female <- data[SET_CFC$Sex == "Female", ]

### For men
## Correlation calculation
cormat <- rcorr(as.matrix(data_male), type = "spearman")

## Make correlation coefficients into dataframe
cormat_r <- cormat$r %>% round(6)
# Remove everything under the diagonal
cormat_r[lower.tri(cormat_r, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat_r <- melt(cormat_r, na.rm = TRUE)
# Rename 'value' variable appropriately
colnames(melted_cormat_r)[3] <- "corr"

## Make p-values into dataframe
cormat_p <- cormat$P %>% round(6)
# Remove everything under the diagonal
cormat_p[lower.tri(cormat_p, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat_p <- melt(cormat_p, na.rm = TRUE)
# Rename 'value' variable appropriately
colnames(melted_cormat_p)[3] <- "p.value"

## Put r and p together
corr_test_df_male <- cbind(melted_cormat_r, melted_cormat_p[, "p.value"])
# rename stubborn 'p-value' variable again
colnames(corr_test_df_male)[4] <- "p.value"

# Remove factor
corr_test_df_male[, "Var1"] <- corr_test_df_male[, "Var1"] %>% as.character()
corr_test_df_male[, "Var2"] <- corr_test_df_male[, "Var2"] %>% as.character()


### For women
## Correlation calculation
cormat <- rcorr(as.matrix(data_female), type = "spearman")

## Make correlation coefficients into dataframe
cormat_r <- cormat$r %>% round(6)
# Remove everything under the diagonal
cormat_r[lower.tri(cormat_r, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat_r <- melt(cormat_r, na.rm = TRUE)
# Rename 'value' variable appropriately
colnames(melted_cormat_r)[3] <- "corr"

## Make p-values into dataframe
cormat_p <- cormat$P %>% round(6)
# Remove everything under the diagonal
cormat_p[lower.tri(cormat_p, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat_p <- melt(cormat_p, na.rm = TRUE)
# Rename 'value' variable appropriately
colnames(melted_cormat_p)[3] <- "p.value"

## Put r and p together
corr_test_df_female <- cbind(melted_cormat_r, melted_cormat_p[, "p.value"])
# rename stubborn 'p-value' variable again
colnames(corr_test_df_female)[4] <- "p.value"

# Remove factor
corr_test_df_female[, "Var1"] <- corr_test_df_female[, "Var1"] %>% as.character()
corr_test_df_female[, "Var2"] <- corr_test_df_female[, "Var2"] %>% as.character()

## Put men and women together
corr_test_df <- rbind(corr_test_df_male, corr_test_df_female)

# Add Cohen's d
rho <- corr_test_df[, "corr"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
corr_test_df$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
corr_test_df$cohen.d.mag <- sapply(corr_test_df$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

# Do FDR-correction on p-values
p.val <- corr_test_df$p.value
p.adj <-  p.adjust(p.val, method = "fdr", n = length(p.val)) # Do fdr-correction
corr_test_df$p.adj <- p.adj
corr_test_df$p.adj.sig <- sapply(corr_test_df$p.adj, function(x) p.value.sig(x)) # Add column with significance
rownames(corr_test_df) <- NULL # Reset rownames

## Bayesian statistics
corrBF <- list(NA)
for (i in 1:nrow(corr_test_df)) {
  BF <- correlationBF(x = data[, corr_test_df[i, "Var1"] ], y = data[, corr_test_df[i, "Var2"] ] )
  corrBF[[i]] <- extractBF(BF, onlybf = TRUE)
}

# Add bayes factors to dataframe
corr_test_df[, "BF"] <- corrBF %>% unlist()
# Add column with Bayes factor interpretation
corr_test_df$BF.evidence <- sapply(corr_test_df$BF, function(x) BF.evidence(x)) # Add column with interpretation

# Save lm results
write.xlsx(corr_test_df, "Correlations_Frontal_sex.SET_CFC.xlsx")

# Remove variables
remove(data)
remove(corr_test_df)
remove(p.val)
remove(p.adj)
remove(p.value.sig)
remove(BF)
remove(corrBF)
remove(i)
remove(cormat)
remove(cormat_p)
remove(cormat_r)
remove(melted_cormat_p)
remove(melted_cormat_r)
remove(corr_test_df_female)
remove(corr_test_df_male)
remove(data_female)
remove(data_male)
remove(BF.evidence)
remove(d, rho, cohen.d.magnitude)


# Parietal CFC correlations (preregistered) ------------------------------------------------------------
# Load packages
library(dplyr) # For data manipulation
library(reshape2) # For melt function
library(Hmisc) # For rcorr function
library(psych) # For converting rho to Cohen's d
library(xlsx) # For exporting Excel results
library(BayesFactor) # For Bayesian statistics
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors

## Create correlation matrix
data <- SET_CFC %>% select("RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                           "recov_Parietal_Avg_dPAC_Z",
                           "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                           "recov_Parietal_Avg_AAC_R",
                           "LSAS", "anx.react", "anx.recov",
                           "Cortisol.1", "cort.react", "cort.recov")
## Correlation calculation
cormat <- rcorr(as.matrix(data), type = "spearman")

## Make correlation coefficients into dataframe
cormat_r <- cormat$r %>% round(6)
# Remove everything under the diagonal
cormat_r[lower.tri(cormat_r, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat_r <- melt(cormat_r, na.rm = TRUE)
# Rename 'value' variable appropriately
colnames(melted_cormat_r)[3] <- "corr"

## Make p-values into dataframe
cormat_p <- cormat$P %>% round(6)
# Remove everything under the diagonal
cormat_p[lower.tri(cormat_p, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat_p <- melt(cormat_p, na.rm = TRUE)
# Rename 'value' variable appropriately
colnames(melted_cormat_p)[3] <- "p.value"

## Put r and p together
corr_test_df <- cbind(melted_cormat_r, melted_cormat_p[, "p.value"])
# rename stubborn 'p-value' variable again
colnames(corr_test_df)[4] <- "p.value"

# Remove factor
corr_test_df[, "Var1"] <- corr_test_df[, "Var1"] %>% as.character()
corr_test_df[, "Var2"] <- corr_test_df[, "Var2"] %>% as.character()

# Add Cohen's d
rho <- corr_test_df[, "corr"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
corr_test_df$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
corr_test_df$cohen.d.mag <- sapply(corr_test_df$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

# Do FDR-correction on p-values
p.val <- corr_test_df$p.value
p.adj <-  p.adjust(p.val, method = "fdr", n = length(p.val)) # Do fdr-correction
corr_test_df$p.adj <- p.adj
corr_test_df$p.adj.sig <- sapply(corr_test_df$p.adj, function(x) p.value.sig(x)) # Add column with significance
rownames(corr_test_df) <- NULL # Reset rownames

## Bayesian statistics
corrBF <- list(NA)
for (i in 1:nrow(corr_test_df)) {
  BF <- correlationBF(x = data[, corr_test_df[i, "Var1"] ], y = data[, corr_test_df[i, "Var2"] ] )
  corrBF[[i]] <- extractBF(BF, onlybf = TRUE)
}

# Add bayes factors to dataframe
corr_test_df[, "BF"] <- corrBF %>% unlist()
# Add column with Bayes factor interpretation
corr_test_df$BF.evidence <- sapply(corr_test_df$BF, function(x) BF.evidence(x)) # Add column with interpretation

# Save lm results
write.xlsx(corr_test_df, "Correlations_Parietal.SET_CFC.xlsx")

# Remove variables
remove(data)
remove(corr_test_df)
remove(p.val)
remove(p.adj)
remove(p.value.sig)
remove(BF)
remove(corrBF)
remove(i)
remove(cormat)
remove(cormat_p)
remove(cormat_r)
remove(melted_cormat_p)
remove(melted_cormat_r)
remove(BF.evidence)
remove(d, rho, cohen.d.magnitude)


# Parietal CFC Correlations per sex (exploratory) ------------------------------------------------------------
# Load packages
library(dplyr) # For data manipulation
library(reshape2) # For melt function
library(Hmisc) # For rcorr function
library(psych) # For converting rho to Cohen's d
library(xlsx) # For exporting Excel results
library(BayesFactor) # For Bayesian statistics
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors

## Create correlation matrix
data <- SET_CFC %>% select("RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                           "recov_Parietal_Avg_dPAC_Z",
                           "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                           "recov_Parietal_Avg_AAC_R",
                           "LSAS", "anx.react", "anx.recov",
                           "Cortisol.1", "cort.react", "cort.recov")
# Select only men
data_male <- data[SET_CFC$Sex == "Male", ]
# Select only women
data_female <- data[SET_CFC$Sex == "Female", ]

### For men
## Correlation calculation
cormat <- rcorr(as.matrix(data_male), type = "spearman")

## Make correlation coefficients into dataframe
cormat_r <- cormat$r %>% round(6)
# Remove everything under the diagonal
cormat_r[lower.tri(cormat_r, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat_r <- melt(cormat_r, na.rm = TRUE)
# Rename 'value' variable appropriately
colnames(melted_cormat_r)[3] <- "corr"

## Make p-values into dataframe
cormat_p <- cormat$P %>% round(6)
# Remove everything under the diagonal
cormat_p[lower.tri(cormat_p, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat_p <- melt(cormat_p, na.rm = TRUE)
# Rename 'value' variable appropriately
colnames(melted_cormat_p)[3] <- "p.value"

## Put r and p together
corr_test_df_male <- cbind(melted_cormat_r, melted_cormat_p[, "p.value"])
# rename stubborn 'p-value' variable again
colnames(corr_test_df_male)[4] <- "p.value"

# Remove factor
corr_test_df_male[, "Var1"] <- corr_test_df_male[, "Var1"] %>% as.character()
corr_test_df_male[, "Var2"] <- corr_test_df_male[, "Var2"] %>% as.character()


### For women
## Correlation calculation
cormat <- rcorr(as.matrix(data_female), type = "spearman")

## Make correlation coefficients into dataframe
cormat_r <- cormat$r %>% round(6)
# Remove everything under the diagonal
cormat_r[lower.tri(cormat_r, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat_r <- melt(cormat_r, na.rm = TRUE)
# Rename 'value' variable appropriately
colnames(melted_cormat_r)[3] <- "corr"

## Make p-values into dataframe
cormat_p <- cormat$P %>% round(6)
# Remove everything under the diagonal
cormat_p[lower.tri(cormat_p, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat_p <- melt(cormat_p, na.rm = TRUE)
# Rename 'value' variable appropriately
colnames(melted_cormat_p)[3] <- "p.value"

## Put r and p together
corr_test_df_female <- cbind(melted_cormat_r, melted_cormat_p[, "p.value"])
# rename stubborn 'p-value' variable again
colnames(corr_test_df_female)[4] <- "p.value"

# Remove factor
corr_test_df_female[, "Var1"] <- corr_test_df_female[, "Var1"] %>% as.character()
corr_test_df_female[, "Var2"] <- corr_test_df_female[, "Var2"] %>% as.character()

## Put men and women together
corr_test_df <- rbind(corr_test_df_male, corr_test_df_female)

# Add Cohen's d
rho <- corr_test_df[, "corr"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
corr_test_df$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
corr_test_df$cohen.d.mag <- sapply(corr_test_df$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d


# Do FDR-correction on p-values
p.val <- corr_test_df$p.value
p.adj <-  p.adjust(p.val, method = "fdr", n = length(p.val)) # Do fdr-correction
corr_test_df$p.adj <- p.adj
corr_test_df$p.adj.sig <- sapply(corr_test_df$p.adj, function(x) p.value.sig(x)) # Add column with significance
rownames(corr_test_df) <- NULL # Reset rownames

## Bayesian statistics
corrBF <- list(NA)
for (i in 1:nrow(corr_test_df)) {
  BF <- correlationBF(x = data[, corr_test_df[i, "Var1"] ], y = data[, corr_test_df[i, "Var2"] ] )
  corrBF[[i]] <- extractBF(BF, onlybf = TRUE)
}

# Add bayes factors to dataframe
corr_test_df[, "BF"] <- corrBF %>% unlist()
# Add column with Bayes factor interpretation
corr_test_df$BF.evidence <- sapply(corr_test_df$BF, function(x) BF.evidence(x)) # Add column with interpretation

# Save lm results
write.xlsx(corr_test_df, "Correlations_Parietal_sex.SET_CFC.xlsx")

# Remove variables
remove(data)
remove(corr_test_df)
remove(p.val)
remove(p.adj)
remove(p.value.sig)
remove(BF)
remove(corrBF)
remove(i)
remove(cormat)
remove(cormat_p)
remove(cormat_r)
remove(melted_cormat_p)
remove(melted_cormat_r)
remove(corr_test_df_female)
remove(corr_test_df_male)
remove(data_female)
remove(data_male)
remove(BF.evidence)
remove(d, rho, cohen.d.magnitude)


# Heatmap correlations (exloratory) -----------------------------------------------------
## Packages
library(ggplot2) # For plotting
library(magrittr) # For piping
library(dplyr) # For data manipulation
library(reshape2) # For melt function
library(data.table) # For unique function
library(Hmisc) # For rcorr function

### Frontal PAC
## Create correlation matrix
data <- SET_CFC %>% select("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                           "recov_Frontal_Avg_dPAC_Z",
                           "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                           "recov_Frontal_Avg_AAC_R",
                           "LSAS", "anx.react", "anx.recov",
                           "Cortisol.1", "cort.react", "cort.recov")
# Set the variable names to be more plot-readable
colnames(data) <- c("Baseline PAC", "PAC reactivity", "PAC recovery",
                    "Baseline AAC", "AAC reactivity", "AAC recovery",
                    "Trait social anxiety", "State anxiety reactivity", "State anxiety recovery",
                    "Baseline cortisol", "Cortisol reactivity", "Cortisol recovery")
# Correlation coefficients
cormat <- rcorr(as.matrix(data), type = "spearman")
cormat <- cormat$r %>% round(2)
# Remove everything under the diagonal
cormat[lower.tri(cormat, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat <- melt(cormat, na.rm = TRUE)

## Plot correlation matrix heatmap
# To save high-res figure
tiff("Heatmap_FrontalCFC.tiff", width = 25, height = 25, units = "cm", res = 300)
# Plot
ggheatmap <- ggplot(melted_cormat, aes(Var1, Var2, fill = value)) + # Use melted correlation matrix
  ggtitle("Frontal delta-beta coupling correlations with stress responses") +
  geom_tile(color = "white") + # White background
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") + # Fill colours
  theme_minimal() + # minimal theme
  theme(title = element_text(vjust = 1, size = 14), # Make the title bigger and move to the right
        axis.text.x = element_text(angle = 90, size = 12), # Angle the x-axis labels 90 degrees and make bigger
        axis.text.y = element_text(size = 12), # Make y-axis labels bigger
        axis.title.x = element_blank(), # Remove 'Var' from the x-axis
        axis.title.y = element_blank(), # Remove 'Var' from the y-axis
        panel.grid.major = element_blank(), # Remove gridlines
        panel.border = element_blank(), # Remove border
        panel.background = element_blank(), # Remove background
        axis.ticks = element_blank(), # Remove axis ticks
        legend.justification = c("right", "bottom"),
        legend.position = c(1, 0)) + 
  scale_x_discrete(position = "top") + # Put the x-axis on top of the plot
  coord_fixed() + # For the tiles to be square
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 4) # Add correlation coefficients
# Print the heatmap
print(ggheatmap)
# Save figure
dev.off()

### Parietal PAC
## Create correlation matrix
data <- SET_CFC %>% select("RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                           "recov_Parietal_Avg_dPAC_Z",
                           "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                           "recov_Parietal_Avg_AAC_R",
                           "LSAS", "anx.react", "anx.recov",
                           "Cortisol.1", "cort.react", "cort.recov")
# Set the variable names to be more plot-readable
colnames(data) <- c("Baseline PAC", "PAC reactivity", "PAC recovery",
                    "Baseline AAC", "AAC reactivity", "AAC recovery",
                    "Trait social anxiety", "State anxiety reactivity", "State anxiety recovery",
                    "Baseline cortisol", "Cortisol reactivity", "Cortisol recovery")
# Correlation coefficients
cormat <- rcorr(as.matrix(data), type = "spearman")
cormat <- cormat$r %>% round(2)
# Remove everything under the diagonal
cormat[lower.tri(cormat, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat <- melt(cormat, na.rm = TRUE)

## Plot correlation matrix heatmap
# To save high-res figure
tiff("Heatmap_ParietalCFC.tiff", width = 25, height = 25, units = "cm", res = 300)
# Plot
ggheatmap <- ggplot(melted_cormat, aes(Var1, Var2, fill = value)) + # Use melted correlation matrix
  ggtitle("Parietal delta-beta coupling correlations with stress responses") +
  geom_tile(color = "white") + # White background
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Spearman\nCorrelation") + # Fill colours
  theme_minimal() + # minimal theme
  theme(title = element_text(vjust = 1, size = 14), # Make the title bigger and move to the right
        axis.text.x = element_text(angle = 90, size = 12), # Angle the x-axis labels 90 degrees and make bigger
        axis.text.y = element_text(size = 12), # Make y-axis labels bigger
        axis.title.x = element_blank(), # Remove 'Var' from the x-axis
        axis.title.y = element_blank(), # Remove 'Var' from the y-axis
        panel.grid.major = element_blank(), # Remove gridlines
        panel.border = element_blank(), # Remove border
        panel.background = element_blank(), # Remove background
        axis.ticks = element_blank(), # Remove axis ticks
        legend.justification = c("right", "bottom"),
        legend.position = c(1, 0)) + 
  scale_x_discrete(position = "top") + # Put the x-axis on top of the plot
  coord_fixed() + # For the tiles to be square
  geom_text(aes(Var1, Var2, label = value), color = "black", size = 4) # Add correlation coefficients
# Print the heatmap
print(ggheatmap)
# Save figure
dev.off()


## Remove unnecessary variables
remove(data)
remove(cormat)
remove(melted_cormat)
remove(ggheatmap)


# location differences between frontal and parietal (preregistered) ---------------------------------------------------------
# For supplements #

# Load packages
library(data.table) # For function 'rbindlist'
library(magrittr) # For piping
library(xlsx) # For exporting to Excel
library(psych) # For converting rho to Cohen's d
library(BayesFactor) # For Bayesian statistics
library(dplyr) # For data manipulation
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors


# Separate CFC data according to location
data_frontal <- select(SET_CFC, "react_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_AAC_R",
                       "recov_Frontal_Avg_dPAC_Z", "recov_Frontal_Avg_AAC_R")
data_parietal <- select(SET_CFC, "react_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_AAC_R",
                        "recov_Parietal_Avg_dPAC_Z", "recov_Parietal_Avg_AAC_R")

# paired Wilcoxon signed rank test
MWU_test <- list() # Initialize results list
for (i in 1:ncol(data_frontal)) {
  MWU_test[[i]] <- wilcox.test(data_frontal[, i],data_parietal[, i], paired = TRUE)
  MWU_test[[i]][["data.name"]] <- paste0(colnames(data_frontal[i]),"vs.",colnames(data_parietal[i])) # Add the apropriate variable name
}

# Convert results to a dataframe
MWU_test_df <- rbindlist(MWU_test)
MWU_test_df <- MWU_test_df[ , -c("parameter", "null.value")] # Remove unnecessary columns
# Calculate rank correlation
MWU_test_df[, "rank.corr"] <- nrow(data_frontal) / MWU_test_df[, "statistic"]
# Replace infinite correlations with 1
MWU_test_df[which(MWU_test_df[, "rank.corr"] == Inf), "rank.corr"] <- 1
# Rename the 'statistic' result appropriately
colnames(MWU_test_df)[which(colnames(MWU_test_df) == "statistic")] <- "W"

# Add Cohen's d
rho <- MWU_test_df[, "rank.corr"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
MWU_test_df$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
MWU_test_df$cohen.d.mag <- sapply(MWU_test_df$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

# Do FDR-correction on p-values
p.val <- MWU_test_df$p.value
p.adj <-  p.adjust(p.val, method = "fdr", n = length(p.val)) # Do fdr-correction
MWU_test_df$p.adj <- p.adj
MWU_test_df$p.adj.sig <- sapply(MWU_test_df$p.adj, function(x) p.value.sig(x)) # Add column with significance

## Bayesian statistics
BF_test <- list() # Initialize results list
# Calculate Bayes factor
for (i in 1:ncol(data_frontal)) {
  BF <- ttestBF(data_frontal[, i], data_parietal[, i], paired = TRUE)
  BF_test[[i]] <- extractBF(BF, onlybf = TRUE)
}

# Add bayes factors to dataframe
MWU_test_df[, "BF"] <- BF_test %>% unlist()
# Add column with Bayes factor interpretation
MWU_test_df$BF.evidence <- sapply(MWU_test_df$BF, function(x) BF.evidence(x)) # Add column with interpretation

# Save lm results
write.xlsx(MWU_test_df, "FrontalParietal_Reactivity.SET_CFC.xlsx")

# Remove variables
remove(MWU_test)
remove(MWU_test_df)
remove(data_frontal)
remove(data_parietal)
remove(p.val)
remove(p.adj)
remove(p.value.sig)
remove(i)
remove(BF)
remove(BF_test)
remove(BF.evidence)
remove(d, rho, cohen.d.magnitude)

# Moderation analyses (not preregistered but confirmatory) -----------------------------------------------------
## Packages
library(ggplot2) # For plotting
library(dplyr) # For data manipulation
library(magrittr) # For piping
library(grid) # To organize multiple subplots in one large plot
library(gridBase) # To organize multiple subplots in one large plot
library(gridExtra) # To organize multiple subplots in one large plot
library(pequod) # For moderation analysis
source("PlotSlopes.R") # Load customized pequod function for prettier plots
options(scipen = 999) # Show numeric instead of scientific decimals

# Load dataset
load("SET_CFC.Rdata")

## Load relevant variables
data <- SET_CFC[, c("react_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_AAC_R",
                    "recov_Frontal_Avg_dPAC_Z", "recov_Frontal_Avg_AAC_R",
                    "anx.react", "anx.recov",
                    "cort.react", "cort.recov")]
# Divide data by median split LSAS
data_LSA <- data[SET_CFC$LSAS_Split == "Low", ] # Select only low trait social anxiety data
data_HSA <- data[SET_CFC$LSAS_Split == "High", ] # Select only high trait social anxiety data

### Predictor: state anxiety reactivity
## Moderation analysis for frontal dPAC reactivity
# Moderated regression (CFC predicted by state anxiety reactivity, moderated by trait anxiety)
mod <- lmres(formula = 'react_Frontal_Avg_dPAC_Z ~ anx.react * LSAS', 
             data = SET_CFC,
             centered = c('anx.react', 'LSAS'))
# Save results
sink("mod_Frontal_dPAC_react_anxiety.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "anx.react", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Frontal_dPAC_react_anxiety.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "State anxiety reactivity-related increase (0 - 100)",
                         namey = "Frontal PAC reactivity-related increase (Z-score)")
# Save Figure
tiff("mod_Frontal_dPAC_react_anxiety.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()

### Scatterplot with median split
## Figure 1: Scatter plot frontal PAC and state anxiety reactivity
tiff("Scatter_FrontalPAC_StateAnx_React.tiff", width = 75, height = 20, units = "cm", res = 300) # Save TIFF file
# Plot simple slopes again
plot(plot_slopes)
## Scatter plot: LSA
corr <- cor(x = data_LSA$anx.react, y = data_LSA$react_Frontal_Avg_dPAC_Z, method = "spearman") %>% round(2) # Calculate spearman correlation
LSA <- ggplot(data_LSA, aes(x = anx.react, y = react_Frontal_Avg_dPAC_Z)) + 
  geom_point(shape = 1, size = 2, stroke = 1.5) + # Add large hollow scatter point
  geom_smooth(method = lm, colour = "black", size = 1.5) + # Add a thick black regression line
  labs(title="Low trait social anxiety group (median split)",
       x="State anxiety reactivity-related increase (0 - 100)", y = "Frontal PAC reactivity-related increase (Z-score)",
       subtitle = "b)", # Add a title, axis labels, and a subtitle
       caption = bquote(rho ~ "=" ~ .(corr) ) ) + # Show correlation coefficient in caption
  theme_classic() + # Remove background and gridlines
  theme(
    axis.title = element_text(size = 15), # Increase size of axis labels
    axis.text = element_text(size = 14, colour = "black"), # Increase size of axis ticks
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), # Increase size of bold title and align in the middle
    plot.subtitle = element_text(size = 20, face = "bold"), # Increase size of subtitle and make bold
    plot.caption = element_text(size = 15, hjust = 0) # Increase size of subcaption and place left
  ) +
  scale_x_continuous(breaks = seq(-25, 100, 25), labels = seq(-25, 100, 25), limits = c(-25, 100)) + # Set axis manually so that they are the same for both LSA and HSA plots
  scale_y_continuous(breaks = seq(-1.0, 1.0, 0.5), labels = seq(-1.0, 1.0, 0.5), limits = c(-1.1, 1.1))
print(LSA)
## Scatter plot: HSA
corr <- cor(x = data_HSA$anx.react, y = data_HSA$react_Frontal_Avg_dPAC_Z, method = "spearman") %>% round(2) # Calculate spearman correlation
HSA <- ggplot(data_HSA, aes(x = anx.react, y = react_Frontal_Avg_dPAC_Z)) + 
  geom_point(shape = 1, size = 2, stroke = 1.5) + # Add large hollow scatter point
  geom_smooth(method = lm, colour = "black", size = 1.5) + # Add a thick black regression line
  labs(title="High trait social anxiety group (median split)",
       x="State anxiety reactivity-related increase (0 - 100)", y = "Frontal PAC reactivity-related increase (Z-score)",
       subtitle = "c)", # Add a title, axis labels, and a subtitle
       caption = bquote(rho ~ "=" ~ .(corr) ) ) + # Show correlation coefficient in caption
  theme_classic() + # Remove background and gridlines
  theme(
    axis.title = element_text(size = 15), # Increase size of axis labels
    axis.text = element_text(size = 14, colour = "black"), # Increase size of axis ticks
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), # Increase size of bold title and align in the middle
    plot.subtitle = element_text(size = 20, face = "bold"), # Increase size of subtitle and make bold
    plot.caption = element_text(size = 15, hjust = 0) # Increase size of subcaption and place left
  ) +
  scale_x_continuous(breaks = seq(-25, 100, 25), labels = seq(-25, 100, 25), limits = c(-25, 100)) + # Set axis manually so that they are the same for both LSA and HSA plots
  scale_y_continuous(breaks = seq(-1.0, 1.0, 0.5), labels = seq(-1.0, 1.0, 0.5), limits = c(-1.1, 1.1))
print(HSA)
# Arrange plots side-by-side
grid.arrange(plot_slopes, LSA, HSA, nrow = 1)
# Print Figure
dev.off()


## Moderation analysis for parietal dPAC reactivity
# Moderated regression (CFC predicted by state anxiety reactivity, moderated by trait anxiety)
mod <- lmres(formula = 'react_Parietal_Avg_dPAC_Z ~ anx.react * LSAS', 
             data = SET_CFC,
             centered = c('anx.react', 'LSAS'))
# Save results
sink("mod_Parietal_dPAC_react_anxiety.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "anx.react", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Parietal_dPAC_react_anxiety.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "State anxiety reactivity (0 - 100)",
                         namey = "Parietal PAC reactivity (Z-score)")
# Save Figure
tiff("mod_Parietal_dPAC_react_anxiety.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()

## Moderation analysis for frontal AAC reactivity
# Moderated regression (CFC predicted by state anxiety reactivity, moderated by trait anxiety)
mod <- lmres(formula = 'react_Frontal_Avg_AAC_R ~ anx.react * LSAS', 
             data = SET_CFC,
             centered = c('anx.react', 'LSAS'))
# Save results
sink("mod_Frontal_AAC_react_anxiety.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "anx.react", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Frontal_AAC_react_anxiety.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "State anxiety reactivity (0 - 100)",
                         namey = "Frontal AAC reactivity (corr.)")
# Save Figure
tiff("mod_Frontal_AAC_react_anxiety.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()

## Moderation analysis for parietal AAC reactivity
# Moderated regression (CFC predicted by state anxiety reactivity, moderated by trait anxiety)
mod <- lmres(formula = 'react_Parietal_Avg_AAC_R ~ anx.react * LSAS', 
             data = SET_CFC,
             centered = c('anx.react', 'LSAS'))
# Save results
sink("mod_Parietal_AAC_react_anxiety.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "anx.react", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Parietal_AAC_react_anxiety.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "State anxiety reactivity (0 - 100)",
                         namey = "Parietal AAC reactivity (corr.)")
# Save Figure
tiff("mod_Parietal_AAC_react_anxiety.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()

### Predictor: cortisol reactivity
## Moderation analysis for frontal dPAC reactivity
# Moderated regression (CFC predicted by state anxiety reactivity, moderated by trait anxiety)
mod <- lmres(formula = 'react_Frontal_Avg_dPAC_Z ~ cort.react * LSAS', 
             data = SET_CFC,
             centered = c('cort.react', 'LSAS'))
# Save results
sink("mod_Frontal_dPAC_react_cortisol.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "cort.react", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Frontal_dPAC_react_cortisol.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "Cortisol reactivity (ng/ml)",
                         namey = "Frontal PAC reactivity (Z-score)")
# Save Figure
tiff("mod_Frontal_dPAC_react_cortisol.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()

## Moderation analysis for parietal dPAC reactivity
# Moderated regression (CFC predicted by state anxiety reactivity, moderated by trait anxiety)
mod <- lmres(formula = 'react_Parietal_Avg_dPAC_Z ~ cort.react * LSAS', 
             data = SET_CFC,
             centered = c('cort.react', 'LSAS'))
# Save results
sink("mod_Parietal_dPAC_react_cortisol.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "cort.react", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Parietal_dPAC_react_cortisol.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "Cortisol reactivity (ng/ml)",
                         namey = "Parietal PAC reactivity (Z-score)")
# Save Figure
tiff("mod_Parietal_dPAC_react_cortisol.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()

## Moderation analysis for frontal AAC reactivity
# Moderated regression (CFC predicted by cortisol reactivity, moderated by trait anxiety)
mod <- lmres(formula = 'react_Frontal_Avg_AAC_R ~ cort.react * LSAS', 
             data = SET_CFC,
             centered = c('cort.react', 'LSAS'))
# Save results
sink("mod_Frontal_AAC_react_cortisol.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "cort.react", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Frontal_AAC_react_cortisol.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "Cortisol reactivity-related increase (ng/ml)",
                         namey = "Frontal AAC reactivity-related increase (corr.)")
# Save Figure
tiff("mod_Frontal_AAC_react_cortisol.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()

### Scatterplot with median split
## Figure 2: Scatter plot frontal AAC and cortisol reactivity
tiff("Scatter_FrontalAAC_Cort_React.tiff", width = 75, height = 20, units = "cm", res = 300) # Save TIFF file
# Plot simple slopes again
plot(plot_slopes)
## Scatter plot: LSA
corr <- cor(x = data_LSA$cort.react, y = data_LSA$react_Frontal_Avg_AAC_R, method = "spearman") %>% round(2) # Calculate spearman correlation
LSA <- ggplot(data_LSA, aes(x = cort.react, y = react_Frontal_Avg_AAC_R)) + 
  geom_point(shape = 1, size = 2, stroke = 1.5) + # Add large hollow scatter point
  geom_smooth(method = lm, colour = "black", size = 1.5) + # Add a thick black regression line
  labs(title="Low trait social anxiety group (median split)",
       x="Cortisol reactivity-related increase (ng/ml)", y = "Frontal AAC reactivity-related increase (corr.)",
       subtitle = "b)", # Add a title, axis labels, and a subtitle
       caption = bquote(rho ~ "=" ~ .(corr) ) ) + # Show correlation coefficient in caption
  theme_classic() + # Remove background and gridlines
  theme(
    axis.title = element_text(size = 15), # Increase size of axis labels
    axis.text = element_text(size = 14, colour = "black"), # Increase size of axis ticks
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), # Increase size of bold title and align in the middle
    plot.subtitle = element_text(size = 20, face = "bold"), # Increase size of subtitle and make bold
    plot.caption = element_text(size = 15, hjust = 0) # Increase size of subcaption and place left
  ) +
  scale_x_continuous(breaks = seq(-5, 15, 5), labels = seq(-5, 15, 5), limits = c(-6, 18)) + # Set axis manually so that they are the same for both LSA and HSA plots
  scale_y_continuous(breaks = seq(-0.05, 0.15, 0.05), labels = seq(-0.05, 0.15, 0.05), limits = c(-0.07, 0.15))
print(LSA)
## Scatter plot: HSA
corr <- cor(x = data_HSA$cort.react, y = data_HSA$react_Frontal_Avg_AAC_R, method = "spearman") %>% round(2) # Calculate spearman correlation
HSA <- ggplot(data_HSA, aes(x = cort.react, y = react_Frontal_Avg_AAC_R)) + 
  geom_point(shape = 1, size = 2, stroke = 1.5) + # Add large hollow scatter point
  geom_smooth(method = lm, colour = "black", size = 1.5) + # Add a thick black regression line
  labs(title="High trait social anxiety group (median split)",
       x="Cortisol reactivity-related increase (ng/ml)", y = "Frontal AAC reactivity-related increase (corr.)",
       subtitle = "c)", # Add a title, axis labels, and a subtitle
       caption = bquote(rho ~ "=" ~ .(corr) ) ) + # Show correlation coefficient in caption
  theme_classic() + # Remove background and gridlines
  theme(
    axis.title = element_text(size = 15), # Increase size of axis labels
    axis.text = element_text(size = 14, colour = "black"), # Increase size of axis ticks
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), # Increase size of bold title and align in the middle
    plot.subtitle = element_text(size = 20, face = "bold"), # Increase size of subtitle and make bold
    plot.caption = element_text(size = 15, hjust = 0) # Increase size of subcaption and place left
  ) +
  scale_x_continuous(breaks = seq(-5, 15, 5), labels = seq(-5, 15, 5), limits = c(-6, 18)) + # Set axis manually so that they are the same for both LSA and HSA plots
  scale_y_continuous(breaks = seq(-0.05, 0.15, 0.05), labels = seq(-0.05, 0.15, 0.05), limits = c(-0.07, 0.15))
print(HSA)
# Arrange plots side-by-side
grid.arrange(plot_slopes, LSA, HSA, nrow = 1)
# Print Figure
dev.off()


## Moderation analysis for parietal AAC reactivity
# Moderated regression (CFC predicted by cortisol reactivity, moderated by trait anxiety)
mod <- lmres(formula = 'react_Parietal_Avg_AAC_R ~ cort.react * LSAS', 
             data = SET_CFC,
             centered = c('cort.react', 'LSAS'))
# Save results
sink("mod_Parietal_AAC_react_cortisol.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "cort.react", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Parietal_AAC_react_cortisol.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "Cortisol reactivity (ng/ml)",
                         namey = "Parietal AAC reactivity (corr.)")
# Save Figure
tiff("mod_Parietal_AAC_react_cortisol.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()


### Predictor: state anxiety recovery
## Moderation analysis for frontal dPAC recovery
# Moderated regression (CFC predicted by state anxiety recovery, moderated by trait anxiety)
mod <- lmres(formula = 'recov_Frontal_Avg_dPAC_Z ~ anx.recov * LSAS', 
             data = SET_CFC,
             centered = c('anx.recov', 'LSAS'))
# Save results
sink("mod_Frontal_dPAC_recov_anxiety.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "anx.recov", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Frontal_dPAC_recov_anxiety.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "State anxiety recovery (0 - 100)",
                         namey = "Frontal PAC recovery (Z-score)")
# Save Figure
tiff("mod_Frontal_dPAC_recov_anxiety.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()

## Moderation analysis for parietal dPAC recovery
# Moderated regression (CFC predicted by state anxiety recovery, moderated by trait anxiety)
mod <- lmres(formula = 'recov_Parietal_Avg_dPAC_Z ~ anx.recov * LSAS', 
             data = SET_CFC,
             centered = c('anx.recov', 'LSAS'))
# Save results
sink("mod_Parietal_dPAC_recov_anxiety.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "anx.recov", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Parietal_dPAC_recov_anxiety.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "State anxiety recovery (0 - 100)",
                         namey = "Parietal PAC recovery (Z-score)")
# Save Figure
tiff("mod_Parietal_dPAC_recov_anxiety.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()

## Moderation analysis for frontal AAC recovery
# Moderated regression (CFC predicted by state anxiety recovery, moderated by trait anxiety)
mod <- lmres(formula = 'recov_Frontal_Avg_AAC_R ~ anx.recov * LSAS', 
             data = SET_CFC,
             centered = c('anx.recov', 'LSAS'))
# Save results
sink("mod_Frontal_AAC_recov_anxiety.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "anx.recov", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Frontal_AAC_recov_anxiety.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "State anxiety recovery (0 - 100)",
                         namey = "Frontal AAC recovery (corr.)")
# Save Figure
tiff("mod_Frontal_AAC_recov_anxiety.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()

## Moderation analysis for parietal AAC recovery
# Moderated regression (CFC predicted by state anxiety recovery, moderated by trait anxiety)
mod <- lmres(formula = 'recov_Parietal_Avg_AAC_R ~ anx.recov * LSAS', 
             data = SET_CFC,
             centered = c('anx.recov', 'LSAS'))
# Save results
sink("mod_Parietal_AAC_recov_anxiety.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "anx.recov", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Parietal_AAC_recov_anxiety.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "State anxiety recovery (0 - 100)",
                         namey = "Parietal AAC recovery (corr.)")
# Save Figure
tiff("mod_Parietal_AAC_recov_anxiety.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()

### Predictor: cortisol recovery
## Moderation analysis for frontal dPAC recovery
# Moderated regression (CFC predicted by cortisol recovery, moderated by trait anxiety)
mod <- lmres(formula = 'recov_Frontal_Avg_dPAC_Z ~ cort.recov * LSAS', 
             data = SET_CFC,
             centered = c('cort.recov', 'LSAS'))
# Save results
sink("mod_Frontal_dPAC_recov_cortisol.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "cort.recov", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Frontal_dPAC_recov_cortisol.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "Cortisol recovery (ng/ml)",
                         namey = "Frontal PAC recovery (Z-score)")
# Save Figure
tiff("mod_Frontal_dPAC_recov_cortisol.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()

## Moderation analysis for parietal dPAC recovery
# Moderated regression (CFC predicted by cortisol recovery, moderated by trait anxiety)
mod <- lmres(formula = 'recov_Parietal_Avg_dPAC_Z ~ cort.recov * LSAS', 
             data = SET_CFC,
             centered = c('cort.recov', 'LSAS'))
# Save results
sink("mod_Parietal_dPAC_recov_cortisol.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "cort.recov", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Parietal_dPAC_recov_cortisol.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "Cortisol recovery (ng/ml)",
                         namey = "Parietal PAC recovery (Z-score)")
# Save Figure
tiff("mod_Parietal_dPAC_recov_cortisol.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()

## Moderation analysis for frontal AAC recovery
# Moderated regression (CFC predicted by cortisol recovery, moderated by trait anxiety)
mod <- lmres(formula = 'recov_Frontal_Avg_AAC_R ~ cort.recov * LSAS', 
             data = SET_CFC,
             centered = c('cort.recov', 'LSAS'))
# Save results
sink("mod_Frontal_AAC_recov_cortisol.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "cort.recov", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Frontal_AAC_recov_cortisol.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "Cortisol recovery-related decrease (ng/ml)",
                         namey = "Frontal AAC recovery-related decrease (corr.)")
# Save Figure
tiff("mod_Frontal_AAC_recov_cortisol.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()

### Scatterplot with median split
## Figure 3: Scatter plot frontal AAC and cortisol recovery
tiff("Scatter_FrontalAAC_Cort_Recov.tiff", width = 75, height = 20, units = "cm", res = 300) # Save TIFF file
# Plot simple slopes again
plot(plot_slopes)
## Scatter plot: LSA
corr <- cor(x = data_LSA$cort.recov, y = data_LSA$recov_Frontal_Avg_AAC_R, method = "spearman") %>% round(2) # Calculate spearman correlation
LSA <- ggplot(data_LSA, aes(x = cort.recov, y = recov_Frontal_Avg_AAC_R)) + 
  geom_point(shape = 1, size = 2, stroke = 1.5) + # Add large hollow scatter point
  geom_smooth(method = lm, colour = "black", size = 1.5) + # Add a thick black regression line
  labs(title="Low trait social anxiety group (median split)",
       x="Cortisol recovery-related decrease (ng/ml)", y = "Frontal AAC recovery-related decrease (corr.)",
       subtitle = "b)", # Add a title, axis labels, and a subtitle
       caption = bquote(rho ~ "=" ~ .(corr) ) ) + # Show correlation coefficient in caption
  theme_classic() + # Remove background and gridlines
  theme(
    axis.title = element_text(size = 15), # Increase size of axis labels
    axis.text = element_text(size = 14, colour = "black"), # Increase size of axis ticks
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), # Increase size of bold title and align in the middle
    plot.subtitle = element_text(size = 20, face = "bold"), # Increase size of subtitle and make bold
    plot.caption = element_text(size = 15, hjust = 0) # Increase size of subcaption and place left
  ) +
  scale_x_continuous(breaks = seq(-10, 0, 2.5), labels = seq(-10, 0, 2.5), limits = c(-10, 0)) + # Set axis manually so that they are the same for both LSA and HSA plots
  scale_y_continuous(breaks = seq(-0.10, 0.05, 0.05), labels = seq(-0.10, 0.05, 0.05), limits = c(-0.12, 0.07))
print(LSA)
## Scatter plot: HSA
corr <- cor(x = data_HSA$cort.recov, y = data_HSA$recov_Frontal_Avg_AAC_R, method = "spearman") %>% round(2) # Calculate spearman correlation
HSA <- ggplot(data_HSA, aes(x = cort.recov, y = recov_Frontal_Avg_AAC_R)) + 
  geom_point(shape = 1, size = 2, stroke = 1.5) + # Add large hollow scatter point
  geom_smooth(method = lm, colour = "black", size = 1.5) + # Add a thick black regression line
  labs(title="High trait social anxiety group (median split)",
       x="Cortisol recovery-related decrease (ng/ml)", y = "Frontal AAC recovery-related decrease (corr.)",
       subtitle = "c)", # Add a title, axis labels, and a subtitle
       caption = bquote(rho ~ "=" ~ .(corr) ) ) + # Show correlation coefficient in caption
  theme_classic() + # Remove background and gridlines
  theme(
    axis.title = element_text(size = 15), # Increase size of axis labels
    axis.text = element_text(size = 14, colour = "black"), # Increase size of axis ticks
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), # Increase size of bold title and align in the middle
    plot.subtitle = element_text(size = 20, face = "bold"), # Increase size of subtitle and make bold
    plot.caption = element_text(size = 15, hjust = 0) # Increase size of subcaption and place left
  ) +
  scale_x_continuous(breaks = seq(-10, 0, 2.5), labels = seq(-10, 0, 2.5), limits = c(-10, 0)) + # Set axis manually so that they are the same for both LSA and HSA plots
  scale_y_continuous(breaks = seq(-0.10, 0.05, 0.05), labels = seq(-0.10, 0.05, 0.05), limits = c(-0.12, 0.07))
print(HSA)
# Arrange plots side-by-side
grid.arrange(plot_slopes, LSA, HSA, nrow = 1)
# Print Figure
dev.off()


## Moderation analysis for parietal AAC recovery
# Moderated regression (CFC predicted by cortisol recovery, moderated by trait anxiety)
mod <- lmres(formula = 'recov_Parietal_Avg_AAC_R ~ cort.recov * LSAS', 
             data = SET_CFC,
             centered = c('cort.recov', 'LSAS'))
# Save results
sink("mod_Parietal_AAC_recov_cortisol.csv")
print(summary(mod))
sink()

# Simple slope analysis
S_slopes <- simpleSlope(mod, pred = "cort.recov", 
                        mod1 = "LSAS")
# Save results
sink("slopes_Parietal_AAC_recov_cortisol.csv")
print(summary(S_slopes))
sink()
# Plot
plot_slopes <- PlotSlopes(S_slopes,
                         #namemod=c("Low trait social anxiety (-1SD)", "High trait social anxiety (+1SD)"),
                         namex = "Cortisol recovery (ng/ml)",
                         namey = "Parietal AAC recovery (corr.)")
# Save Figure
tiff("mod_Parietal_AAC_recov_cortisol.tiff", width = 30, height = 20, units = "cm", res = 300) # Save TIFF file
plot(plot_slopes)
dev.off()


# Delete unnecessary variables
remove(mod)
remove(plot_slopes)
remove(S_slopes)
remove(PlotSlopes)
remove(data)
remove(data_LSA)
remove(data_HSA)
remove(HSA)
remove(LSA)
remove(corr)
