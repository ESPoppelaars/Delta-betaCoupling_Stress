# Script by E.S. Poppelaars.
# All packages up to date as of 30-09-2019.

# Setup ----------------------------------------------------------------

## Packages
library(tidyverse)
library(magrittr)
library(xlsx)
library(haven)

## Load earlier data (behavioral, physiology)
# Load data from SPSS
SETData <- read_sav("SETData.sav")

# Save SET as dataframe
SET <- SETData %>% as.data.frame()

# Set Sex as a factor
SET <- within(SET, {
  Sex <- factor(Sex, levels = 1:2, 
                labels = c("Male", "Female"))
})
class(SET$Sex) # Check class

# Set ExperimenterSex as a factor
SET <- within(SET, {
  ExperimenterSex <- factor(ExperimenterSex, levels = 1:6, 
                            labels = c("Single female", "Single male", "Two male", 
                                       "Two female", "One male & one female", "Two male & one female"))
})
class(SET$ExperimenterSex) # Check class

# Set AATVersion as a factor
SET <- within(SET, {
  AATVersion <- factor(AATVersion, levels = 1:2, 
                       labels = c("Approach yellow & avoid grey",
                                  "Avoid yellow & approach grey"))
})
class(SET$AATVersion) # Check class

# Set OvulationTests as a factor
SET <- within(SET, {
  OvulationTests <- factor(OvulationTests, levels = 0:1, 
                           labels = c("Not used",
                                      "Used"))
})
class(SET$OvulationTests) # Check class

# Composite variable ResourcesDemands
cor.test(SET$Cognitive_Resources, SET$Cognitive_Demands) # Calculate correlation
SET$ResourcesDemands <- SET$Cognitive_Resources - SET$Cognitive_Demands
attributes(SET$ResourcesDemands)$label <- "Resources and demands"

## Log transform cortisol
# Select cortisol variables
data <- SET %>% select("Cortisol.1", "Cortisol.2", "Cortisol.3", 
                       "Cortisol.4", "Cortisol.5", "Cortisol.6",
                       "Cortisol.7")
## Log transform each column
datalog <- apply(data, 2, log) %>% as.data.frame()
# Rename column names to append '.log'
datalog <- datalog %>% rename_all(function(x) paste0(x, ".log"))
## Add to dataframe
SET <- cbind(SET, datalog)
## Remove variables
remove(data, datalog)

# Save file
save(SET, file = "SET.RData")

## Load EEG data
CFC <- read.xlsx("SET_CFC_MatlabOutput.xlsx", 1)
# Select only variables to be analyzed
CFC_brief <- CFC %>% select(Subject, contains("Avg_dPAC_Z"), contains("Avg_AAC_R"))

# Merge dataframes
SET_CFC <- merge.data.frame(SET, CFC_brief, by.x = "Subject", by.y = "Subject", all.x = TRUE)

# Save new dataset
save(SET_CFC, file = "SET_CFC.RData")

# Remove old variables
remove(SET)
remove(CFC)
remove(CFC_brief)
remove(SETData)


# Calculate reactivity (preregistered) ----------------------------------------------------

## Packages
library(car) # for densityPlot

## Frontal dPAC
# Get reactivity increase (peak - baseline)
SET_CFC$react_Frontal_Avg_dPAC_Z <- SET_CFC$Anticip_Frontal_Avg_dPAC_Z - SET_CFC$RS_Frontal_Avg_dPAC_Z
# Density plot
densityPlot(SET_CFC$react_Frontal_Avg_dPAC_Z, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$react_Frontal_Avg_dPAC_Z)

## Parietal dPAC
# Get reactivity increase (peak - baseline)
SET_CFC$react_Parietal_Avg_dPAC_Z <- SET_CFC$Anticip_Parietal_Avg_dPAC_Z - SET_CFC$RS_Parietal_Avg_dPAC_Z
# Density plot
densityPlot(SET_CFC$react_Parietal_Avg_dPAC_Z, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$react_Parietal_Avg_dPAC_Z)

## Frontal AAC
# Get reactivity increase (peak - baseline)
SET_CFC$react_Frontal_Avg_AAC_R <- SET_CFC$Anticip_Frontal_Avg_AAC_R - SET_CFC$RS_Frontal_Avg_AAC_R
# Density plot
densityPlot(SET_CFC$react_Frontal_Avg_AAC_R, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$react_Frontal_Avg_AAC_R)

## Parietal AAC
# Get reactivity increase (peak - baseline)
SET_CFC$react_Parietal_Avg_AAC_R <- SET_CFC$Anticip_Parietal_Avg_AAC_R - SET_CFC$RS_Parietal_Avg_AAC_R
# Density plot
densityPlot(SET_CFC$react_Parietal_Avg_AAC_R, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$react_Parietal_Avg_AAC_R)

# Save new variables in the dataset
save(SET_CFC, file = "SET_CFC.RData")

# Check whether there are no missing values
any(is.na(SET_CFC))

# Calculation of recovery (exploratory) ---------------------------------------------------

# Packages
library(magrittr) # for Piping
library(car) # for densityPlot

# Load data
load("SET_CFC.RData")

## Frontal dPAC
# Get recovery decrease (recov - peak)
SET_CFC$recov_Frontal_Avg_dPAC_Z <- SET_CFC$EarlyRecov_Frontal_Avg_dPAC_Z - SET_CFC$Anticip_Frontal_Avg_dPAC_Z
# Density plot
densityPlot(SET_CFC$recov_Frontal_Avg_dPAC_Z, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$recov_Frontal_Avg_dPAC_Z)

## Parietal dPAC
# Get recovery decrease (recov - peak)
SET_CFC$recov_Parietal_Avg_dPAC_Z <- SET_CFC$EarlyRecov_Parietal_Avg_dPAC_Z - SET_CFC$Anticip_Parietal_Avg_dPAC_Z
# Density plot
densityPlot(SET_CFC$recov_Parietal_Avg_dPAC_Z, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$recov_Parietal_Avg_dPAC_Z)

## Frontal AAC
# Get recovery decrease (recov - peak)
SET_CFC$recov_Frontal_Avg_AAC_R <- SET_CFC$EarlyRecov_Frontal_Avg_AAC_R - SET_CFC$Anticip_Frontal_Avg_AAC_R
# Density plot
densityPlot(SET_CFC$recov_Frontal_Avg_AAC_R, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$recov_Frontal_Avg_AAC_R)

## Parietal AAC
# Get recovery decrease (recov - peak)
SET_CFC$recov_Parietal_Avg_AAC_R <- SET_CFC$EarlyRecov_Parietal_Avg_AAC_R - SET_CFC$Anticip_Parietal_Avg_AAC_R
# Density plot
densityPlot(SET_CFC$recov_Parietal_Avg_AAC_R, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$recov_Frontal_Avg_AAC_R)


# Reactivity/recovery other variables (preregistered/exploratory) ---------------------------------------------------

# Packages
library(magrittr) # for Piping
library(car) # for densityPlot
library(dplyr) # for data manipulation

## Anxiety
# Get reactivity increase (peak - RS)
SET_CFC$anx.react <- SET_CFC$Anx.2 - SET_CFC$Anx.1
# Density plot
densityPlot(SET_CFC$anx.react, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$anx.react)
# Get recovery decrease (recov - peak)
SET_CFC$anx.recov <- SET_CFC$Anx.4 - SET_CFC$Anx.2
# Density plot
densityPlot(SET_CFC$anx.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$anx.recov)

## Approach motivation
# Get reactivity increase (peak - RS)
SET_CFC$appr.react <- SET_CFC$Appr.2 - SET_CFC$Appr.1
# Density plot
densityPlot(SET_CFC$appr.react, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$appr.react)
# Get recovery decrease (recov - peak)
SET_CFC$appr.recov <- SET_CFC$Appr.4 - SET_CFC$Appr.2
# Density plot
densityPlot(SET_CFC$appr.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$appr.recov)

## BP
# Get reactivity increase (peak - RS)
SET_CFC$bp.react <- SET_CFC$BP.4 - SET_CFC$BP.2
# Density plot
densityPlot(SET_CFC$bp.react, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$bp.react)
# Get recovery decrease (recov - peak)
SET_CFC$bp.recov <- SET_CFC$BP.6 - SET_CFC$BP.4
# Density plot
densityPlot(SET_CFC$bp.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$bp.recov)

## Heart rate
# Get reactivity increase (peak - RS)
SET_CFC$hr.react <- SET_CFC$HR.4 - SET_CFC$HR.2
# Density plot
densityPlot(SET_CFC$hr.react, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$hr.react)
# Get recovery decrease (recov - peak)
SET_CFC$hr.recov <- SET_CFC$HR.8 - SET_CFC$HR.4
# Density plot
densityPlot(SET_CFC$hr.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$hr.recov)

## PEP
# Get reactivity increase (peak - RS)
SET_CFC$pep.react <- SET_CFC$PEP.4 - SET_CFC$PEP.2
# Density plot
densityPlot(SET_CFC$pep.react, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$pep.react)
# Get recovery decrease (recov - peak)
SET_CFC$pep.recov <- SET_CFC$PEP.8 - SET_CFC$PEP.4
# Density plot
densityPlot(SET_CFC$pep.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$pep.recov)

## Cardiac output
# Get reactivity increase (peak - RS)
SET_CFC$co.react <- SET_CFC$CO.4 - SET_CFC$CO.2
# Density plot
densityPlot(SET_CFC$co.react, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$co.react)
# Get recovery decrease (recov - peak)
SET_CFC$co.recov <- SET_CFC$CO.8 - SET_CFC$CO.4
# Density plot
densityPlot(SET_CFC$co.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$co.recov)

## TPR
# Get reactivity increase (peak - RS)
SET_CFC$tpr.react <- SET_CFC$TPR.4 - SET_CFC$TPR.2
# Density plot
densityPlot(SET_CFC$tpr.react, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$tpr.react)
# Get recovery decrease (recov - peak)
SET_CFC$tpr.recov <- SET_CFC$TPR.6 - SET_CFC$TPR.4
# Density plot
densityPlot(SET_CFC$tpr.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$tpr.recov)

## TCI
# Get reactivity increase (peak - RS)
SET_CFC$tci.react <- SET_CFC$TCI.4 - SET_CFC$TCI.2
# Density plot
densityPlot(SET_CFC$tci.react, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$tci.react)
# Get recovery decrease (recov - peak)
SET_CFC$tci.recov <- SET_CFC$TCI.6 - SET_CFC$TCI.4
# Density plot
densityPlot(SET_CFC$tci.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$tci.recov)

## RSA
# Get reactivity increase (peak - RS)
SET_CFC$rsa.react <- SET_CFC$RSA.4 - SET_CFC$RSA.2
# Density plot
densityPlot(SET_CFC$rsa.react, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$rsa.react)
# Get recovery decrease (recov - peak)
SET_CFC$rsa.recov <- SET_CFC$RSA.8 - SET_CFC$RSA.4
# Density plot
densityPlot(SET_CFC$rsa.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$rsa.recov)

## RR
# Get reactivity increase (peak - RS)
SET_CFC$rr.react <- SET_CFC$RR.4 - SET_CFC$RR.2
# Density plot
densityPlot(SET_CFC$rr.react, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$rr.react)
# Get recovery decrease (recov - peak)
SET_CFC$rr.recov <- SET_CFC$RR.8 - SET_CFC$RR.4
# Density plot
densityPlot(SET_CFC$rr.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$rr.recov)

## Cortisol
# Get reactivity increase (peak - RS)
SET_CFC$cort.react <- SET_CFC$Cortisol.3.log - SET_CFC$Cortisol.1.log
# Density plot
densityPlot(SET_CFC$cort.react, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$cort.react)
# Get recovery decrease (recov - peak)
SET_CFC$cort.recov <- SET_CFC$Cortisol.7.log - SET_CFC$Cortisol.3.log
# Density plot
densityPlot(SET_CFC$cort.recov, SET_CFC$Sex)
# Summary (no missing values?)
summary(SET_CFC$cort.recov)


## Make sure all the recovery variables are of class numeric.
sapply(SET_CFC, class) # Check class.
SET_CFC <- SET_CFC %>% mutate_if(funs(class(.) == "AsIs"), as.numeric)
sapply(SET_CFC, class) # Check class again.

# Save dataSET_CFC
save(SET_CFC, file = "SET_CFC.RData")


# Area under the curve ----------------------------------------------------

## Packages
library(dplyr)
library(magrittr)

### Select frontal PAC data
data <- SET_CFC %>% select(RS_Frontal_Avg_dPAC_Z,
                           Anticip_Frontal_Avg_dPAC_Z,
                           EarlyRecov_Frontal_Avg_dPAC_Z,
                           LateRecov_Frontal_Avg_dPAC_Z)
## Calculate AUC
# Initialize variables
obs <- nrow(data) # Number of observations
t <- as.vector(c(0, 30, 49, 79)) # Time points of each measurement
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
                           Anticip_Parietal_Avg_dPAC_Z,
                           EarlyRecov_Parietal_Avg_dPAC_Z,
                           LateRecov_Parietal_Avg_dPAC_Z)
## Calculate AUC
# Initialize variables
obs <- nrow(data) # Number of observations
t <- as.vector(c(0, 30, 49, 79)) # Time points of each measurement
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
                           Anticip_Frontal_Avg_AAC_R,
                           EarlyRecov_Frontal_Avg_AAC_R,
                           LateRecov_Frontal_Avg_AAC_R)
## Calculate AUC
# Initialize variables
obs <- nrow(data) # Number of observations
t <- as.vector(c(0, 30, 49, 79)) # Time points of each measurement
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
                           Anticip_Parietal_Avg_AAC_R,
                           EarlyRecov_Parietal_Avg_AAC_R,
                           LateRecov_Parietal_Avg_AAC_R)
## Calculate AUC
# Initialize variables
obs <- nrow(data) # Number of observations
t <- as.vector(c(0, 30, 49, 79)) # Time points of each measurement
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

# Make sure all the recovery variables are of class numeric.
sapply(SET_CFC, class) # Check class.
library(dplyr)
SET_CFC <- SET_CFC %>% mutate_if(funs(class(.) == "matrix"), as.numeric)
sapply(SET_CFC, class) # Check class again.


### Cortisol
## AUC
# Initialize data
t = as.vector(c(0, 38, 49, 54, 59, 64, 69)) # For hormones

# For cortisol
data <- SET_CFC %$% cbind.data.frame(Cortisol.1.log, Cortisol.2.log, Cortisol.3.log, 
                                 Cortisol.4.log, Cortisol.5.log, Cortisol.6.log, 
                                 Cortisol.7.log)

# Initialize variables
auc = matrix(NA, nrow = nrow(data), ncol = length(t)-1)
# First part of formula for calculating AUCg
for (j in 1:nrow(data)) {
  for (i in 1:I(length(t)-1)) {
    auc[j, i] <- I(data[j, i+1] + data[j, i]) * I(t[i+1]-t[i]) / 2
  }
}
# Second part of formula for calculating AUCg
AUCg <- rowSums(auc)

# Subtract the ground of AUCg to get AUC1
AUC1 <- matrix(NA, nrow = nrow(data), ncol = 1)
for (i in 1:nrow(data)) {
  AUC1[i] <- AUCg[i] - I(data[i, 1] * t[length(t)])
}

# Save into new dataset
SET_CFC$cort.auc <- AUC1 %>% as.numeric()


### Save dataframe
save(SET_CFC, file = "SET_CFC.RData")

### Remove temporary variables
remove(auc, AUC1, AUCg, data, i, j, t, n, obs)


# Grubbs outlier calculation (preregistered) ----------------------------------------------

# Packages
library(outliers) # Grubbs test
library(magrittr) # Piping
library(xlsx) # Exporting to Excel

# Load data
load("SET_CFC.RData")
data <- SET_CFC %>% select("Subject", 
                           "RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                           "recov_Frontal_Avg_dPAC_Z",
                           "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                           "recov_Frontal_Avg_AAC_R",
                           "LSAS", "anx.react", "anx.recov",
                           "Cortisol.1.log", "cort.react", "cort.recov",
                           "RSA.2", "rsa.react", "rsa.recov")

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

## Put the t-test results in a table
# Extract results from list
t.table1 <- sapply(grub, function(x) {
  c(test = x$method,
    var = x$data.name,
    test.stat = x$statistic[["G"]],
    subject.number = x$outlier.subject.number,
    p.value = x$p.value)
})

t.table2 <- sapply(grub2, function(x) {
  c(test = x$method,
    var = x$data.name,
    test.stat = x$statistic[["G"]],
    subject.number = x$outlier.subject.number,
    p.value = x$p.value)
})

if (nrow(t.table1) > 0 | nrow(t.table2) > 0) {
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
  write.xlsx(t.table.sig, "outliers_SET_CFC.xlsx")
  
  # Count the number of unique subject numbers
  num.outl <- t.table.sig$subject.number %>% unique() %>% length()
  
  # Delete the outliers
  SET_CFC.outl.del <- SET_CFC
  for (i in 1:nrow(t.table.sig)) {
    SET_CFC.outl.del[t.table.sig[i, "subject.number"], t.table.sig[i, "var"] ] <- NA
  }
  
} else {
  print("No outliers")
}

### Save dataframe
save(SET_CFC.outl.del, file = "SET_CFC.outl.del.RData")

## Remove temporary variables
remove(grub)
remove(grub2)
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
source('p.value.sig.R') # Custom function to check the significance of p.values

# Load data
load("SET_CFC.outl.del.RData")
data <- SET_CFC.outl.del %>% select("Subject", 
                           "RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                           "recov_Frontal_Avg_dPAC_Z",
                           "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                           "recov_Frontal_Avg_AAC_R",
                           "LSAS", "anx.react", "anx.recov",
                           "Cortisol.1.log", "cort.react", "cort.recov",
                           "RSA.2", "rsa.react", "rsa.recov")

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
write.xlsx(normTest_df, "normTest.SET_CFC.outl.del_brief.xlsx")

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
SET_CFC.outl.del$LSAS_Split <- factor(
  SET_CFC.outl.del$LSAS <= median(SET_CFC.outl.del$LSAS), 
  levels = c(TRUE, FALSE), 
  labels = c("Low", "High"))
# Check class
class(SET_CFC.outl.del$LSAS_Split)
# Check length of each group
length(SET_CFC.outl.del$LSAS_Split[SET_CFC.outl.del$LSAS_Split == "Low"])
length(SET_CFC.outl.del$LSAS_Split[SET_CFC.outl.del$LSAS_Split == "High"])

# Save new dataset
save(SET_CFC.outl.del, file = "SET_CFC.outl.del.RData")


# Multiple imputation of missing data -----------------------------------------------------

# Packages
library(mice) # Data imputation
library(dplyr) # Data manipulation
library(magrittr) # Piping
library(xlsx) # Exporting to Excel
library(beepr) # Play a sound after execution
library(ggplot2) # Display density plots

# Load data
load("SET_CFC.outl.del.RData")

## Make sure all catagorical variables are of class 'factor', 
# and all continuous variables are of class 'numeric'.
sapply(SET_CFC.outl.del, class) # Check class.
SET_CFC.outl.del <- SET_CFC.outl.del %>% mutate_if(funs(class(.) == "AsIs"), as.numeric)

## Predictor matrix
# Create a default predictor matrix.
pred <- make.predictorMatrix(SET_CFC.outl.del) # (rows are predicted by the columns.)
# Export the default predictor matrix to Excel, to adjust manually.
write.xlsx(pred, "predictorMatrixDefault.xlsx")
## Manual adjustment of predictor matrix in Excel!
# Import adjusted matrix.
pred.adj <- read.xlsx("predicorMatrixAdj.xlsx", sheetName = "Sheet1")
# Adjust the imported pred back to the correct matrix format for mice.
rownames(pred.adj) <- pred.adj$NA.
pred.adj <- pred.adj[, -1]
pred.adj <- as.matrix(pred.adj)
# Make extra sure that the diagonal is zero.
diag(pred.adj) <- 0
# Save the matrix.
save(pred.adj, file = "pred.adj.RData")
#load("pred.adj.RData")

## Passive imputation.
meth <- make.method(SET_CFC.outl.del) # Complete variables will automatically be empty.

# Do not predict missing sex / sex hormone value (MNAR).
meth["Progesterone"]<- ""
meth["AveragelengthMCs"]<- ""
meth["OvulationTests"]<- ""

# Do not predict early/late anticipation EEG variables.
meth["EarlyAnticip_Frontal_Avg_dPAC_Z"]<- ""
meth["EarlyAnticip_Parietal_Avg_dPAC_Z"]<- ""
meth["EarlyAnticip_Frontal_Avg_AAC_R"]<- ""
meth["EarlyAnticip_Parietal_Avg_AAC_R"]<- ""
meth["LateAnticip_Frontal_Avg_dPAC_Z"]<- ""
meth["LateAnticip_Parietal_Avg_dPAC_Z"]<- ""
meth["LateAnticip_Frontal_Avg_AAC_R"]<- ""
meth["LateAnticip_Parietal_Avg_AAC_R"]<- ""

# No passive imputation for complex variables
meth["cort.auc"]<- ""
meth["parietal_AAC.auc"]<- ""
meth["parietal_PAC.auc"]<- ""
meth["frontal_AAC.auc"]<- ""
meth["frontal_PAC.auc"]<- ""

# Passive imputation for BMI
meth["BMI"]<- "~ I(Weight / I(Height^2) )"

# Passive imputation for TCI
meth["TCI.1"]<- "~ I(scale(CO.1) - scale(TPR.1) )"
meth["TCI.2"]<- "~ I(scale(CO.2) - scale(TPR.2) )"
meth["TCI.3"]<- "~ I(scale(CO.3) - scale(TPR.3) )"
meth["TCI.4"]<- "~ I(scale(CO.4) - scale(TPR.4) )"
meth["TCI.5"]<- "~ I(scale(CO.7) - scale(TPR.5) )"
meth["TCI.6"]<- "~ I(scale(CO.8) - scale(TPR.6) )"
meth["TCI.7"]<- "~ I(scale(CO.9) - scale(TPR.7) )"
meth["TCI.8"]<- "~ I(scale(CO.10) - scale(TPR.8) )"

# Passive imputation for log-transformed variables
meth["Cortisol.1.log"]<- "~ I( I(log(Cortisol.1)) )"
meth["Cortisol.2.log"]<- "~ I( I(log(Cortisol.2)) )"
meth["Cortisol.3.log"]<- "~ I( I(log(Cortisol.3)) )"
meth["Cortisol.4.log"]<- "~ I( I(log(Cortisol.4)) )"
meth["Cortisol.5.log"]<- "~ I( I(log(Cortisol.5)) )"
meth["Cortisol.6.log"]<- "~ I( I(log(Cortisol.6)) )"
meth["Cortisol.7.log"]<- "~ I( I(log(Cortisol.7)) )"

# Passive imputation for ResourcesDemands
meth["ResourcesDemands"]<- "~ I(Cognitive_Resources - Cognitive_Demands )"

# Passive imputation for reactivity variables.
meth["react_Frontal_Avg_dPAC_Z"]<- "~ I( Anticip_Frontal_Avg_dPAC_Z - RS_Frontal_Avg_dPAC_Z )"
meth["react_Parietal_Avg_dPAC_Z"]<- "~ I( Anticip_Parietal_Avg_dPAC_Z - RS_Parietal_Avg_dPAC_Z )"
meth["react_Frontal_Avg_AAC_R"]<- "~ I( Anticip_Frontal_Avg_AAC_R - RS_Frontal_Avg_AAC_R )"
meth["react_Parietal_Avg_AAC_R"]<- "~ I( Anticip_Parietal_Avg_AAC_R - RS_Parietal_Avg_AAC_R )"
meth["recov_Frontal_Avg_dPAC_Z"]<- "~ I( EarlyRecov_Frontal_Avg_dPAC_Z - Anticip_Frontal_Avg_dPAC_Z )"
meth["recov_Parietal_Avg_dPAC_Z"]<- "~ I( EarlyRecov_Parietal_Avg_dPAC_Z - Anticip_Parietal_Avg_dPAC_Z )"
meth["recov_Frontal_Avg_AAC_R"]<- "~ I( EarlyRecov_Frontal_Avg_AAC_R - Anticip_Frontal_Avg_AAC_R )"
meth["recov_Parietal_Avg_AAC_R"]<- "~ I( EarlyRecov_Parietal_Avg_AAC_R - Anticip_Parietal_Avg_AAC_R )"
meth["anx.react"]<- "~ I( Anx.2 - Anx.1 )"
meth["anx.recov"]<- "~ I( Anx.4 - Anx.2 )"
meth["appr.react"]<- "~ I( Appr.2 - Appr.1 )"
meth["appr.recov"]<- "~ I( Appr.4 - Appr.2 )"
meth["hr.react"]<- "~ I( HR.4 - HR.2 )"
meth["hr.recov"]<- "~ I( HR.8 - HR.4 )"
meth["pep.react"]<- "~ I( PEP.4 - PEP.2 )"
meth["pep.recov"]<- "~ I( PEP.8 - PEP.4 )"
meth["co.react"]<- "~ I( CO.4 - CO.2 )"
meth["co.recov"]<- "~ I( CO.8 - CO.4 )"
meth["tpr.react"]<- "~ I( TPR.4 - TPR.2 )"
meth["tpr.recov"]<- "~ I( TPR.6 - TPR.4 )"
meth["tci.react"]<- "~ I( TCI.4 - TCI.2 )"
meth["tci.recov"]<- "~ I( TCI.6 - TCI.4 )"
meth["rsa.react"]<- "~ I( RSA.4 - RSA.2 )"
meth["rsa.recov"]<- "~ I( RSA.8 - RSA.4 )"
meth["rr.react"]<- "~ I( RR.4 - RR.2 )"
meth["rr.recov"]<- "~ I( RR.8 - RR.4 )"
meth["cort.react"]<- "~ I( Cortisol.3.log - Cortisol.1.log )"
meth["cort.recov"]<- "~ I( Cortisol.7.log - Cortisol.3.log )"

# Check the method vector.
View(meth)
# Save the vector.
save(meth, file = "meth.RData")
#load("meth.RData")

# Missing data percentage
mis <- 100-(sum(complete.cases(SET_CFC.outl.del))*100/nrow(SET_CFC.outl.del)) %>% round(0)

## Imputations in mice.
SET_CFC.outl.del.imp <- mice(SET_CFC.outl.del, # Dataset.
                    method = meth, # Custom passive imputation formula matrix.
                    predictorMatrix = pred.adj, # Custom predictor matrix.
                    #m = 2, # For testing: Low number of datasets to impute.
                    m = mis, # Number of datasets to impute relate to percentage of missing data.
                    maxit = 150, # Number of passes through the data. (Normally, 50 is enough to reach convergence.)
                    seed = 123, # Random seed for reproducibility.
                    print = FALSE); # Don't print the endless list of computations.
beep("ping") # Play sound when computations are finished.

# Save the imp object to avoid having to re-run it.
save(SET_CFC.outl.del.imp, file = "SET_CFC.outl.del.imp.RData")

# For loading the imp object later.
#load("SET_CFC.outl.del.imp.RData")

# track warnings.
warnings()

## Check multicolinearity.
SET_CFC.outl.del.imp$loggedEvents # If NULL, there are no multicolinearity issues.

## Plots
SET.confplots <- plot(SET_CFC.outl.del.imp, layout = c(2, 2)) # Plot the iterations and check convergence.
SET.confplots
# The passive imputations plots will be straight lines, all other variables need to have overlapping squiggly lines that are close together on the right side.
# -> No variance for variables only missing a single observation (HR.1, HR.2, RSA.1, RSA.2).
# Boxplots
SET.bwplots <- bwplot(SET_CFC.outl.del.imp, cex = 0.5, layout = c(1, 3)) # See boxplots of the imputated data for each set and variable to compare with original data.
SET.bwplots
# Stripplots
SET.stripplots <- stripplot(SET_CFC.outl.del.imp, cex = 0.5, layout = c(1, 3))# See stripplots of the imputated data for each set and variable to compare with original data.
SET.stripplots

# Check out the imputed data
head(SET_CFC.outl.del.imp$imp$react_Frontal_Avg_dPAC_Z)

## Check if the imputed data distribution follows the existing distributions.
# State anxiety reactivity
ggplot(SET_CFC.outl.del,aes(x=anx.react)) + 
  geom_density(data=data.frame(SET_CFC.outl.del$Subject, mice::complete(SET_CFC.outl.del.imp,6)), alpha = 0.2, fill = "blue", size = 1) +
  geom_density(data=SET_CFC.outl.del, alpha = 0.2, fill = "Red") +
  labs(title="State anxiety reactivity distribution by complete (red) and imputed (blue / thick line) data") +
  labs(x="State anxiety")
# RSA reactivity
ggplot(SET_CFC.outl.del,aes(x=rsa.react)) + 
  geom_density(data=data.frame(SET_CFC.outl.del$Subject, mice::complete(SET_CFC.outl.del.imp,6)), alpha = 0.2, fill = "blue", size = 1) +
  geom_density(data=SET_CFC.outl.del, alpha = 0.2, fill = "Red") +
  labs(title="RSA reactivity distribution by complete (red) and imputed (blue / thick line) data") +
  labs(x="RSA reactivity")
# Cortisol reactivity
ggplot(SET_CFC.outl.del,aes(x=cort.react)) + 
  geom_density(data=data.frame(SET_CFC.outl.del$Subject, mice::complete(SET_CFC.outl.del.imp,6)), alpha = 0.2, fill = "blue", size = 1) +
  geom_density(data=SET_CFC.outl.del, alpha = 0.2, fill = "Red") +
  labs(title="Cortisol reactivity distribution by complete (red) and imputed (blue / thick line) data") +
  labs(x="Cortisol reactivity")
# Frontal PAC reactivity
ggplot(SET_CFC.outl.del,aes(x=react_Frontal_Avg_dPAC_Z)) + 
  geom_density(data=data.frame(SET_CFC.outl.del$Subject, mice::complete(SET_CFC.outl.del.imp,6)), alpha = 0.2, fill = "blue", size = 1) +
  geom_density(data=SET_CFC.outl.del, alpha = 0.2, fill = "Red") +
  labs(title="Frontal PAC reactivity distribution by complete (red) and imputed (blue / thick line) data") +
  labs(x="Frontal PAC reactivity")
labs(x="State anxiety")
# Parietal PAC reactivity
ggplot(SET_CFC.outl.del,aes(x=react_Parietal_Avg_dPAC_Z)) + 
  geom_density(data=data.frame(SET_CFC.outl.del$Subject, mice::complete(SET_CFC.outl.del.imp,6)), alpha = 0.2, fill = "blue", size = 1) +
  geom_density(data=SET_CFC.outl.del, alpha = 0.2, fill = "Red") +
  labs(title="Parietal PAC reactivity distribution by complete (red) and imputed (blue / thick line) data") +
  labs(x="Parietal PAC reactivity")
# Frontal AAC reactivity
ggplot(SET_CFC.outl.del,aes(x=react_Frontal_Avg_AAC_R)) + 
  geom_density(data=data.frame(SET_CFC.outl.del$Subject, mice::complete(SET_CFC.outl.del.imp,6)), alpha = 0.2, fill = "blue", size = 1) +
  geom_density(data=SET_CFC.outl.del, alpha = 0.2, fill = "Red") +
  labs(title="Frontal AAC reactivity distribution by complete (red) and imputed (blue / thick line) data") +
  labs(x="Frontal AAC reactivity")
# Parietal AAC reactivity
ggplot(SET_CFC.outl.del,aes(x=react_Parietal_Avg_AAC_R)) + 
  geom_density(data=data.frame(SET_CFC.outl.del$Subject, mice::complete(SET_CFC.outl.del.imp,6)), alpha = 0.2, fill = "blue", size = 1) +
  geom_density(data=SET_CFC.outl.del, alpha = 0.2, fill = "Red") +
  labs(title="Parietal AAC reactivity distribution by complete (red) and imputed (blue / thick line) data") +
  labs(x="Parietal AAC reactivity")
# Frontal PAC recovery
ggplot(SET_CFC.outl.del,aes(x=recov_Frontal_Avg_dPAC_Z)) + 
  geom_density(data=data.frame(SET_CFC.outl.del$Subject, mice::complete(SET_CFC.outl.del.imp,6)), alpha = 0.2, fill = "blue", size = 1) +
  geom_density(data=SET_CFC.outl.del, alpha = 0.2, fill = "Red") +
  labs(title="Frontal PAC recovery distribution by complete (red) and imputed (blue / thick line) data") +
  labs(x="Frontal PAC recovery")
# Parietal PAC recovery
ggplot(SET_CFC.outl.del,aes(x=recov_Parietal_Avg_dPAC_Z)) + 
  geom_density(data=data.frame(SET_CFC.outl.del$Subject, mice::complete(SET_CFC.outl.del.imp,6)), alpha = 0.2, fill = "blue", size = 1) +
  geom_density(data=SET_CFC.outl.del, alpha = 0.2, fill = "Red") +
  labs(title="Parietal PAC recovery distribution by complete (red) and imputed (blue / thick line) data") +
  labs(x="Parietal PAC recovery")
# Frontal AAC recovery
ggplot(SET_CFC.outl.del,aes(x=recov_Frontal_Avg_AAC_R)) + 
  geom_density(data=data.frame(SET_CFC.outl.del$Subject, mice::complete(SET_CFC.outl.del.imp,6)), alpha = 0.2, fill = "blue", size = 1) +
  geom_density(data=SET_CFC.outl.del, alpha = 0.2, fill = "Red") +
  labs(title="Frontal AAC recovery distribution by complete (red) and imputed (blue / thick line) data") +
  labs(x="Frontal AAC recovery")
# Parietal AAC recovery
ggplot(SET_CFC.outl.del,aes(x=recov_Parietal_Avg_AAC_R)) + 
  geom_density(data=data.frame(SET_CFC.outl.del$Subject, mice::complete(SET_CFC.outl.del.imp,6)), alpha = 0.2, fill = "blue", size = 1) +
  geom_density(data=SET_CFC.outl.del, alpha = 0.2, fill = "Red") +
  labs(title="Parietal AAC recovery distribution by complete (red) and imputed (blue / thick line) data") +
  labs(x="Parietal AAC recovery")


## Remove temporary variables
remove(meth)
remove(pred)
remove(pred.adj)
remove(mis)
remove(SET.stripplots)
remove(SET.bwplots)
remove(SET.confplots)


# Area under the curve - imputed ------------------------------------------
# Calculation of missing data for variables that could not easily be passively be imputed.

# Packages
library(mice) # Imputation
library(dplyr) # Data manipulation
library(magrittr) # Piping
library(xlsx) # Exporting to Excel

# Load data
load("SET_CFC.outl.del.imp.RData")

## Transform imputed data to a long-format data frame, without the original data
long.imp <- complete(SET_CFC.outl.del.imp, action='long', include=TRUE)

### Calculation of AUC for Frontal PAC
# Calculate AUC for every dataset
data <- long.imp %$% cbind.data.frame(.imp, 
                                      Subject,
                                      RS_Frontal_Avg_dPAC_Z,
                                      Anticip_Frontal_Avg_dPAC_Z,
                                      EarlyRecov_Frontal_Avg_dPAC_Z,
                                      LateRecov_Frontal_Avg_dPAC_Z)
colnames(data)[1] <- "imp"
## Calculate AUC for every imp dataset
# Initialize variables
imp <- SET_CFC.outl.del.imp$m # Number of imputations
obs <- nrow(subset(data, data$imp == 1)) # Number of observations
misrows <- data[!complete.cases(data), ] %>% rownames() %>% as.numeric() # Calculate rownames that contain missing datapoints
misobs <- length(misrows)
misSubs <- subset(data, data$imp == 0, select = Subject) # Get subject numbers
misSubs <- misSubs[misrows, ] # Which subject numbers have missing data
misData <- data %>% filter_at(vars(Subject), any_vars(. %in% misSubs)) # Extract all data for the subjects with missing observations
t <- as.vector(c(0, 30, 49, 79)) # Time points of each measurement
auc <- array(NA, dim = c(length(misSubs), length(t)-1, imp))
AUCg <- matrix(NA, nrow = length(misSubs), ncol = imp)
AUC1 <- matrix(NA, nrow = length(misSubs), ncol = imp)

# Loop for all imputed datasets
for (m in 1:imp) {
  # Subset current imputed dataset (without the imp and Subect column)
  subdata <- subset(misData, imp == m, select = -c(imp, Subject))
  
  # First part of formula for calculating AUCg
  for (n in 1:misobs) {
    for (i in 1:I(length(t)-1)) {
      auc[n, i, m] <- I(subdata[n, i+1] + subdata[n, i]) * I(t[i+1]-t[i]) / 2
    }
  }
  
  # Second part of formula for calculating AUCg
  AUCg[, m] <- rowSums(auc[, , m])
  
  # Subtract the ground of AUCg to get AUC1
  for (i in 1:misobs) {
    AUC1[i, m] <- AUCg[i, m] - I(subdata[i, 1] * t[length(t)])
  }
}

# SET rownames
rownames(AUC1) <- misrows
# SET columnnames
colnames(AUC1) <- 1:imp %>% as.character()

## Save new data
# Duplicate old imputed object
SET_CFC.outl.del.imp.extra <- SET_CFC.outl.del.imp
# Put new variables into imputed values of imputed datasets
SET_CFC.outl.del.imp.extra[["imp"]][["frontal_PAC.auc"]] <- AUC1


### Calculation of AUC for Parietal PAC
# Calculate AUC for every dataset
data <- long.imp %$% cbind.data.frame(.imp, 
                                      Subject,
                                      RS_Parietal_Avg_dPAC_Z,
                                      Anticip_Parietal_Avg_dPAC_Z,
                                      EarlyRecov_Parietal_Avg_dPAC_Z,
                                      LateRecov_Parietal_Avg_dPAC_Z)
colnames(data)[1] <- "imp"
## Calculate AUC for every imp dataset
# Initialize variables
imp <- SET_CFC.outl.del.imp$m # Number of imputations
obs <- nrow(subset(data, data$imp == 1)) # Number of observations
misrows <- data[!complete.cases(data), ] %>% rownames() %>% as.numeric() # Calculate rownames that contain missing datapoints
misobs <- length(misrows)
misSubs <- subset(data, data$imp == 0, select = Subject) # Get subject numbers
misSubs <- misSubs[misrows, ] # Which subject numbers have missing data
misData <- data %>% filter_at(vars(Subject), any_vars(. %in% misSubs)) # Extract all data for the subjects with missing observations
t <- as.vector(c(0, 30, 49, 79)) # Time points of each measurement
auc <- array(NA, dim = c(length(misSubs), length(t)-1, imp))
AUCg <- matrix(NA, nrow = length(misSubs), ncol = imp)
AUC1 <- matrix(NA, nrow = length(misSubs), ncol = imp)

# Loop for all imputed datasets
for (m in 1:imp) {
  # Subset current imputed dataset (without the imp and Subect column)
  subdata <- subset(misData, imp == m, select = -c(imp, Subject))
  
  # First part of formula for calculating AUCg
  for (n in 1:misobs) {
    for (i in 1:I(length(t)-1)) {
      auc[n, i, m] <- I(subdata[n, i+1] + subdata[n, i]) * I(t[i+1]-t[i]) / 2
    }
  }
  
  # Second part of formula for calculating AUCg
  AUCg[, m] <- rowSums(auc[, , m])
  
  # Subtract the ground of AUCg to get AUC1
  for (i in 1:misobs) {
    AUC1[i, m] <- AUCg[i, m] - I(subdata[i, 1] * t[length(t)])
  }
}

# SET rownames
rownames(AUC1) <- misrows
# SET columnnames
colnames(AUC1) <- 1:imp %>% as.character()

## Save new data
# Put new variables into imputed values of imputed datasets
SET_CFC.outl.del.imp.extra[["imp"]][["parietal_PAC.auc"]] <- AUC1


### Calculation of AUC for Frontal AAC
# Calculate AUC for every dataset
data <- long.imp %$% cbind.data.frame(.imp, 
                                      Subject,
                                      RS_Frontal_Avg_AAC_R,
                                      Anticip_Frontal_Avg_AAC_R,
                                      EarlyRecov_Frontal_Avg_AAC_R,
                                      LateRecov_Frontal_Avg_AAC_R)
colnames(data)[1] <- "imp"
## Calculate AUC for every imp dataset
# Initialize variables
imp <- SET_CFC.outl.del.imp$m # Number of imputations
obs <- nrow(subset(data, data$imp == 1)) # Number of observations
misrows <- data[!complete.cases(data), ] %>% rownames() %>% as.numeric() # Calculate rownames that contain missing datapoints
misobs <- length(misrows)
misSubs <- subset(data, data$imp == 0, select = Subject) # Get subject numbers
misSubs <- misSubs[misrows, ] # Which subject numbers have missing data
misData <- data %>% filter_at(vars(Subject), any_vars(. %in% misSubs)) # Extract all data for the subjects with missing observations
t <- as.vector(c(0, 30, 49, 79)) # Time points of each measurement
auc <- array(NA, dim = c(length(misSubs), length(t)-1, imp))
AUCg <- matrix(NA, nrow = length(misSubs), ncol = imp)
AUC1 <- matrix(NA, nrow = length(misSubs), ncol = imp)


# Loop for all imputed datasets
for (m in 1:imp) {
  # Subset current imputed dataset (without the imp and Subect column)
  subdata <- subset(misData, imp == m, select = -c(imp, Subject))
  
  # First part of formula for calculating AUCg
  for (n in 1:misobs) {
    for (i in 1:I(length(t)-1)) {
      auc[n, i, m] <- I(subdata[n, i+1] + subdata[n, i]) * I(t[i+1]-t[i]) / 2
    }
  }
  
  # Second part of formula for calculating AUCg
  AUCg[, m] <- rowSums(auc[, , m])
  
  # Subtract the ground of AUCg to get AUC1
  for (i in 1:misobs) {
    AUC1[i, m] <- AUCg[i, m] - I(subdata[i, 1] * t[length(t)])
  }
}

# SET rownames
rownames(AUC1) <- misrows
# SET columnnames
colnames(AUC1) <- 1:imp %>% as.character()

## Save new data
# Put new variables into imputed values of imputed datasets
SET_CFC.outl.del.imp.extra[["imp"]][["frontal_AAC.auc"]] <- AUC1


### Calculation of AUC for Parietal AAC
# Calculate AUC for every dataset
data <- long.imp %$% cbind.data.frame(.imp, 
                                      Subject,
                                      RS_Parietal_Avg_AAC_R,
                                      Anticip_Parietal_Avg_AAC_R,
                                      EarlyRecov_Parietal_Avg_AAC_R,
                                      LateRecov_Parietal_Avg_AAC_R)
colnames(data)[1] <- "imp"
## Calculate AUC for every imp dataset
# Initialize variables
imp <- SET_CFC.outl.del.imp$m # Number of imputations
obs <- nrow(subset(data, data$imp == 1)) # Number of observations
misrows <- data[!complete.cases(data), ] %>% rownames() %>% as.numeric() # Calculate rownames that contain missing datapoints
misobs <- length(misrows)
misSubs <- subset(data, data$imp == 0, select = Subject) # Get subject numbers
misSubs <- misSubs[misrows, ] # Which subject numbers have missing data
misData <- data %>% filter_at(vars(Subject), any_vars(. %in% misSubs)) # Extract all data for the subjects with missing observations

t <- as.vector(c(0, 30, 49, 79)) # Time points of each measurement
auc <- array(NA, dim = c(length(misSubs), length(t)-1, imp))
AUCg <- matrix(NA, nrow = length(misSubs), ncol = imp)
AUC1 <- matrix(NA, nrow = length(misSubs), ncol = imp)



# Loop for all imputed datasets
for (m in 1:imp) {
  # Subset current imputed dataset (without the imp and Subect column)
  subdata <- subset(misData, imp == m, select = -c(imp, Subject))
  
  # First part of formula for calculating AUCg
  for (n in 1:misobs) {
    for (i in 1:I(length(t)-1)) {
      auc[n, i, m] <- I(subdata[n, i+1] + subdata[n, i]) * I(t[i+1]-t[i]) / 2
    }
  }
  
  # Second part of formula for calculating AUCg
  AUCg[, m] <- rowSums(auc[, , m])
  
  # Subtract the ground of AUCg to get AUC1
  for (i in 1:misobs) {
    AUC1[i, m] <- AUCg[i, m] - I(subdata[i, 1] * t[length(t)])
  }
}

# SET rownames
rownames(AUC1) <- misrows
# SET columnnames
colnames(AUC1) <- 1:imp %>% as.character()

## Save new data
# Put new variables into imputed values of imputed datasets
SET_CFC.outl.del.imp.extra[["imp"]][["parietal_AAC.auc"]] <- AUC1



## Check new data
# Complete imputed data into new variable
long.imp.extra <- complete(SET_CFC.outl.del.imp.extra, action='long', include=FALSE)
long.imp.extra <- long.imp.extra %$% cbind.data.frame(frontal_PAC.auc, 
                                                      parietal_PAC.auc,
                                                      frontal_AAC.auc,
                                                      parietal_AAC.auc)
View(long.imp.extra)

# Save new data
save(SET_CFC.outl.del.imp.extra, file = "SET_CFC.outl.del.imp.extra.RData")

## Remove temporary variables
remove(misData)
remove(misobs)
remove(misrows)
remove(misSubs)
remove(long.imp)
remove(data)
remove(m)
remove(imp)
remove(obs)
remove(t)
remove(auc)
remove(AUCg)
remove(AUC1)
remove(subdata)
remove(n)
remove(i)
remove(long.imp.extra)