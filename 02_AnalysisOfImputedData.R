# Script by E.S. Poppelaars.
# All packages up to date as of 30-09-2019.


# Normality tests (preregistered) ---------------------------------------------------------
# Load packages
library(dplyr) # Data manipulation
library(magrittr) # Piping
library(xlsx) # Exporting to Excel
library(mice) # For multiple imputed data
source('p.value.sig.R') # Custom function to check the significance of p.values

# Load data
load("SET_CFC.outl.del.imp.RData")
# Select data
data <- mice::complete(SET_CFC.outl.del.imp, action = "long") # Create long dataset
data <- select(data, c(".imp", "RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z", # Select variables
                       "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                       "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                       "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                       "LSAS", "Anx.1", "anx.react",
                       "PEP.2", "pep.react",
                       "RSA.2", "rsa.react",
                       "RR.2", "rr.react",
                       "Cortisol.1.log", "cort.react"))
colnames(data)[1] <- "imp" # Remove the dot from the imp column for easier handling

# Initialize variables
m <- SET_CFC.outl.del.imp$m # Number of imputed datasets
normEst <- matrix(NA, nrow = I(ncol(data)-1), ncol = m) # Matrix to put the Shapiro-Wilk statistic per imputed dataset
normP <- matrix(NA, nrow = I(ncol(data)-1), ncol = m) # Matrix to put the p-values per imputed dataset
n.obs <- matrix(NA, nrow = I(ncol(data)-1), ncol = m) # Matrix to put the number of observations
varEst <- matrix(NA, nrow = I(ncol(data)-1), ncol = m) # Matrix to put the variance of the variables per imputed dataset
normAv <- matrix(NA, nrow = I(ncol(data)-1), ncol = 1) # Matrix to put the pooled statistic estimates
normPAv <- matrix(NA, nrow = I(ncol(data)-1), ncol = 1) # Matrix to put the pooled p-value estimates
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed datasets
  subdata <- data %>% filter(imp == j) %>% select(-imp) %>% as.matrix() # Select imputed dataset
  for (i in 1:ncol(subdata)) { # For all variables in the data
    SW <- shapiro.test(subdata[, i]) # Calculate normality
    normEst[i, j] <- SW[["statistic"]] # Extract Shapiro-Wilk statistic
    normP[i, j] <- SW[["p.value"]] # Extract p-value
    n.obs[i, j] <- length(subdata[, i]) # Calculate sample size
    varEst[i, j] <- var(subdata[, i]) / n.obs[i, j] # The standard error of the estimate (necessary for pooling)
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:I(ncol(data)-1)) { # For every variable
  normAv[i, 1] <- pool.scalar(normEst[i, ], varEst[i, ], n = n.obs[i,], k = 1)[["qbar"]] %>% unlist() %>% round(3) # For statistic
  normPAv[i, 1] <- pool.scalar(normP[i, ], varEst[i, ], n = n.obs[i,], k = 1)[["qbar"]] %>% unlist() %>% round(3) # For p-value
}

# Add estimates to dataframe
normTest <- colnames(data[, -1]) %>% as.data.frame(stringsAsFactors = FALSE) # Initialize dataframe with variable names
colnames(normTest) <- "variable" # Set column-name appropriately
normTest[, "Shapiro-Wilk.stat"] <- normAv %>% as.data.frame() # Add pooled statistic estimates
normTest[, "p.value"] <- normPAv %>% as.data.frame() # Add pooled p-value estimates

# Do FDR-correction on p-values
normTest[, "p.adj"] <- p.adjust(normTest[, "p.value"], method = "bonferroni", n = length(normTest[, "p.value"])) # Do fdr-correction
normTest$p.adj.sig <- sapply(normTest$p.adj, function(x) p.value.sig(x)) # Add column with corrected significance interpretation
normTest$p.value.sig <- sapply(normTest$p.value, function(x) p.value.sig(x)) # Add column with uncorrected significance interpretation

# Save results
write.xlsx(normTest, "normTest.SET_CFC.outl.del.imp.xlsx")

## Remove temporary variables
remove(normAv)
remove(varEst)
remove(normEst)
remove(normP)
remove(normPAv)
remove(n.obs)
remove(normTest)
remove(i)
remove(j)
remove(m)
remove(data)
remove(subdata)
remove(SW)
remove(p.value.sig)


# Exporting descriptives (preregistered) --------------------------------------------------

# Packages
library(mice) # Imputation
library(dplyr) # Data manipulation (select_if)
library(magrittr) # Piping
library(xlsx) # Exporting to Excel

# Load data
load("SET_CFC.outl.del.imp.RData")

### Descriptives for all vars
# Initialize variables
nfunc <- 4 # Number of functions
nvar <- SET_CFC.outl.del.imp$data %>% select_if(is.numeric) %>% ncol() # Number of numeric variables
m <- SET_CFC.outl.del.imp$m # Number of imputations
Q <- array(NA,dim=c(m,nvar,nfunc)) # Three-dimensional matrix to put all the estimates
U <- matrix(NA, nrow = m, ncol = nvar) # Matrix to put the variance of the variables per imputed dataset
est <- matrix(NA, nrow = nfunc, ncol = nvar) # Matrix to put the pooled estimates
# Initialize functions
n.obs <- function(x) {sum(complete.cases(na.omit(x)))} # Calculates the number of observations
se <- function(x) {sqrt(var(x)/length(x))} # Calculates the standard error of the mean
# Calculate descriptives for all vars and all imputed datasets
for (i in 1:m) { # For all imputed datasets
  Data <- mice::complete(SET_CFC.outl.del.imp, i) %>% select_if(is.numeric) # Select numeric variables of current dataset
  for (j in 1:nvar) { # For all variables in the data
    data <- Data[, j] %>% na.omit() # Select currect variable without NA's
    Q[i, j, 1] <- mean(data) # Mean
    Q[i, j, 2] <- sd(data) # Standard deviation
    Q[i, j, 3] <- se(data) # Standard error
    Q[i, j, 4] <- n.obs(data) # The number of observations
    U[i, j] <- var(data) / Q[i, j, 4] # The standard error of the estimate (necessary for pooling)
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (f in 1:nfunc) { # For every function
  for (j in 1:nvar) { # For every variable
    est[f, j] <- pool.scalar(Q[, j, f], U[, j], n = Q[, j, 4], k = 1)[["qbar"]] %>% unlist() %>% round(3)
  }
}
# Save summary
imp.summary <- est # Save the data
colnames(imp.summary) <- SET_CFC.outl.del.imp$data %>% select_if(is.numeric) %>% colnames() # Set the column names
rownames(imp.summary) <- c("Mean", "Standard deviation", "Standard error of the mean", "Number of observations") # Set the row names
imp.summary <- imp.summary %>% as.data.frame() # Transform into a dataframe
imp.summary <- imp.summary[, -c(which(colnames(SET_CFC.outl.del.imp$data)=="Subject"))] # Remove subject number variable
write.xlsx(imp.summary, "summary_SET_CFC.outl.del.imp.xlsx") # Save an Excel file
save(imp.summary, file = "imp.summary.RData")


### Descriptives for males and females separately
# Initialize variables
label <- c("Male", "Female") # Label of the group to analyze.
nfunc <- 4 # Number of functions
nvar <- SET_CFC.outl.del.imp$data %>% select_if(is.numeric) %>% ncol() # Number of numeric variables
m <- SET_CFC.outl.del.imp$m # Number of imputations
Q <- array(NA,dim=c(m,nvar,I(nfunc*length(label)))) # Three-dimensional matrix to put all the estimates
U <- array(NA,dim=c(m,nvar,length(label))) # Three-dimensional matrix to put the variance of the variables per imputed dataset and per label
est <- matrix(NA, nrow = I(nfunc*length(label)), ncol = nvar) # Matrix to put the pooled estimates
# Calculate descriptives for all vars and all imputed datasets for men and women separately
for (i in 1:m) { # For all imputed datasets
  Data1 <- subset(mice::complete(SET_CFC.outl.del.imp, i), Sex == label[1]) %>% select_if(is.numeric)  # Select numeric variables of current dataset for men
  Data2 <- subset(mice::complete(SET_CFC.outl.del.imp, i), Sex == label[2]) %>% select_if(is.numeric)  # Select numeric variables of current dataset for women
  for (j in 1:nvar) { # For all variables in the data
    data1 <- Data1[, j] %>% na.omit() # Select currect variable without NA's for men
    data2 <- Data2[, j] %>% na.omit # Select currect variable without NA's for women
    Q[i, j, 1] <- mean(data1) # Mean
    Q[i, j, 2] <- sd(data1) # Standard deviation
    Q[i, j, 3] <- se(data1) # Standard error
    Q[i, j, 4] <- n.obs(data1) # The number of observations
    U[i, j, 1] <- var(data1) / Q[i, j, 4] # The standard error of the estimate (necessary for pooling)
    Q[i, j, 5] <- mean(data2) # Mean
    Q[i, j, 6] <- sd(data2) # Standard deviation
    Q[i, j, 7] <- se(data2) # Standard error
    Q[i, j, 8] <- n.obs(data2) # The number of observations
    U[i, j, 2] <- var(data2) / Q[i, j, 8] # The standard error of the estimate (necessary for pooling)
  }
}
# Pool the descriptives into one estimate with three decimals for all vars for men and women separately
for (f in 1:I(nfunc*length(label))) { # For every function * the number of groups
  for (j in 1:nvar) { # For every variable
    if (f <= nfunc) {est[f, j] <- pool.scalar(Q[, j, f], U[, j, 1], n = Q[, j, 4], k = 1)[["qbar"]] %>% unlist() %>% round(3)
    } else {est[f, j] <- pool.scalar(Q[, j, f], U[, j, 2], n = Q[, j, 8], k = 1)[["qbar"]] %>% unlist() %>% round(3)
    }
  }
}
# Save summary per sex
imp.summary.sex <- est # Save the data
colnames(imp.summary.sex) <- SET_CFC.outl.del.imp$data %>% select_if(is.numeric) %>% colnames() # Set the column names
rownames(imp.summary.sex) <- c("Mean: Male", "Standard deviation: Male", 
                               "Standard error of the mean: Male", "Number of observations: Male",
                               "Mean: Female", "Standard deviation: Female", 
                               "Standard error of the mean: Female", "Number of observations: Female") # Set the row names
imp.summary.sex <- imp.summary.sex %>% as.data.frame() # Transform into a dataframe
imp.summary.sex <- imp.summary.sex[, -c(which(colnames(SET_CFC.outl.del.imp$data)=="Subject"))] # Remove subject number variable
write.xlsx(imp.summary.sex, "summary.sex_SET_CFC.outl.del.imp.xlsx") # Save an Excel file
save(imp.summary.sex, file = "imp.summary.sex.RData")


### Descriptives for high and low LSAS separately
# Initialize variables
label = c("Low", "High") # Label of the group to analyze.
nfunc <- 4 # Number of functions
nvar <- SET_CFC.outl.del.imp$data %>% select_if(is.numeric) %>% ncol() # Number of numeric variables
m <- SET_CFC.outl.del.imp$m # Number of imputations
Q <- array(NA,dim=c(m,nvar,I(nfunc*length(label)))) # Three-dimensional matrix to put all the estimates
U <- array(NA,dim=c(m,nvar,length(label))) # Three-dimensional matrix to put the variance of the variables per imputed dataset and per label
est <- matrix(NA, nrow = I(nfunc*length(label)), ncol = nvar) # Matrix to put the pooled estimates
# Calculate descriptives for all vars and all imputed datasets for low LSAS and high LSAS separately
for (i in 1:m) { # For all imputed datasets
  Data1 <- subset(mice::complete(SET_CFC.outl.del.imp, i), LSAS_Split == label[1]) %>% select_if(is.numeric)  # Select numeric variables of current dataset for low LSAS
  Data2 <- subset(mice::complete(SET_CFC.outl.del.imp, i), LSAS_Split == label[2]) %>% select_if(is.numeric)  # Select numeric variables of current dataset for high LSAS
  for (j in 1:nvar) { # For all variables in the data
    data1 <- Data1[, j] %>% na.omit() # Select currect variable without NA's for low LSAS
    data2 <- Data2[, j] %>% na.omit # Select currect variable without NA's for high LSAS
    Q[i, j, 1] <- mean(data1) # Mean
    Q[i, j, 2] <- sd(data1) # Standard deviation
    Q[i, j, 3] <- se(data1) # Standard error
    Q[i, j, 4] <- n.obs(data1) # The number of observations
    U[i, j, 1] <- var(data1) / Q[i, j, 4] # The standard error of the estimate (necessary for pooling)
    Q[i, j, 5] <- mean(data2) # Mean
    Q[i, j, 6] <- sd(data2) # Standard deviation
    Q[i, j, 7] <- se(data2) # Standard error
    Q[i, j, 8] <- n.obs(data2) # The number of observations
    U[i, j, 2] <- var(data2) / Q[i, j, 8] # The standard error of the estimate (necessary for pooling)
  }
}
# Pool the descriptives into one estimate with three decimals for all vars for low LSAS and high LSAS separately
for (f in 1:I(nfunc*length(label))) { # For every function * the number of groups
  for (j in 1:nvar) { # For every variable
    if (f <= nfunc) {est[f, j] <- pool.scalar(Q[, j, f], U[, j, 1], n = Q[, j, 4], k = 1)[["qbar"]] %>% unlist() %>% round(3)
    } else {est[f, j] <- pool.scalar(Q[, j, f], U[, j, 2], n = Q[, j, 8], k = 1)[["qbar"]] %>% unlist() %>% round(3)
    }
  }
}
# Save summary per LSAS group
imp.summary.LSAS <- est # Save the data
colnames(imp.summary.LSAS) <- SET_CFC.outl.del.imp$data %>% select_if(is.numeric) %>% colnames() # Set the column names
rownames(imp.summary.LSAS) <- c("Mean: Low LSAS", "Standard deviation: Low LSAS", 
                               "Standard error of the mean: Low LSAS", "Number of observations: Low LSAS",
                               "Mean: High LSAS", "Standard deviation: High LSAS", 
                               "Standard error of the mean: High LSAS", "Number of observations: High LSAS") # Set the row names
imp.summary.LSAS <- imp.summary.LSAS %>% as.data.frame() # Transform into a dataframe
imp.summary.LSAS <- imp.summary.LSAS[, -c(which(colnames(SET_CFC.outl.del.imp$data)=="Subject"))] # Remove subject number variable
write.xlsx(imp.summary.LSAS, "summary.LSAS_SET_CFC.outl.del.imp.xlsx") # Save an Excel file
save(imp.summary.LSAS, file = "imp.summary.LSAS.RData")


## Remove temporary variables (do not remove "imp.summary.sex" yet)
remove(f)
remove(j)
remove(i)
remove(nfunc)
remove(nvar)
remove(m)
remove(Q)
remove(U)
remove(est)
remove(n.obs)
remove(se)
remove(imp.summary)
remove(imp.summary.sex)
remove(imp.summary.LSAS)
remove(label)
remove(data)
remove(data1)
remove(data2)
remove(Data)
remove(Data1)
remove(Data2)


# Compare summary statistics ----------------------------------------------
## Packages
library(magrittr) # For piping

# Load imputed summary statistics
load("imp.summary.RData")

## Compare summary statistics between original and imputed variables
# Select all relevant variables
var <- c("anx.react", "pep.react", 
         "rsa.react", "rr.react", "cort.react", 
         "react_Frontal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z", 
         "react_Frontal_Avg_AAC_R", "react_Parietal_Avg_AAC_R")
# Loop over variables to print summary statistics
for (i in 1:length(var)) { 
  if (sum(!complete.cases(SET_CFC.outl.del[, var[i]]), na.rm = TRUE) > 0) { # Only print comparisons for variables that contain any missing values
    print(c(paste0("Summary for ", var[i]), # Announce the variable name
            "Orig mean" = mean(SET_CFC.outl.del[, var[i]], na.rm = TRUE) %>% round(3), # Calculate mean of original data
            "Orig SD" = sd(SET_CFC.outl.del[, var[i]], na.rm = TRUE) %>% round(3), # Calculate standard deviation of original data
            "Orig SEM" = sqrt(var(SET_CFC.outl.del[, var[i]], na.rm = TRUE)/sum(complete.cases(SET_CFC.outl.del[, var[i]]), na.rm = TRUE)) %>% round(3), # Calculate standard error of the mean of original data
            "Orig miss" = sum(!complete.cases(SET_CFC.outl.del[, var[i]]), na.rm = TRUE), # Calculate the number of missing values for original data
            "Imp mean" = imp.summary["Mean", var[i]], # Find mean of imputed data
            "Imp SD" = imp.summary["Standard deviation", var[i]], # Find standard deviation of imputed data
            "Imp SEM" = imp.summary["Standard error of the mean", var[i]] # Find standard error of the mean of imputed data
    ))
  } else { # If variables don't contain missing values, don't print summary statistics
    print(paste0("No missing values for ", var[i])) 
  }
}

## Remove temporary variables
remove(var)
remove(i)
remove(imp.summary)


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
load("imp.summary.RData")

### Make plots with base R (using "imp.summary_group")
# Save default parameters
restore <- par(no.readonly = TRUE)


## Figure 1: Cross-frequency coupling
tiff("FIG1.tiff", width = 40, height = 25, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left

## A) Frontal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.60, 1.10), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.60,
     xright = 13,
     ytop = 1.10,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.60,
     xright = 18,
     ytop = 1.10,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 1.1, x1 = 60, y1 = 1.1, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 1.0, x1 = 60, y1 = 1.0, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.90, x1 = 60, y1 = 0.90, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.80, x1 = 60, y1 = 0.80, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 0.70, x1 = 60, y1 = 0.70, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 0.60, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.60, 1.10, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary["Mean", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary["Mean", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] + imp.summary["Standard error of the mean", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary["Mean", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] - imp.summary["Standard error of the mean", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means minus SE
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
title(main = "Frontal phase-amplitude coupling", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("a)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


## B) Parietal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.60, 1.10), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.60,
     xright = 13,
     ytop = 1.10,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.60,
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
# Add axes
axis(1, pos = 0.60, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.60, 1.10, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary["Mean", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary["Mean", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] + imp.summary["Standard error of the mean", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary["Mean", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] - imp.summary["Standard error of the mean", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.2, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Z-score", # Y label
      cex.lab = 2.2, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Parietal phase-amplitude coupling", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


## C) Frontal AAC
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
axis(1, pos = 0.0, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.0, 0.05, 0.01)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary["Mean", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary["Mean", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] + imp.summary["Standard error of the mean", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary["Mean", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] - imp.summary["Standard error of the mean", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means minus SE
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
title(main = "Frontal amplitude-amplitude coupling", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("c)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left



## D) Parietal AAC
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
axis(1, pos = 0.0, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.0, 0.05, 0.01)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary["Mean", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary["Mean", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] + imp.summary["Standard error of the mean", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary["Mean", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] - imp.summary["Standard error of the mean", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means minus SE
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
title(main = "Parietal amplitude-amplitude correlation", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("d)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left



## Plot all subfigures of Figure 1 simultaneously
# Main title
mtext("Delta-beta coupling",
      outer = TRUE, # Add to outer margins
      cex = 3.5) # Make bigger
# Print Figure
dev.off()
# Reset parameters
par(restore)



## Figure 2: Other stress responses
tiff("FIG2.tiff", width = 40, height = 37.5, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(3, 2), oma = c(0, 0, 5, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left


## A) Pre-ejection period
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(100, 130), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 100,
     xright = 13,
     ytop = 130,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 100,
     xright = 18,
     ytop = 130,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 130, x1 = 60, y1 = 130, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 120, x1 = 60, y1 = 120, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 110, x1 = 60, y1 = 110, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 100, x1 = 60, y1 = 100, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 100, cex.axis = 2.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.5, las = 1, at = seq(100, 130, 10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add data
lines(x = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
      y = imp.summary["Mean", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add error bars
arrows(x0 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary["Mean", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")] + imp.summary["Standard error of the mean", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")]), # Means plus SE
       x1 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary["Mean", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")] - imp.summary["Standard error of the mean", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "PEP (ms)", # Y label
      cex.lab = 2.5, # Increase size
      line = 2.5) # Move outwards
# Add title
title(main = "Pre-ejection period", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("a)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left



## B) Respiratory sinus arrythmia
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(7.0, 8.5), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 7.0,
     xright = 13,
     ytop = 8.5,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 7.0,
     xright = 18,
     ytop = 8.5,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 8.5, x1 = 60, y1 = 8.5, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 8.0, x1 = 60, y1 = 8.0, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 7.5, x1 = 60, y1 = 7.5, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 7.0, cex.axis = 2.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.5, las = 1, at = seq(7.0, 8.5, .5)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add data
lines(x = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
      y = imp.summary["Mean", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add error bars
arrows(x0 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary["Mean", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")] + imp.summary["Standard error of the mean", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")]), # Means plus SE
       x1 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary["Mean", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")] - imp.summary["Standard error of the mean", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = expression("RSA" ~ (ms^{2})), # Y label
      cex.lab = 2.5, # Increase size
      line = 2.5) # Move outwards
# Add title
title(main = "Respiratory sinus arrythmia", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


## C) Respiratory rate
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(13, 17), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 13,
     xright = 13,
     ytop = 17,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 13,
     xright = 18,
     ytop = 17,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 17, x1 = 60, y1 = 17, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 16, x1 = 60, y1 = 16, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 15, x1 = 60, y1 = 15, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 14, x1 = 60, y1 = 14, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 13, cex.axis = 2.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.5, las = 1, at = seq(13, 17, 1)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add data
lines(x = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
      y = imp.summary["Mean", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add error bars
arrows(x0 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary["Mean", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")] + imp.summary["Standard error of the mean", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")]), # Means plus SE
       x1 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary["Mean", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")] - imp.summary["Standard error of the mean", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "RR (breaths per min)", # Y label
      cex.lab = 2.5, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "Respiratory rate", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("c)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


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
axis(1, pos = 5, cex.axis = 2.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.5, las = 1, at = seq(5, 65, 20)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add data
lines(x = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
      y = imp.summary["Mean", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add error bars
arrows(x0 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary["Mean", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] + imp.summary["Standard error of the mean", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means plus SE
       x1 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary["Mean", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] - imp.summary["Standard error of the mean", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Visual analogue scale (0 - 100)", # Y label
      cex.lab = 2.5, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "State anxiety", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("d)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)


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
axis(1, pos = 3, cex.axis = 2.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.5, las = 1, at = seq(3, 8, 1)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add data
lines(x = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
      y = imp.summary["Mean", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add error bars
arrows(x0 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary["Mean", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] + imp.summary["Standard error of the mean", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means plus SE
       x1 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary["Mean", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] - imp.summary["Standard error of the mean", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Free salivary cortisol (ng/ml)", # Y label
      cex.lab = 2.5, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "Cortisol", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("e)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)



## Plot all subfigures of Figure 2 simultaneously
# Main title
mtext("Stress responses",
      outer = TRUE, # Add to outer margins
      cex = 3.5, # Make bigger
      line = 1) # Move the text slightly upwards
# Print Figure
dev.off()
# Reset parameters
par(restore)


# Remove temporary variables
remove(imp.summary)
remove(restore)


# Line / bar plots men-women (preregistered) -------------------------------------------------------------------
# For supplements

## Packages
library(grid) # To organize multiple subplots in one large plot
library(gridBase) # To organize multiple subplots in one large plot
library(gridExtra) # To organize multiple subplots in one large plot
library(ggplot2) # Plotting packages (used for LSAS density plots)
library(reshape2) # To reshape into long format to use with ggplot

# Load data
load("imp.summary.sex.RData")

### Make plots with base R (using "imp.summary.sex")
# Save default parameters
restore <- par(no.readonly = TRUE)

## Figure 1: Cross-frequency coupling: divided by men and women
tiff("FIG1_sex.tiff", width = 40, height = 25, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left


## A) Frontal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.50, 1.20), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.50,
     xright = 13,
     ytop = 1.20,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.50,
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
# Add axes
axis(1, pos = 0.50, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.50, 1.20, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Male", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Male", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] + imp.summary.sex["Standard error of the mean: Male", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Male", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] - imp.summary.sex["Standard error of the mean: Male", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Female", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Female", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] + imp.summary.sex["Standard error of the mean: Female", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Female", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] - imp.summary.sex["Standard error of the mean: Female", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means minus SE
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
title(main = "Frontal phase-amplitude coupling", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("a)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


## B) Parietal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.50, 1.20), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.50,
     xright = 13,
     ytop = 1.20,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.50,
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
# Add axes
axis(1, pos = 0.50, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.50, 1.20, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Male", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Male", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] + imp.summary.sex["Standard error of the mean: Male", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Male", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] - imp.summary.sex["Standard error of the mean: Male", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Female", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Female", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] + imp.summary.sex["Standard error of the mean: Female", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Female", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] - imp.summary.sex["Standard error of the mean: Female", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means minus SE
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
title(ylab = "Z-score", # Y label
      cex.lab = 2.2, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Parietal phase-amplitude coupling", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


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
# Add men data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Male", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Male", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] + imp.summary.sex["Standard error of the mean: Male", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Male", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] - imp.summary.sex["Standard error of the mean: Male", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Female", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Female", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] + imp.summary.sex["Standard error of the mean: Female", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Female", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] - imp.summary.sex["Standard error of the mean: Female", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means minus SE
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
title(main = "Frontal amplitude-amplitude coupling", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("c)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


## D) Parietal AAC
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
segments(x0 = -30, y0 = 0.0, x1 = 60, y1 = 0.0, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = -0.01, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(-0.01, 0.05, 0.01)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Male", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Male", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] + imp.summary.sex["Standard error of the mean: Male", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Male", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] - imp.summary.sex["Standard error of the mean: Male", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Female", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Female", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] + imp.summary.sex["Standard error of the mean: Female", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Female", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] - imp.summary.sex["Standard error of the mean: Female", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means minus SE
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
title(main = "Parietal amplitude-amplitude coupling", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("d)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left



## Plot all subfigures of Figure 1 simultaneously
# Main title
mtext("Delta-beta coupling",
      outer = TRUE, # Add to outer margins
      cex = 3.5) # Make bigger
# Overlay clear plot (in order to use legend location "top)
reset <- function() {
  par(mfrow = c(1, 1), oma = c(0, 0, 1.5, 0), mar = c(0, 0, 0, 0), new = TRUE)
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



## Figure 2: Other stress responses: divided by men and women
tiff("FIG2_sex.tiff", width = 40, height = 37.5, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(3, 2), oma = c(0, 0, 5, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left


## A) Pre-ejection period
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(95, 135), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 95,
     xright = 13,
     ytop = 135,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 95,
     xright = 18,
     ytop = 135,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 135, x1 = 60, y1 = 135, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 125, x1 = 60, y1 = 125, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 115, x1 = 60, y1 = 115, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 105, x1 = 60, y1 = 105, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 95, cex.axis = 2.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.5, las = 1, at = seq(95, 135, 10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Male", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Male", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")] + imp.summary.sex["Standard error of the mean: Male", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")]), # Means plus SE
       x1 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Male", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")] - imp.summary.sex["Standard error of the mean: Male", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Female", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey30", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Female", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")] + imp.summary.sex["Standard error of the mean: Male", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")]), # Means plus SE
       x1 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Female", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")] - imp.summary.sex["Standard error of the mean: Male", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey30") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "PEP (ms)", # Y label
      cex.lab = 2.5, # Increase size
      line = 2.5) # Move outwards
# Add title
title(main = "Pre-ejection period", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("a)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


## B) Respiratory sinus arrythmia
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(7.0, 8.5), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 7.0,
     xright = 13,
     ytop = 8.5,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 7.0,
     xright = 18,
     ytop = 8.5,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 8.5, x1 = 60, y1 = 8.5, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 8.0, x1 = 60, y1 = 8.0, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 7.5, x1 = 60, y1 = 7.5, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 7.0, cex.axis = 2.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.5, las = 1, at = seq(7.0, 8.5, .5)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Male", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Male", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")] + imp.summary.sex["Standard error of the mean: Male", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")]), # Means plus SE
       x1 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Male", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")] - imp.summary.sex["Standard error of the mean: Male", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Female", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey30", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Female", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")] + imp.summary.sex["Standard error of the mean: Male", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")]), # Means plus SE
       x1 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Female", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")] - imp.summary.sex["Standard error of the mean: Male", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey30") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = expression("RSA" ~ (ms^{2})), # Y label
      cex.lab = 2.52, # Increase size
      line = 2.5) # Move outwards
# Add title
title(main = "Respiratory sinus arrythmia", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


## C) Respiratory rate
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(13, 17), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 13,
     xright = 13,
     ytop = 17,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 13,
     xright = 18,
     ytop = 17,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 17, x1 = 60, y1 = 17, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 16, x1 = 60, y1 = 16, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 15, x1 = 60, y1 = 15, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 14, x1 = 60, y1 = 14, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 13, cex.axis = 2.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.5, las = 1, at = seq(13, 17, 1)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Male", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Male", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")] + imp.summary.sex["Standard error of the mean: Male", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")]), # Means plus SE
       x1 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Male", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")] - imp.summary.sex["Standard error of the mean: Male", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Female", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey30", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Female", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")] + imp.summary.sex["Standard error of the mean: Male", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")]), # Means plus SE
       x1 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Female", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")] - imp.summary.sex["Standard error of the mean: Male", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey30") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "RR (breaths per min)", # Y label
      cex.lab = 2.5, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "Respiratory rate", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("c)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


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
axis(1, pos = 5, cex.axis = 2.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.5, las = 1, at = seq(5, 65, 20)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Male", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Male", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] + imp.summary.sex["Standard error of the mean: Male", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means plus SE
       x1 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Male", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] - imp.summary.sex["Standard error of the mean: Male", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Female", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Female", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] + imp.summary.sex["Standard error of the mean: Female", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means plus SE
       x1 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Female", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] - imp.summary.sex["Standard error of the mean: Female", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Visual analogue scale (0 - 100)", # Y label
      cex.lab = 2.5, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "State anxiety", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("d)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)


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
axis(1, pos = 3, cex.axis = 2.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.5, las = 1, at = seq(3, 8, 1)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Male", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Male", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] + imp.summary.sex["Standard error of the mean: Male", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means plus SE
       x1 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Male", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] - imp.summary.sex["Standard error of the mean: Male", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.sex["Mean: Female", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.sex["Mean: Female", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] + imp.summary.sex["Standard error of the mean: Female", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means plus SE
       x1 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.sex["Mean: Female", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] - imp.summary.sex["Standard error of the mean: Female", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2.5, # Make line width slightly thicker
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Free salivary cortisol (ng/ml)", # Y label
      cex.lab = 2.5, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "Cortisol", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("e)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)


## Plot all subfigures of Figure 2 simultaneously
# Main title
mtext("Stress responses",
      outer = TRUE, # Add to outer margins
      cex = 3.5, # Make bigger
      line = 1) # Move the text slightly upwards
# Overlay clear plot (in order to use legend location "top)
reset <- function() {
  par(mfrow = c(1, 1), oma = c(0, 0, 1.5, 0), mar = c(0, 0, 0, 0), new = TRUE)
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
remove(imp.summary.sex)
remove(reset)
remove(restore)


# Line / bar plots high-low LSAS (exploratory) -------------------------------------------------------------------
# For supplements

## Packages
library(grid) # To organize multiple subplots in one large plot
library(gridBase)
library(gridExtra) # To organize multiple subplots in one large plot
library(ggplot2) # Plotting packages (used for LSAS density plots)
library(reshape2) # To reshape into long format to use with ggplot
library(magrittr) # For piping
library(dplyr) # For data manipulation

# Load data
load("imp.summary.LSAS.RData")

### Make plots with base R (using "imp.summary.LSAS")
# Save default parameters
restore <- par(no.readonly = TRUE)

## Figure 1: Cross-frequency coupling: divided by high and low LSAS
tiff("FIG1_LSAS.tiff", width = 40, height = 25, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(2, 2), oma = c(0, 0, 3, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left


## A) Frontal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.50, 1.20), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.50,
     xright = 13,
     ytop = 1.20,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.50,
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
# Add axes
axis(1, pos = 0.50, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.50, 1.20, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: Low LSAS", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] + imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] - imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: High LSAS", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] + imp.summary.LSAS["Standard error of the mean: High LSAS", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")] - imp.summary.LSAS["Standard error of the mean: High LSAS", c("RS_Frontal_Avg_dPAC_Z", "Anticip_Frontal_Avg_dPAC_Z", "EarlyRecov_Frontal_Avg_dPAC_Z", "LateRecov_Frontal_Avg_dPAC_Z")]), # Means minus SE
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
title(main = "Frontal phase-amplitude coupling", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("a)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


## B) Parietal dPAC
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(0.50, 1.20), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 0.50,
     xright = 13,
     ytop = 1.20,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 0.50,
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
# Add axes
axis(1, pos = 0.50, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.50, 1.20, 0.10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: Low LSAS", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] + imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] - imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: High LSAS", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] + imp.summary.LSAS["Standard error of the mean: High LSAS", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")] - imp.summary.LSAS["Standard error of the mean: High LSAS", c("RS_Parietal_Avg_dPAC_Z", "Anticip_Parietal_Avg_dPAC_Z", "EarlyRecov_Parietal_Avg_dPAC_Z", "LateRecov_Parietal_Avg_dPAC_Z")]), # Means minus SE
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
title(ylab = "Z-score", # Y label
      cex.lab = 2.2, # Increase size
      line = 3) # Move outwards
# Add title
title(main = "Parietal phase-amplitude coupling", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


## C) Frontal AAC
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
axis(1, pos = 0.0, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.0, 0.05, 0.01)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: Low LSAS", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] + imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] - imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: High LSAS", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] + imp.summary.LSAS["Standard error of the mean: High LSAS", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")] - imp.summary.LSAS["Standard error of the mean: High LSAS", c("RS_Frontal_Avg_AAC_R", "Anticip_Frontal_Avg_AAC_R", "EarlyRecov_Frontal_Avg_AAC_R", "LateRecov_Frontal_Avg_AAC_R")]), # Means minus SE
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
title(main = "Frontal amplitude-amplitude coupling", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("c)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


## D) Parietal AAC
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
axis(1, pos = 0.0, cex.axis = 2.3, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.3, las = 1, at = seq(0.0, 0.05, 0.01)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add men data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: Low LSAS", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width slightly thicker
      col = "grey10") # Make line almost-black
# Add men error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] + imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] - imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add women data
lines(x = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: High LSAS", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add women error bars
arrows(x0 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] + imp.summary.LSAS["Standard error of the mean: High LSAS", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means plus SE
       x1 = c(-25, 5, 24, 54), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")] - imp.summary.LSAS["Standard error of the mean: High LSAS", c("RS_Parietal_Avg_AAC_R", "Anticip_Parietal_Avg_AAC_R", "EarlyRecov_Parietal_Avg_AAC_R", "LateRecov_Parietal_Avg_AAC_R")]), # Means minus SE
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
title(main = "Parietal amplitude-amplitude coupling", # Title of plot
      cex.main = 2.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("d)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


## Plot all subfigures of Figure 1 simultaneously
# Main title
mtext("Delta-beta coupling",
      outer = TRUE, # Add to outer margins
      cex = 3.5) # Make bigger
# Overlay clear plot (in order to use legend location "top)
reset <- function() {
  par(mfrow = c(1, 1), oma = c(0, 0, 1.5, 0), mar = c(0, 0, 0, 0), new = TRUE)
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



## Figure 2: Other stress responses: divided by low LSAS and high LSAS
tiff("FIG2_LSAS.tiff", width = 40, height = 37.5, units = "cm", res = 300) # Save TIFF file
par(mfrow = c(3, 2), oma = c(0, 0, 5, 0), mar = c(4, 5, 5, 2), family = "serif") # Create multiple plots in one figure, with some extra margins on top and left


## A) Pre-ejection period
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(95, 135), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 95,
     xright = 13,
     ytop = 135,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 95,
     xright = 18,
     ytop = 135,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 135, x1 = 60, y1 = 135, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 125, x1 = 60, y1 = 125, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 115, x1 = 60, y1 = 115, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 105, x1 = 60, y1 = 105, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 95, cex.axis = 2.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.5, las = 1, at = seq(95, 135, 10)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add low LSAS data
lines(x = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: Low LSAS", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add low LSAS error bars
arrows(x0 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")] + imp.summary.LSAS["Standard error of the mean: Low LSAS", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")]), # Means plus SE
       x1 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")] - imp.summary.LSAS["Standard error of the mean: Low LSAS", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add high LSAS data
lines(x = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: High LSAS", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey30", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add high LSAS error bars
arrows(x0 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")] + imp.summary.LSAS["Standard error of the mean: Low LSAS", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")]), # Means plus SE
       x1 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")] - imp.summary.LSAS["Standard error of the mean: Low LSAS", c("PEP.1", "PEP.2", "PEP.3", "PEP.4", "PEP.7", "PEP.8", "PEP.9", "PEP.10")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey30") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "PEP (ms)", # Y label
      cex.lab = 2.5, # Increase size
      line = 2.5) # Move outwards
# Add title
title(main = "Pre-ejection period", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("a)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


## B) Respiratory sinus arrythmia
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(7.0, 8.5), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 7.0,
     xright = 13,
     ytop = 8.5,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 7.0,
     xright = 18,
     ytop = 8.5,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 8.5, x1 = 60, y1 = 8.5, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 8.0, x1 = 60, y1 = 8.0, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 7.5, x1 = 60, y1 = 7.5, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 7.0, cex.axis = 2.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.5, las = 1, at = seq(7.0, 8.5, .5)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add low LSAS data
lines(x = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: Low LSAS", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add low LSAS error bars
arrows(x0 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")] + imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")]), # Means plus SE
       x1 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")] - imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add high LSAS data
lines(x = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: High LSAS", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey30", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add high LSAS error bars
arrows(x0 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")] + imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")]), # Means plus SE
       x1 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")] - imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RSA.1", "RSA.2", "RSA.3", "RSA.4", "RSA.7", "RSA.8", "RSA.9", "RSA.10")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey30") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = expression("RSA" ~ (ms^{2})), # Y label
      cex.lab = 2.52, # Increase size
      line = 2.5) # Move outwards
# Add title
title(main = "Respiratory sinus arrythmia", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("b)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


## C) Respiratory rate
# Main plot
plot(NA, # No data yet
     axes = FALSE, # Remove axes to draw self
     xlim = c(-30, 60), # Set limits of x-axis
     ylim = c(13, 18), # Set limits of y-axis
     xlab = "", # Leave label of x-axis empty to add later (more inwards)
     ylab = "", # Leave label of y-axis empty to add later (more inwards)
     main = "") # Leave title empty to add later
# Add background colors
rect(xleft = 0, # Preparatory SET manipulation (light pink)
     ybottom = 13,
     xright = 13,
     ytop = 18,
     col = "pink",
     lty = 0) # Don't add border
rect(xleft = 13, # SET (dark pink)
     ybottom = 13,
     xright = 18,
     ytop = 18,
     col = "pink2",
     lty = 0) # Don't add border
# Add grid lines
segments(x0 = -30, y0 = 18, x1 = 60, y1 = 18, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 17, x1 = 60, y1 = 17, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 16, x1 = 60, y1 = 16, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 15, x1 = 60, y1 = 15, col = "grey50", lwd = 1)
segments(x0 = -30, y0 = 14, x1 = 60, y1 = 14, col = "grey50", lwd = 1)
# Add axes
axis(1, pos = 13, cex.axis = 2.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.5, las = 1, at = seq(13, 18, 1)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add low LSAS data
lines(x = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: Low LSAS", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add low LSAS error bars
arrows(x0 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")] + imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")]), # Means plus SE
       x1 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")] - imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add high LSAS data
lines(x = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: High LSAS", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey30", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add high LSAS error bars
arrows(x0 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")] + imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")]), # Means plus SE
       x1 = c(-25, -22.5, 5, 7.5, 24, 26.5, 54, 56.5), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")] - imp.summary.LSAS["Standard error of the mean: Low LSAS", c("RR.1", "RR.2", "RR.3", "RR.4", "RR.7", "RR.8", "RR.9", "RR.10")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey30") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "RR (breaths per min)", # Y label
      cex.lab = 2.5, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "Respiratory rate", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("c)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left


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
axis(1, pos = 5, cex.axis = 2.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.5, las = 1, at = seq(5, 65, 20)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add low LSAS data
lines(x = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: Low LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add low LSAS error bars
arrows(x0 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] + imp.summary.LSAS["Standard error of the mean: Low LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means plus SE
       x1 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] - imp.summary.LSAS["Standard error of the mean: Low LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add high LSAS data
lines(x = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: High LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add high LSAS error bars
arrows(x0 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] + imp.summary.LSAS["Standard error of the mean: High LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means plus SE
       x1 = c(-19, 11, 19, 30, 35, 40, 45, 50), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")] - imp.summary.LSAS["Standard error of the mean: High LSAS", c("Anx.1", "Anx.2", "Anx.3", "Anx.4", "Anx.5", "Anx.6", "Anx.7", "Anx.8")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2,  # Make line width slightly thicker 
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Visual analogue scale (0 - 100)", # Y label
      cex.lab = 2.5, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "State anxiety", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("d)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)


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
axis(1, pos = 3, cex.axis = 2.5, at = seq(-30, 60, 15)) # Draw new x-axis, with a slightly smaller size
axis(2, pos = -30, cex.axis = 2.5, las = 1, at = seq(3, 8, 1)) # Draw new y-axis, with a slightly smaller size and horizontal labels
# Add low LSAS data
lines(x = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: Low LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")], # Means of measurements
      pch = 17, # Use filled triangles
      type = "l", # Plot lines
      lwd = 2.2, # Make line width thicker
      col = "grey10") # Make line almost-black
# Add low LSAS error bars
arrows(x0 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] + imp.summary.LSAS["Standard error of the mean: Low LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means plus SE
       x1 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: Low LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] - imp.summary.LSAS["Standard error of the mean: Low LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 1.75) # Make line width slightly thicker 
# Add high LSAS data
lines(x = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
      y = imp.summary.LSAS["Mean: High LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")], # Means of measurements
      pch = 19, # Use filled circles
      type = "l", # Plot lines
      col = "grey40", # Draw lines in dark grey
      lty = 5, # Dashed (long) lines
      lwd = 2.5) # Make line width thicker
# Add high LSAS error bars
arrows(x0 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y0 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] + imp.summary.LSAS["Standard error of the mean: High LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means plus SE
       x1 = c(-18, 20, 31, 36, 41, 46, 51), # Time points of measurements, relative to start SET manipulation
       y1 = as.numeric(imp.summary.LSAS["Mean: High LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")] - imp.summary.LSAS["Standard error of the mean: High LSAS", c("Cortisol.1", "Cortisol.2", "Cortisol.3", "Cortisol.4", "Cortisol.5", "Cortisol.6", "Cortisol.7")]), # Means minus SE
       length = 0.05, # Size of cap
       angle = 90, # Draw vertical lines
       code = 3, # Draw lines of cap on both sides
       lwd = 2.5, # Make line width slightly thicker
       col = "grey40") # Make error bars dark grey
# Add x-axis label
title(xlab = "Time (min) respective to start SET", # X label
      cex.lab = 2.5, # Increase size
      line = 2) # Move inwards
# Add y-axis label
title(ylab = "Free salivary cortisol (ng/ml)", # Y label
      cex.lab = 2.5, # Increase size
      line = 2) # Move outwards
# Add title
title(main = "Cortisol", # Title of plot
      cex.main = 3.5, # Increase size
      line = 0.5) # Move slightly inwards
# Figure number
mtext(bquote(bold("e)")), side = 3, adj = -0.03, cex = 2.5) # Add the number of the subfigure top left (moved slightly to the right)


## Plot all subfigures of Figure 2 simultaneously
# Main title
mtext("Stress responses",
      outer = TRUE, # Add to outer margins
      cex = 3.5, # Make bigger
      line = 1) # Move the text slightly upwards
# Overlay clear plot (in order to use legend location "top)
reset <- function() {
  par(mfrow = c(1, 1), oma = c(0, 0, 1.5, 0), mar = c(0, 0, 0, 0), new = TRUE)
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
remove(imp.summary.LSAS)
remove(reset)
remove(restore)



# Sex differences in frontal/parietal CFC (preregistered) -----------------------------------------------------------------

## Packages
library(mice) # Imputation analysis
library(magrittr) # Piping
library(xlsx) # Exporting to Excel
library(MKmisc) # t-test for multiple imputation
library(miceadds) # For subsetting
library(BayesFactor) # For Bayes factors
library(psych) # For converting t-value to Cohen's d
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors


### Compare variables on sex
## Initialize datasets
data <- SET_CFC.outl.del.imp %$% 
  cbind.data.frame(Sex, 
                   RS_Frontal_Avg_dPAC_Z,
                   RS_Frontal_Avg_AAC_R,
                   react_Frontal_Avg_dPAC_Z,
                   react_Frontal_Avg_AAC_R,
                   RS_Parietal_Avg_dPAC_Z,
                   RS_Parietal_Avg_AAC_R,
                   react_Parietal_Avg_dPAC_Z,
                   react_Parietal_Avg_AAC_R)

## T-tests
# Initialize variables
t <- list(NA) # Saves all t-test results
b <- list(NA) # Saves all Bayes factor results
# Do a t-test for all variables to compare males and females (removing the first variable 'Sex')
for (x in 1:I(ncol(data[["analyses"]][[1]])-1)) {
  t[[x]] <- mi.t.test(data$analyses, 
                      x = colnames(data[["analyses"]][[1]])[[x+1]], 
                      y = "Sex")
  t[[x]]["data.name"] <- colnames(data[["analyses"]][[1]])[[x+1]]
  ## Bayes factor tests
  # Dataset of the first group
  n1 <- subset_datlist(SET_CFC.outl.del.imp, index=1, 
                       subset = SET_CFC.outl.del.imp[[1]]$Sex == "Male",
                       select=colnames(data[["analyses"]][[1]])[[x+1]])
  n1 <- nrow(n1[[1]]) # Number of observations of the first group
  # Dataset of the second group
  n2 <- subset_datlist(SET_CFC.outl.del.imp, index=1, 
                       subset = SET_CFC.outl.del.imp[[1]]$Sex == "Female",
                       select=colnames(data[["analyses"]][[1]])[[x+1]])
  n2 <- nrow(n2[[1]]) # Number of observations of the second group
  # Get Bayes factor
  b[[x]] <- ttest.tstat(t[[x]][["statistic"]][["t"]], n1, n2, simple = TRUE)
}

## Put the t-test results in a table
# Extract results from list
t.table <- sapply(t, function(x) {
  c(test = x$method,
    test.stat = x$statistic[["t"]],
    df = x$parameter[["df"]],
    p.value = x$p.value)
})

# Save the results as a dataframe
t.table <- as.data.frame(t.table)
names <- sapply(t, function(x) {c(name = x$data.name)}) # Get the variable names
colnames(t.table) <- as.character(names) # Set the columns as variable names
t.table <- t(t.table) %>% as.data.frame(stringsAsFactors = FALSE) # Transpose the dataframe
t.table[, 2:4] <- lapply(t.table[, 2:4], function(x) as.numeric(as.character(x))) %>% 
  lapply(function(x) round(x, 5)) # Set the numeric columns to class numeric and round to 5 decimals
t.table$BF <- b # Add the Bayes factor variable to the table
t.table[, 5] <- t.table[, 5] %>% as.numeric() %>% 
  round(5) # Set the BF column to class numeric and round to 5 decimals

# Add Cohen's d
t.val <- t.table[, "test.stat"] # Extract t-value
t.val <- abs(t.val) # Calculate absolute t-value
d <- t2d(t.val, n=n1, n2=n2) # Calculate Cohen's d from t-value
t.table$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
t.table$cohen.d.mag <- sapply(t.table$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

## Adjust p-values with fdr-correction
t.table[, "p.value.adj"] <- p.adjust(t.table[, "p.value"], method = "fdr", n = length(t.table[, "p.value"])) # Do fdr-correction

## Check significance
t.table[, "p.adj.sig"] <- sapply(t.table[, "p.value.adj"], function(x) p.value.sig(x)) # Add column with adjusted significance
t.table[, "p.value.sig"] <- sapply(t.table[, "p.value"], function(x) p.value.sig(x)) # Add column with adjusted significance

## Add column with Bayes factor interpretation
t.table[, "BF evidence"] <- sapply(t.table[, "BF"], function(x) BF.evidence(x))

# Order based on p-value
t.table[, "variable"] <- rownames(t.table) # Save variable name
t.table <- t.table[, c(ncol(t.table), seq(1:I(ncol(t.table)-1)))] # Put variable name column in front
t.table <- t.table %>% arrange(p.value) # Reorder rows

## Save the results
write.xlsx(t.table, "diffsex.FrontalParietalCFC.xlsx")

# Remove temporary variables
remove(t.table)
remove(x)
remove(names)
remove(data)
remove(t)
remove(b)
remove(n1)
remove(n2)
remove(BF.evidence)
remove(cohen.d.magnitude)
remove(p.value.sig)
remove(d)
remove(t.val)


# Sex differences in other stress responses (exploratory) -----------------------------------------------------------------

## Packages
library(mice) # Imputation analysis
library(magrittr) # Piping
library(xlsx) # Exporting to Excel
library(MKmisc) # t-test for multiple imputation
library(miceadds) # For subsetting
library(BayesFactor) # For Bayes factors
library(psych) # For converting t-value to Cohen's d
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors


### Compare variables on sex
## Initialize datasets
data <- SET_CFC.outl.del.imp %$% 
  cbind.data.frame(Sex, 
                   LSAS,
                   Anx.1, 
                   anx.react,
                   PEP.2, 
                   pep.react, 
                   RSA.2,
                   rsa.react,
                   RR.2,
                   rr.react,
                   Cortisol.1.log,
                   cort.react,
                   EnglishCompetence)

## T-tests
# Initialize variables
t <- list(NA) # Saves all t-test results
b <- list(NA) # Saves all Bayes factor results
# Do a t-test for all variables to compare males and females (removing the first variable 'Sex')
for (x in 1:I(ncol(data[["analyses"]][[1]])-1)) {
  t[[x]] <- mi.t.test(data$analyses, 
                      x = colnames(data[["analyses"]][[1]])[[x+1]], 
                      y = "Sex")
  t[[x]]["data.name"] <- colnames(data[["analyses"]][[1]])[[x+1]]
  ## Bayes factor tests
  # Dataset of the first group
  n1 <- subset_datlist(SET_CFC.outl.del.imp, index=1, 
                       subset = SET_CFC.outl.del.imp[[1]]$Sex == "Male",
                       select=colnames(data[["analyses"]][[1]])[[x+1]])
  n1 <- nrow(n1[[1]]) # Number of observations of the first group
  # Dataset of the second group
  n2 <- subset_datlist(SET_CFC.outl.del.imp, index=1, 
                       subset = SET_CFC.outl.del.imp[[1]]$Sex == "Female",
                       select=colnames(data[["analyses"]][[1]])[[x+1]])
  n2 <- nrow(n2[[1]]) # Number of observations of the second group
  # Get Bayes factor
  b[[x]] <- ttest.tstat(t[[x]][["statistic"]][["t"]], n1, n2, simple = TRUE)
}

## Put the t-test results in a table
# Extract results from list
t.table <- sapply(t, function(x) {
  c(test = x$method,
    test.stat = x$statistic[["t"]],
    df = x$parameter[["df"]],
    p.value = x$p.value)
})

# Save the results as a dataframe
t.table <- as.data.frame(t.table)
names <- sapply(t, function(x) {c(name = x$data.name)}) # Get the variable names
colnames(t.table) <- as.character(names) # Set the columns as variable names
t.table <- t(t.table) %>% as.data.frame(stringsAsFactors = FALSE) # Transpose the dataframe
t.table[, 2:4] <- lapply(t.table[, 2:4], function(x) as.numeric(as.character(x))) %>% 
  lapply(function(x) round(x, 5)) # Set the numeric columns to class numeric and round to 5 decimals
t.table$BF <- b # Add the Bayes factor variable to the table
t.table[, 5] <- t.table[, 5] %>% as.numeric() %>% 
  round(5) # Set the BF column to class numeric and round to 5 decimals

## Regression to test effect of Sex on rsa (correcting for rr)
# For baseline
rsa.test <- with(SET_CFC.outl.del.imp, lm(RSA.2 ~ Sex + RR.2))
rsa.test.pooled <- summary(pool(rsa.test))
t.table["RSA.2 (rr)", ] <- data.frame(test = "Lineair regression (t-value)", 
                                      test.stat = rsa.test.pooled[2, 3],
                                      df = rsa.test.pooled[2, 4],
                                      p.value = rsa.test.pooled[2, 5])
t.table["RSA.2 (rr)", "test"] <- "Lineair regression (t-value)" # Set Regression test name again for stubborn df
# Bayes factor tests
# Dataset of the first group
n1 <- subset_datlist(SET_CFC.outl.del.imp, index=1, 
                     subset = SET_CFC.outl.del.imp[[1]]$Sex == "Male",
                     select="RSA.2")
n1 <- nrow(n1[[1]]) # Number of observations of the first group
# Dataset of the second group
n2 <- subset_datlist(SET_CFC.outl.del.imp, index=1, 
                     subset = SET_CFC.outl.del.imp[[1]]$Sex == "Female",
                     select="RSA.2")
n2 <- nrow(n2[[1]]) # Number of observations of the Second group
# Get Bayes factor
t.table["RSA.2 (rr)", "BF"] <- ttest.tstat(t.table["RSA.2 (rr)", "test.stat"], n1, n2, simple = TRUE)


## Regression to test effect of Sex on rsa (correcting for rr)
# For reactivity
rsa.test <- with(SET_CFC.outl.del.imp, lm(rsa.react ~ Sex + rr.react))
rsa.test.pooled <- summary(pool(rsa.test))
t.table["rsa.react (rr)", ] <- data.frame(test = "Lineair regression (t-value)", 
                                          test.stat = rsa.test.pooled[2, 3],
                                          df = rsa.test.pooled[2, 4],
                                          p.value = rsa.test.pooled[2, 5])
t.table["rsa.react (rr)", "test"] <- "Lineair regression (t-value)" # Set Regression test name again for stubborn df
# Bayes factor tests
# Dataset of the first group
n1 <- subset_datlist(SET_CFC.outl.del.imp, index=1, 
                     subset = SET_CFC.outl.del.imp[[1]]$Sex == "Male",
                     select="rsa.react")
n1 <- nrow(n1[[1]]) # Number of observations of the first group
# Dataset of the second group
n2 <- subset_datlist(SET_CFC.outl.del.imp, index=1, 
                     subset = SET_CFC.outl.del.imp[[1]]$Sex == "Female",
                     select="rsa.react")
n2 <- nrow(n2[[1]]) # Number of observations of the Second group
# Get Bayes factor
t.table["rsa.react (rr)", "BF"] <- ttest.tstat(t.table["rsa.react (rr)", "test.stat"], n1, n2, simple = TRUE)



# Add Cohen's d
t.val <- t.table[, "test.stat"] # Extract t-value
t.val <- abs(t.val) # Calculate absolute t-value
d <- t2d(t.val, n=n1, n2=n2) # Calculate Cohen's d from t-value
t.table$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
t.table$cohen.d.mag <- sapply(t.table$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

## Adjust p-values with fdr-correction
t.table[, "p.value.adj"] <- p.adjust(t.table[, "p.value"], method = "fdr", n = length(t.table[, "p.value"])) # Do fdr-correction

## Check significance
t.table[, "p.adj.sig"] <- sapply(t.table[, "p.value.adj"], function(x) p.value.sig(x)) # Add column with adjusted significance
t.table[, "p.value.sig"] <- sapply(t.table[, "p.value"], function(x) p.value.sig(x)) # Add column with adjusted significance

## Add column with Bayes factor interpretation
t.table[, "BF evidence"] <- sapply(t.table[, "BF"], function(x) BF.evidence(x))

# Order based on p-value
t.table[, "variable"] <- rownames(t.table) # Save variable name
t.table <- t.table[, c(ncol(t.table), seq(1:I(ncol(t.table)-1)))] # Put variable name column in front
t.table <- t.table %>% arrange(p.value) # Reorder rows

## Save the results
write.xlsx(t.table, "diffsex.StressResponses.xlsx")

# Remove temporary variables
remove(t.table)
remove(rsa.test)
remove(rsa.test.pooled)
remove(x)
remove(names)
remove(data)
remove(t)
remove(b)
remove(n1)
remove(n2)
remove(BF.evidence)
remove(cohen.d.magnitude)
remove(p.value.sig)
remove(d)
remove(t.val)


# Correlations with EnglishCompetence ------------------------------------------------------------
# Check for influence from confounding variable

# Packages
library(mice) # Multiple imputation functions
library(dplyr) # Data transformation
library(magrittr) # Piping
library(xlsx) # Exporting to Excel
library(miceadds) # Multiple imputation correlation
library(psych) # For converting rho to Cohen's d
library(BayesFactor) # For Bayesian statistics
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors


# Load data
load("SET_CFC.outl.del.imp.RData")

## Correlations
corr <- micombine.cor(mi.res = SET_CFC.outl.del.imp, variables = 
                        c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                          "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                          "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                          "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                          "LSAS", "Anx.1", "anx.react",
                          "RSA.2", "rsa.react",
                          "RR.2", "rr.react",
                          "PEP.2", "pep.react",
                          "Cortisol.1.log", "cort.react",
                          "EnglishCompetence"))
# Remove double rows (second half)
corr <- corr[c(1:I(nrow(corr) / 2)), ]
# Remove everything but correlations involving EnglishCompetence in either one of the columns
corr <- with(corr, corr[ (grepl( "EnglishCompetence", variable1) | grepl( "EnglishCompetence", variable2)) , ])
rownames(corr) <- NULL # Reset rownames
corr[["variable1"]] <- as.character(corr[["variable1"]]) # Remove factors
corr[["variable2"]] <- as.character(corr[["variable2"]]) # Remove factors
drops <- c("rse", "fisher_r", "fisher_rse", "fmi", "t", "lower95", "upper95") # Select unnecessary columns to remove
corr <- corr[ , !(names(corr) %in% drops)] # Remove unnecessary columns
## Calculate BayesFactors
# Select data
Dataset <- mice::complete(SET_CFC.outl.del.imp, action = "long")
Dataset <- select(Dataset, c(".imp", "Sex", "RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                             "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                             "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                             "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                             "LSAS", "Anx.1", "anx.react",
                             "RSA.2", "rsa.react",
                             "RR.2", "rr.react",
                             "PEP.2", "pep.react",
                             "Cortisol.1.log", "cort.react",
                             "EnglishCompetence"))
colnames(Dataset)[1] <- "imp"
# Initialize variables
m <- SET_CFC.outl.del.imp$m # Number of imputed datasets
corrBF <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the BayesFactors per imputed dataset
n.obs <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the number of observations
correst <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the variance of the variables per imputed dataset
est <- matrix(NA, nrow = nrow(corr), ncol = 1) # Matrix to put the pooled estimates
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed datasets
  subdata <- Dataset %>% filter(imp == j) %>% select(-imp, -Sex) %>% as.matrix() # Select imputed dataset
  for (i in 1:nrow(corr)) { # For all variables in the data
    BF <- correlationBF(y = subdata[, corr[i, "variable1"]], x = subdata[, corr[i, "variable2"]]) # Calculate BayesFactor
    corrBF[i, j] <- extractBF(BF, onlybf = TRUE) # Extract only the BayesFactor
    n.obs[i, j] <- length(subdata[, corr[i, "variable1"]]) # Calculate sample size
    correst[i, j] <- mean( c(var(subdata[, corr[i, "variable1"]]), var(subdata[, corr[i, "variable2"]])) ) / n.obs[i, j] # The standard error of the estimate (necessary for pooling)
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:nrow(corr)) { # For every variable
  est[i, 1] <- pool.scalar(corrBF[i, ], correst[i, ], n = n.obs[i,], k = 1)[["qbar"]] %>% unlist() %>% round(3)
}
# Add bayes factors to dataframe
corr[, "BF"] <- est
# Add column with Bayes factor interpretation
corr[, "BF.evidence"] <- sapply(corr$BF, function(x) BF.evidence(x)) # Add column with interpretation
# Do fdr-correction
corr[, "p.value.adj"] <- p.adjust(corr[, "p"], method = "fdr", n = nrow(corr))
# Check significance
corr[, "p.adj.sig"] <- sapply(corr[, "p.value.adj"], function(x) p.value.sig(x)) # Corrected
corr[, "p.value.sig"] <- sapply(corr[, "p"], function(x) p.value.sig(x)) # Uncorrected
# Add Cohen's d
rho <- corr[, "r"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
corr$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
corr$cohen.d.mag <- sapply(corr$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d
# Order based on p-value
corr <- corr %>% arrange(p)

## Save results
# base
write.xlsx(corr, "Correlations_EnglishCompetence.xlsx")


## Remove temporary variables
remove(corr)
remove(p.value.sig)
remove(subdata)
remove(d)
remove(drops)
remove(i)
remove(j)
remove(m)
remove(Dataset)
remove(correst)
remove(corrBF)
remove(est)
remove(n.obs)
remove(rho)
remove(BF.evidence)
remove(BF)
remove(cohen.d.magnitude)


# Sig CFC/stress reactivity/recovery (preregistered/exploratory) ----------------------------------------------------------

## Packages
library(mice) # Imputation analysis
library(magrittr) # Piping
library(xlsx) # Exporting to Excel
library(MKmisc) # t-test for multiple imputation
library(miceadds) # For subsetting
library(psych) # For converting t-value to Cohen's d
library(BayesFactor) # For bayesian statistics with uninformed priors
source("Aladins package\\BF_t.R") # For bayesian statistics with informed priors: "Aladins Bayes Factor in R" (downloaded from https://doi.org/10.17045/sthlmuni.4981154.v3)
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors


# Initialize data
data_obtained <- SET_CFC.outl.del.imp %$% 
  cbind.data.frame(react_Frontal_Avg_dPAC_Z, react_Frontal_Avg_AAC_R, 
                   react_Parietal_Avg_dPAC_Z, react_Parietal_Avg_AAC_R,
                   anx.react, rsa.react,
                   rr.react, pep.react, 
                   cort.react)

### T-tests 
# Initialize variables
t <- list(NA) # Saves all t-test results
# Do a t-test for all variables
for (x in 1:I(ncol(data_obtained[["analyses"]][[1]])) ) {
  t[[x]] <- mi.t.test(data_obtained$analyses, 
                      x = colnames(data_obtained[["analyses"]][[1]])[[x]],
                      alternative = "two.sided")
  t[[x]]["data.name"] <- colnames(data_obtained[["analyses"]][[1]])[[x]]
}
## Put the t-test results in a table
# Extract results from list
t.table <- sapply(t, function(x) {
  c(test = x$method,
    test.stat = x$statistic[["t"]],
    df = x$parameter[["df"]],
    p.value = x$p.value)
})
# Save the results as a dataframe
t.table <- as.data.frame(t.table)
names <- sapply(t, function(x) {c(name = x$data.name)}) # Get the variable names
colnames(t.table) <- as.character(names) # Set the columns as variable names
t.table <- t(t.table) %>% as.data.frame(stringsAsFactors = FALSE) # Transpose the dataframe
t.table[, 2:4] <- lapply(t.table[, 2:4], function(x) as.numeric(as.character(x))) %>% 
  lapply(function(x) round(x, 5)) # Set the numeric columns to numeric and round to 5 decimals
t.table[, 4] <- t.table[, 4] %>% as.numeric() %>% round(5) # Set the BF column to class numeric and round to 5 decimals

### Bayes factors with informed or uninformed priors, depending on availability
# BF10 = likelihood of the data given H1, relative to the likehood of the data given H0
# E.g., BF10 = 4 means that there is four times more evidence for H1 than for H0.
# Select data that have matching previously-observed data in long format
data_obtained_long <- mice::complete(SET_CFC.outl.del.imp, action = "long")
data_obtained_long <- select(data_obtained_long, c(".imp", "react_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_AAC_R", 
                                                   "anx.react"))
colnames(data_obtained_long)[1] <- "imp" # Reset imp column name without dot
# Select CFC reactivity data from previous study
load("LSA_HSA_brief.RData") # Load prior data
data_theory <- select(LSA_HSA_brief, c("react_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_AAC_R",
                                 "anx.react")) %>% as.matrix()
BF <- list() # Initialize results list for uninformed priors
# Initialize variables for pooling informed priors
m <- SET_CFC.outl.del.imp$m # Number of imputed datasets
BF_test <- matrix(NA, nrow = I(ncol(data_obtained[["analyses"]][[1]])), ncol = m) # Matrix to put the BayesFactors per imputed dataset
n.obs <- matrix(NA, nrow = I(ncol(data_obtained[["analyses"]][[1]])), ncol = m) # Matrix to put the number of observations  per imputed dataset
t_est <- matrix(NA, nrow = I(ncol(data_obtained[["analyses"]][[1]])), ncol = m) # Matrix to put the variance of the variables per imputed dataset
est <- matrix(NA, nrow = I(ncol(data_obtained[["analyses"]][[1]])), ncol = 1) # Matrix to put the pooled estimates over all imputed datasets
se <- function(x) {sqrt(var(x)/length(x))} # Standard error of the mean (SEM) function
## Calculate Bayes factor
for (i in 1:I(ncol(data_obtained[["analyses"]][[1]])) ) {
  # Check whether there are priors available for this variable
  if (colnames(data_obtained[["analyses"]][[1]])[[i]] %in% colnames(data_theory)) {
    ## If yes, use informed prior
    t.table[i, "Prior"] <- "Informed prior" # Save as uninformed prior
    # Loop over all imputed datasets
    for (j in 1:m) { # For all imputed datasets
      subdata <- data_obtained_long %>% filter(imp == j) %>% select( colnames(data_obtained[["analyses"]][[1]])[[i]] ) %>% as.matrix() # Select variables imputed dataset
      # Calculate obtained data
      meanobtained <- subdata %>% mean() %>% round(3) # Calculate mean reactivity
      semobtained <- subdata %>% se() %>% round(3) # Calculate SEM
      dfobtained <- subdata %>% complete.cases() %>% sum() %>% round(3) %>% -1 # Calculate df (sample size - 1)
      # Calculate prior data
      meantheory <- data_theory[, colnames(subdata)] %>% mean() %>% round(3) # Calculate mean reactivity
      sdtheory <- data_theory[, colnames(subdata)] %>% se() %>% round(3) # Calculate SEM
      dftheory <- data_theory[, colnames(subdata)] %>% complete.cases() %>% sum() %>% round(3) %>% -1 # Calculate df (sample size - 1)
      # Calculate Bayes factor
      BF_test[[i, j]] <- BF_t(meantheory, sdtheory, dftheory, meanobtained, semobtained, dfobtained, colnames(data_obtained)[[i]])
      n.obs[[i, j]] <- length(subdata) # Calculate sample size
      t_est[[i, j]] <- mean( c(var(subdata), var(data_theory[, colnames(subdata)])) ) / n.obs[i, j] # The standard error of the estimate (necessary for pooling)
    }
    # Pool the descriptives into one estimate with three decimals for all vars
    BF[[i]] <- pool.scalar(BF_test[i, ], t_est[i, ], n = n.obs[i, ], k = 1)[["qbar"]] %>% unlist() %>% round(3)
    
  } else {
    ## If no, use uninformed prior
    t.table[i, "Prior"] <- "Uninformed prior" # Save as uninformed prior
    n1 <- data_obtained_long %>% filter(imp == 1) %>% nrow() # Number of observations of the group
    # Get Bayes factor
    BF[[i]] <- ttest.tstat(t.table[colnames(data_obtained[["analyses"]][[1]])[[i]], "test.stat"], n1, simple = TRUE)
  }
}
# Add bayes factors to dataframe
t.table[, "BF"] <- BF %>% unlist()

## Regression to test significance of rsa reactivity (correcting for rr)
rsa.test <- with(SET_CFC.outl.del.imp, lm(rsa.react ~ rr.react))
rsa.test.pooled <- summary(pool(rsa.test))
# Add to table
t.table["rsa.react (rr)", ] <- data.frame(test = "Lineair regression (t-value)", 
                                          test.stat = rsa.test.pooled[1, 3],
                                          df = rsa.test.pooled[1,4],
                                          p.value = rsa.test.pooled[1, 5])
t.table["rsa.react (rr)", "test"] <- "Lineair regression (t-value)" # Set Regression test name again for stubborn df
# Bayes factor tests
# Dataset of the group
n1 <- nrow(data_obtained$analyses[[1]]) # Number of observations of the first group
# Get Bayes factor
t.table["rsa.react (rr)", "BF"] <- ttest.tstat(t.table["rsa.react (rr)", "test.stat"], n1, simple = TRUE)
t.table["rsa.react (rr)", "Prior"] <- "Uninformed prior" # Save as uninformed prior


# Add column with Bayes factor interpretation
t.table[, "BF.evidence"] <- sapply(t.table$BF, function(x) BF.evidence(x)) # Add column with interpretation

## FDR-correction
t.table[, "p.value.adj"] <- p.adjust(t.table[, "p.value"], method = "fdr", n = length(t.table[, "p.value"])) # Do fdr-correction
t.table[, "p.adj.sig"] <- sapply(t.table[, "p.value.adj"], function(x) p.value.sig(x)) # Add column with interpretation of adjusted significance
t.table[, "p.value.sig"] <- sapply(t.table[, "p.value"], function(x) p.value.sig(x)) # Add column with interpretation of significance

## Add Cohen's d
t.val <- t.table[, "test.stat"] # Extract t-value
t.val <- abs(t.val) # Calculate absolute t-value
d <- t2d(t.val, n=n1) # Calculate Cohen's d from t-value
t.table$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
t.table$cohen.d.mag <- sapply(t.table$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

# Order based on p-value
t.table[, "variable"] <- rownames(t.table) # Save variable name
t.table <- t.table[, c(ncol(t.table), seq(1:I(ncol(t.table)-1)))] # Put variable name column in front
t.table <- t.table %>% arrange(p.value) # Reorder rows

# Export results
write.xlsx(t.table, "diffzero_SET_CFC.outl.del.imp.xlsx")

# Remove temporary variables
remove(t.table)
remove(rsa.test)
remove(rsa.test.pooled)
remove(x)
remove(names)
remove(t)
remove(p.value.sig)
remove(n1)
remove(d)
remove(dfobtained)
remove(dftheory)
remove(i)
remove(j)
remove(m)
remove(subdata)
remove(meanobtained)
remove(meantheory)
remove(semobtained)
remove(sdtheory)
remove(n.obs)
remove(est)
remove(data_theory)
remove(data_obtained)
remove(data_obtained_long)
remove(BF_test)
remove(BF)
remove(LSA_HSA_brief)
remove(t_est)
remove(t.val)
remove(BF_t)
remove(BF.evidence)
remove(cohen.d.magnitude)
remove(se)


# Location differences between frontal and parietal (preregistered) -----------------------------------------------------------------

## Packages
library(mice) # Imputation analysis
library(magrittr) # Piping
library(xlsx) # Exporting to Excel
library(MKmisc) # t-test for multiple imputation
library(miceadds) # For subsetting
library(BayesFactor) # For Bayes factors
library(psych) # For converting t-value to Cohen's d
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors


### Compare variables on sex
## Initialize datasets
data <- SET_CFC.outl.del.imp %$% 
  cbind.data.frame(RS_Frontal_Avg_dPAC_Z, RS_Frontal_Avg_AAC_R,
                   react_Frontal_Avg_dPAC_Z, react_Frontal_Avg_AAC_R,
                   RS_Parietal_Avg_dPAC_Z, RS_Parietal_Avg_AAC_R,
                   react_Parietal_Avg_dPAC_Z, react_Parietal_Avg_AAC_R)

## T-tests
# Initialize variables
t <- list(NA) # Saves all t-test results
b <- list(NA) # Saves all Bayes factor results
# Do a t-test for all variables to compare males and females (removing the first variable 'Sex')
for (x in 1:I( ncol(data[["analyses"]][[1]])/2 )) {
  t[[x]] <- mi.t.test(data$analyses, 
                      x = colnames(data[["analyses"]][[1]])[[x]], 
                      y = colnames(data[["analyses"]][[1]])[[x+ I(ncol(data[["analyses"]][[1]])/2) ]],
                      paired = TRUE)
  #t[[x]]["data.name"] <- colnames(data[["analyses"]][[1]])[[x]]
  ## Bayes factor tests
  # Dataset of the first group
  n <- nrow(data[["analyses"]][[1]]) # Number of observations of the first group
  # Get Bayes factor
  b[[x]] <- ttest.tstat(t[[x]][["statistic"]][["t"]], n1 = n, n2 = n, simple = TRUE)
}

## Put the t-test results in a table
# Extract results from list
t.table <- sapply(t, function(x) {
  c(test = x$method,
    test.stat = x$statistic[["t"]],
    df = x$parameter[["df"]],
    p.value = x$p.value)
})

# Save the results as a dataframe
t.table <- as.data.frame(t.table)
names <- sapply(t, function(x) {c(name = x$data.name)}) # Get the variable names
colnames(t.table) <- as.character(names) # Set the columns as variable names
t.table <- t(t.table) %>% as.data.frame(stringsAsFactors = FALSE) # Transpose the dataframe
t.table[, 2:4] <- lapply(t.table[, 2:4], function(x) as.numeric(as.character(x))) %>% 
  lapply(function(x) round(x, 5)) # Set the numeric columns to class numeric and round to 5 decimals
t.table$BF <- b # Add the Bayes factor variable to the table
t.table[, 5] <- t.table[, 5] %>% as.numeric() %>% 
  round(5) # Set the BF column to class numeric and round to 5 decimals

## Add column with Bayes factor interpretation
t.table[, "BF evidence"] <- sapply(t.table[, "BF"], function(x) BF.evidence(x))

# Add Cohen's d
t.val <- t.table[, "test.stat"] # Extract t-value
t.val <- abs(t.val) # Calculate absolute t-value
d <- t2d(t.val, n=n, n2=n) # Calculate Cohen's d from t-value
t.table$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
t.table$cohen.d.mag <- sapply(t.table$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

## Adjust p-values with fdr-correction
t.table[, "p.value.adj"] <- p.adjust(t.table[, "p.value"], method = "fdr", n = length(t.table[, "p.value"])) # Do fdr-correction

## Check significance
t.table[, "p.adj.sig"] <- sapply(t.table[, "p.value.adj"], function(x) p.value.sig(x)) # Add column with adjusted significance
t.table[, "p.value.sig"] <- sapply(t.table[, "p.value"], function(x) p.value.sig(x)) # Add column with adjusted significance

# Order based on p-value
t.table[, "variable"] <- rownames(t.table) # Save variable name
t.table <- t.table[, c(ncol(t.table), seq(1:I(ncol(t.table)-1)))] # Put variable name column in front
t.table <- t.table %>% arrange(p.value) # Reorder rows

## Save the results
write.xlsx(t.table, "diffLocation.FrontalParietalCFC.xlsx")

# Remove temporary variables
remove(t.table)
remove(x)
remove(names)
remove(data)
remove(t)
remove(b)
remove(n)
remove(BF.evidence)
remove(cohen.d.magnitude)
remove(p.value.sig)
remove(d)
remove(t.val)


# CFC correlations (preregistered) ------------------------------------------------------------

# Packages
library(mice) # Multiple imputation functions
library(dplyr) # Data transformation
library(magrittr) # Piping
library(xlsx) # Exporting to Excel
library(miceadds) # Multiple imputation correlation
library(psych) # For converting rho to Cohen's d
library(BayesFactor) # For Bayesian statistics
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors


# Load data
load("SET_CFC.outl.del.imp.RData")


## Correlations
corr <- micombine.cor(mi.res = SET_CFC.outl.del.imp, variables = 
                        c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                          "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                          "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                          "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                          "LSAS", "Anx.1", "anx.react",
                          "PEP.2", "pep.react",
                          "RSA.2", "rsa.react",
                          "RR.2", "rr.react",
                          "Cortisol.1.log", "cort.react"))
# Remove double rows (second half)
corr <- corr[c(1:I(nrow(corr) / 2)), ]

# Remove everything but correlations involving PAC/AAC in either one but not both columns
corr <- with(corr, corr[ (grepl( "_Avg_", variable1) | grepl( "_Avg_", variable2)) 
                         &
                           !(grepl( "_Avg_", variable1) & grepl( "_Avg_", variable2))
                         , ])
corr[["variable1"]] <- as.character(corr[["variable1"]]) # Remove factors
corr[["variable2"]] <- as.character(corr[["variable2"]]) # Remove factors


## Calculate BayesFactors
# Select data
Dataset <- mice::complete(SET_CFC.outl.del.imp, action = "long")
Dataset <- select(Dataset, c(".imp", "RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                             "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                             "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                             "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                             "LSAS", "Anx.1", "anx.react",
                             "PEP.2", "pep.react",
                             "RSA.2", "rsa.react",
                             "RR.2", "rr.react",
                             "Cortisol.1.log", "cort.react"))
colnames(Dataset)[1] <- "imp"
# Initialize variables
m <- SET_CFC.outl.del.imp$m # Number of imputed datasets
corrBF <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the BayesFactors per imputed dataset
n.obs <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the number of observations
correst <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the variance of the variables per imputed dataset
est <- matrix(NA, nrow = nrow(corr), ncol = 1) # Matrix to put the pooled estimates
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed datasets
  subdata <- Dataset %>% filter(imp == j) %>% select(-imp) %>% as.matrix() # Select imputed dataset
  for (i in 1:nrow(corr)) { # For all variables in the data
    BF <- correlationBF(y = subdata[, corr[i, "variable1"]], x = subdata[, corr[i, "variable2"]]) # Calculate BayesFactor
    corrBF[i, j] <- extractBF(BF, onlybf = TRUE) # Extract only the BayesFactor
    n.obs[i, j] <- length(subdata[, corr[i, "variable1"]]) # Calculate sample size
    correst[i, j] <- mean( c(var(subdata[, corr[i, "variable1"]]), var(subdata[, corr[i, "variable2"]])) ) / n.obs[i, j] # The standard error of the estimate (necessary for pooling)
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:nrow(corr)) { # For every variable
  est[i, 1] <- pool.scalar(corrBF[i, ], correst[i, ], n = n.obs[i,], k = 1)[["qbar"]] %>% unlist() %>% round(3)
}

# Add bayes factors to dataframe
corr[, "BF"] <- est
# Add column with Bayes factor interpretation
corr[, "BF.evidence"] <- sapply(corr$BF, function(x) BF.evidence(x)) # Add column with interpretation

### Partial correlations - rsa with rr
## For baseline
corr2 <- micombine.cor(mi.res = SET_CFC.outl.del.imp, variables = 
                        c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                          "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                          "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                          "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                          "RSA.2"), partial = ~"RR.2")
# Remove double rows (second half)
corr2 <- corr2[c(1:I(nrow(corr2) / 2)), ]
# Remove factors
corr2[["variable1"]] <- as.character(corr2[["variable1"]])
corr2[["variable2"]] <- as.character(corr2[["variable2"]])
# Remove every row except for the correlations involving rsa in either one but not both columns
corr2 <- with(corr2, corr2[ (grepl( "RSA.2", variable1) | grepl( "RSA.2", variable2)) 
                         &
                           !(grepl( "RSA.2", variable1) & grepl( "RSA.2", variable2))
                         , ])
# Rename rsa to rsa (rr)
corr2[, "variable1"] <- sub("RSA.2","RSA.2 (RR)" , corr2[, "variable1"])
corr2[, "variable2"] <- sub("RSA.2","RSA.2 (RR)" , corr2[, "variable2"])
# Add Bayefactors columns
corr2[, "BF"] <- NA
corr2[, "BF.evidence"] <- NA
# Bind correlations and partial correlations together
corr <- rbind(corr, corr2)

## For reactivity
corr2 <- micombine.cor(mi.res = SET_CFC.outl.del.imp, variables = 
                         c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                           "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                           "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                           "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                           "rsa.react"), partial = ~"rr.react")
# Remove double rows (second half)
corr2 <- corr2[c(1:I(nrow(corr2) / 2)), ]
# Remove factors
corr2[["variable1"]] <- as.character(corr2[["variable1"]])
corr2[["variable2"]] <- as.character(corr2[["variable2"]])
# Remove every row except for the correlations involving rsa in either one but not both columns
corr2 <- with(corr2, corr2[ (grepl( "rsa.react", variable1) | grepl( "rsa.react", variable2)) 
                            &
                              !(grepl( "rsa.react", variable1) & grepl( "rsa.react", variable2))
                            , ])
# Rename rsa to rsa (rr)
corr2[, "variable1"] <- sub("rsa.react","rsa.react (RR)" , corr2[, "variable1"])
corr2[, "variable2"] <- sub("rsa.react","rsa.react (RR)" , corr2[, "variable2"])
# Add Bayefactors columns
corr2[, "BF"] <- NA
corr2[, "BF.evidence"] <- NA
# Bind correlations and partial correlations together
corr <- rbind(corr, corr2)

## Structure correlation table
rownames(corr) <- NULL # Reset rownames
drops <- c("rse", "fisher_r", "fisher_rse", "fmi", "t", "lower95", "upper95") # Select unnecessary columns to remove
corr <- corr[ , !(names(corr) %in% drops)] # Remove unnecessary columns

# Do fdr-correction
corr[, "p.value.adj"] <- p.adjust(corr[, "p"], method = "fdr", n = nrow(corr))
# Check significance
corr[, "p.adj.sig"] <- sapply(corr[, "p.value.adj"], function(x) p.value.sig(x)) # Corrected
corr[, "p.value.sig"] <- sapply(corr[, "p"], function(x) p.value.sig(x)) # Uncorrected

# Add Cohen's d
rho <- corr[, "r"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
corr$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
corr$cohen.d.mag <- sapply(corr$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d


# Order based on p-value
corr <- corr %>% arrange(p)


## Save results
# base
write.xlsx(corr, "Correlations_SET_CFC.xlsx")


## Remove temporary variables
remove(corr)
remove(corr2)
remove(p.value.sig)
remove(subdata)
remove(d)
remove(drops)
remove(i)
remove(j)
remove(m)
remove(Dataset)
remove(correst)
remove(corrBF)
remove(est)
remove(n.obs)
remove(rho)
remove(BF.evidence)
remove(BF)
remove(cohen.d.magnitude)


# CFC correlations per sex (exploratory) ------------------------------------------------------------
# Packages
library(mice) # Multiple imputation functions
library(dplyr) # Data transformation
library(magrittr) # Piping
library(xlsx) # Exporting to Excel
library(miceadds) # Multiple imputation correlation
library(psych) # For converting rho to Cohen's d
library(BayesFactor) # For Bayesian statistics
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors


# Load data
load("SET_CFC.outl.del.imp.RData")

### Men
## Select male subset
maleData <- subset_datlist(SET_CFC.outl.del.imp, 
                       subset = SET_CFC.outl.del.imp[[1]]$Sex == "Male",
                       select=c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                                "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                                "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                                "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                                "LSAS", "Anx.1", "anx.react",
                                "PEP.2", "pep.react",
                                "RSA.2", "rsa.react",
                                "RR.2", "rr.react",
                                "Cortisol.1.log", "cort.react"),
                       toclass="mids")

## Do correlation
corr <- micombine.cor(mi.res = maleData)
## Remove double rows (second half)
corr <- corr[c(1:I(nrow(corr) / 2)), ]
# Remove everything but correlations involving PAC/AAC in either one but not both columns
corr <- with(corr, corr[ (grepl( "_Avg_", variable1) | grepl( "_Avg_", variable2)) 
                         &
                           !(grepl( "_Avg_", variable1) & grepl( "_Avg_", variable2))
                         , ])
# Remove factors
corr[["variable1"]] <- as.character(corr[["variable1"]]) 
corr[["variable2"]] <- as.character(corr[["variable2"]])

## Calculate BayesFactors
# Select data
Dataset <- mice::complete(maleData, action = "long")
colnames(Dataset)[1] <- "imp" # Rename imp column
drops <- c(".id") # Select unnecessary columns to remove
Dataset <- Dataset[ , !(names(Dataset) %in% drops)] # Remove unnecessary columns
# Initialize variables
m <- SET_CFC.outl.del.imp$m # Number of imputed datasets
corrBF <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the BayesFactors per imputed dataset
n.obs <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the number of observations
correst <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the variance of the variables per imputed dataset
est <- matrix(NA, nrow = nrow(corr), ncol = 1) # Matrix to put the pooled estimates
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed datasets
  subdata <- Dataset %>% filter(imp == j) %>% select(-imp) %>% as.matrix() # Select imputed dataset
  for (i in 1:nrow(corr)) { # For all variables in the data
    BF <- correlationBF(y = subdata[, corr[i, "variable1"]], x = subdata[, corr[i, "variable2"]]) # Calculate BayesFactor
    corrBF[i, j] <- extractBF(BF, onlybf = TRUE) # Extract only the BayesFactor
    n.obs[i, j] <- length(subdata[, corr[i, "variable1"]]) # Calculate sample size
    correst[i, j] <- mean( c(var(subdata[, corr[i, "variable1"]]), var(subdata[, corr[i, "variable2"]])) ) / n.obs[i, j] # The standard error of the estimate (necessary for pooling)
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:nrow(corr)) { # For every variable
  est[i, 1] <- pool.scalar(corrBF[i, ], correst[i, ], n = n.obs[i,], k = 1)[["qbar"]] %>% unlist() %>% round(3)
}
# Add bayes factors to dataframe
corr[, "BF"] <- est
# Add column with Bayes factor interpretation
corr[, "BF.evidence"] <- sapply(corr$BF, function(x) BF.evidence(x)) # Add column with interpretation


### Partial correlations - rsa with rr
## For baseline
corr2 <- micombine.cor(mi.res = maleData, variables = 
                         c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                           "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                           "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                           "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                           "RSA.2"), partial = ~"RR.2")
# Remove double rows (second half)
corr2 <- corr2[c(1:I(nrow(corr2) / 2)), ]
# Remove factors
corr2[["variable1"]] <- as.character(corr2[["variable1"]])
corr2[["variable2"]] <- as.character(corr2[["variable2"]])
# Remove every row except for the correlations involving rsa in either one but not both columns
corr2 <- with(corr2, corr2[ (grepl( "RSA.2", variable1) | grepl( "RSA.2", variable2)) 
                            &
                              !(grepl( "RSA.2", variable1) & grepl( "RSA.2", variable2))
                            , ])
# Rename rsa to rsa (rr)
corr2[, "variable1"] <- sub("RSA.2","RSA.2 (RR)" , corr2[, "variable1"])
corr2[, "variable2"] <- sub("RSA.2","RSA.2 (RR)" , corr2[, "variable2"])
# Add Bayefactors columns
corr2[, "BF"] <- NA
corr2[, "BF.evidence"] <- NA
# Bind correlations and partial correlations together
corr <- rbind(corr, corr2)


## For reactivity
corr2 <- micombine.cor(mi.res = maleData, variables = 
                         c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                           "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                           "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                           "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                           "rsa.react"), partial = ~"rr.react")
# Remove double rows (second half)
corr2 <- corr2[c(1:I(nrow(corr2) / 2)), ]
# Remove factors
corr2[["variable1"]] <- as.character(corr2[["variable1"]])
corr2[["variable2"]] <- as.character(corr2[["variable2"]])
# Remove every row except for the correlations involving rsa in either one but not both columns
corr2 <- with(corr2, corr2[ (grepl( "rsa.react", variable1) | grepl( "rsa.react", variable2)) 
                            &
                              !(grepl( "rsa.react", variable1) & grepl( "rsa.react", variable2))
                            , ])
# Rename rsa to rsa (rr)
corr2[, "variable1"] <- sub("rsa.react","rsa.react (RR)" , corr2[, "variable1"])
corr2[, "variable2"] <- sub("rsa.react","rsa.react (RR)" , corr2[, "variable2"])
# Add Bayefactors columns
corr2[, "BF"] <- NA
corr2[, "BF.evidence"] <- NA
# Bind correlations and partial correlations together
corr <- rbind(corr, corr2)


# Restructure correlation matrix
rownames(corr) <- NULL # Reset rownames
drops <- c("rse", "fisher_r", "fisher_rse", "fmi", "t", "lower95", "upper95") # Select unnecessary columns to remove
corr <- corr[ , !(names(corr) %in% drops)] # Remove unnecessary columns
corr[, "Sex"] <- "Male" # Add 'male' indicator

## Add Cohen's d
rho <- corr[, "r"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
corr$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
corr$cohen.d.mag <- sapply(corr$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

## Save to ultimate dataframe
corrs <- corr


### Women
## Select female subset
femaleData <- subset_datlist(SET_CFC.outl.del.imp, 
                           subset = SET_CFC.outl.del.imp[[1]]$Sex == "Female",
                           select=c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                                    "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                                    "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                                    "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                                    "LSAS", "Anx.1", "anx.react",
                                    "PEP.2", "pep.react",
                                    "RSA.2", "rsa.react",
                                    "RR.2", "rr.react",
                                    "Cortisol.1.log", "cort.react"),
                           toclass="mids")

## Do correlation
corr <- micombine.cor(mi.res = femaleData)
## Remove double rows (second half)
corr <- corr[c(1:I(nrow(corr) / 2)), ]
# Remove everything but correlations involving PAC/AAC in either one but not both columns
corr <- with(corr, corr[ (grepl( "_Avg_", variable1) | grepl( "_Avg_", variable2)) 
                         &
                           !(grepl( "_Avg_", variable1) & grepl( "_Avg_", variable2))
                         , ])
# Remove factors
corr[["variable1"]] <- as.character(corr[["variable1"]]) 
corr[["variable2"]] <- as.character(corr[["variable2"]])

## Calculate BayesFactors
# Select data
Dataset <- mice::complete(femaleData, action = "long")
colnames(Dataset)[1] <- "imp" # Rename imp column
drops <- c(".id") # Select unnecessary columns to remove
Dataset <- Dataset[ , !(names(Dataset) %in% drops)] # Remove unnecessary columns
# Initialize variables
m <- SET_CFC.outl.del.imp$m # Number of imputed datasets
corrBF <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the BayesFactors per imputed dataset
n.obs <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the number of observations
correst <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the variance of the variables per imputed dataset
est <- matrix(NA, nrow = nrow(corr), ncol = 1) # Matrix to put the pooled estimates
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed datasets
  subdata <- Dataset %>% filter(imp == j) %>% select(-imp) %>% as.matrix() # Select imputed dataset
  for (i in 1:nrow(corr)) { # For all variables in the data
    BF <- correlationBF(y = subdata[, corr[i, "variable1"]], x = subdata[, corr[i, "variable2"]]) # Calculate BayesFactor
    corrBF[i, j] <- extractBF(BF, onlybf = TRUE) # Extract only the BayesFactor
    n.obs[i, j] <- length(subdata[, corr[i, "variable1"]]) # Calculate sample size
    correst[i, j] <- mean( c(var(subdata[, corr[i, "variable1"]]), var(subdata[, corr[i, "variable2"]])) ) / n.obs[i, j] # The standard error of the estimate (necessary for pooling)
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:nrow(corr)) { # For every variable
  est[i, 1] <- pool.scalar(corrBF[i, ], correst[i, ], n = n.obs[i,], k = 1)[["qbar"]] %>% unlist() %>% round(3)
}
# Add bayes factors to dataframe
corr[, "BF"] <- est
# Add column with Bayes factor interpretation
corr[, "BF.evidence"] <- sapply(corr$BF, function(x) BF.evidence(x)) # Add column with interpretation


### Partial correlations - rsa with rr
## For baseline
corr2 <- micombine.cor(mi.res = femaleData, variables = 
                         c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                           "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                           "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                           "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                           "RSA.2"), partial = ~"RR.2")
# Remove double rows (second half)
corr2 <- corr2[c(1:I(nrow(corr2) / 2)), ]
# Remove factors
corr2[["variable1"]] <- as.character(corr2[["variable1"]])
corr2[["variable2"]] <- as.character(corr2[["variable2"]])
# Remove every row except for the correlations involving rsa in either one but not both columns
corr2 <- with(corr2, corr2[ (grepl( "RSA.2", variable1) | grepl( "RSA.2", variable2)) 
                            &
                              !(grepl( "RSA.2", variable1) & grepl( "RSA.2", variable2))
                            , ])
# Rename rsa to rsa (rr)
corr2[, "variable1"] <- sub("RSA.2","RSA.2 (RR)" , corr2[, "variable1"])
corr2[, "variable2"] <- sub("RSA.2","RSA.2 (RR)" , corr2[, "variable2"])
# Add Bayefactors columns
corr2[, "BF"] <- NA
corr2[, "BF.evidence"] <- NA
# Bind correlations and partial correlations together
corr <- rbind(corr, corr2)

## For reactivity
corr2 <- micombine.cor(mi.res = femaleData, variables = 
                         c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                           "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                           "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                           "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                           "rsa.react"), partial = ~"rr.react")
# Remove double rows (second half)
corr2 <- corr2[c(1:I(nrow(corr2) / 2)), ]
# Remove factors
corr2[["variable1"]] <- as.character(corr2[["variable1"]])
corr2[["variable2"]] <- as.character(corr2[["variable2"]])
# Remove every row except for the correlations involving rsa in either one but not both columns
corr2 <- with(corr2, corr2[ (grepl( "rsa.react", variable1) | grepl( "rsa.react", variable2)) 
                            &
                              !(grepl( "rsa.react", variable1) & grepl( "rsa.react", variable2))
                            , ])
# Rename rsa to rsa (rr)
corr2[, "variable1"] <- sub("rsa.react","rsa.react (RR)" , corr2[, "variable1"])
corr2[, "variable2"] <- sub("rsa.react","rsa.react (RR)" , corr2[, "variable2"])
# Add Bayefactors columns
corr2[, "BF"] <- NA
corr2[, "BF.evidence"] <- NA
# Bind correlations and partial correlations together
corr <- rbind(corr, corr2)


# Restructure correlation matrix
rownames(corr) <- NULL # Reset rownames
drops <- c("rse", "fisher_r", "fisher_rse", "fmi", "t", "lower95", "upper95") # Select unnecessary columns to remove
corr <- corr[ , !(names(corr) %in% drops)] # Remove unnecessary columns
corr[, "Sex"] <- "Female" # Add 'female' indicator

## Add Cohen's d
rho <- corr[, "r"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
corr$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
corr$cohen.d.mag <- sapply(corr$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

## Save to ultimate dataframe
corrs <- rbind(corrs, corr)

## Do fdr-correction
corrs[, "p.value.adj"] <- p.adjust(corrs[, "p"], method = "fdr", n = nrow(corrs))
# Check significance
corrs[, "p.adj.sig"] <- sapply(corrs[, "p.value.adj"], function(x) p.value.sig(x)) # corrsected
corrs[, "p.value.sig"] <- sapply(corrs[, "p"], function(x) p.value.sig(x)) # Uncorrsected


# Order based on p-value
corrs <- corrs %>% arrange(p)


## Save results
# base
write.xlsx(corrs, "Correlations.sex_SET_CFC.xlsx")


## Remove temporary variables
remove(corr)
remove(corr2)
remove(corrs)
remove(p.value.sig)
remove(subdata)
remove(d)
remove(drops)
remove(i)
remove(j)
remove(m)
remove(Dataset)
remove(correst)
remove(corrBF)
remove(est)
remove(n.obs)
remove(rho)
remove(BF.evidence)
remove(BF)
remove(cohen.d.magnitude)
remove(femaleData)
remove(maleData)



# CFC correlations per LSAS group (exploratory) ------------------------------------------------------------
# Packages
library(mice) # Multiple imputation functions
library(dplyr) # Data transformation
library(magrittr) # Piping
library(xlsx) # Exporting to Excel
library(miceadds) # Multiple imputation correlation
library(psych) # For converting rho to Cohen's d
library(BayesFactor) # For Bayesian statistics
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors


# Load data
load("SET_CFC.outl.del.imp.RData")

### Men
## Select male subset
lowData <- subset_datlist(SET_CFC.outl.del.imp, 
                          subset = SET_CFC.outl.del.imp[[1]]$LSAS_Split == "Low",
                          select=c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                                   "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                                   "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                                   "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                                   "LSAS", "Anx.1", "anx.react",
                                   "PEP.2", "pep.react",
                                   "RSA.2", "rsa.react",
                                   "RR.2", "rr.react",
                                   "Cortisol.1.log", "cort.react"),
                          toclass="mids")

## Do correlation
corr <- micombine.cor(mi.res = lowData)
## Remove double rows (second half)
corr <- corr[c(1:I(nrow(corr) / 2)), ]
# Remove everything but correlations involving PAC/AAC in either one but not both columns
corr <- with(corr, corr[ (grepl( "_Avg_", variable1) | grepl( "_Avg_", variable2)) 
                         &
                           !(grepl( "_Avg_", variable1) & grepl( "_Avg_", variable2))
                         , ])
# Remove factors
corr[["variable1"]] <- as.character(corr[["variable1"]]) 
corr[["variable2"]] <- as.character(corr[["variable2"]])

## Calculate BayesFactors
# Select data
Dataset <- mice::complete(lowData, action = "long")
colnames(Dataset)[1] <- "imp" # Rename imp column
drops <- c(".id") # Select unnecessary columns to remove
Dataset <- Dataset[ , !(names(Dataset) %in% drops)] # Remove unnecessary columns
# Initialize variables
m <- SET_CFC.outl.del.imp$m # Number of imputed datasets
corrBF <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the BayesFactors per imputed dataset
n.obs <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the number of observations
correst <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the variance of the variables per imputed dataset
est <- matrix(NA, nrow = nrow(corr), ncol = 1) # Matrix to put the pooled estimates
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed datasets
  subdata <- Dataset %>% filter(imp == j) %>% select(-imp) %>% as.matrix() # Select imputed dataset
  for (i in 1:nrow(corr)) { # For all variables in the data
    BF <- correlationBF(y = subdata[, corr[i, "variable1"]], x = subdata[, corr[i, "variable2"]]) # Calculate BayesFactor
    corrBF[i, j] <- extractBF(BF, onlybf = TRUE) # Extract only the BayesFactor
    n.obs[i, j] <- length(subdata[, corr[i, "variable1"]]) # Calculate sample size
    correst[i, j] <- mean( c(var(subdata[, corr[i, "variable1"]]), var(subdata[, corr[i, "variable2"]])) ) / n.obs[i, j] # The standard error of the estimate (necessary for pooling)
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:nrow(corr)) { # For every variable
  est[i, 1] <- pool.scalar(corrBF[i, ], correst[i, ], n = n.obs[i,], k = 1)[["qbar"]] %>% unlist() %>% round(3)
}
# Add bayes factors to dataframe
corr[, "BF"] <- est
# Add column with Bayes factor interpretation
corr[, "BF.evidence"] <- sapply(corr$BF, function(x) BF.evidence(x)) # Add column with interpretation


### Partial correlations - rsa with rr
## For baseline
corr2 <- micombine.cor(mi.res = lowData, variables = 
                         c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                           "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                           "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                           "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                           "RSA.2"), partial = ~"RR.2")
# Remove double rows (second half)
corr2 <- corr2[c(1:I(nrow(corr2) / 2)), ]
# Remove factors
corr2[["variable1"]] <- as.character(corr2[["variable1"]])
corr2[["variable2"]] <- as.character(corr2[["variable2"]])
# Remove every row except for the correlations involving rsa in either one but not both columns
corr2 <- with(corr2, corr2[ (grepl( "RSA.2", variable1) | grepl( "RSA.2", variable2)) 
                            &
                              !(grepl( "RSA.2", variable1) & grepl( "RSA.2", variable2))
                            , ])
# Rename rsa to rsa (rr)
corr2[, "variable1"] <- sub("RSA.2","RSA.2 (RR)" , corr2[, "variable1"])
corr2[, "variable2"] <- sub("RSA.2","RSA.2 (RR)" , corr2[, "variable2"])
# Add Bayefactors columns
corr2[, "BF"] <- NA
corr2[, "BF.evidence"] <- NA
# Bind correlations and partial correlations together
corr <- rbind(corr, corr2)


## For reactivity
corr2 <- micombine.cor(mi.res = lowData, variables = 
                         c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                           "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                           "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                           "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                           "rsa.react"), partial = ~"rr.react")
# Remove double rows (second half)
corr2 <- corr2[c(1:I(nrow(corr2) / 2)), ]
# Remove factors
corr2[["variable1"]] <- as.character(corr2[["variable1"]])
corr2[["variable2"]] <- as.character(corr2[["variable2"]])
# Remove every row except for the correlations involving rsa in either one but not both columns
corr2 <- with(corr2, corr2[ (grepl( "rsa.react", variable1) | grepl( "rsa.react", variable2)) 
                            &
                              !(grepl( "rsa.react", variable1) & grepl( "rsa.react", variable2))
                            , ])
# Rename rsa to rsa (rr)
corr2[, "variable1"] <- sub("rsa.react","rsa.react (RR)" , corr2[, "variable1"])
corr2[, "variable2"] <- sub("rsa.react","rsa.react (RR)" , corr2[, "variable2"])
# Add Bayefactors columns
corr2[, "BF"] <- NA
corr2[, "BF.evidence"] <- NA
# Bind correlations and partial correlations together
corr <- rbind(corr, corr2)


# Restructure correlation matrix
rownames(corr) <- NULL # Reset rownames
drops <- c("rse", "fisher_r", "fisher_rse", "fmi", "t", "lower95", "upper95") # Select unnecessary columns to remove
corr <- corr[ , !(names(corr) %in% drops)] # Remove unnecessary columns
corr[, "LSAS"] <- "Low" # Add 'male' indicator

## Add Cohen's d
rho <- corr[, "r"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
corr$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
corr$cohen.d.mag <- sapply(corr$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

## Save to ultimate dataframe
corrs <- corr


### Women
## Select female subset
highData <- subset_datlist(SET_CFC.outl.del.imp, 
                           subset = SET_CFC.outl.del.imp[[1]]$LSAS_Split == "High",
                           select=c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                                    "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                                    "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                                    "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                                    "LSAS", "Anx.1", "anx.react",
                                    "PEP.2", "pep.react",
                                    "RSA.2", "rsa.react",
                                    "RR.2", "rr.react",
                                    "Cortisol.1.log", "cort.react"),
                           toclass="mids")

## Do correlation
corr <- micombine.cor(mi.res = highData)
## Remove double rows (second half)
corr <- corr[c(1:I(nrow(corr) / 2)), ]
# Remove everything but correlations involving PAC/AAC in either one but not both columns
corr <- with(corr, corr[ (grepl( "_Avg_", variable1) | grepl( "_Avg_", variable2)) 
                         &
                           !(grepl( "_Avg_", variable1) & grepl( "_Avg_", variable2))
                         , ])
# Remove factors
corr[["variable1"]] <- as.character(corr[["variable1"]]) 
corr[["variable2"]] <- as.character(corr[["variable2"]])

## Calculate BayesFactors
# Select data
Dataset <- mice::complete(highData, action = "long")
colnames(Dataset)[1] <- "imp" # Rename imp column
drops <- c(".id") # Select unnecessary columns to remove
Dataset <- Dataset[ , !(names(Dataset) %in% drops)] # Remove unnecessary columns
# Initialize variables
m <- SET_CFC.outl.del.imp$m # Number of imputed datasets
corrBF <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the BayesFactors per imputed dataset
n.obs <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the number of observations
correst <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the variance of the variables per imputed dataset
est <- matrix(NA, nrow = nrow(corr), ncol = 1) # Matrix to put the pooled estimates
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed datasets
  subdata <- Dataset %>% filter(imp == j) %>% select(-imp) %>% as.matrix() # Select imputed dataset
  for (i in 1:nrow(corr)) { # For all variables in the data
    BF <- correlationBF(y = subdata[, corr[i, "variable1"]], x = subdata[, corr[i, "variable2"]]) # Calculate BayesFactor
    corrBF[i, j] <- extractBF(BF, onlybf = TRUE) # Extract only the BayesFactor
    n.obs[i, j] <- length(subdata[, corr[i, "variable1"]]) # Calculate sample size
    correst[i, j] <- mean( c(var(subdata[, corr[i, "variable1"]]), var(subdata[, corr[i, "variable2"]])) ) / n.obs[i, j] # The standard error of the estimate (necessary for pooling)
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:nrow(corr)) { # For every variable
  est[i, 1] <- pool.scalar(corrBF[i, ], correst[i, ], n = n.obs[i,], k = 1)[["qbar"]] %>% unlist() %>% round(3)
}
# Add bayes factors to dataframe
corr[, "BF"] <- est
# Add column with Bayes factor interpretation
corr[, "BF.evidence"] <- sapply(corr$BF, function(x) BF.evidence(x)) # Add column with interpretation


### Partial correlations - rsa with rr
## For baseline
corr2 <- micombine.cor(mi.res = highData, variables = 
                         c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                           "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                           "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                           "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                           "RSA.2"), partial = ~"RR.2")
# Remove double rows (second half)
corr2 <- corr2[c(1:I(nrow(corr2) / 2)), ]
# Remove factors
corr2[["variable1"]] <- as.character(corr2[["variable1"]])
corr2[["variable2"]] <- as.character(corr2[["variable2"]])
# Remove every row except for the correlations involving rsa in either one but not both columns
corr2 <- with(corr2, corr2[ (grepl( "RSA.2", variable1) | grepl( "RSA.2", variable2)) 
                            &
                              !(grepl( "RSA.2", variable1) & grepl( "RSA.2", variable2))
                            , ])
# Rename rsa to rsa (rr)
corr2[, "variable1"] <- sub("RSA.2","RSA.2 (RR)" , corr2[, "variable1"])
corr2[, "variable2"] <- sub("RSA.2","RSA.2 (RR)" , corr2[, "variable2"])
# Add Bayefactors columns
corr2[, "BF"] <- NA
corr2[, "BF.evidence"] <- NA
# Bind correlations and partial correlations together
corr <- rbind(corr, corr2)

## For reactivity
corr2 <- micombine.cor(mi.res = highData, variables = 
                         c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                           "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                           "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                           "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                           "rsa.react"), partial = ~"rr.react")
# Remove double rows (second half)
corr2 <- corr2[c(1:I(nrow(corr2) / 2)), ]
# Remove factors
corr2[["variable1"]] <- as.character(corr2[["variable1"]])
corr2[["variable2"]] <- as.character(corr2[["variable2"]])
# Remove every row except for the correlations involving rsa in either one but not both columns
corr2 <- with(corr2, corr2[ (grepl( "rsa.react", variable1) | grepl( "rsa.react", variable2)) 
                            &
                              !(grepl( "rsa.react", variable1) & grepl( "rsa.react", variable2))
                            , ])
# Rename rsa to rsa (rr)
corr2[, "variable1"] <- sub("rsa.react","rsa.react (RR)" , corr2[, "variable1"])
corr2[, "variable2"] <- sub("rsa.react","rsa.react (RR)" , corr2[, "variable2"])
# Add Bayefactors columns
corr2[, "BF"] <- NA
corr2[, "BF.evidence"] <- NA
# Bind correlations and partial correlations together
corr <- rbind(corr, corr2)


# Restructure correlation matrix
rownames(corr) <- NULL # Reset rownames
drops <- c("rse", "fisher_r", "fisher_rse", "fmi", "t", "lower95", "upper95") # Select unnecessary columns to remove
corr <- corr[ , !(names(corr) %in% drops)] # Remove unnecessary columns
corr[, "LSAS"] <- "High" # Add 'female' indicator

## Add Cohen's d
rho <- corr[, "r"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
corr$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
corr$cohen.d.mag <- sapply(corr$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

## Save to ultimate dataframe
corrs <- rbind(corrs, corr)

## Do fdr-correction
corrs[, "p.value.adj"] <- p.adjust(corrs[, "p"], method = "fdr", n = nrow(corrs))
# Check significance
corrs[, "p.adj.sig"] <- sapply(corrs[, "p.value.adj"], function(x) p.value.sig(x)) # corrsected
corrs[, "p.value.sig"] <- sapply(corrs[, "p"], function(x) p.value.sig(x)) # Uncorrsected


# Order based on p-value
corrs <- corrs %>% arrange(p)


## Save results
# base
write.xlsx(corrs, "Correlations.LSAS_SET_CFC.xlsx")


## Remove temporary variables
remove(corr)
remove(corr2)
remove(corrs)
remove(p.value.sig)
remove(subdata)
remove(d)
remove(drops)
remove(i)
remove(j)
remove(m)
remove(Dataset)
remove(correst)
remove(corrBF)
remove(est)
remove(n.obs)
remove(rho)
remove(BF.evidence)
remove(BF)
remove(cohen.d.magnitude)
remove(highData)
remove(lowData)


# Correlations AAC with PAC reactivity (exploratory) ------------------------------------------------------------

# Packages
library(mice) # Multiple imputation functions
library(dplyr) # Data transformation
library(magrittr) # Piping
library(xlsx) # Exporting to Excel
library(miceadds) # Multiple imputation correlation
library(psych) # For converting rho to Cohen's d
library(BayesFactor) # For Bayesian statistics
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors


# Load data
load("SET_CFC.outl.del.imp.RData")


## Correlations
corr <- micombine.cor(mi.res = SET_CFC.outl.del.imp, variables = 
                        c("react_Frontal_Avg_dPAC_Z",
                          "react_Frontal_Avg_AAC_R",
                          "react_Parietal_Avg_dPAC_Z",
                          "react_Parietal_Avg_AAC_R"))
# Remove double rows (second half)
corr <- corr[c(1:I(nrow(corr) / 2)), ]


## Calculate BayesFactors
# Select data
Dataset <- mice::complete(SET_CFC.outl.del.imp, action = "long")
Dataset <- select(Dataset, c(".imp", "react_Frontal_Avg_dPAC_Z",
                             "react_Frontal_Avg_AAC_R",
                             "react_Parietal_Avg_dPAC_Z",
                             "react_Parietal_Avg_AAC_R"))
colnames(Dataset)[1] <- "imp"
# Initialize variables
m <- SET_CFC.outl.del.imp$m # Number of imputed datasets
corrBF <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the BayesFactors per imputed dataset
n.obs <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the number of observations
correst <- matrix(NA, nrow = nrow(corr), ncol = m) # Matrix to put the variance of the variables per imputed dataset
est <- matrix(NA, nrow = nrow(corr), ncol = 1) # Matrix to put the pooled estimates
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed datasets
  subdata <- Dataset %>% filter(imp == j) %>% select(-imp) %>% as.matrix() # Select imputed dataset
  for (i in 1:nrow(corr)) { # For all variables in the data
    BF <- correlationBF(y = subdata[, corr[i, "variable1"]], x = subdata[, corr[i, "variable2"]]) # Calculate BayesFactor
    corrBF[i, j] <- extractBF(BF, onlybf = TRUE) # Extract only the BayesFactor
    n.obs[i, j] <- length(subdata[, corr[i, "variable1"]]) # Calculate sample size
    correst[i, j] <- mean( c(var(subdata[, corr[i, "variable1"]]), var(subdata[, corr[i, "variable2"]])) ) / n.obs[i, j] # The standard error of the estimate (necessary for pooling)
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:nrow(corr)) { # For every variable
  est[i, 1] <- pool.scalar(corrBF[i, ], correst[i, ], n = n.obs[i,], k = 1)[["qbar"]] %>% unlist() %>% round(3)
}

# Add bayes factors to dataframe
corr[, "BF"] <- est
# Add column with Bayes factor interpretation
corr[, "BF.evidence"] <- sapply(corr$BF, function(x) BF.evidence(x)) # Add column with interpretation

## Structure correlation table
rownames(corr) <- NULL # Reset rownames
drops <- c("rse", "fisher_r", "fisher_rse", "fmi", "t", "lower95", "upper95") # Select unnecessary columns to remove
corr <- corr[ , !(names(corr) %in% drops)] # Remove unnecessary columns

# Do fdr-correction
corr[, "p.value.adj"] <- p.adjust(corr[, "p"], method = "fdr", n = nrow(corr))
# Check significance
corr[, "p.adj.sig"] <- sapply(corr[, "p.value.adj"], function(x) p.value.sig(x)) # Corrected
corr[, "p.value.sig"] <- sapply(corr[, "p"], function(x) p.value.sig(x)) # Uncorrected

# Add Cohen's d
rho <- corr[, "r"] # Extract rho
d <-  r2d(rho) # Calculate Cohen's d from rho
corr$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
corr$cohen.d.mag <- sapply(corr$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d


# Order based on p-value
corr <- corr %>% arrange(p)


## Save results
# base
write.xlsx(corr, "SupplCorrelations_PAC_AAC.xlsx")


## Remove temporary variables
remove(corr)
remove(p.value.sig)
remove(subdata)
remove(d)
remove(drops)
remove(i)
remove(j)
remove(m)
remove(Dataset)
remove(correst)
remove(corrBF)
remove(est)
remove(n.obs)
remove(rho)
remove(BF.evidence)
remove(BF)
remove(cohen.d.magnitude)


# Heatmap correlations (exloratory) -----------------------------------------------------
## Packages
library(ggplot2) # For plotting
library(magrittr) # For piping
library(dplyr) # For data manipulation
library(reshape2) # For melt function
library(tidyr) # For spread function
library(miceadds) # Multiple imputation correlation

## Create correlation matrix
data <- SET_CFC.outl.del.imp %$% 
  cbind.data.frame(RS_Frontal_Avg_dPAC_Z, react_Frontal_Avg_dPAC_Z,
                   RS_Parietal_Avg_dPAC_Z, react_Parietal_Avg_dPAC_Z,
                   RS_Frontal_Avg_AAC_R, react_Frontal_Avg_AAC_R,
                   RS_Parietal_Avg_AAC_R, react_Parietal_Avg_AAC_R,
                   LSAS, Anx.1, anx.react, 
                   PEP.2, pep.react,
                   RSA.2, rsa.react,
                   RR.2, rr.react,
                   Cortisol.1.log, cort.react)

# Correlation coefficients
corr <- micombine.cor(mi.res = data$analyses) # Calculate correlation
cormat <- attr(corr,"r_matrix") # Extract matrix
cormat <- cormat %>% round(2) # Round values
# Set the variable names to be more plot-readable
colnames(cormat) <- c("Frontal baseline PAC", "Frontal PAC reactivity",
                      "Parietal baseline PAC", "Parietal PAC reactivity",
                      "Frontal baseline AAC", "Frontal AAC reactivity",
                      "Parietal baseline AAC", "Parietal AAC reactivity",
                      "Trait social anxiety", 
                      "Baseline state anxiety", "State anxiety reactivity",
                      "Baseline PEP", "PEP reactivity", 
                      "Baseline RSA", "RSA reactivity", 
                      "Baseline RR", "RR reactivity", 
                      "Baseline cortisol", "Cortisol reactivity")
rownames(cormat) <- c("Frontal baseline PAC", "Frontal PAC reactivity",
                      "Parietal baseline PAC", "Parietal PAC reactivity",
                      "Frontal baseline AAC", "Frontal AAC reactivity",
                      "Parietal baseline AAC", "Parietal AAC reactivity",
                      "Trait social anxiety", 
                      "Baseline state anxiety", "State anxiety reactivity",
                      "Baseline PEP", "PEP reactivity", 
                      "Baseline RSA", "RSA reactivity", 
                      "Baseline RR", "RR reactivity", 
                      "Baseline cortisol", "Cortisol reactivity")
# Remove everything under the diagonal
cormat[lower.tri(cormat, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat <- melt(cormat, na.rm = TRUE)
rownames(melted_cormat) <- NULL # Reset rownames

# Extract p-values to highlight the significant correlations
cormat_p <- corr # Save the correlation results
cormat_p <- select(cormat_p, c(variable1, variable2, p)) # Select only the variables names and p-values
cormat_p <- spread(cormat_p, key = variable1, value = p) %>% as.matrix() # Reshape from long to wide format
rownames(cormat_p) <- cormat_p[, 1] # Set first variable as rownames
cormat_p <- cormat_p[, -1] # Remove first variable
# Reorder into the same order as the cormat
ordering <- factor(colnames(cormat_p), levels = c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                                                  "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                                                  "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                                                  "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                                                  "LSAS", "Anx.1", "anx.react",
                                                  "PEP.2", "pep.react",
                                                  "RSA.2", "rsa.react",
                                                  "RR.2", "rr.react",
                                                  "Cortisol.1.log", "cort.react"))
cormat_p <- cormat_p [order(ordering), # Set order
                      order(ordering)]
cormat_p[lower.tri(cormat_p, diag = TRUE)] <- NA # Remove everything under the diagonal
melted_cormat_p <- melt(cormat_p, na.rm = TRUE) # Melt the correlation matrix
rownames(melted_cormat_p) <- NULL # Reset rownames
melted_cormat_p$value <- as.numeric(levels(melted_cormat_p$value))[melted_cormat_p$value] # Make numeric
melted_cormat$value.sig <- melted_cormat$value # Create new variable that will contain only significant correlation coefficients
melted_cormat$value.nonsig <- melted_cormat$value # Create new variable that will contain only non-significant correlation coefficients
melted_cormat <- melted_cormat %>% mutate(value.sig = replace(value.sig, melted_cormat_p$value > .05, NA)) # Make NA the correlation coefficients with non-significant p-values
melted_cormat <- melted_cormat %>% mutate(value.nonsig = replace(value.nonsig, melted_cormat_p$value <= .05, NA)) # Make NA the correlation coefficients with significant p-values


## Plot correlation matrix heatmap
# To save high-res figure
tiff("Heatmap_CFC.tiff", width = 25, height = 25, units = "cm", res = 300)
# Plot
ggheatmap <- ggplot(melted_cormat, aes(Var1, Var2, fill = value)) + # Use melted correlation matrix
  ggtitle("Delta-beta coupling correlations with stress responses") +
  geom_tile(color = "white") + # White background
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") + # Fill colours
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
  #geom_text(aes(Var1, Var2, label = value), color = "black", size = 4) + # Add correlation coefficients
  geom_text(aes(Var1, Var2, label = value.nonsig), color = "black", size = 3.5) + # Add correlation coefficients
  geom_text(aes(Var1, Var2, label = value.sig), color = "black", size = 3.5, fontface = "bold") + # Add correlation coefficients
  geom_rect(mapping=aes(xmin=0.5, xmax=8.5, ymin=18.5, ymax=7.5), fill = NA, color="red", size = 1) # Add red rectangle around CFC - stress responses correlations
# Print the heatmap
print(ggheatmap)
# Save figure
dev.off()


## Remove unnecessary variables
remove(data)
remove(corr)
remove(cormat)
remove(cormat_p)
remove(melted_cormat)
remove(melted_cormat_p)
remove(ggheatmap)
remove(ordering)


# Regressions -------------------------------------------------------------

# Packages
library(mice) # Imputation analysis
library(magrittr) # Piping
library(dplyr) # Data manipulation
library(broom) # For exporting lm results
library(car) # For outlier test and vif test
library(xlsx) # For exporting to excel
library(miceadds) # For mi correlations and standardization
library(psych) # For converting correlation coefficients to cohen's d
library(BayesFactor) # For Bayesian statistics
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors

# Load data
load("SET_CFC.outl.del.imp.RData")
Data <- datlist_create(SET_CFC.outl.del.imp) # Create a datlist
Data.list <- scale_datlist(Data, # Standardize selected variables
                           orig_var = c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                                        "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                                        "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                                        "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                                        "LSAS", "Anx.1", "anx.react",
                                        "PEP.2", "pep.react",
                                        "RSA.2", "rsa.react",
                                        "RR.2", "rr.react",
                                        "Cortisol.1.log", "cort.react",
                                        "EnglishCompetence"), 
                           trafo_var = c("ZRS_Frontal_Avg_dPAC_Z", "Zreact_Frontal_Avg_dPAC_Z",
                                         "ZRS_Frontal_Avg_AAC_R", "Zreact_Frontal_Avg_AAC_R",
                                         "ZRS_Parietal_Avg_dPAC_Z", "Zreact_Parietal_Avg_dPAC_Z",
                                         "ZRS_Parietal_Avg_AAC_R", "Zreact_Parietal_Avg_AAC_R",
                                         "ZLSAS", "ZAnx.1", "Zanx.react",
                                         "ZPEP.2", "Zpep.react",
                                         "ZRSA.2", "Zrsa.react",
                                         "ZRR.2", "Zrr.react",
                                         "ZCortisol.1.log", "Zcort.react",
                                         "ZEnglishCompetence"))
Data <- datlist2mids(Data.list) # Convert back to a mids

# Initialize variables for Bayesian statistics
Data_long <- mice::complete(Data, action = "long") # Transform mids into long dataset
colnames(Data_long)[1] <- "imp" # Change the ".imp" variable into "imp"
m <- Data$m # Number of imputions
n.obs <-Data_long %>% filter(imp == 1) %>% nrow() # Numbers of observations in the imputed dataset


### Full models

### Model 1
## Specify model number
mod <- 1
## Specify dependant variable for partial correlations
dependant <- "ZRS_Frontal_Avg_dPAC_Z"
## Fit model
fit.lm.imp <- lm.mids(ZRS_Frontal_Avg_dPAC_Z ~ ZLSAS + ZAnx.1 + ZPEP.2 + ZRSA.2 + 
                        ZRR.2 + ZCortisol.1.log, data = Data)
## Pool regression results
pool.lm.imp <- pool(fit.lm.imp) # Pool
pool.rsqr.imp <- pool.r.squared(fit.lm.imp) %>% as.data.frame() # R-squared
pool.rsqr.imp[, "model"] <- mod # Add model number
sum.lm.imp <- summary(pool.lm.imp) # Summary
sum.lm.imp[, "Model"] <- mod # Save the lm model number
sum.lm.imp[, "Var"] <- rownames(sum.lm.imp) # Save the variable names
sum.lm.imp[, "Dep"] <- dependant # Save name of dependent variable
sum.lm.imp <- sum.lm.imp[-1, ] # Remove the intercept
## Check multicollinearity
multicol <- list(NA) # Initialize results list
# Compute VIF for each variable and imputed dataset
for (i in 1:length(fit.lm.imp$analyses)) {
  multicol[[i]] <- vif(fit.lm.imp$analyses[[i]])
}
# Put all VIF's into dataframe
multicol.table.1 <- matrix(nrow = I(nrow(sum.lm.imp)-1), ncol = length(multicol))
for (i in 1:nrow(multicol.table.1)) {
  for (j in 1:ncol(multicol.table.1))
    multicol.table.1[i, j] <- multicol[[j]][[i]]
}
rownames(multicol.table.1) <- rownames(sum.lm.imp)[-1] # Set rownames of variables
# Compute range of VIFs per variable
multicol.table.2 <- multicol.table.1[, c(1:2)] # Initialize new results matrix with two columns
multicol.table.2[, 1] <- apply(multicol.table.1, 1, min) # Calculate min VIF
multicol.table.2[, 2] <- apply(multicol.table.1, 1, max) # Calculate max VIF
multicol.table.2 <- t(multicol.table.2) %>% as.data.frame() # Transpose matrix and save as dataframe
rownames(multicol.table.2) <- c("min_VIF", "max_VIF") # Set descriptive rownames
multicol.table.2 <- multicol.table.2 %>% t() %>% as.data.frame() # Save as transposed dataframe
multicol.table.2[, "var"] <- rownames(multicol.table.2) # Save variable names
rownames(multicol.table.2) <- NULL # Reset rownames
multicol.table.2[ , "model"] <- mod # Save model name
## Partial correlations
# Get predictor variable names
a <- fit.lm.imp[[1]] %>% as.character() # Extract the call for fit.lm.imp
a <- a[2] # Select only the actual call
b <- strsplit(a, " ") # Split the call on spaces to get individual variables
c <- unlist(b) # Unlist to get character vectors
d <- c[!c %in% c("+", "*", "~", "(", ")")] # Remove entries containing special characters: +, *, ~, (, and )
e <- d[-c(1)] # Remove the first entry
f <- unique(e) # Remove double entries
pred <- f
# Loop for all variables in the regression results, skipping the intercept
corr <- list(NA) # Initialize results list
for (i in 1:length(pred)) {
  # Compute correlation between dependent variable and single predictor, correcting for all other predictors
  form <- paste(pred[-i], collapse = "+")  # Create single character string of all predictors to be partialled out
  corr.temp <- micombine.cor(Data, variables = c(dependant, pred[i]), 
                             partial = as.formula(paste("~", form)))
  corr[[i]] <- corr.temp[1, ] # Save only the first row (second row is the same)
}
# Put the correlation results in a table
# Extract results from list
corr.table <- sapply(corr, function(x) {
  c(var1 = levels(x$variable1)[1],
    var2 = levels(x$variable1)[2],
    rho = x$r,
    p.value = x$p)
})
# Save the results as a dataframe
corr.table <- t(corr.table) %>% as.data.frame(stringsAsFactors = FALSE) # Transpose the dataframe
corr.table[, 3:4] <- lapply(corr.table[, 3:4], function(x) as.numeric(as.character(x))) %>% 
  lapply(function(x) round(x, 5)) # Set the numeric columns to numeric and round to 5 decimals
corr.table[, "cohen.d"] <- r2d(corr.table[, "rho"]) # Compute cohen's d
corr.table[, "Model"] <- mod # Save the lm model number
## Bayes factors
# Initialize variables
BFest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the BayesFactors estimates
varest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the variance of the dependent variable
est <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = 1) # Matrix to put the pooled estimates
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed Data_longs
  subdata <- Data_long %>% filter(imp == j) # Select imputed Data_long
  BF <- generalTestBF(formula = as.formula(fit.lm.imp[["call"]][["formula"]]), # Calculate BayesFactor
                      data = subdata, whichModels = "bottom", progress = FALSE)
  BFest[, j] <- extractBF(BF, onlybf = TRUE) # Extract only the BayesFactors
  # Calculate the standard error of the estimate per variable (necessary for pooling)
  for (i in 1:nrow(sum.lm.imp)) { # Loop over all variables/models
    if (grepl(":", sum.lm.imp[i, "Var"])) { # If the variable contains an interaction
      Var <- sub("\\:.*", "", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Select the variable name before ":"
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else if (grepl("SexFemale", sum.lm.imp[i, "Var"])) { # If the main effect contains Sex
      Var <- sub("SexFemale", "Sex", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Rename SexFemale to Sex
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else { # For 'normal' variables
      varest[i, j] <- mean(c( var(subdata[, sum.lm.imp[i, "Var"]]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    }
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:nrow(sum.lm.imp)) { # For every variable
  est[i, 1] <- pool.scalar(BFest[i, ], varest[i, ], n = n.obs, k = 1)[["qbar"]] %>% unlist() %>% round(3)
}
# Add bayes factors to dataframe
sum.lm.imp[, "BF"] <- est
# Save results
rownames(sum.lm.imp) <- NULL # Reset rownames of regression table
lm.table <- sum.lm.imp # Save regression table
corrs <- corr.table # Save correlation table
multicol.table <- multicol.table.2 # Save multicollinearity
rsqr.imp <- pool.rsqr.imp # Save R-squared results


### Model 2
## Specify model number
mod <- 2
## Specify dependant variable for partial correlations
dependant <- "ZRS_Parietal_Avg_dPAC_Z"
## Fit model
fit.lm.imp <- lm.mids(ZRS_Parietal_Avg_dPAC_Z ~ ZLSAS + ZAnx.1 + ZPEP.2 + ZRSA.2 + 
                        ZRR.2 + ZCortisol.1.log, data = Data)
## Pool regression results
pool.lm.imp <- pool(fit.lm.imp) # Pool
pool.rsqr.imp <- pool.r.squared(fit.lm.imp) %>% as.data.frame() # R-squared
pool.rsqr.imp[, "model"] <- mod # Add model number
sum.lm.imp <- summary(pool.lm.imp) # Summary
sum.lm.imp[, "Model"] <- mod # Save the lm model number
sum.lm.imp[, "Var"] <- rownames(sum.lm.imp) # Save the variable names
sum.lm.imp[, "Dep"] <- dependant # Save name of dependent variable
sum.lm.imp <- sum.lm.imp[-1, ] # Remove the intercept
## Check multicollinearity
multicol <- list(NA) # Initialize results list
# Compute VIF for each variable and imputed dataset
for (i in 1:length(fit.lm.imp$analyses)) {
  multicol[[i]] <- vif(fit.lm.imp$analyses[[i]])
}
# Put all VIF's into dataframe
multicol.table.1 <- matrix(nrow = I(nrow(sum.lm.imp)-1), ncol = length(multicol))
for (i in 1:nrow(multicol.table.1)) {
  for (j in 1:ncol(multicol.table.1))
    multicol.table.1[i, j] <- multicol[[j]][[i]]
}
rownames(multicol.table.1) <- rownames(sum.lm.imp)[-1] # Set rownames of variables
# Compute range of VIFs per variable
multicol.table.2 <- multicol.table.1[, c(1:2)] # Initialize new results matrix with two columns
multicol.table.2[, 1] <- apply(multicol.table.1, 1, min) # Calculate min VIF
multicol.table.2[, 2] <- apply(multicol.table.1, 1, max) # Calculate max VIF
multicol.table.2 <- t(multicol.table.2) %>% as.data.frame() # Transpose matrix and save as dataframe
rownames(multicol.table.2) <- c("min_VIF", "max_VIF") # Set descriptive rownames
multicol.table.2 <- multicol.table.2 %>% t() %>% as.data.frame() # Save as transposed dataframe
multicol.table.2[, "var"] <- rownames(multicol.table.2) # Save variable names
rownames(multicol.table.2) <- NULL # Reset rownames
multicol.table.2[ , "model"] <- mod # Save model name
## Partial correlations
# Get predictor variable names
a <- fit.lm.imp[[1]] %>% as.character() # Extract the call for fit.lm.imp
a <- a[2] # Select only the actual call
b <- strsplit(a, " ") # Split the call on spaces to get individual variables
c <- unlist(b) # Unlist to get character vectors
d <- c[!c %in% c("+", "*", "~", "(", ")")] # Remove entries containing special characters: +, *, ~, (, and )
e <- d[-c(1)] # Remove the first entry
f <- unique(e) # Remove double entries
pred <- f
# Loop for all variables in the regression results, skipping the intercept
corr <- list(NA) # Initialize results list
for (i in 1:length(pred)) {
  # Compute correlation between dependent variable and single predictor, correcting for all other predictors
  form <- paste(pred[-i], collapse = "+")  # Create single character string of all predictors to be partialled out
  corr.temp <- micombine.cor(Data, variables = c(dependant, pred[i]), 
                             partial = as.formula(paste("~", form)))
  corr[[i]] <- corr.temp[1, ] # Save only the first row (second row is the same)
}
# Put the correlation results in a table
# Extract results from list
corr.table <- sapply(corr, function(x) {
  c(var1 = levels(x$variable1)[1],
    var2 = levels(x$variable1)[2],
    rho = x$r,
    p.value = x$p)
})
# Save the results as a dataframe
corr.table <- t(corr.table) %>% as.data.frame(stringsAsFactors = FALSE) # Transpose the dataframe
corr.table[, 3:4] <- lapply(corr.table[, 3:4], function(x) as.numeric(as.character(x))) %>% 
  lapply(function(x) round(x, 5)) # Set the numeric columns to numeric and round to 5 decimals
corr.table[, "cohen.d"] <- r2d(corr.table[, "rho"]) # Compute cohen's d
corr.table[, "Model"] <- mod # Save the lm model number
## Bayes factors
# Initialize variables
BFest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the BayesFactors estimates
varest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the variance of the dependent variable
est <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = 1) # Matrix to put the pooled estimates
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed Data_longs
  subdata <- Data_long %>% filter(imp == j) # Select imputed Data_long
  BF <- generalTestBF(formula = as.formula(fit.lm.imp[["call"]][["formula"]]), # Calculate BayesFactor
                      data = subdata, whichModels = "bottom", progress = FALSE)
  BFest[, j] <- extractBF(BF, onlybf = TRUE) # Extract only the BayesFactors
  # Calculate the standard error of the estimate per variable (necessary for pooling)
  for (i in 1:nrow(sum.lm.imp)) { # Loop over all variables/models
    if (grepl(":", sum.lm.imp[i, "Var"])) { # If the variable contains an interaction
      Var <- sub("\\:.*", "", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Select the variable name before ":"
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else if (grepl("SexFemale", sum.lm.imp[i, "Var"])) { # If the main effect contains Sex
      Var <- sub("SexFemale", "Sex", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Rename SexFemale to Sex
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else { # For 'normal' variables
      varest[i, j] <- mean(c( var(subdata[, sum.lm.imp[i, "Var"]]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    }
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:nrow(sum.lm.imp)) { # For every variable
  est[i, 1] <- pool.scalar(BFest[i, ], varest[i, ], n = n.obs, k = 1)[["qbar"]] %>% unlist() %>% round(3)
}
# Add bayes factors to dataframe
sum.lm.imp[, "BF"] <- est
# Save results
rownames(sum.lm.imp) <- NULL # Reset rownames of regression table
lm.table <- rbind(lm.table, sum.lm.imp) # Save regression table
corrs <- rbind(corrs, corr.table) # Save correlation table
multicol.table <- rbind(multicol.table, multicol.table.2) # Save multicollinearity
rsqr.imp <- rbind(rsqr.imp, pool.rsqr.imp) # Save R-squared results


### Model 3
## Specify model number
mod <- 3
## Specify dependant variable for partial correlations
dependant <- "ZRS_Frontal_Avg_AAC_R"
## Fit model
fit.lm.imp <- lm.mids(ZRS_Frontal_Avg_AAC_R ~ ZLSAS + ZAnx.1 + ZPEP.2 + ZRSA.2 + 
                        ZRR.2 + ZCortisol.1.log, data = Data)
## Pool regression results
pool.lm.imp <- pool(fit.lm.imp) # Pool
pool.rsqr.imp <- pool.r.squared(fit.lm.imp) %>% as.data.frame() # R-squared
pool.rsqr.imp[, "model"] <- mod # Add model number
sum.lm.imp <- summary(pool.lm.imp) # Summary
sum.lm.imp[, "Model"] <- mod # Save the lm model number
sum.lm.imp[, "Var"] <- rownames(sum.lm.imp) # Save the variable names
sum.lm.imp[, "Dep"] <- dependant # Save name of dependent variable
sum.lm.imp <- sum.lm.imp[-1, ] # Remove the intercept
## Check multicollinearity
multicol <- list(NA) # Initialize results list
# Compute VIF for each variable and imputed dataset
for (i in 1:length(fit.lm.imp$analyses)) {
  multicol[[i]] <- vif(fit.lm.imp$analyses[[i]])
}
# Put all VIF's into dataframe
multicol.table.1 <- matrix(nrow = I(nrow(sum.lm.imp)-1), ncol = length(multicol))
for (i in 1:nrow(multicol.table.1)) {
  for (j in 1:ncol(multicol.table.1))
    multicol.table.1[i, j] <- multicol[[j]][[i]]
}
rownames(multicol.table.1) <- rownames(sum.lm.imp)[-1] # Set rownames of variables
# Compute range of VIFs per variable
multicol.table.2 <- multicol.table.1[, c(1:2)] # Initialize new results matrix with two columns
multicol.table.2[, 1] <- apply(multicol.table.1, 1, min) # Calculate min VIF
multicol.table.2[, 2] <- apply(multicol.table.1, 1, max) # Calculate max VIF
multicol.table.2 <- t(multicol.table.2) %>% as.data.frame() # Transpose matrix and save as dataframe
rownames(multicol.table.2) <- c("min_VIF", "max_VIF") # Set descriptive rownames
multicol.table.2 <- multicol.table.2 %>% t() %>% as.data.frame() # Save as transposed dataframe
multicol.table.2[, "var"] <- rownames(multicol.table.2) # Save variable names
rownames(multicol.table.2) <- NULL # Reset rownames
multicol.table.2[ , "model"] <- mod # Save model name
## Partial correlations
# Get predictor variable names
a <- fit.lm.imp[[1]] %>% as.character() # Extract the call for fit.lm.imp
a <- a[2] # Select only the actual call
b <- strsplit(a, " ") # Split the call on spaces to get individual variables
c <- unlist(b) # Unlist to get character vectors
d <- c[!c %in% c("+", "*", "~", "(", ")")] # Remove entries containing special characters: +, *, ~, (, and )
e <- d[-c(1)] # Remove the first entry
f <- unique(e) # Remove double entries
pred <- f
# Loop for all variables in the regression results, skipping the intercept
corr <- list(NA) # Initialize results list
for (i in 1:length(pred)) {
  # Compute correlation between dependent variable and single predictor, correcting for all other predictors
  form <- paste(pred[-i], collapse = "+")  # Create single character string of all predictors to be partialled out
  corr.temp <- micombine.cor(Data, variables = c(dependant, pred[i]), 
                             partial = as.formula(paste("~", form)))
  corr[[i]] <- corr.temp[1, ] # Save only the first row (second row is the same)
}
# Put the correlation results in a table
# Extract results from list
corr.table <- sapply(corr, function(x) {
  c(var1 = levels(x$variable1)[1],
    var2 = levels(x$variable1)[2],
    rho = x$r,
    p.value = x$p)
})
# Save the results as a dataframe
corr.table <- t(corr.table) %>% as.data.frame(stringsAsFactors = FALSE) # Transpose the dataframe
corr.table[, 3:4] <- lapply(corr.table[, 3:4], function(x) as.numeric(as.character(x))) %>% 
  lapply(function(x) round(x, 5)) # Set the numeric columns to numeric and round to 5 decimals
corr.table[, "cohen.d"] <- r2d(corr.table[, "rho"]) # Compute cohen's d
corr.table[, "Model"] <- mod # Save the lm model number
## Bayes factors
# Initialize variables
BFest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the BayesFactors estimates
varest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the variance of the dependent variable
est <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = 1) # Matrix to put the pooled estimates
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed Data_longs
  subdata <- Data_long %>% filter(imp == j) # Select imputed Data_long
  BF <- generalTestBF(formula = as.formula(fit.lm.imp[["call"]][["formula"]]), # Calculate BayesFactor
                      data = subdata, whichModels = "bottom", progress = FALSE)
  BFest[, j] <- extractBF(BF, onlybf = TRUE) # Extract only the BayesFactors
  # Calculate the standard error of the estimate per variable (necessary for pooling)
  for (i in 1:nrow(sum.lm.imp)) { # Loop over all variables/models
    if (grepl(":", sum.lm.imp[i, "Var"])) { # If the variable contains an interaction
      Var <- sub("\\:.*", "", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Select the variable name before ":"
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else if (grepl("SexFemale", sum.lm.imp[i, "Var"])) { # If the main effect contains Sex
      Var <- sub("SexFemale", "Sex", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Rename SexFemale to Sex
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else { # For 'normal' variables
      varest[i, j] <- mean(c( var(subdata[, sum.lm.imp[i, "Var"]]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    }
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:nrow(sum.lm.imp)) { # For every variable
  est[i, 1] <- pool.scalar(BFest[i, ], varest[i, ], n = n.obs, k = 1)[["qbar"]] %>% unlist() %>% round(3)
}
# Add bayes factors to dataframe
sum.lm.imp[, "BF"] <- est
# Save results
rownames(sum.lm.imp) <- NULL # Reset rownames of regression table
lm.table <- rbind(lm.table, sum.lm.imp) # Save regression table
corrs <- rbind(corrs, corr.table) # Save correlation table
multicol.table <- rbind(multicol.table, multicol.table.2) # Save multicollinearity
rsqr.imp <- rbind(rsqr.imp, pool.rsqr.imp) # Save R-squared results


### Model 4
## Specify model number
mod <- 4
## Specify dependant variable for partial correlations
dependant <- "ZRS_Parietal_Avg_AAC_R"
## Fit model
fit.lm.imp <- lm.mids(ZRS_Parietal_Avg_AAC_R ~ ZLSAS + ZAnx.1 + ZPEP.2 + ZRSA.2 + 
                        ZRR.2 + ZCortisol.1.log, data = Data)
## Pool regression results
pool.lm.imp <- pool(fit.lm.imp) # Pool
pool.rsqr.imp <- pool.r.squared(fit.lm.imp) %>% as.data.frame() # R-squared
pool.rsqr.imp[, "model"] <- mod # Add model number
sum.lm.imp <- summary(pool.lm.imp) # Summary
sum.lm.imp[, "Model"] <- mod # Save the lm model number
sum.lm.imp[, "Var"] <- rownames(sum.lm.imp) # Save the variable names
sum.lm.imp[, "Dep"] <- dependant # Save name of dependent variable
sum.lm.imp <- sum.lm.imp[-1, ] # Remove the intercept
## Check multicollinearity
multicol <- list(NA) # Initialize results list
# Compute VIF for each variable and imputed dataset
for (i in 1:length(fit.lm.imp$analyses)) {
  multicol[[i]] <- vif(fit.lm.imp$analyses[[i]])
}
# Put all VIF's into dataframe
multicol.table.1 <- matrix(nrow = I(nrow(sum.lm.imp)-1), ncol = length(multicol))
for (i in 1:nrow(multicol.table.1)) {
  for (j in 1:ncol(multicol.table.1))
    multicol.table.1[i, j] <- multicol[[j]][[i]]
}
rownames(multicol.table.1) <- rownames(sum.lm.imp)[-1] # Set rownames of variables
# Compute range of VIFs per variable
multicol.table.2 <- multicol.table.1[, c(1:2)] # Initialize new results matrix with two columns
multicol.table.2[, 1] <- apply(multicol.table.1, 1, min) # Calculate min VIF
multicol.table.2[, 2] <- apply(multicol.table.1, 1, max) # Calculate max VIF
multicol.table.2 <- t(multicol.table.2) %>% as.data.frame() # Transpose matrix and save as dataframe
rownames(multicol.table.2) <- c("min_VIF", "max_VIF") # Set descriptive rownames
multicol.table.2 <- multicol.table.2 %>% t() %>% as.data.frame() # Save as transposed dataframe
multicol.table.2[, "var"] <- rownames(multicol.table.2) # Save variable names
rownames(multicol.table.2) <- NULL # Reset rownames
multicol.table.2[ , "model"] <- mod # Save model name
## Partial correlations
# Get predictor variable names
a <- fit.lm.imp[[1]] %>% as.character() # Extract the call for fit.lm.imp
a <- a[2] # Select only the actual call
b <- strsplit(a, " ") # Split the call on spaces to get individual variables
c <- unlist(b) # Unlist to get character vectors
d <- c[!c %in% c("+", "*", "~", "(", ")")] # Remove entries containing special characters: +, *, ~, (, and )
e <- d[-c(1)] # Remove the first entry
f <- unique(e) # Remove double entries
pred <- f
# Loop for all variables in the regression results, skipping the intercept
corr <- list(NA) # Initialize results list
for (i in 1:length(pred)) {
  # Compute correlation between dependent variable and single predictor, correcting for all other predictors
  form <- paste(pred[-i], collapse = "+")  # Create single character string of all predictors to be partialled out
  corr.temp <- micombine.cor(Data, variables = c(dependant, pred[i]), 
                             partial = as.formula(paste("~", form)))
  corr[[i]] <- corr.temp[1, ] # Save only the first row (second row is the same)
}
# Put the correlation results in a table
# Extract results from list
corr.table <- sapply(corr, function(x) {
  c(var1 = levels(x$variable1)[1],
    var2 = levels(x$variable1)[2],
    rho = x$r,
    p.value = x$p)
})
# Save the results as a dataframe
corr.table <- t(corr.table) %>% as.data.frame(stringsAsFactors = FALSE) # Transpose the dataframe
corr.table[, 3:4] <- lapply(corr.table[, 3:4], function(x) as.numeric(as.character(x))) %>% 
  lapply(function(x) round(x, 5)) # Set the numeric columns to numeric and round to 5 decimals
corr.table[, "cohen.d"] <- r2d(corr.table[, "rho"]) # Compute cohen's d
corr.table[, "Model"] <- mod # Save the lm model number
## Bayes factors
# Initialize variables
BFest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the BayesFactors estimates
varest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the variance of the dependent variable
est <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = 1) # Matrix to put the pooled estimates
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed Data_longs
  subdata <- Data_long %>% filter(imp == j) # Select imputed Data_long
  BF <- generalTestBF(formula = as.formula(fit.lm.imp[["call"]][["formula"]]), # Calculate BayesFactor
                      data = subdata, whichModels = "bottom", progress = FALSE)
  BFest[, j] <- extractBF(BF, onlybf = TRUE) # Extract only the BayesFactors
  # Calculate the standard error of the estimate per variable (necessary for pooling)
  for (i in 1:nrow(sum.lm.imp)) { # Loop over all variables/models
    if (grepl(":", sum.lm.imp[i, "Var"])) { # If the variable contains an interaction
      Var <- sub("\\:.*", "", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Select the variable name before ":"
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else if (grepl("SexFemale", sum.lm.imp[i, "Var"])) { # If the main effect contains Sex
      Var <- sub("SexFemale", "Sex", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Rename SexFemale to Sex
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else { # For 'normal' variables
      varest[i, j] <- mean(c( var(subdata[, sum.lm.imp[i, "Var"]]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    }
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:nrow(sum.lm.imp)) { # For every variable
  est[i, 1] <- pool.scalar(BFest[i, ], varest[i, ], n = n.obs, k = 1)[["qbar"]] %>% unlist() %>% round(3)
}
# Add bayes factors to dataframe
sum.lm.imp[, "BF"] <- est
# Save results
rownames(sum.lm.imp) <- NULL # Reset rownames of regression table
lm.table <- rbind(lm.table, sum.lm.imp) # Save regression table
corrs <- rbind(corrs, corr.table) # Save correlation table
multicol.table <- rbind(multicol.table, multicol.table.2) # Save multicollinearity
rsqr.imp <- rbind(rsqr.imp, pool.rsqr.imp) # Save R-squared results


### Model 5
## Specify model number
mod <- 5
## Specify dependant variable for partial correlations
dependant <- "Zreact_Frontal_Avg_dPAC_Z"
## Fit model
fit.lm.imp <- lm.mids(Zreact_Frontal_Avg_dPAC_Z ~ ZLSAS +Zanx.react + Zpep.react + Zrsa.react + 
                        Zrr.react + Zcort.react, data = Data)
## Pool regression results
pool.lm.imp <- pool(fit.lm.imp) # Pool
pool.rsqr.imp <- pool.r.squared(fit.lm.imp) %>% as.data.frame() # R-squared
pool.rsqr.imp[, "model"] <- mod # Add model number
sum.lm.imp <- summary(pool.lm.imp) # Summary
sum.lm.imp[, "Model"] <- mod # Save the lm model number
sum.lm.imp[, "Var"] <- rownames(sum.lm.imp) # Save the variable names
sum.lm.imp[, "Dep"] <- dependant # Save name of dependent variable
sum.lm.imp <- sum.lm.imp[-1, ] # Remove the intercept
## Check multicollinearity
multicol <- list(NA) # Initialize results list
# Compute VIF for each variable and imputed dataset
for (i in 1:length(fit.lm.imp$analyses)) {
  multicol[[i]] <- vif(fit.lm.imp$analyses[[i]])
}
# Put all VIF's into dataframe
multicol.table.1 <- matrix(nrow = I(nrow(sum.lm.imp)-1), ncol = length(multicol))
for (i in 1:nrow(multicol.table.1)) {
  for (j in 1:ncol(multicol.table.1))
    multicol.table.1[i, j] <- multicol[[j]][[i]]
}
rownames(multicol.table.1) <- rownames(sum.lm.imp)[-1] # Set rownames of variables
# Compute range of VIFs per variable
multicol.table.2 <- multicol.table.1[, c(1:2)] # Initialize new results matrix with two columns
multicol.table.2[, 1] <- apply(multicol.table.1, 1, min) # Calculate min VIF
multicol.table.2[, 2] <- apply(multicol.table.1, 1, max) # Calculate max VIF
multicol.table.2 <- t(multicol.table.2) %>% as.data.frame() # Transpose matrix and save as dataframe
rownames(multicol.table.2) <- c("min_VIF", "max_VIF") # Set descriptive rownames
multicol.table.2 <- multicol.table.2 %>% t() %>% as.data.frame() # Save as transposed dataframe
multicol.table.2[, "var"] <- rownames(multicol.table.2) # Save variable names
rownames(multicol.table.2) <- NULL # Reset rownames
multicol.table.2[ , "model"] <- mod # Save model name
## Partial correlations
# Get predictor variable names
a <- fit.lm.imp[[1]] %>% as.character() # Extract the call for fit.lm.imp
a <- a[2] # Select only the actual call
b <- strsplit(a, " ") # Split the call on spaces to get individual variables
c <- unlist(b) # Unlist to get character vectors
d <- c[!c %in% c("+", "*", "~", "(", ")")] # Remove entries containing special characters: +, *, ~, (, and )
e <- d[-c(1)] # Remove the first entry
f <- unique(e) # Remove double entries
pred <- f
# Loop for all variables in the regression results, skipping the intercept
corr <- list(NA) # Initialize results list
for (i in 1:length(pred)) {
  # Compute correlation between dependent variable and single predictor, correcting for all other predictors
  form <- paste(pred[-i], collapse = "+")  # Create single character string of all predictors to be partialled out
  corr.temp <- micombine.cor(Data, variables = c(dependant, pred[i]), 
                             partial = as.formula(paste("~", form)))
  corr[[i]] <- corr.temp[1, ] # Save only the first row (second row is the same)
}
# Put the correlation results in a table
# Extract results from list
corr.table <- sapply(corr, function(x) {
  c(var1 = levels(x$variable1)[1],
    var2 = levels(x$variable1)[2],
    rho = x$r,
    p.value = x$p)
})
# Save the results as a dataframe
corr.table <- t(corr.table) %>% as.data.frame(stringsAsFactors = FALSE) # Transpose the dataframe
corr.table[, 3:4] <- lapply(corr.table[, 3:4], function(x) as.numeric(as.character(x))) %>% 
  lapply(function(x) round(x, 5)) # Set the numeric columns to numeric and round to 5 decimals
corr.table[, "cohen.d"] <- r2d(corr.table[, "rho"]) # Compute cohen's d
corr.table[, "Model"] <- mod # Save the lm model number
## Bayes factors
# Initialize variables
BFest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the BayesFactors estimates
varest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the variance of the dependent variable
est <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = 1) # Matrix to put the pooled estimates
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed Data_longs
  subdata <- Data_long %>% filter(imp == j) # Select imputed Data_long
  BF <- generalTestBF(formula = as.formula(fit.lm.imp[["call"]][["formula"]]), # Calculate BayesFactor
                     data = subdata, whichModels = "bottom", progress = FALSE)
  BFest[, j] <- extractBF(BF, onlybf = TRUE) # Extract only the BayesFactors
  # Calculate the standard error of the estimate per variable (necessary for pooling)
  for (i in 1:nrow(sum.lm.imp)) { # Loop over all variables/models
    if (grepl(":", sum.lm.imp[i, "Var"])) { # If the variable contains an interaction
      Var <- sub("\\:.*", "", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Select the variable name before ":"
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else if (grepl("SexFemale", sum.lm.imp[i, "Var"])) { # If the main effect contains Sex
      Var <- sub("SexFemale", "Sex", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Rename SexFemale to Sex
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else { # For 'normal' variables
      varest[i, j] <- mean(c( var(subdata[, sum.lm.imp[i, "Var"]]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    }
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:nrow(sum.lm.imp)) { # For every variable
  est[i, 1] <- pool.scalar(BFest[i, ], varest[i, ], n = n.obs, k = 1)[["qbar"]] %>% unlist() %>% round(3)
}
# Add bayes factors to dataframe
sum.lm.imp[, "BF"] <- est
# Save results
rownames(sum.lm.imp) <- NULL # Reset rownames of regression table
lm.table <- rbind(lm.table, sum.lm.imp) # Save regression table
corrs <- rbind(corrs, corr.table) # Save correlation table
multicol.table <- rbind(multicol.table, multicol.table.2) # Save multicollinearity
rsqr.imp <- rbind(rsqr.imp, pool.rsqr.imp) # Save R-squared results


### Model 6
## Specify model number
mod <- 6
## Specify dependant variable for partial correlations
dependant <- "Zreact_Parietal_Avg_dPAC_Z"
## Fit model
fit.lm.imp <- lm.mids(Zreact_Parietal_Avg_dPAC_Z ~ ZLSAS +Zanx.react + Zpep.react + Zrsa.react + 
                        Zrr.react + Zcort.react, data = Data)
## Pool regression results
pool.lm.imp <- pool(fit.lm.imp) # Pool
pool.rsqr.imp <- pool.r.squared(fit.lm.imp) %>% as.data.frame() # R-squared
pool.rsqr.imp[, "model"] <- mod # Add model number
sum.lm.imp <- summary(pool.lm.imp) # Summary
sum.lm.imp[, "Model"] <- mod # Save the lm model number
sum.lm.imp[, "Var"] <- rownames(sum.lm.imp) # Save the variable names
sum.lm.imp[, "Dep"] <- dependant # Save name of dependent variable
sum.lm.imp <- sum.lm.imp[-1, ] # Remove the intercept
## Check multicollinearity
multicol <- list(NA) # Initialize results list
# Compute VIF for each variable and imputed dataset
for (i in 1:length(fit.lm.imp$analyses)) {
  multicol[[i]] <- vif(fit.lm.imp$analyses[[i]])
}
# Put all VIF's into dataframe
multicol.table.1 <- matrix(nrow = I(nrow(sum.lm.imp)-1), ncol = length(multicol))
for (i in 1:nrow(multicol.table.1)) {
  for (j in 1:ncol(multicol.table.1))
    multicol.table.1[i, j] <- multicol[[j]][[i]]
}
rownames(multicol.table.1) <- rownames(sum.lm.imp)[-1] # Set rownames of variables
# Compute range of VIFs per variable
multicol.table.2 <- multicol.table.1[, c(1:2)] # Initialize new results matrix with two columns
multicol.table.2[, 1] <- apply(multicol.table.1, 1, min) # Calculate min VIF
multicol.table.2[, 2] <- apply(multicol.table.1, 1, max) # Calculate max VIF
multicol.table.2 <- t(multicol.table.2) %>% as.data.frame() # Transpose matrix and save as dataframe
rownames(multicol.table.2) <- c("min_VIF", "max_VIF") # Set descriptive rownames
multicol.table.2 <- multicol.table.2 %>% t() %>% as.data.frame() # Save as transposed dataframe
multicol.table.2[, "var"] <- rownames(multicol.table.2) # Save variable names
rownames(multicol.table.2) <- NULL # Reset rownames
multicol.table.2[ , "model"] <- mod # Save model name
## Partial correlations
# Get predictor variable names
a <- fit.lm.imp[[1]] %>% as.character() # Extract the call for fit.lm.imp
a <- a[2] # Select only the actual call
b <- strsplit(a, " ") # Split the call on spaces to get individual variables
c <- unlist(b) # Unlist to get character vectors
d <- c[!c %in% c("+", "*", "~", "(", ")")] # Remove entries containing special characters: +, *, ~, (, and )
e <- d[-c(1)] # Remove the first entry
f <- unique(e) # Remove double entries
pred <- f
# Loop for all variables in the regression results, skipping the intercept
corr <- list(NA) # Initialize results list
for (i in 1:length(pred)) {
  # Compute correlation between dependent variable and single predictor, correcting for all other predictors
  form <- paste(pred[-i], collapse = "+")  # Create single character string of all predictors to be partialled out
  corr.temp <- micombine.cor(Data, variables = c(dependant, pred[i]), 
                             partial = as.formula(paste("~", form)))
  corr[[i]] <- corr.temp[1, ] # Save only the first row (second row is the same)
}
# Put the correlation results in a table
# Extract results from list
corr.table <- sapply(corr, function(x) {
  c(var1 = levels(x$variable1)[1],
    var2 = levels(x$variable1)[2],
    rho = x$r,
    p.value = x$p)
})
# Save the results as a dataframe
corr.table <- t(corr.table) %>% as.data.frame(stringsAsFactors = FALSE) # Transpose the dataframe
corr.table[, 3:4] <- lapply(corr.table[, 3:4], function(x) as.numeric(as.character(x))) %>% 
  lapply(function(x) round(x, 5)) # Set the numeric columns to numeric and round to 5 decimals
corr.table[, "cohen.d"] <- r2d(corr.table[, "rho"]) # Compute cohen's d
corr.table[, "Model"] <- mod # Save the lm model number
## Bayes factors
# Initialize variables
BFest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the BayesFactors estimates
varest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the variance of the dependent variable
est <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = 1) # Matrix to put the pooled estimates
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed Data_longs
  subdata <- Data_long %>% filter(imp == j) # Select imputed Data_long
  BF <- generalTestBF(formula = as.formula(fit.lm.imp[["call"]][["formula"]]), # Calculate BayesFactor
                      data = subdata, whichModels = "bottom", progress = FALSE)
  BFest[, j] <- extractBF(BF, onlybf = TRUE) # Extract only the BayesFactors
  # Calculate the standard error of the estimate per variable (necessary for pooling)
  for (i in 1:nrow(sum.lm.imp)) { # Loop over all variables/models
    if (grepl(":", sum.lm.imp[i, "Var"])) { # If the variable contains an interaction
      Var <- sub("\\:.*", "", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Select the variable name before ":"
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else if (grepl("SexFemale", sum.lm.imp[i, "Var"])) { # If the main effect contains Sex
      Var <- sub("SexFemale", "Sex", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Rename SexFemale to Sex
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else { # For 'normal' variables
      varest[i, j] <- mean(c( var(subdata[, sum.lm.imp[i, "Var"]]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    }
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:nrow(sum.lm.imp)) { # For every variable
  est[i, 1] <- pool.scalar(BFest[i, ], varest[i, ], n = n.obs, k = 1)[["qbar"]] %>% unlist() %>% round(3)
}
# Add bayes factors to dataframe
sum.lm.imp[, "BF"] <- est
# Save results
rownames(sum.lm.imp) <- NULL # Reset rownames of regression table
lm.table <- rbind(lm.table, sum.lm.imp) # Save regression table
corrs <- rbind(corrs, corr.table) # Save correlation table
multicol.table <- rbind(multicol.table, multicol.table.2) # Save multicollinearity
rsqr.imp <- rbind(rsqr.imp, pool.rsqr.imp) # Save R-squared results


### Model 7
## Specify model number
mod <- 7
## Specify dependant variable for partial correlations
dependant <- "Zreact_Frontal_Avg_AAC_R"
## Fit model
fit.lm.imp <- lm.mids(Zreact_Frontal_Avg_AAC_R ~ ZLSAS + Zanx.react + Zpep.react + Zrsa.react + 
                        Zrr.react + Zcort.react, data = Data)
## Pool regression results
pool.lm.imp <- pool(fit.lm.imp) # Pool
pool.rsqr.imp <- pool.r.squared(fit.lm.imp) %>% as.data.frame() # R-squared
pool.rsqr.imp[, "model"] <- mod # Add model number
sum.lm.imp <- summary(pool.lm.imp) # Summary
sum.lm.imp[, "Model"] <- mod # Save the lm model number
sum.lm.imp[, "Var"] <- rownames(sum.lm.imp) # Save the variable names
sum.lm.imp[, "Dep"] <- dependant # Save name of dependent variable
sum.lm.imp <- sum.lm.imp[-1, ] # Remove the intercept
## Check multicollinearity
multicol <- list(NA) # Initialize results list
# Compute VIF for each variable and imputed dataset
for (i in 1:length(fit.lm.imp$analyses)) {
  multicol[[i]] <- vif(fit.lm.imp$analyses[[i]])
}
# Put all VIF's into dataframe
multicol.table.1 <- matrix(nrow = I(nrow(sum.lm.imp)-1), ncol = length(multicol))
for (i in 1:nrow(multicol.table.1)) {
  for (j in 1:ncol(multicol.table.1))
    multicol.table.1[i, j] <- multicol[[j]][[i]]
}
rownames(multicol.table.1) <- rownames(sum.lm.imp)[-1] # Set rownames of variables
# Compute range of VIFs per variable
multicol.table.2 <- multicol.table.1[, c(1:2)] # Initialize new results matrix with two columns
multicol.table.2[, 1] <- apply(multicol.table.1, 1, min) # Calculate min VIF
multicol.table.2[, 2] <- apply(multicol.table.1, 1, max) # Calculate max VIF
multicol.table.2 <- t(multicol.table.2) %>% as.data.frame() # Transpose matrix and save as dataframe
rownames(multicol.table.2) <- c("min_VIF", "max_VIF") # Set descriptive rownames
multicol.table.2 <- multicol.table.2 %>% t() %>% as.data.frame() # Save as transposed dataframe
multicol.table.2[, "var"] <- rownames(multicol.table.2) # Save variable names
rownames(multicol.table.2) <- NULL # Reset rownames
multicol.table.2[ , "model"] <- mod # Save model name
## Partial correlations
# Get predictor variable names
a <- fit.lm.imp[[1]] %>% as.character() # Extract the call for fit.lm.imp
a <- a[2] # Select only the actual call
b <- strsplit(a, " ") # Split the call on spaces to get individual variables
c <- unlist(b) # Unlist to get character vectors
d <- c[!c %in% c("+", "*", "~", "(", ")")] # Remove entries containing special characters: +, *, ~, (, and )
e <- d[-c(1)] # Remove the first entry
f <- unique(e) # Remove double entries
pred <- f
# Loop for all variables in the regression results, skipping the intercept
corr <- list(NA) # Initialize results list
for (i in 1:length(pred)) {
  # Compute correlation between dependent variable and single predictor, correcting for all other predictors
  form <- paste(pred[-i], collapse = "+")  # Create single character string of all predictors to be partialled out
  corr.temp <- micombine.cor(Data, variables = c(dependant, pred[i]), 
                             partial = as.formula(paste("~", form)))
  corr[[i]] <- corr.temp[1, ] # Save only the first row (second row is the same)
}
# Put the correlation results in a table
# Extract results from list
corr.table <- sapply(corr, function(x) {
  c(var1 = levels(x$variable1)[1],
    var2 = levels(x$variable1)[2],
    rho = x$r,
    p.value = x$p)
})
# Save the results as a dataframe
corr.table <- t(corr.table) %>% as.data.frame(stringsAsFactors = FALSE) # Transpose the dataframe
corr.table[, 3:4] <- lapply(corr.table[, 3:4], function(x) as.numeric(as.character(x))) %>% 
  lapply(function(x) round(x, 5)) # Set the numeric columns to numeric and round to 5 decimals
corr.table[, "cohen.d"] <- r2d(corr.table[, "rho"]) # Compute cohen's d
corr.table[, "Model"] <- mod # Save the lm model number
## Bayes factors
# Initialize variables
BFest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the BayesFactors estimates
varest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the variance of the dependent variable
est <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = 1) # Matrix to put the pooled estimates
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed Data_longs
  subdata <- Data_long %>% filter(imp == j) # Select imputed Data_long
  BF <- generalTestBF(formula = as.formula(fit.lm.imp[["call"]][["formula"]]), # Calculate BayesFactor
                      data = subdata, whichModels = "bottom", progress = FALSE)
  BFest[, j] <- extractBF(BF, onlybf = TRUE) # Extract only the BayesFactors
  # Calculate the standard error of the estimate per variable (necessary for pooling)
  for (i in 1:nrow(sum.lm.imp)) { # Loop over all variables/models
    if (grepl(":", sum.lm.imp[i, "Var"])) { # If the variable contains an interaction
      Var <- sub("\\:.*", "", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Select the variable name before ":"
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else if (grepl("SexFemale", sum.lm.imp[i, "Var"])) { # If the main effect contains Sex
      Var <- sub("SexFemale", "Sex", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Rename SexFemale to Sex
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else { # For 'normal' variables
      varest[i, j] <- mean(c( var(subdata[, sum.lm.imp[i, "Var"]]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    }
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:nrow(sum.lm.imp)) { # For every variable
  est[i, 1] <- pool.scalar(BFest[i, ], varest[i, ], n = n.obs, k = 1)[["qbar"]] %>% unlist() %>% round(3)
}
# Add bayes factors to dataframe
sum.lm.imp[, "BF"] <- est
# Save results
rownames(sum.lm.imp) <- NULL # Reset rownames of regression table
lm.table <- rbind(lm.table, sum.lm.imp) # Save regression table
corrs <- rbind(corrs, corr.table) # Save correlation table
multicol.table <- rbind(multicol.table, multicol.table.2) # Save multicollinearity
rsqr.imp <- rbind(rsqr.imp, pool.rsqr.imp) # Save R-squared results


### Model 8
## Specify model number
mod <- 8
## Specify dependant variable for partial correlations
dependant <- "Zreact_Parietal_Avg_AAC_R"
## Fit model
fit.lm.imp <- lm.mids(Zreact_Parietal_Avg_AAC_R ~ ZLSAS + Zanx.react + Zpep.react + Zrsa.react + 
                        Zrr.react + Zcort.react, data = Data)
## Pool regression results
pool.lm.imp <- pool(fit.lm.imp) # Pool
pool.rsqr.imp <- pool.r.squared(fit.lm.imp) %>% as.data.frame() # R-squared
pool.rsqr.imp[, "model"] <- mod # Add model number
sum.lm.imp <- summary(pool.lm.imp) # Summary
sum.lm.imp[, "Model"] <- mod # Save the lm model number
sum.lm.imp[, "Var"] <- rownames(sum.lm.imp) # Save the variable names
sum.lm.imp[, "Dep"] <- dependant # Save name of dependent variable
sum.lm.imp <- sum.lm.imp[-1, ] # Remove the intercept
## Check multicollinearity
multicol <- list(NA) # Initialize results list
# Compute VIF for each variable and imputed dataset
for (i in 1:length(fit.lm.imp$analyses)) {
  multicol[[i]] <- vif(fit.lm.imp$analyses[[i]])
}
# Put all VIF's into dataframe
multicol.table.1 <- matrix(nrow = I(nrow(sum.lm.imp)-1), ncol = length(multicol))
for (i in 1:nrow(multicol.table.1)) {
  for (j in 1:ncol(multicol.table.1))
    multicol.table.1[i, j] <- multicol[[j]][[i]]
}
rownames(multicol.table.1) <- rownames(sum.lm.imp)[-1] # Set rownames of variables
# Compute range of VIFs per variable
multicol.table.2 <- multicol.table.1[, c(1:2)] # Initialize new results matrix with two columns
multicol.table.2[, 1] <- apply(multicol.table.1, 1, min) # Calculate min VIF
multicol.table.2[, 2] <- apply(multicol.table.1, 1, max) # Calculate max VIF
multicol.table.2 <- t(multicol.table.2) %>% as.data.frame() # Transpose matrix and save as dataframe
rownames(multicol.table.2) <- c("min_VIF", "max_VIF") # Set descriptive rownames
multicol.table.2 <- multicol.table.2 %>% t() %>% as.data.frame() # Save as transposed dataframe
multicol.table.2[, "var"] <- rownames(multicol.table.2) # Save variable names
rownames(multicol.table.2) <- NULL # Reset rownames
multicol.table.2[ , "model"] <- mod # Save model name
## Partial correlations
# Get predictor variable names
a <- fit.lm.imp[[1]] %>% as.character() # Extract the call for fit.lm.imp
a <- a[2] # Select only the actual call
b <- strsplit(a, " ") # Split the call on spaces to get individual variables
c <- unlist(b) # Unlist to get character vectors
d <- c[!c %in% c("+", "*", "~", "(", ")")] # Remove entries containing special characters: +, *, ~, (, and )
e <- d[-c(1)] # Remove the first entry
f <- unique(e) # Remove double entries
pred <- f
# Loop for all variables in the regression results, skipping the intercept
corr <- list(NA) # Initialize results list
for (i in 1:length(pred)) {
  # Compute correlation between dependent variable and single predictor, correcting for all other predictors
  form <- paste(pred[-i], collapse = "+")  # Create single character string of all predictors to be partialled out
  corr.temp <- micombine.cor(Data, variables = c(dependant, pred[i]), 
                             partial = as.formula(paste("~", form)))
  corr[[i]] <- corr.temp[1, ] # Save only the first row (second row is the same)
}
# Put the correlation results in a table
# Extract results from list
corr.table <- sapply(corr, function(x) {
  c(var1 = levels(x$variable1)[1],
    var2 = levels(x$variable1)[2],
    rho = x$r,
    p.value = x$p)
})
# Save the results as a dataframe
corr.table <- t(corr.table) %>% as.data.frame(stringsAsFactors = FALSE) # Transpose the dataframe
corr.table[, 3:4] <- lapply(corr.table[, 3:4], function(x) as.numeric(as.character(x))) %>% 
  lapply(function(x) round(x, 5)) # Set the numeric columns to numeric and round to 5 decimals
corr.table[, "cohen.d"] <- r2d(corr.table[, "rho"]) # Compute cohen's d
corr.table[, "Model"] <- mod # Save the lm model number
## Bayes factors
# Initialize variables
BFest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the BayesFactors estimates
varest <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = m) # Matrix to put the variance of the dependent variable
est <- matrix(NA, nrow = nrow(sum.lm.imp), ncol = 1) # Matrix to put the pooled estimates
# Loop over all imputed Data_longs
# Loop over all imputed datasets
for (j in 1:m) { # For all imputed Data_longs
  subdata <- Data_long %>% filter(imp == j) # Select imputed Data_long
  BF <- generalTestBF(formula = as.formula(fit.lm.imp[["call"]][["formula"]]), # Calculate BayesFactor
                      data = subdata, whichModels = "bottom", progress = FALSE)
  BFest[, j] <- extractBF(BF, onlybf = TRUE) # Extract only the BayesFactors
  # Calculate the standard error of the estimate per variable (necessary for pooling)
  for (i in 1:nrow(sum.lm.imp)) { # Loop over all variables/models
    if (grepl(":", sum.lm.imp[i, "Var"])) { # If the variable contains an interaction
      Var <- sub("\\:.*", "", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Select the variable name before ":"
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else if (grepl("SexFemale", sum.lm.imp[i, "Var"])) { # If the main effect contains Sex
      Var <- sub("SexFemale", "Sex", grep(":", sum.lm.imp[i, "Var"], value = TRUE)) # Rename SexFemale to Sex
      varest[i, j] <- mean(c( var(subdata[, Var]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    } else { # For 'normal' variables
      varest[i, j] <- mean(c( var(subdata[, sum.lm.imp[i, "Var"]]), var(subdata[, sum.lm.imp[i, "Dep"]]) )) / 
        n.obs 
    }
  }
}
# Pool the descriptives into one estimate with three decimals for all vars
for (i in 1:nrow(sum.lm.imp)) { # For every variable
  est[i, 1] <- pool.scalar(BFest[i, ], varest[i, ], n = n.obs, k = 1)[["qbar"]] %>% unlist() %>% round(3)
}
# Add bayes factors to dataframe
sum.lm.imp[, "BF"] <- est
# Save results
rownames(sum.lm.imp) <- NULL # Reset rownames of regression table
lm.table <- rbind(lm.table, sum.lm.imp) # Save regression table
corrs <- rbind(corrs, corr.table) # Save correlation table
multicol.table <- rbind(multicol.table, multicol.table.2) # Save multicollinearity
rsqr.imp <- rbind(rsqr.imp, pool.rsqr.imp) # Save R-squared results


### Export VIF and R-squared
# For R-squared
rownames(rsqr.imp) <- NULL # Reset rownames
write.xlsx(rsqr.imp, "lm.rsqr_SET_CFC.outl.del.imp.xlsx") # Export R-squared results
# For VIF
rownames(multicol.table) <- NULL # Reset rownames
multicol.table <- multicol.table[-c(grep("Zrr", multicol.table[, "var"], ignore.case = TRUE)), ] # Remove RR from results
multicol.table[I(nrow(multicol.table)+1), "min_VIF"] <- multicol.table[, "min_VIF"] %>% min() # Add overall min in a new row
multicol.table[nrow(multicol.table), "max_VIF"] <- multicol.table[-nrow(multicol.table), "max_VIF"] %>% max() # Add overall max
multicol.table[nrow(multicol.table), "model"] <- "Total"
multicol.table[ , c(1, 2)] <- multicol.table[ , c(1, 2)] %>% round(2) # Round numeric values
write.xlsx(multicol.table, "lm.VIF_SET_CFC.outl.del.imp.xlsx") # Export VIF results

### FDR-correction
# For regression
lm.table <- lm.table[-c(grep("Zrr", lm.table[, "Var"], ignore.case = TRUE)), ] # Remove RR from results
rownames(lm.table) <- NULL # Reset rownames
lm.table[, "p.adj"] <- p.adjust(lm.table[ ,5], method = "fdr", n = nrow(lm.table)) # Do fdr-correction
lm.table[, "p.adj.sig"] <- sapply(lm.table[, "p.adj"], function(x) p.value.sig(x)) # Add column with corrected significance
lm.table[, "p.value.sig"] <- sapply(lm.table[, "p.value"], function(x) p.value.sig(x)) # Add column with uncorrected significance
lm.table[, "BF.evidence"] <- sapply(lm.table[, "BF"], function(x) BF.evidence(x)) # Add column with Bayes factor interpretation
write.xlsx(lm.table, "lm.SET_CFC.outl.del.imp.xlsx") # Export results
# For partial correlations
corrs <- corrs[-c(grep("Zrr", corrs[, "var1"], ignore.case = TRUE), grep("Zrr", corrs[, "var2"], ignore.case = TRUE)), ] # Remove RR from results
rownames(corrs) <- NULL # Reset rownames
corrs[, "p.adj"] <- p.adjust(corrs[,4], method = "fdr", n = nrow(corrs)) # Do fdr-correction
corrs[, "p.adj.sig"] <- sapply(corrs[, "p.adj"], function(x) p.value.sig(x)) # Add column with corrected significance
corrs[, "p.value.sig"] <- sapply(corrs[, "p.value"], function(x) p.value.sig(x)) # Add column with uncorrected significance
corrs[, "cohen.d.mag"] <- sapply(corrs[, "cohen.d"], function(x) cohen.d.magnitude(x)) # Add column with cohen's d magnitude
write.xlsx(corrs, "lm.corrs_SET_CFC.outl.del.imp.xlsx") # Export results


## Remove temporary variables
remove(Data)
remove(fit.lm.imp)
remove(pool.lm.imp)
remove(pool.rsqr.imp)
remove(rsqr.imp)
remove(sum.lm.imp)
remove(p.value.sig)
remove(multicol.table)
remove(multicol.table.1)
remove(multicol.table.2)
remove(corr)
remove(corr.table)
remove(corr.temp)
remove(dependant)
remove(i)
remove(j)
remove(multicol)
remove(corrs)
remove(lm.table)
remove(mod)
remove(Data.list)
remove(a, b, c, d, e, f)
remove(form)
remove(pred)
remove(cohen.d.magnitude)
remove(BF.evidence)
remove(m)
remove(Data_long)
remove(subdata)
remove(BF)
remove(n.obs)
remove(BFest)
remove(varest)
remove(est)


# Scatterplots of interesting associations --------------------------------

## Packages
library(grid) # To organize multiple subplots in one large plot
library(gridBase) # To organize multiple subplots in one large plot
library(gridExtra) # To organize multiple subplots in one large plot
library(ggplot2) # Plotting packages (used for LSAS density plots)
library(reshape2) # To reshape into long format to use with ggplot
library(magrittr) # For piping
library(dplyr) # For data manipulation
library(mice) # For multiple imputed datasets
library(miceadds) # For multiple imputed correlation

# Load data
load("SET_CFC.outl.del.imp.RData")

# Select imputed dataset to illustrate
m <- 1

### Scatterplots
tiff("Scatter_SigCorrs.tiff", width = 75, height = 20, units = "cm", res = 300) # Save TIFF file

## Figure 1: RS_Parietal_Avg_dPAC_Z with RSA.2
corr <- micombine.cor(mi.res = SET_CFC.outl.del.imp, variables =  # Calculate spearman correlation
                        c("RSA.2", "RS_Parietal_Avg_dPAC_Z"))
corr <- corr[1, "r"] %>% round(2) # Extract correlation coefficient and round
data <- mice::complete(SET_CFC.outl.del.imp, m) # # Select a imputed dataset for illustration purposes
Fig1 <- ggplot(data, aes(x = RSA.2, y = RS_Parietal_Avg_dPAC_Z)) + # Make a plot
  geom_point(shape = 1, size = 2, stroke = 1.5) + # Add large hollow scatter point
  geom_smooth(method = lm, colour = "black", size = 1.5) + # Add a thick black regression line
  labs(title="Baseline of parietal PAC vs. RSA", # Add a title, axis labels, and a subtitle
       y = "Baseline parietal PAC (Z-score)", x = expression("Baseline RSA" ~ (ms^{2})),
       subtitle = "a)", 
       caption = bquote(italic("r =") ~ .(corr) ) ) + # Show correlation coefficient in caption
  theme_classic() + # Remove background and gridlines
  theme(
    axis.title = element_text(size = 15), # Increase size of axis labels
    axis.text = element_text(size = 14, colour = "black"), # Increase size of axis ticks
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), # Increase size of bold title and align in the middle
    plot.subtitle = element_text(size = 20, face = "bold"), # Increase size of subtitle and make bold
    plot.caption = element_text(size = 15, hjust = 0) # Increase size of subcaption and place left
  )
print(Fig1)


## Figure 2: react_Parietal_Avg_AAC_R with pep.react
corr <- micombine.cor(mi.res = SET_CFC.outl.del.imp, variables =  # Calculate spearman correlation
                        c("pep.react", "react_Parietal_Avg_AAC_R"))
corr <- corr[1, "r"] %>% round(2) # Extract correlation coefficient and round
data <- mice::complete(SET_CFC.outl.del.imp, m) # # Select a imputed dataset for illustration purposes
Fig2 <- ggplot(data, aes(x = pep.react, y = react_Parietal_Avg_AAC_R)) + # Make a plot
  geom_point(shape = 1, size = 2, stroke = 1.5) + # Add large hollow scatter point
  geom_smooth(method = lm, colour = "black", size = 1.5) + # Add a thick black regression line
  labs(title="Reactivity of parietal AAC vs. PEP", # Add a title, axis labels, and a subtitle
       y = "Parietal AAC reactivity (corr.)", x = "PEP reactivity (ms)",
       subtitle = "b)", 
       caption = bquote(italic("r =") ~ .(corr) ) ) + # Show correlation coefficient in caption
  theme_classic() + # Remove background and gridlines
  theme(
    axis.title = element_text(size = 15), # Increase size of axis labels
    axis.text = element_text(size = 14, colour = "black"), # Increase size of axis ticks
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), # Increase size of bold title and align in the middle
    plot.subtitle = element_text(size = 20, face = "bold"), # Increase size of subtitle and make bold
    plot.caption = element_text(size = 15, hjust = 0) # Increase size of subcaption and place left
  )
print(Fig2)


## Figure 3: react_Parietal_Avg_AAC_R with LSAS
corr <- micombine.cor(mi.res = SET_CFC.outl.del.imp, variables =  # Calculate spearman correlation
                        c("LSAS", "react_Parietal_Avg_AAC_R"))
corr <- corr[1, "r"] %>% round(2) # Extract correlation coefficient and round
data <- mice::complete(SET_CFC.outl.del.imp, m) # # Select a imputed dataset for illustration purposes
Fig3 <- ggplot(data, aes(x = LSAS, y = react_Parietal_Avg_AAC_R)) + # Make a plot
  geom_point(shape = 1, size = 2, stroke = 1.5) + # Add large hollow scatter point
  geom_smooth(method = lm, colour = "black", size = 1.5) + # Add a thick black regression line
  labs(title="Reactivity of parietal AAC vs. Trait anxiety", # Add a title, axis labels, and a subtitle
       y = "Parietal AAC reactivity (corr.)", x = "Trait social anxiety",
       subtitle = "c)", 
       caption = bquote(italic("r =") ~ .(corr) ) ) + # Show correlation coefficient in caption
  theme_classic() + # Remove background and gridlines
  theme(
    axis.title = element_text(size = 15), # Increase size of axis labels
    axis.text = element_text(size = 14, colour = "black"), # Increase size of axis ticks
    plot.title = element_text(size = 20, hjust = 0.5, face = "bold"), # Increase size of bold title and align in the middle
    plot.subtitle = element_text(size = 20, face = "bold"), # Increase size of subtitle and make bold
    plot.caption = element_text(size = 15, hjust = 0) # Increase size of subcaption and place left
  )
print(Fig3)


# Arrange plots side-by-side
grid.arrange(Fig1, Fig2, Fig3, nrow = 1)
# Print Figure
dev.off()

## Remove temporary variables
remove(Fig1, Fig2, Fig3)
remove(data)
remove(corr)
remove(m)


# Heatmap correlations per LSAS group (exloratory) -----------------------------------------------------
## Packages
library(ggplot2) # For plotting
library(magrittr) # For piping
library(dplyr) # For data manipulation
library(reshape2) # For melt function
library(tidyr) # For spread function
library(miceadds) # Multiple imputation correlation
library(grid) # To organize multiple subplots in one large plot
library(gridBase) # To organize multiple subplots in one large plot
library(gridExtra) # To organize multiple subplots in one large plot

### Low LSAS
## Select subset
lowData <- subset_datlist(SET_CFC.outl.del.imp, 
                          subset = SET_CFC.outl.del.imp[[1]][["LSAS_Split"]] == "Low",
                          select=c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                                   "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                                   "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                                   "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                                   "LSAS", "Anx.1", "anx.react", 
                                   "PEP.2", "pep.react",
                                   "RSA.2", "rsa.react",
                                   "RR.2", "rr.react",
                                   "Cortisol.1.log", "cort.react"),
                          toclass="mids")


# Correlation coefficients
corr <- micombine.cor(mi.res = lowData) # Calculate correlation
cormat <- attr(corr,"r_matrix") # Extract matrix
cormat <- cormat %>% round(2) # Round values
# Set the variable names to be more plot-readable
colnames(cormat) <- c("Frontal baseline PAC", "Frontal PAC reactivity",
                      "Parietal baseline PAC", "Parietal PAC reactivity",
                      "Frontal baseline AAC", "Frontal AAC reactivity",
                      "Parietal baseline AAC", "Parietal AAC reactivity",
                      "Trait social anxiety", 
                      "Baseline state anxiety", "State anxiety reactivity",
                      "Baseline PEP", "PEP reactivity", 
                      "Baseline RSA", "RSA reactivity", 
                      "Baseline RR", "RR reactivity", 
                      "Baseline cortisol", "Cortisol reactivity")
rownames(cormat) <- c("Frontal baseline PAC", "Frontal PAC reactivity",
                      "Parietal baseline PAC", "Parietal PAC reactivity",
                      "Frontal baseline AAC", "Frontal AAC reactivity",
                      "Parietal baseline AAC", "Parietal AAC reactivity",
                      "Trait social anxiety",  
                      "Baseline state anxiety", "State anxiety reactivity",
                      "Baseline PEP", "PEP reactivity", 
                      "Baseline RSA", "RSA reactivity", 
                      "Baseline RR", "RR reactivity", 
                      "Baseline cortisol", "Cortisol reactivity")
# Remove everything under the diagonal
cormat[lower.tri(cormat, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat <- melt(cormat, na.rm = TRUE)
rownames(melted_cormat) <- NULL # Reset rownames

# Extract p-values to highlight the significant correlations
cormat_p <- corr # Save the correlation results
cormat_p <- select(cormat_p, c(variable1, variable2, p)) # Select only the variables names and p-values
cormat_p <- spread(cormat_p, key = variable1, value = p) %>% as.matrix() # Reshape from long to wide format
rownames(cormat_p) <- cormat_p[, 1] # Set first variable as rownames
cormat_p <- cormat_p[, -1] # Remove first variable
# Reorder into the same order as the cormat
ordering <- factor(colnames(cormat_p), levels = c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                                                  "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                                                  "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                                                  "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                                                  "LSAS", "Anx.1", "anx.react",
                                                  "PEP.2", "pep.react",
                                                  "RSA.2", "rsa.react",
                                                  "RR.2", "rr.react",
                                                  "Cortisol.1.log", "cort.react"))
cormat_p <- cormat_p [order(ordering), # Set order
                      order(ordering)]
cormat_p[lower.tri(cormat_p, diag = TRUE)] <- NA # Remove everything under the diagonal
melted_cormat_p <- melt(cormat_p, na.rm = TRUE) # Melt the correlation matrix
rownames(melted_cormat_p) <- NULL # Reset rownames
melted_cormat_p$value <- as.numeric(levels(melted_cormat_p$value))[melted_cormat_p$value] # Make numeric
melted_cormat$value.sig <- melted_cormat$value # Create new variable that will contain only significant correlation coefficients
melted_cormat$value.nonsig <- melted_cormat$value # Create new variable that will contain only non-significant correlation coefficients
melted_cormat <- melted_cormat %>% mutate(value.sig = replace(value.sig, melted_cormat_p$value > .05, NA)) # Make NA the correlation coefficients with non-significant p-values
melted_cormat <- melted_cormat %>% mutate(value.nonsig = replace(value.nonsig, melted_cormat_p$value <= .05, NA)) # Make NA the correlation coefficients with significant p-values


## Plot correlation matrix heatmap
# To save high-res figure
tiff("Heatmap_LSAS_CFC.tiff", width = 50, height = 25, units = "cm", res = 300)
# Plot
ggheatmap_low <- ggplot(melted_cormat, aes(Var1, Var2, fill = value)) + # Use melted correlation matrix
  ggtitle("Correlations in low trait anxiety") +
  geom_tile(color = "white") + # White background
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") + # Fill colours
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
  #geom_text(aes(Var1, Var2, label = value), color = "black", size = 4) + # Add correlation coefficients
  geom_text(aes(Var1, Var2, label = value.nonsig), color = "black", size = 3.5) + # Add correlation coefficients
  geom_text(aes(Var1, Var2, label = value.sig), color = "black", size = 3.5, fontface = "bold") + # Add correlation coefficients
  geom_rect(mapping=aes(xmin=0.5, xmax=8.5, ymin=18.5, ymax=7.5), fill = NA, color="red", size = 1) # Add red rectangle around CFC - stress responses correlations
# Print the heatmap
print(ggheatmap_low)


### High LSAS
## Select subset
highData <- subset_datlist(SET_CFC.outl.del.imp, 
                           subset = SET_CFC.outl.del.imp[[1]][["LSAS_Split"]] == "High",
                           select=c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                                    "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                                    "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                                    "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                                    "LSAS", "Anx.1", "anx.react", 
                                    "PEP.2", "pep.react",
                                    "RSA.2", "rsa.react",
                                    "RR.2", "rr.react",
                                    "Cortisol.1.log", "cort.react"),
                           toclass="mids")


# Correlation coefficients
corr <- micombine.cor(mi.res = highData) # Calculate correlation
cormat <- attr(corr,"r_matrix") # Extract matrix
cormat <- cormat %>% round(2) # Round values
# Set the variable names to be more plot-readable
colnames(cormat) <- c("Frontal baseline PAC", "Frontal PAC reactivity",
                      "Parietal baseline PAC", "Parietal PAC reactivity",
                      "Frontal baseline AAC", "Frontal AAC reactivity",
                      "Parietal baseline AAC", "Parietal AAC reactivity",
                      "Trait social anxiety",   
                      "Baseline state anxiety", "State anxiety reactivity",
                      "Baseline PEP", "PEP reactivity", 
                      "Baseline RSA", "RSA reactivity", 
                      "Baseline RR", "RR reactivity", 
                      "Baseline cortisol", "Cortisol reactivity")
rownames(cormat) <- c("Frontal baseline PAC", "Frontal PAC reactivity",
                      "Parietal baseline PAC", "Parietal PAC reactivity",
                      "Frontal baseline AAC", "Frontal AAC reactivity",
                      "Parietal baseline AAC", "Parietal AAC reactivity",
                      "Trait social anxiety",   
                      "Baseline state anxiety", "State anxiety reactivity",
                      "Baseline PEP", "PEP reactivity", 
                      "Baseline RSA", "RSA reactivity", 
                      "Baseline RR", "RR reactivity", 
                      "Baseline cortisol", "Cortisol reactivity")
# Remove everything under the diagonal
cormat[lower.tri(cormat, diag = TRUE)] <- NA
# Melt the correlation matrix
melted_cormat <- melt(cormat, na.rm = TRUE)
rownames(melted_cormat) <- NULL # Reset rownames

# Extract p-values to highlight the significant correlations
cormat_p <- corr # Save the correlation results
cormat_p <- select(cormat_p, c(variable1, variable2, p)) # Select only the variables names and p-values
cormat_p <- spread(cormat_p, key = variable1, value = p) %>% as.matrix() # Reshape from long to wide format
rownames(cormat_p) <- cormat_p[, 1] # Set first variable as rownames
cormat_p <- cormat_p[, -1] # Remove first variable
# Reorder into the same order as the cormat
ordering <- factor(colnames(cormat_p), levels = c("RS_Frontal_Avg_dPAC_Z", "react_Frontal_Avg_dPAC_Z",
                                                  "RS_Parietal_Avg_dPAC_Z", "react_Parietal_Avg_dPAC_Z",
                                                  "RS_Frontal_Avg_AAC_R", "react_Frontal_Avg_AAC_R",
                                                  "RS_Parietal_Avg_AAC_R", "react_Parietal_Avg_AAC_R",
                                                  "LSAS", "Anx.1", "anx.react",
                                                  "PEP.2", "pep.react",
                                                  "RSA.2", "rsa.react",
                                                  "RR.2", "rr.react",
                                                  "Cortisol.1.log", "cort.react"))
cormat_p <- cormat_p [order(ordering), # Set order
                      order(ordering)]
cormat_p[lower.tri(cormat_p, diag = TRUE)] <- NA # Remove everything under the diagonal
melted_cormat_p <- melt(cormat_p, na.rm = TRUE) # Melt the correlation matrix
rownames(melted_cormat_p) <- NULL # Reset rownames
melted_cormat_p$value <- as.numeric(levels(melted_cormat_p$value))[melted_cormat_p$value] # Make numeric
melted_cormat$value.sig <- melted_cormat$value # Create new variable that will contain only significant correlation coefficients
melted_cormat$value.nonsig <- melted_cormat$value # Create new variable that will contain only non-significant correlation coefficients
melted_cormat <- melted_cormat %>% mutate(value.sig = replace(value.sig, melted_cormat_p$value > .05, NA)) # Make NA the correlation coefficients with non-significant p-values
melted_cormat <- melted_cormat %>% mutate(value.nonsig = replace(value.nonsig, melted_cormat_p$value <= .05, NA)) # Make NA the correlation coefficients with significant p-values


## Plot correlation matrix heatmap
# Plot
ggheatmap_high <- ggplot(melted_cormat, aes(Var1, Var2, fill = value)) + # Use melted correlation matrix
  ggtitle("Correlations in high trait anxiety") +
  geom_tile(color = "white") + # White background
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") + # Fill colours
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
  #geom_text(aes(Var1, Var2, label = value), color = "black", size = 4) + # Add correlation coefficients
  geom_text(aes(Var1, Var2, label = value.nonsig), color = "black", size = 3.5) + # Add correlation coefficients
  geom_text(aes(Var1, Var2, label = value.sig), color = "black", size = 3.5, fontface = "bold") + # Add correlation coefficients
  geom_rect(mapping=aes(xmin=0.5, xmax=8.5, ymin=18.5, ymax=7.5), fill = NA, color="red", size = 1) # Add red rectangle around CFC - stress responses correlations
# Print the heatmap
print(ggheatmap_high)

# Arrange plots side-by-side
grid.arrange(ggheatmap_low, ggheatmap_high, nrow = 1)
# Print Figure
dev.off()

## Remove unnecessary variables
remove(lowData)
remove(highData)
remove(corr)
remove(cormat)
remove(cormat_p)
remove(melted_cormat)
remove(melted_cormat_p)
remove(ggheatmap_low)
remove(ggheatmap_high)
remove(ordering)



# Sig area under the curve (exploratory) ----------------------------------------------------------
# For supplements/footnote

## Packages
library(mice) # Imputation analysis
library(magrittr) # Piping
library(xlsx) # Exporting to Excel
library(dplyr) # Data manipulation
library(MKmisc) # t-test for multiple imputation
library(miceadds) # For subsetting
library(psych) # For converting t-value to Cohen's d
library(BayesFactor) # For bayesian statistics with uninformed priors
source("Aladins package\\BF_t.R") # For bayesian statistics with informed priors: "Aladins Bayes Factor in R" (downloaded from https://doi.org/10.17045/sthlmuni.4981154.v3)
source('cohen.d.magnitude.R') # Custom function to check the magnitude of Cohen's d values
source('p.value.sig.R') # Custom function to check the significance of p.values
source('BF.evidence.R') # Custom function to check the interpretation of Bayes Factors


# Initialize data
data_obtained <- SET_CFC.outl.del.imp.extra %$% 
  cbind.data.frame(frontal_PAC.auc, 
                   parietal_PAC.auc,
                   frontal_AAC.auc,
                   parietal_AAC.auc,
                   anx.auc, 
                   pep.auc,
                   rsa.auc,
                   rr.auc,
                   cort.auc)

### T-tests 
# Initialize variables
t <- list(NA) # Saves all t-test results
# Do a t-test for all variables
for (x in 1:I(ncol(data_obtained[["analyses"]][[1]])) ) {
  t[[x]] <- mi.t.test(data_obtained$analyses, 
                      x = colnames(data_obtained[["analyses"]][[1]])[[x]],
                      alternative = "two.sided")
  t[[x]]["data.name"] <- colnames(data_obtained[["analyses"]][[1]])[[x]]
}
## Put the t-test results in a table
# Extract results from list
t.table <- sapply(t, function(x) {
  c(test = x$method,
    test.stat = x$statistic[["t"]],
    df = x$parameter[["df"]],
    p.value = x$p.value)
})
# Save the results as a dataframe
t.table <- as.data.frame(t.table)
names <- sapply(t, function(x) {c(name = x$data.name)}) # Get the variable names
colnames(t.table) <- as.character(names) # Set the columns as variable names
t.table <- t(t.table) %>% as.data.frame(stringsAsFactors = FALSE) # Transpose the dataframe
t.table[, 2:4] <- lapply(t.table[, 2:4], function(x) as.numeric(as.character(x))) %>% 
  lapply(function(x) round(x, 5)) # Set the numeric columns to numeric and round to 5 decimals
t.table[, 4] <- t.table[, 4] %>% as.numeric() %>% round(5) # Set the BF column to class numeric and round to 5 decimals

### Bayes factors with informed or uninformed priors, depending on availability
# BF10 = likelihood of the data given H1, relative to the likehood of the data given H0
# E.g., BF10 = 4 means that there is four times more evidence for H1 than for H0.
# Select data that have matching previously-observed data in long format
data_obtained_long <- mice::complete(SET_CFC.outl.del.imp.extra, action = "long")
data_obtained_long <- select(data_obtained_long, c(".imp", 
                                                   "frontal_PAC.auc",
                                                   "frontal_AAC.auc", 
                                                   "anx.auc"))
colnames(data_obtained_long)[1] <- "imp" # Reset imp column name without dot
# Select CFC reactivity data from previous study
load("LSA_HSA_brief.RData") # Load prior data
data_theory <- select(LSA_HSA_brief, c("frontal_PAC.auc", "frontal_AAC.auc", "anx.auc")) %>% as.matrix()
BF <- list() # Initialize results list for uninformed priors
# Initialize variables for pooling informed priors
m <- SET_CFC.outl.del.imp.extra$m # Number of imputed datasets
BF_test <- matrix(NA, nrow = I(ncol(data_obtained[["analyses"]][[1]])), ncol = m) # Matrix to put the BayesFactors per imputed dataset
n.obs <- matrix(NA, nrow = I(ncol(data_obtained[["analyses"]][[1]])), ncol = m) # Matrix to put the number of observations  per imputed dataset
t_est <- matrix(NA, nrow = I(ncol(data_obtained[["analyses"]][[1]])), ncol = m) # Matrix to put the variance of the variables per imputed dataset
est <- matrix(NA, nrow = I(ncol(data_obtained[["analyses"]][[1]])), ncol = 1) # Matrix to put the pooled estimates over all imputed datasets
se <- function(x) {sqrt(var(x)/length(x))} # Standard error of the mean (SEM) function
## Calculate Bayes factor
for (i in 1:I(ncol(data_obtained[["analyses"]][[1]])) ) {
  # Check whether there are priors available for this variable
  if (colnames(data_obtained[["analyses"]][[1]])[[i]] %in% colnames(data_theory)) {
    ## If yes, use informed prior
    t.table[i, "Prior"] <- "Informed prior" # Save as uninformed prior
    # Loop over all imputed datasets
    for (j in 1:m) { # For all imputed datasets
      subdata <- data_obtained_long %>% filter(imp == j) %>% select( colnames(data_obtained[["analyses"]][[1]])[[i]] ) %>% as.matrix() # Select variables imputed dataset
      # Calculate obtained data
      meanobtained <- subdata %>% mean() %>% round(3) # Calculate mean reactivity
      semobtained <- subdata %>% se() %>% round(3) # Calculate SEM
      dfobtained <- subdata %>% complete.cases() %>% sum() %>% round(3) %>% -1 # Calculate df (sample size - 1)
      # Calculate prior data
      meantheory <- data_theory[, colnames(subdata)] %>% mean() %>% round(3) # Calculate mean reactivity
      sdtheory <- data_theory[, colnames(subdata)] %>% se() %>% round(3) # Calculate SEM
      dftheory <- data_theory[, colnames(subdata)] %>% complete.cases() %>% sum() %>% round(3) %>% -1 # Calculate df (sample size - 1)
      # Calculate Bayes factor
      BF_test[[i, j]] <- BF_t(meantheory, sdtheory, dftheory, meanobtained, semobtained, dfobtained, colnames(data_obtained)[[i]])
      n.obs[[i, j]] <- length(subdata) # Calculate sample size
      t_est[[i, j]] <- mean( c(var(subdata), var(data_theory[, colnames(subdata)])) ) / n.obs[i, j] # The standard error of the estimate (necessary for pooling)
    }
    # Pool the descriptives into one estimate with three decimals for all vars
    BF[[i]] <- pool.scalar(BF_test[i, ], t_est[i, ], n = n.obs[i, ], k = 1)[["qbar"]] %>% unlist() %>% round(3)
    
  } else {
    ## If no, use uninformed prior
    t.table[i, "Prior"] <- "Uninformed prior" # Save as uninformed prior
    n1 <- data_obtained_long %>% filter(imp == 1) %>% nrow() # Number of observations of the group
    # Get Bayes factor
    BF[[i]] <- ttest.tstat(t.table[colnames(data_obtained[["analyses"]][[1]])[[i]], "test.stat"], n1, simple = TRUE)
  }
}
# Add bayes factors to dataframe
t.table[, "BF"] <- BF %>% unlist()
# Add column with Bayes factor interpretation
t.table[, "BF.evidence"] <- sapply(t.table$BF, function(x) BF.evidence(x)) # Add column with interpretation

## FDR-correction
t.table[, "p.value.adj"] <- p.adjust(t.table[, "p.value"], method = "fdr", n = length(t.table[, "p.value"])) # Do fdr-correction
t.table[, "p.adj.sig"] <- sapply(t.table[, "p.value.adj"], function(x) p.value.sig(x)) # Add column with interpretation of adjusted significance
t.table[, "p.value.sig"] <- sapply(t.table[, "p.value"], function(x) p.value.sig(x)) # Add column with interpretation of significance

## Add Cohen's d
t.val <- t.table[, "test.stat"] # Extract t-value
t.val <- abs(t.val) # Calculate absolute t-value
d <- t2d(t.val, n=n1) # Calculate Cohen's d from t-value
t.table$cohen.d <- abs(d) # Put the absolute of Cohen's d in dataframe
t.table$cohen.d.mag <- sapply(t.table$cohen.d, function(x) cohen.d.magnitude(x)) # Add column with magnitude of cohen's d

# Order based on p-value
t.table[, "variable"] <- rownames(t.table) # Save variable name
t.table <- t.table[, c(ncol(t.table), seq(1:I(ncol(t.table)-1)))] # Put variable name column in front
t.table <- t.table %>% arrange(p.value) # Reorder rows

# Export results
write.xlsx(t.table, "AUC_SET_CFC.outl.del.imp.extra.xlsx")

# Remove temporary variables
remove(t.table)
remove(x)
remove(names)
remove(t)
remove(p.value.sig)
remove(n1)
remove(d)
remove(dfobtained)
remove(dftheory)
remove(i)
remove(j)
remove(m)
remove(subdata)
remove(meanobtained)
remove(meantheory)
remove(semobtained)
remove(sdtheory)
remove(n.obs)
remove(est)
remove(data_theory)
remove(data_obtained)
remove(data_obtained_long)
remove(BF_test)
remove(BF)
remove(LSA_HSA_brief)
remove(t_est)
remove(t.val)
remove(BF_t)
remove(BF.evidence)
remove(cohen.d.magnitude)
remove(se)
