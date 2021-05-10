# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Script name:  results.R
# 
# Author:       Caroline Nettekoven, 2021
# Contact:      nettekoven-enquiries@web.de 
# 
# Description:  Script to analyse behavioural and imaging data from 
#               ultra-high field (7T) MRS project ingestigating M1 GABA
#               and functional connectivity in visuomotor adaptation in
#               humans.
# 
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# --------------------------------------------------------------------
# Packages
library(ppcor)
library(Hmisc)
library(tidyr)
library(dplyr)
library(irr)
library(foreign)
library(tidyverse)



# Set working directory (change dir path to cloned github repository)
dir = "/Users/CN/Documents/Projects/Joystick_M1_MRS/Analysis/m1_gaba_adaptation"
setwd(dir)


# Import general functions
source("./PlotCorrelation_line_if_significant.R")
source("./Test_difference_spearman.R")

# Import data
data_rotation <- read.csv("./data_rotation.csv")
data_control <- read.csv("./data_control.csv")

corrmethod = "spearman"
# ----------------------Correlation : M1_Cerebellar_ConnectivityChange - Retention --------------------------

data_to_correlate <- data.frame("ConnectivityChange" = data_rotation$M1_Cerebellar_ConnectivityChange,
                   "Retention" = data_rotation$Retention
)
X <- "ConnectivityChange"
Y <- "Retention"
xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)


# ---------------------- Control Correlation : M1_Cerebellar_ConnectivityChange - Adaptation --------------------------
data_to_correlate <- data.frame("ConnectivityChange" = data_rotation$M1_Cerebellar_ConnectivityChange,
                   "Adaptation" = data_rotation$Adaptation
)
X <- "ConnectivityChange"
Y <- "Adaptation"
xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)


# Fisher Difference: 
data_to_correlate_1 <- data.frame(data_rotation$M1_Cerebellar_ConnectivityChange,data_rotation$Retention)
results1 <- rcorr(as.matrix(data_to_correlate_1), type = "spearman")
results2 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
Test_difference_spearman(results1, results2)

# ---------------------- Control Correlation : Left Cerebellum Left M1 Connectivity - Retention --------------------------
data_to_correlate <- data.frame("ConnectivityChange" = data_rotation$M1_LeftCerebellar_ConnectivityChange,
                   "Retention" = data_rotation$Retention
)
X <- "ConnectivityChange"
Y <- "Retention"
xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)

# Fisher Difference: 
data_to_correlate_1 <- data.frame(data_rotation$M1_Cerebellar_ConnectivityChange,data_rotation$Retention)
results1 <- rcorr(as.matrix(data_to_correlate_1), type = "spearman")
results2 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
Test_difference_spearman(results1, results2)


# ---------------------- Control Correlation :  Control Condition Connectivity Change - Retention --------------------------
data_to_correlate <- data.frame("ConnectivityChange" = data_control$M1_Cerebellar_ConnectivityChange,
                   "Retention" = data_control$Retention
)
X <- "ConnectivityChange"
Y <- "Retention"
xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)


# Fisher Difference: 
data_to_correlate_1 <- data.frame(data_rotation$M1_Cerebellar_ConnectivityChange,data_rotation$Retention)
results1 <- rcorr(as.matrix(data_to_correlate_1), type = "spearman")
results2 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
Test_difference_spearman(results1, results2)


# ----------------------Correlation : M1_Cerebellar_ConnectivityChange - GABA --------------------------
data_to_correlate <- data.frame("ConnectivityChange" = data_rotation$M1_Cerebellar_ConnectivityChange,
                   "GABA" = data_rotation$GABA
)
X <- "ConnectivityChange"
Y <- "GABA"
xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)


# control for GM
model1  = lm(M1_Cerebellar_ConnectivityChange ~ GM, data=data_rotation)
model2  = lm(GABA ~ GM, data=data_rotation)

data_to_correlate <- data.frame("ConnectivityChange" = as.vector(residuals.lm(model1)),
                              "GABA" = as.vector(residuals.lm(model2))
)
X <- "ConnectivityChange"
Y <- "GABA"
xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)


# Stats: GABA - Connectivity Change
result.GABA <- rcorr(data_rotation$M1_Cerebellar_ConnectivityChange,
                     data_rotation$GABA, corrmethod)

# Stats: GABA - Connectivity Change - Controlling for GM
resultpartial.GABA.GM <- pcor.test(data_rotation$M1_Cerebellar_ConnectivityChange,
                                   data_rotation$GABA,
                                   data_rotation$GM, corrmethod)

# ---------------------- Control Correlation : M1_LeftCerebellar_ConnectivityChange - GABA --------------------------
# control for GM
model1  = lm(M1_LeftCerebellar_ConnectivityChange ~ GM, data=data_rotation)
model2  = lm(GABA ~ GM, data=data_rotation)

data_to_correlate <- data.frame("ConnectivityChange" = as.vector(residuals.lm(model1)),
                   "GABA" = as.vector(residuals.lm(model2))
)
X <- "ConnectivityChange"
Y <- "GABA"
xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)

# Fisher Difference:
results1 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
results1b <- pcor.test(data_rotation$M1_Cerebellar_ConnectivityChange,data_rotation$GABA,data_rotation$GM, corrmethod)
results1$r[2] <- results1b$estimate
results1$n[2] <- results1b$n

results2 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
results2b <- pcor.test(data_rotation$M1_LeftCerebellar_ConnectivityChange,data_rotation$GABA,data_rotation$GM, corrmethod)
results2$r[2] <- results2b$estimate
results2$n[2] <- results2b$n

Test_difference_spearman(results1, results2)

# ---------------------- Control Correlation : M1_Cerebellar_ConnectivityChange - Glu --------------------------
resultpartial.Glu.GM <- pcor.test(data_rotation$M1_Cerebellar_ConnectivityChange,
                                  data_rotation$Glu,
                                  data_rotation$GM, corrmethod)
# control for GM
model1  = lm(M1_Cerebellar_ConnectivityChange ~ GM, data=data_rotation)
model2  = lm(Glu ~ GM, data=data_rotation)
data_to_correlate <- data.frame("ConnectivityChange" = as.vector(residuals.lm(model1)),
                   "Glu" = as.vector(residuals.lm(model2))
)
X <- "ConnectivityChange"
Y <- "Glu"
xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)

data_to_correlate <- data.frame("ConnectivityChange" = data_rotation$M1_Cerebellar_ConnectivityChange,
                   "Glu" = data_rotation$Glu
)
X <- "ConnectivityChange"
Y <- "Glu"
xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)


# Fisher Difference:
results1 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
results1b <- pcor.test(data_rotation$M1_Cerebellar_ConnectivityChange,data_rotation$GABA,data_rotation$GM, corrmethod)
results1$r[2] <- results1b$estimate
results1$n[2] <- results1b$n

results2 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
results2b <- pcor.test(data_rotation$M1_Cerebellar_ConnectivityChange,data_rotation$Glu,data_rotation$GM, corrmethod)
results2$r[2] <- results2b$estimate
results2$n[2] <- results2b$n

Test_difference_spearman(results1, results2)

# ---------------------- Control Correlation : Control Condition M1_Cerebellar_ConnectivityChange - GABA --------------------------

# control for GM
model1  = lm(M1_Cerebellar_ConnectivityChange ~ GM, data=data_control[complete.cases(data_control$GABA),])
model2  = lm(GABA ~ GM, data=data_control[complete.cases(data_control$GABA),])
data_to_correlate <- data.frame("ConnectivityChange" = as.vector(residuals.lm(model1)),
                   "GABA" = as.vector(residuals.lm(model2))
)
X <- "ConnectivityChange"
Y <- "GABA"
xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)


pcor.test(data_control[complete.cases(data_control$GABA),]$Retention,data_control[complete.cases(data_control$GABA),]$GABA,data_control[complete.cases(data_control$GABA),]$GM, corrmethod)
# Fisher Difference:

results1 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
results1b <- pcor.test(data_rotation$Retention,data_rotation$GABA,data_rotation$GM, corrmethod)
results1$r[2] <- results1b$estimate
results1$n[2] <- results1b$n

results2 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
results2b <- pcor.test(data_control[complete.cases(data_control$GABA),]$Retention,data_control[complete.cases(data_control$GABA),]$GABA,data_control[complete.cases(data_control$GABA),]$GM, corrmethod)
results2$r[2] <- results2b$estimate
results2$n[2] <- results2b$n

Test_difference_spearman(results1, results2)

# ----------------------Correlation : GABA - Retention --------------------------

data_to_correlate <- data.frame( "GABA" = data_rotation$GABA,
                    "Retention" = data_rotation$Retention
                  
)
X <- "GABA"
Y <- "Retention"

xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)


# ______________________ Controlling for GM __________________________

model1  = lm(Retention ~ GM, data=data_rotation)
model2  = lm(GABA ~ GM, data=data_rotation)

data_to_correlate <- data.frame("GABA" = as.vector(residuals.lm(model2)),
                   "Retention" = as.vector(residuals.lm(model1))
)
X <- "GABA"
Y <- "Retention"

xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)


resultpartial.GABA.Retention.GM <- pcor.test(data_rotation$Retention,
                                             data_rotation$GABA,
                                             data_rotation$GM, corrmethod)

# ---------------------- Control Correlation : Glu - Retention --------------------------

# control for GM
model1  = lm(Retention ~ GM, data=data_rotation)
model2  = lm(Glu ~ GM, data=data_rotation)
data_to_correlate <- data.frame("Retention" = as.vector(residuals.lm(model1)),
                   "Glu" = as.vector(residuals.lm(model2))
)
X <- "Retention"
Y <- "Glu"
xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)


pcor.test(data_rotation$Retention,data_rotation$Glu,data_rotation$GM, corrmethod)
pcor.test(data_rotation$Retention,data_rotation$GABA,data_rotation$Glu, corrmethod)

data_to_correlate <- data.frame(data_rotation$Retention,data_rotation$Glu, data_rotation$GABA, data_rotation$GM)
pcor(data_to_correlate, corrmethod)


y.data <- data.frame(
  Retention=data_rotation$Retention,
  GABA=data_rotation$GABA,
  Glu=data_rotation$Glu)
pcor(y.data, corrmethod) 
# Fisher Difference:

results1 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
results1b <- pcor.test(data_rotation$Retention,data_rotation$GABA,data_rotation$GM, corrmethod)
results1$r[2] <- results1b$estimate
results1$n[2] <- results1b$n

results2 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
results2b <- pcor.test(data_rotation$Retention,data_rotation$Glu,data_rotation$GM, corrmethod)
results2$r[2] <- results2b$estimate
results2$n[2] <- results2b$n

Test_difference_spearman(results1, results2)
# ---------------------- Control Correlation : GABA - Adaptation --------------------------

data_to_correlate <- data.frame("GABA" = data_rotation$GABA,
                   "Adaptation" = data_rotation$Adaptation
)
X <- "GABA"
Y <- "Adaptation"
xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)


# control for GM
model1  = lm(Adaptation ~ GM, data=data_rotation)
model2  = lm(GABA ~ GM, data=data_rotation)
data_to_correlate <- data.frame("GABA" = as.vector(residuals.lm(model2)),
                   "Adaptation" = as.vector(residuals.lm(model1))
)
X <- "GABA"
Y <- "Adaptation"

xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)



# Fisher Difference: 

results1 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
results1b <- pcor.test(data_rotation$Retention,data_rotation$GABA,data_rotation$GM, corrmethod)
results1$r[2] <- results1b$estimate
results1$n[2] <- results1b$n

results2 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
results2b <- pcor.test(data_rotation$Adaptation,data_rotation$GABA,data_rotation$GM, corrmethod)
results2$r[2] <- results2b$estimate
results2$n[2] <- results2b$n

Test_difference_spearman(results1, results2)

# ---------------------- Control Correlation : Control Condition GABA - Retention --------------------------

# control for GM
model1  = lm(Retention ~ GM, data=data_control[complete.cases(data_control$GABA),])
model2  = lm(GABA ~ GM, data=data_control[complete.cases(data_control$GABA),])

data_to_correlate <- data.frame("Retention" = as.vector(residuals.lm(model1)),
                   "GABA" = as.vector(residuals.lm(model2)))
X <- "Retention"
Y <- "GABA"
xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)


# Fisher Difference:

results1 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
results1b <- pcor.test(data_rotation$M1_Cerebellar_ConnectivityChange,data_rotation$GABA,data_rotation$GM, corrmethod)
results1$r[2] <- results1b$estimate
results1$n[2] <- results1b$n

results2 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
cases_controlgaba <- data_control[complete.cases(data_control$GABA),]
results2b <- pcor.test(cases_controlgaba$M1_Cerebellar_ConnectivityChange,cases_controlgaba$GABA,cases_controlgaba$GM, corrmethod)
results2$r[2] <- results2b$estimate
results2$n[2] <- results2b$n

Test_difference_spearman(results1, results2)

# ----------------------Correlation : Cerebellar Network Strength Change - Adaptation --------------------------

data_to_correlate <- data.frame("NetworkChange" = data_rotation$Cerebellar_Network_Change,
                   "Adaptation" = data_rotation$Adaptation
)
X <- "NetworkChange"
Y <- "Adaptation"
xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)



# ---------------------- Control Correlation : Default Mode Network Strength Change  - Adaptation --------------------------

data_to_correlate <- data.frame("NetworkChange_DMN" = data_rotation$DMN_Change,
                   "Adaptation" = data_rotation$Adaptation
)
X <- "NetworkChange_DMN"
Y <- "Adaptation"
xyresults <- rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)


# Fisher Difference: 
data_to_correlate_1 <- data.frame(data_rotation$Cerebellar_Network_Change,
                       data_rotation$Adaptation)
results1 <- rcorr(as.matrix(data_to_correlate_1), type = "spearman")
results2 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
Test_difference_spearman(results1, results2)


# ---------------------- Control Correlation : Cerebellar Network Strength Change - Retention --------------------------
data_to_correlate = data.frame("ConnectivityChange_WholeNetwork" = data_rotation$Cerebellar_Network_Change,
                  "Retention" = data_rotation$Retention
)
X = "ConnectivityChange_WholeNetwork"
Y = "Retention"
xyresults = rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)
# 

# Fisher Difference: 
data_to_correlate_1 <- data.frame(data_rotation$Cerebellar_Network_Change,
                       data_rotation$Adaptation)
results1 <- rcorr(as.matrix(data_to_correlate_1), type = "spearman")
results2 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
Test_difference_spearman(results1, results2)

# ---------------------- Control Correlation : Cerebellar Network Strength Change - Adaptation (Control Condition) --------------------------
data_to_correlate = data.frame("ConnectivityChange_WholeNetwork" = data_control$Cerebellar_Network_Change,
                  "Adaptation" = data_control$Adaptation
)
X = "ConnectivityChange_WholeNetwork"
Y = "Adaptation"
xyresults = rcorr(as.matrix(data_to_correlate), type = corrmethod)
PlotCorrelation_line_if_significant(data_to_correlate, xyresults,X,Y)
# 

# Fisher Difference: 
data_to_correlate_1 <- data.frame(data_rotation$Cerebellar_Network_Change,
                    data_rotation$Adaptation)
results1 <- rcorr(as.matrix(data_to_correlate_1), type = "spearman")
results2 <- rcorr(as.matrix(data_to_correlate), type = "spearman")
Test_difference_spearman(results1, results2)

