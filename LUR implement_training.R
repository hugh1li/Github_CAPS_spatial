# 1. prerequisite ---------------------------------------------------------
# install and load packages
install.packages("tidyverse")
install.packages("car")
install.packages("ape")
install.packages("DAAG")
library(tidyverse)
library(car)
library(ape)
library(DAAG)
# load functions
source('make_lur.R')

# import your x and y 
data <- read_csv('LUR input heinz.csv')

# 2. run model ---------------------------------------------------------------
PM <- make_lur(data, response='PM', dep_col = 133, exclude=c("Latitude","Longitude"))

# 3. diagnostics --------------------------------------------------------------
# p value (> 0.1), cook's D (influential sample, > 1), VIF (colinearity of x, > 3), Moran's I (spatial autocorrelation, p < 0.05), mean studentized prediction residual (MSPR), root mean square of studentized residuals (RMS) produced by leave-one-out cross validation (LOOCV), LOOCV R2
# p value
"PM ~  + CSMPM + RAIL500 + LUINDUS1000 + DISTINVALL + LUCOMM5000 + LUINDUS500"
PM1 <-lm(formula("PM ~  + CSMPM + RAIL500 + LUINDUS1000 + DISTINVALL + LUCOMM5000  "), data)
summary(PM1)
PM2 <-lm(formula("PM ~  + CSMPM + RAIL500 + LUINDUS1000 + DISTINVALL"), data)
summary(PM2)
PM3 <-lm(formula("PM ~  + CSMPM + RAIL500 + LUINDUS1000 "), data)
summary(PM3)

# cook's D
plot(PM3, which = 4)

# VIF
vif(PM3)

# Moran's I 
# https://stats.idre.ucla.edu/r/faq/how-can-i-calculate-morans-i-in-r/
data.dists <- as.matrix(dist(cbind(data$Longitude,data$Latitude)))
data.dists.inv <- 1/data.dists
diag(data.dists.inv) <- 0
Moran.I(data$PM, data.dists.inv)

# LOOCV R2
loocv_PM <- cv.lm(PM3$model, PM3, m=36, legend.pos = "topright")
cor(loocv_PM$PM,loocv_PM$cvpred)**2

# mean studentized prediction residuals (sd used n-1)
M_PM<-rstudent(PM3)
mean(M_PM)
# root mean square of studentized
sqrt(mean(M_PM^2))






