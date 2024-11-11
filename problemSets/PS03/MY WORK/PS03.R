# Applied Statistical Analysis I
# Problem Set 3

#####################
setwd("/Users/eimhi/Documents/GitHub/StatsI_Fall2024/problemSets/PS03")
getwd()
#####################



#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", "package:graphics", "package:grDevices", "package:utils", "package:datasets", "package:methods", "package:base")
  package.list <- search()[ifelse(unlist(gregexpr("package:", search()))==1, TRUE, FALSE)]
  package.list <- setdiff(package.list, basic.packages)
  if (length(package.list)>0)  for (package in package.list) detach(package,  character.only=TRUE)
}
detachAllPackages()

# load libraries
pkgTest <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[,  "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg,  dependencies = TRUE)
  sapply(pkg,  require,  character.only = TRUE)
}

# here is where you load any necessary packages
# ex: stringr
# lapply(c("stringr"),  pkgTest)

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/incumbents_subset.csv")

##################
### Question 1 ###
##################

# interested in knowing how difference in campaign spending between incumbent
# and challenger affects the incumbent’s vote share.

# 1. Run a regression where the outcome variable is voteshare 
# the explanatory variable is difflog.
reg_q1_vs_difflog <- lm(voteshare ~ difflog, data = inc.sub)

# 2. Make a scatterplot of the two variables and add the regression line
png(file="vs_difflog_plot.png")
plot(inc.sub$difflog, inc.sub$voteshare, 
     main="Scatterplot of difflog vs voteshare", 
     xlab="difflog", 
     ylab="voteshare")
abline(reg_q1_vs_difflog, col="darkblue")
dev.off()

# 3. Save the residuals of the model in a separate object
resid_q1 <- residuals(reg_q1_vs_difflog)
print(resid_q1)

# 4. Write the prediction equation
coef(reg_q1_vs_difflog)

# prediction equation
# voteshare = 0.5790 + 0.0417(difflog)

# so if the incumbent outspends the challenger 
# their vote share tends to increase

# But the effect size is modest
# a one unit increase in difflog 
# only raises the vote share by about 4.17%

##################
### Question 2 ###
##################

# interested in knowing how difference between incumbent and challenger’s 
# spending and the vote share of the presidential candidate of the incumbent’s 
# party are related

# 1. run regression where the outcome variable is presvote 
# the explanatory variable is difflog.
reg_q2_pv_difflog <- lm(presvote ~ difflog, data = inc.sub)

# 2. make a scatterplot of the two variables and add the regression line
png(file="pv_difflog_plot_3.png")
plot(inc.sub$difflog, inc.sub$presvote, 
     main="Scatterplot of difflog vs presvote", 
     xlab="difflog", 
     ylab="presvote")
abline(reg_q2_pv_difflog, col="blue")
dev.off()

# 3. save the residuals of the model in a separate object
resid_q2 <- residuals(reg_q2_pv_difflog)
print(resid_q2)

# 4. write the prediction equation
coef(reg_q2_pv_difflog)

# prediction equation
# presvote = 0.5076 + 0.0238(difflog)

# as incumbent spends more relative to the challenger 
# the presidential candidate from incumbent’s party 
# is slightly more likely 
# to gain higher vote share in the district

# thhe 2.38 % increase per unit of difflog 
# suggests that the incumbent’s spending has only a minor impact
# more likely other factors affect it more

##################
### Question 3 ###
##################

#  interested in knowing how the vote share of the presidential candidate of
# incumbent’s party is associated with the incumbent’s electoral success

# 1. run regression where the outcome variable is voteshare 
# the explanatory variable is presvote
reg_q3_vs_pv <- lm(voteshare ~ presvote, data = inc.sub)

# 2. make a scatterplot of the two variables and add the regression line
png(file="vs_pv_plot.png")
plot(inc.sub$presvote, inc.sub$voteshare, 
     main="Scatterplot of presvote vs voteshare", 
     xlab="presvote", 
     ylab="voteshare")
abline(reg_q3_vs_pv, col="darkgreen")
dev.off()

# just saving the residuals of the model anyway for consistency
resid_q3 <- residuals(reg_q3_vs_pv)
print(resid_q3)

# 4. write the prediction equation
coef(reg_q3_vs_pv)

# prediction equation
# voteshare = 0.4413 + 0.388 × presvote

# When the presidential candidate from the incumbent performs better 
# incumbentt tends to also see a rise in voteshare

# the 1 % increase in presvote generates a 0.388% increase in the incumbent vs 
# this suggests alignment with a popular pres candidate can boost an inc support


##################
### Question 4 ###
##################

# The residuals from part (a) tell us how much of the variation in voteshare 
# is not explained by the difference in spending between incumbent & challenger

# The residuals in part (b) tell us how much of the variation in presvote is not
# explained by the difference in spending between incumbent and challenger 
# in the district.


# 1. run a regression where the outcome variable is the residuals from Q 1
# explanatory variable is the residuals from Q 2.
reg_q4_resid12 <- lm(resid_q1 ~ resid_q2)

# 2. make a scatterplot of the two residuals and add the regression line
png(file="resid_plot.png")
plot(resid_q2, resid_q1, 
     main="Scatterplot of residuals (Q1 vs Q2)", 
     xlab="Residuals from Q2", 
     ylab="Residuals from Q1")
abline(reg_q4_resid12, col="blue")
dev.off()

# 3. write the prediction equation
coef(reg_q4_resid12)

# prediction equation
# resid_q1 = −5.93×10*−18 + 0.2569 × resid_q2

# positive slope indicates the unexplained variation in pv is positively 
# associated with the unexplained variation in vs

# a 1 unit increase in the residuals of pv g to an enerates 
# an increase of approx 0.2569 in the residuals of vs
# once again, positive association

##################
### Question 5 ###
##################

# What if the incumbent’s vote share is affected by both the president’s 
# popularity and the difference in spending between incumbent and challenger?
  
# 1. Run a regression where the outcome variable is the incumbent’s voteshare
# explanatory variables are difflog and presvote.
reg_q5_vs_dl_pv <- lm(voteshare ~ difflog + presvote, data = inc.sub)

# 2. Write the prediction equation
coef(reg_q5_vs_dl_pv)

# prediction equation
# voteshare = 0.4486 + 0.0355 × difflog + 0.2569 × presvote

# difflog
# as incumbent spends more compared to the challenger 
# their voteshare slightly increases

# presvote
# larger coeff for presvote indicates that the incumbent vs
# more strongly affected by the pres candidate’s performance 

# potentially showing a rising tide carries all boats phenomenon
# wheere the big race for president could determine lower level races
# more locally for example
