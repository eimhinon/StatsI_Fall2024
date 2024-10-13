# Applied Statistical Analysis I
# Problem Set 2

setwd("/Users/eimhi/Documents/GitHub/StatsI_Fall2024/problemSets/PS02")
getwd()
#####################

#######################
# Problem 1 - Political Science
######################
# start off by creating the contingency table 
bribe_data <- matrix(c(14, 6, 7, 7, 7, 1), nrow = 2, byrow = TRUE) 

# assign row and column names
rownames(bribe_data) <- c("Upper Class", "Lower Class")
colnames(bribe_data) <- c("Not Stopped", "Bribe Requested", "Stopped/Given Warning")
                     
# print the table with labels
print(bribe_data)

# a) Calculate X^2 manually
# first, going to need to get the totals for the rows and columns
# then, get the observed and expected frequencies

upper_class_total <- 14 + 6 + 7
lower_class_total <- 7 + 7 + 1
grand_total <- upper_class_total + lower_class_total 

not_stopped_total <- 14 + 7
bribe_requested_total <- 6 + 7 
stopped_warning_total <- 7 + 1

# fe is (row total / grand total) multiplied by column total
uc_ns_fo <- 14
uc_ns_fe <- (upper_class_total / grand_total) * not_stopped_total

uc_br_fo <- 6
uc_br_fe <- (upper_class_total / grand_total) * bribe_requested_total

uc_sw_fo <- 7
uc_sw_fe <- (upper_class_total / grand_total) * stopped_warning_total

lc_ns_fo <- 7
lc_ns_fe <- (lower_class_total / grand_total) * not_stopped_total

lc_br_fo <- 7
lc_br_fe <- (lower_class_total / grand_total) * bribe_requested_total

lc_sw_fo <- 1
lc_sw_fe <- (lower_class_total / grand_total) * stopped_warning_total

X2 <- ((uc_ns_fo - uc_ns_fe)^2 / uc_ns_fe) + ((uc_br_fo - uc_br_fe)^2 / uc_br_fe) + ((uc_sw_fo - uc_sw_fe)^2 / uc_sw_fe) + ((lc_ns_fo - lc_ns_fe)^2 / lc_ns_fe) + ((lc_br_fo - lc_br_fe)^2 / lc_br_fe) + ((lc_sw_fo - lc_sw_fe)^2 / lc_sw_fe)
X2
# X2 = 3.791168

# chi sq test to look over

# b) i) calculate the p-value from the X^2 i just got

# degrees of freedom
# df - (rows - 1) (columns - 1)

df <- (2 - 1) * (3 - 1)

# p-value
# going to use the pchisq function in r using the X^2 i just calculated,
# the degrees of freedom

pchisq(X2, df, lower.tail = FALSE)

# p-value = 0.1502306

# b) ii) what do i conclude when alpha = 0.1

# Since the calculated p-value is 0.1502306 and therefore, higher than our alpha,
# we fail to reject the null hypothesis 


# c) standardised residuals for each cell in the table

# to get the standardised residuals, first need to get the standard error
# and then the Z stat

# standard error
# se = sqrt(f_expected(1-row pop)(1-column pop))

# the z stat
# z = (f_observed-f_expected)/se

# Upper Class - Not Stopped 
uc_ns_se = sqrt(uc_ns_fe * (1 - (upper_class_total / grand_total)) * (1 - (not_stopped_total / grand_total)))
uc_ns_z = (uc_ns_fo - uc_ns_fe) / uc_ns_se

# Upper Class - Bribe Requested 
uc_br_se = sqrt(uc_br_fe * (1 - (upper_class_total / grand_total)) * (1 - (bribe_requested_total / grand_total)))
uc_br_z = (uc_br_fo - uc_br_fe) / uc_br_se

# Upper Class - Stopped / Warning
uc_sw_se = sqrt(uc_sw_fe * (1 - (upper_class_total / grand_total)) * (1 - (stopped_warning_total / grand_total)))
uc_sw_z = (uc_sw_fo - uc_sw_fe) / uc_sw_se

# Lower Class - Not Stopped 
lc_ns_se = sqrt(lc_ns_fe * (1 - (lower_class_total / grand_total)) * (1 - (not_stopped_total / grand_total)))
lc_ns_z = (lc_ns_fo - lc_ns_fe) / lc_ns_se

# Lower Class - Bribe Requested 
lc_br_se = sqrt(lc_br_fe * (1 - (lower_class_total / grand_total)) * (1 - (bribe_requested_total / grand_total)))
lc_br_z = (lc_br_fo - lc_br_fe) / lc_br_se

# Lower Class - Stopped / Warning
lc_sw_se = sqrt(lc_sw_fe * (1 - (lower_class_total / grand_total)) * (1 - (stopped_warning_total / grand_total)))
lc_sw_z = (lc_sw_fo - lc_sw_fe) / lc_sw_se

# new table
bribe_data_z <- matrix(c(0.3220, -1.6420, 1.5230, -0.3220, 1.6420, -1.5230), nrow = 2, byrow = TRUE)

# assign row and column names
rownames(bribe_data_z) <- c("Upper Class", "Lower Class")
colnames(bribe_data_z) <- c("Not Stopped", "Bribe Requested", "Stopped/Given Warning")

# print the table with labels
print(bribe_data_z)

#######################
# Problem 2 - Economics
#######################

# get the data
econ_data <- read.csv("https://raw.githubusercontent.com/kosukeimai/qss/master/PREDICTION/women.csv")
econ_data


# a) hypothesis formulation
# two tailed hypothesis test
# does res policy affect new water facilities?

# null hypothesis (H0): reservation policy has no effect on the number of 
# new/repaired water facilities (𝛽policy = 0)

# alternate hypothesis (Ha): reservation policy does have an effect on the 
# number of new/repaired water facilities (𝛽policy is not = 0)


# b) run a bivariate regression
# have to use the variables of water and reserved for my regression

# fit the linear regression model
model <- lm(water~reserved, data=econ_data)

# t test for the slope
summary(model)

# check the p-value
# p-value is equal to 0.0197

# c) interpret coefficient estimate for reservation policy

# model summary

# lm(formula = water ~ reserved, data = econ_data)

# Residuals:
#   Min      1Q  Median      3Q     Max 
# -23.991 -14.738  -7.865   2.262 316.009 

# Coefficients:
#            Estimate Std. Error  t value  Pr(>|t|)    
# (Intercept) 14.738      2.286   6.446 4.22e-10 ***
#   reserved    9.252      3.948   2.344   0.0197 *  
  ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# Residual standard error: 33.45 on 320 degrees of freedom
# Multiple R-squared:  0.01688,	Adjusted R-squared:  0.0138 
# F-statistic: 5.493 on 1 and 320 DF,  p-value: 0.0197
  

