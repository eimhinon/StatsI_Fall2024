# Applied Statistical Analysis I
# Problem Set 4

#####################
setwd("/Users/eimhi/Documents/GitHub/StatsI_Fall2024/problemSets/PS04")
getwd()
#####################


# load library
install.packages("car") 
library(car)
help(car)

# load data
data(Prestige)
help(Prestige)


#########################
### Question 1 - ECON ###
#########################

# (A) Create a new variable professional by recoding the variable type so that 
# professionals are coded as 1, 
# and blue and white collar workers are coded as 0 (Hint: ifelse).
Prestige$professional <- ifelse(Prestige$type == "prof", 1, 0)
print(Prestige$professional)

#note that there are NAs for athletes, babysitters, farmers and newsboys

# (B) Run a linear model with prestige as an outcome and income, professional, 
# and the interaction of the two as predictors 
# (Note: this is a continuous × dummy interaction.)
prestige_model <- lm(prestige ~ income * professional, data = Prestige)

# get the summary
summary(prestige_model)

# (C) Write the prediction equation based on the result

# prestige = 21.1422589 + 0.0031709 x income + 37.7812800 x professional - 0.0023257 x (income x professiomal)

# (D) Interpret the coefficient for income

# income coeff = 0.0031709
# so for each unit increase in income for non professionals (when prof = 0)
# it increase their prestige by a really small amount

# for a blue or white collar person to increase their prestige by 3.2 points
# theyd have to earn a $1000 more

# (E) Interpret the coefficient for professional
# prof coeff = 37.7812800  

# for anyone in a professional job, assuming the same income as a blue or white 
# collar worker
# they would have 37.78 more prestige points

# shows a big effect for those in professional jobs

# (F) What is the effect of a $1,000 increase in income on prestige score for 
# professional occupations? In other words, we are interested in the marginal 
# effect of income when the variable professional takes the value of 1. 
# Calculate the change in ˆy associated with a $1k increase in income using (c)

# marginal effect is income coeff + interaction coeff

income_coeff <- coef(prestige_model)["income"]
interaction_coeff <- coef(prestige_model)["income:professional"]

marginal_effect_income <- income_coeff + interaction_coeff
print(marginal_effect_income)
# 0.0008452 

0.0008452 * 1000
# 0.8452

# prestige only increases by ~ 0.9
# indicating its not all about the money for professionals
# the job titles could be doing more of that signalling

# (G) What is the effect of changing one’s occupations from non-professional to 
# professional when her income is $6,000? 
# We are interested in the marginal effect of professional jobs when the 
# variable income takes the value of 6, 000. Calculate the change in ˆy based on
# your answer for (c)

# get the professional coeff
prof_coeff <- coef(prestige_model)["professional"]
# 37.7812800  

# the effect is going to be the prof coeff + interaction coeff x 6000

mod_interaction_coeff <- interaction_coeff * 6000
# -13.95425 

# so the effect is going to be 
prof_coeff + mod_interaction_coeff
#  23.82703

# prestige increases by 23.82703
# indicatingthat if someone earns 6k and switches from wc/bc to prof
# their prestige increase but the income is an inhibiting factor
# reducing the potential effect


###########################
### Question 2 - POLSCI ###
###########################

# (A) Use the results from a linear regression to determine whether having 
# these yard signs in a precinct affects vote share
# conduct a hypothesis test with a = .05

# 1 - assumptions
# normality, homoscedasticity, independence

# 2 - hypoth formulation
# null
# beta (lawn signs) = 0

# alternate
# beta (lawn signs) is not = 0

# 3 - test stat
# precinct with yard signs has 30 samples
# coefficient
lawn_coeff <- 0.042

# standard error
lawn_se <- 0.016

# tstat
lawn_t <- lawn_coeff / lawn_se
# 2.625 

# 4 - p - value
# so for the testing
# compare with p-value
# where df = 131 - 3 (number of explanatory variables here)
lawn_p <- 2 * pt(-abs(lawn_t), df = 128)
# 0.00972002

# 5 - draw a conclusion
# since p is less than a = 0.05 
# we reject the null and see that lawn signs in yards have a statistically 
# significiant effect on votes

# (B) Use the results from a linear regression to determine whether being next 
# to precincts with these yard signs affects vote share 
# conduct a hypothesis test with a = .05

# 1 - assumptions
# normality, homoscedasticity, independence

# 2 - hypoth formulation
# null
# beta (adjacent to lawn signs) = 0

# alternate
# beta (adjacent to lawn signs) is not = 0

# 3 - test stat
# precinct with yard signs has 30 samples
# coefficient
adj_lawn_coeff <- 0.042

# standard error
adj_lawn_se <- 0.013

# tstat
adj_lawn_t <- adj_lawn_coeff / adj_lawn_se
# 3.230769 

# 4 - p - value
# so for the testing
# compare with p - value
# where df = 131 - 3 (number of predictors; lawn, adjacent, ctrl) 
adj_lawn_p <- 2 * pt(-abs(adj_lawn_t), df = 128)
print(adj_lawn_p)
# 0.00156946

# 5 - draw a conclusion
# since p is less than a = 0.05 
# we reject the null and see that being adjacent to precincts with lawn signs in 
# yards have a statistically significiant effect on vote share

# (C) Interpret the coefficient for the constant term substantively
# constant = 0.302 

# 30.2% is the vote share for mcauliffes competitors in 
# when precincts do not have yard signs
# and are not adjacent to precincts with yard signs

# this interests us because it helps better understand the isolated effects
# of the explanatory variables
# any changes above or below 30.2% are pretty much caused because of the variables

# so if there are no yard signs and irrelevant whether close to precincts with
# yard signs
# mcauliffes opponent will receive less than a third of the votes

# if i was the opponent and did not know about the fit of the model, 
# it shows that i would have to make the campaign visible
# and do my best to influence votes

# (D) Evaluate the model fit for this regression. What does this tell us about 
# the importance of yard signs versus other factors that are not modeled?

# r_squared <- 0.094

# model only explains 9.4 % of total variability in vote share
# suggesting to us that yard signs and adjacency have some influence 
# but the most of the variation comes from other factors not included

# yard signs and adjacency are statistically significant
# and do affect voteshare
# but only in a minor way, as shown by the low r squared

# other factors play a larger part in determining voteshare
# could eb economic climate (like interest rates, sovereign debt rates as seen
# currently in the US)
# could be precinct demographics (i.e. younger voters might vote more for 
# mcauliffes opponents)
# could be down to strategy and other tools in the campaign toolkit
# like podcast appearances (supposedly a major decider in the recent US election)



