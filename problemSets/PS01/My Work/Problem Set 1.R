# Applied Statistical Analysis I
# Problem Set 1

#####################
setwd("/Users/eimhi/Documents/GitHub/StatsI_Fall2024/problemSets/PS01")
getwd()
#####################


#######################
# Problem 1 - Education
#######################

# Part 1
# What am I trying to find out?
# The 90% confidence interval for the average IQ in the sample school

# Data Collection

#Load data
y <- c(105, 69, 86, 100, 82, 111, 104, 110, 87, 108, 87, 90, 94, 113, 112, 98, 80, 97, 95, 111, 114, 89, 95, 126, 98)
length(y)
hist(y) 
# It does not seem to be normally distributed and n<30


#Trying to find the 90% confidence interval

# What do I need to calculate the confidence interval?

SchoolMean <- mean(y)   #Point estimate
SchoolSD <- sd(y)       #Standard Deviation
SchoolError <- sd(y) / sqrt(length((y))) #Standard Error
print(SchoolMean)
print(SchoolSD)
print(SchoolError)


#Establish the t statistic
# The sample does not seem to be normally distributed and n<30
# So I am going to use qt to get the t-distribution
SchoolDF <- length(y) - 1
SchoolDF
SchoolT <- qt(1 - 0.05, SchoolDF) # Using this to get the critical value
SchoolT


upper_90 <- (mean(y))+(SchoolT)*(sd(y)/sqrt(length(y))) # Upper Bound
lower_90 <- (mean(y))-(SchoolT)*(sd(y)/sqrt(length(y))) # Lower Bound

c(lower_90, upper_90) #The 90% confidence interval
CI90 <- c(lower_90, upper_90)
CI90


# Part 2 - Hypothesis Testing
# What am I trying to find out?
# Is the average IQ in our sample school is higher than the national average?

# Going to use the 5 Steps of Hypothesis Testing as my structure

# 1. Assumptions about our data
# We can assume that the population is approximately normally distributed,
# that the scores of the IQ test are independent of eachother and
# that the previous sample is representive of the school and random

# 2. Formulating our hypotheses
# H0 - The mean IQ of our sample school less than or equal to 100 (national average)
# H1 - The mean IQ of our sample school is greater than 100

# Its going to be a one-sided test because we are trying to find out if the
# school average is *greater than* the national average

# 3. Calculate the test statistic 
NationalMean <- 100
# For finding the t-statistic, I'll use the formula
# (Mean of sample - assumed mean) / (Standard dev. / sqrt of N)
# But I'll use the standard error we already calculated as the numerator
TStat <- (SchoolMean - NationalMean) / (SchoolError)
TStat

# 4. Calculate the p-value
P <- pt(TStat, SchoolDF, lower.tail = FALSE)
P

# 5. Draw a conclusion
# We fail to reject the null hypothesis as the p-value calculated of 0.7215 is 
# greater than 0.05 and therefore, cannot conclude that the average IQ of the
# students is greater than the national average

# Verification
t.test(y, mu = 100, conf.level = 0.95, alternative = "greater")

###############################
# Problem 2 - Political Economy
###############################

# Load my dataset
USexpenditure <- read.table("https://raw.githubusercontent.com/ASDS-TCD/StatsI_Fall2024/main/datasets/expenditure.txt", header=T)

# View the data

View(USexpenditure)
head(USexpenditure)
str(USexpenditure)

# Get a quick summary of the data

summary(USexpenditure)

# Part A

# Plotting each of the 6 relationships using the plot() function

# Relationship between X1 and Y

png(file="X1_Y_Plot.png")
plot(USexpenditure$X1 ~ USexpenditure$Y,
     main="The Relationship between Housing Assistance 
     Expenditure and Personal Income",
     xlab="Per capita personal income (in dollars)",
     ylab="Per capita expenditure on Housing Assistance (in dollars)",
     col = "green")

# Relationship between X2 and Y

png(file="X2_Y_Plot.png")
plot(USexpenditure$X2 ~ USexpenditure$Y,
     main="Relationship between Housing Assistance 
     Expenditure and Financial Insecurity",
      xlab="Number of Financially Insecure residents per 100,000",
      ylab="Per capita expenditure on Housing Assistance (in dollars)",
     col = "red")

# Relationship between X3 and Y

png(file="X3_Y_Plot.png")
plot(USexpenditure$X3 ~ USexpenditure$Y,
      main="Relationship between Housing Assistance 
     Expenditure and Number of Urban Residents per 1000",
      xlab="Number of urban area residents per 1000",
      ylab="Per capita expenditure on Housing Assistance (in dollars)",
     col = "blue")

# Relationship between X1 and X2

png(file="X1_X2_Plot.png")
plot(USexpenditure$X1 ~ USexpenditure$X2,
     main="The Relationship between Personal Income 
     and Financially Insecurity",
     xlab="Per capita personal income (in dollars)",
     ylab="Number of Financially Insecure residents per 100,000",
     col = "pink")

# Relationship between X1 and X3

png(file="X1_X3_Plot.png")
plot(USexpenditure$X1 ~ USexpenditure$X3,
     main="Relationship between Personal Income and Number of Urban Residents",
     xlab="Per capita personal income (in dollars)",
     ylab="Number of urban area residents per 1000",
     col = "purple")

# Relationship between X2 and X3

png(file="X2_X3_Plot.png")
plot(USexpenditure$X2 ~ USexpenditure$X3,
     main="Relationship between Financial Insecurity and Number of Urban Residents per 1000",
     xlab="Number of Financially Insecure residents per 100,000",
     ylab="Number of urban area residents per 1000",
     col = "yellow")



# Part B

# Plotting the relationship between Y and Region to find out which region
# allocates the most towards expenditure on housing assistance

png(file="Y_R_Boxplot2.png")
boxplot(USexpenditure$Y ~ USexpenditure$Region,
        main="Housing Assistance Expenditure per capita by Region",
        xlab="Regions in the data set",
        ylab="Per capita expenditure on Housing Assistance (in dollars)",
        col = c("lightblue", "purple", "pink","yellow"),
        names = c("Northeast", "North Central", "South", "West")
        )

# Part C

# Revised plotting of the relationship between X1 and Y
# Need to also include Region and show each respective one using colours
# and symbols

png(file="X1_Y_Plot2.png")
plot(USexpenditure$Y ~ USexpenditure$X1,
     main="The Relationship between Housing Assistance Expenditure 
     and Personal Income",
     xlab="Per capita personal income (in dollars)",
     ylab="Per capita expenditure on Housing Assistance (in dollars)",
     col = "black")

colours <- c("lightblue", "purple", "pink","yellow") #Same colours as the ones 
# from the boxplot
symbols <- c(0, 1, 8, 11) #Symbols to easier distinguish each region 

png(file="X1_Y_BetterPlot.png")
plot(USexpenditure$Y ~ USexpenditure$X1,
     main="The Relationship between Housing Assistance Expenditure 
     and Personal Income",
     xlab="Per capita personal income (in dollars)",
     ylab="Per capita expenditure on Housing Assistance (in dollars)",
     col=colours[USexpenditure$Region],
     pch=symbols[USexpenditure$Region]
)

#Adding a legend to the top left of the plot
legend("topleft", 
       legend=c("Northeast", "North Central", "South", "West"), 
       col=c("lightblue", "purple", "pink","yellow"),
       pch=1)

dev.off()


