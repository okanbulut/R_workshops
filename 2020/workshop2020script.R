
#######################################################
##                       R Workshop                  ##
##                       Okan Bulut                  ##
##                    bulut@ualberta                 ##
##                   February 28, 2020               ##
#######################################################

# EXERCISE 1 ----

# To set the working directory. Replace "location of your folder"
# with the location of the "workshop2020" folder in your computer
# For example, it is "C:/Users/bulut/Desktop/workshop2020" in 
# my computer

setwd("location of your folder")

# To check the current working director
getwd()

# Installing and activating packages
# You must be connected to the Internet!
install.packages("ggplot2")
install.packages("lattice")

library("lattice")
library("ggplot2")

#-----------------------------------------------------------------#

# EXERCISE 2 ----

# Read the medical data in
medical <- read.csv("medical.csv", header=TRUE)

# or
install.packages("xlsx")
library("xlsx")

medical <- read.xlsx("medical.xlsx", sheetName="medical")

#-----------------------------------------------------------------#

# EXERCISE 3 ----

# Create a histogram of "depression2" by "substance". The 
# code below is a template. You can change it to create a plot of 
# depression2 by substance

histogram(~ depression2 | substance, data = medical, xlab="Depression")

# Create a scatterplot of the "depression2" by "mental2"
# Also, add colours by "substance". The code below is a template. 
# Change it to create the scatterplot for this exercise. 

ggplot(data = medical, aes(depression2, mental2, colour = substance)) +  
  geom_point(size = 3) + labs(colour = "Substance", x = "Depression", y = "Mental")


#-----------------------------------------------------------------#

# INFERENTIAL STATISTICS ----

# One-sample t-test
t.test(medical$depression1, mu=25, conf.level = 0.95, alternative = "two.sided")

# Independent-samples t-test
male <- medical[medical$sex=="male", "depression1"]
female <- medical[medical$sex=="female", "depression1"]
t.test(male, female, conf.level = 0.95, alternative = "two.sided")

# Repeated-measures (i.e., paired) t-test
t.test(medical$depression1, medical$depression2, paired = TRUE,
       conf.level = 0.95, alternative = "two.sided")

# Analysis of Variance (ANOVA)
tapply(medical$depression1, medical$substance, mean)
anova(lm(medical$depression1 ~ medical$substance))

# Correlation
cor(medical[,c("depression1","mental1","physical1", "avg_drinks", "age")], 
    method = "pearson")

# Regression
model <- lm(depression1 ~ mental1 + physical1 + avg_drinks, data = medical)
summary(model)

