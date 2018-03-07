
#######################################################
##                       R Workshop                  ##
##                       Okan Bulut                  ##
##                    bulut@ualberta                 ##
##                      March 6, 2018                ##
#######################################################


# To set the working directory. Replace "location of your folder"
# with the folder that you want to use as working directory
setwd("location of your folder")

# To check the current working director
getwd()

# Installing and activating packages
install.packages("ggplot2")
library("ggplot2")

install.packages("lattice")
library("lattice")

# Basic calculations
6 + 20
89 - 53
424 * 68
1110 / 37
20+(30*3)
(20/5)+(144/12)

# Some more complex calculations
10^2
sqrt(81)
log(5)
exp(1.5)
cos(100)
sin(180)

# Defining new variables
weight <- c(60, 72, 80, 84, 56)
weight

height <- c(1.70, 1.75, 1.80, 1.90, 1.60)
height

weight2 <- weight*2.20462
weight2

reading <- c(80, 75, 50, 44, 65)
math <- c(90, 65, 60, 38, 70)
total <- reading + math
total

# Character variables
cities <- c("Edmonton", "Calgary", "Red Deer", "Spruce Grove")
cities

gender <- c("1", "2", "2", "1", "2")
gender

# R is case-sensitive
cities <- c("Edmonton", "Calgary", "Red Deer", "Spruce Grove")
Cities # returns an error message
CITIES # returns an error message

# Exercise 1
mpg <- c(21, 21, 22.8, 21.4, 18.7)
hp <- c(110, 110, 93, 110, 175)
cor(mpg, hp)


# Defining data sets

# Method 1
mpg <- c(21, 21, 22.8, 21.4, 18.7)
hp <- c(110, 110, 93, 110, 175)
mydata <- cbind(mpg, hp)
mydata

# Method 2
car1 <- c(21, 110)
car2 <- c(21, 110)
car3 <- c(22.8, 93)
car4 <- c(21.4, 110)
car5 <- c(18.7, 175)
mydata <- rbind(car1, car2, car3, car4, car5)
mydata

# Method 3
mydata <- data.frame(mpg = c(21, 21, 22.8, 21.4, 18.7),
                     hp = c(110, 110, 93, 110, 175))
mydata


# To view and print data
View(mydata)
head(mydata)

# Packages for importing data
install.packages("xlsx")
library("xlsx")

install.packages("foreign")
library("foreign")

interest_csv <- read.csv("interest.csv", header=TRUE)
interest_xlsx <- read.xlsx("interest.xlsx", sheetName="interest")
interest_spss <- read.spss("interest.sav", to.data.frame=TRUE)

# To check the data
head(interest)
dim(interest)
str(interest)

head(interest$gender2)
head(interest[,c("gender2", "age")])
interest[5,2]
interest[1:5,1:3]
interest[c(1,2,3,4,5),c(1,2,3)]
interest[1:5, c("id", "gender", "gender2")]

# Subsetting data
interest_female <- subset(interest, gender2=="Female")
interest_female <- interest[interest$gender2=="Female", ]
female_50 <- subset(interest, gender2=="Female" & age < 50)

# Recoding variables
interest$educ2 <- ifelse(interest$educ > 12, "College", "High School")
interest$gender_binary <- ifelse(interest$gender=="Female", 1, 0)

install.packages("car")
library("car")
interest$educ2 <- recode(interest$educ, "13:18='College'; 8:12='High School'")
interest$gender_binary <- recode(interest$gender2, "'Female'=1; 'Male'=0")

# Summarizing data
summary(interest[,c("gender2", "age", "educ", "vocab", "reading")])

table(interest$gender2)
table(interest$educ2)
table(interest$gender2, interest$educ2)

mean(interest$age)
median(interest$age)
var(interest$age)
sd(interest$age)
min(interest$age)
max(interest$age)

#Mean age by gender
tapply(interest$age, interest$gender2, mean)
#Median age by education
tapply(interest$age, interest$educ2, median)
#Standard deviation of reading score by gender
tapply(interest$reading, interest$gender2, sd)
#Variance of reading score by education
tapply(interest$reading, interest$educ2, var)


install.packages("skimr")
library("skimr")
skim(interest[,c("age","educ","reading","vocab","mathmtcs")])

# Graphics
boxplot(interest$mathmtcs, main="Mathematics Scores")
hist(interest$reading, main="Reading Scores", xlab="Reading")

boxplot(interest$stress ~ interest$gender2, xlab="Gender", ylab="Stress",
        main="Stress Level by Gender", names = c("Female", "Male"))

library("lattice")
histogram(~ analyrea | educ2, data = interest, xlab="Analytical Reasoning")


plot(interest$reading, interest$vocab, main="A Scatterplot", 
     xlab="Reading", ylab="Vocabulary")
barplot(table(interest$gender2), main = "Gender", names = c("Female", "Male"),
        ylab = "Frequency", col = c("blue", "orange"))

library("ggplot2")
ggplot(interest, aes(reading, vocab, colour = educ2)) +  
  geom_point(size = 3) + 
  labs(colour = "Education", x = "Reading", y = "Vocabulary")

ggplot(interest, aes(gender2, reading, fill=educ2)) + 
  labs(x="" , y="Reading", fill="Education") + 
  geom_boxplot()

ggplot(interest, aes(reading, vocab)) + geom_point(shape=1, size=3) + 
  geom_smooth(method=lm , color="red", se=TRUE) + 
  labs(x="Reading" , y="Vocabulary")

# Inferential statistics
t.test(interest$reading, mu=50, conf.level = 0.95, alternative = "two.sided")

male <- interest[interest$gender2=="Male", "analyrea"]
female <- interest[interest$gender2=="Female", "analyrea"]
t.test(male, female, conf.level = 0.95, alternative = "two.sided")


interest$age2[interest$age < 35] <- "Group 1"
interest$age2[interest$age >= 35 & interest$age < 45] <- "Group 2"
interest$age2[interest$age >= 45] <- "Group 3"
table(interest$age2)
anova(lm(interest$analyrea ~ interest$age2))

cor(interest$vocab, interest$reading, method = "pearson")

install.packages("Hmisc")
library("Hmisc")
rcorr(interest$vocab, interest$reading, type="pearson")
















