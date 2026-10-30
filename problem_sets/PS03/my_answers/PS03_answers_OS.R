#####################
# load libraries
# set wd
# clear global .envir
#####################

# remove objects
rm(list=ls())
# detach all libraries
detachAllPackages <- function() {
  basic.packages <- c("package:stats", 
                      "package:graphics", 
                      "package:grDevices", 
                      "package:utils", 
                      "package:datasets", 
                      "package:methods", 
                      "package:base")
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

install.packages("tidyverse")
install.packages("ggplot2")

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# read in data
inc.sub <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsI_2026/main/datasets/incumbents_subset.csv")

#####################
# Problem 1, Part 1
#####################

#We are interested in knowing how the diﬀerence in campaign spending between 
#incumbent and challenger aﬀects the incumbent’s vote share.

#Run a regression where the outcome variable is voteshare and the explanatory 
#variable is difflog.

reg_voteshare_difflog <- lm(inc.sub$voteshare ~ inc.sub$difflog, data=inc.sub)
reg_voteshare_difflog
summary(reg_voteshare_difflog)

#####################
# Problem 1, Part 2
#####################

#Make a scatterplot of the two variables and add the regression line.

#Just the scatterplot:
library(ggplot2)
ggplot(inc.sub, aes(x=difflog, y=voteshare)) +
  geom_point() +
  labs(title = "Incumbent vote share by difference in campaign spending",
       x = "Log of difference in campaign spending between incumbent and challenger",
       y = "Incumbent's share of vote")

#With regression line:
ggplot(inc.sub, aes(x=difflog, y=voteshare)) +
  geom_point() +
  labs(title = "Incumbent vote share by difference in campaign spending",
       x = "Log of difference in campaign spending between incumbent and challenger",
       y = "Incumbent's share of vote") +
  geom_smooth(
    aes(x=difflog, y=voteshare),
    method = "lm",
    formula = y~x,
    se=FALSE)

#####################
# Problem 1, Part 3
#####################

#Save the residuals of the model in a separate object.

residuals_voteshare_difflog <- resid(reg_voteshare_difflog)

#####################
# Problem 1, Part 4
#####################

#Write the prediction equation.

summary(reg_voteshare_difflog)
#Produces voteshare_hat = 0.579031 + 0.041666(difflog) + error

#####################
# Problem 2, Part 1
#####################

#We are interested in knowing how the diﬀerence between incumbent and 
#challenger’s spending and the vote share of the presidential candidate of the 
#incumbent’s party are related.

#Run a regression where the outcome variable is presvote and the explanatory 
#variable is difflog.

reg_presvote_difflog <- lm(inc.sub$presvote ~ inc.sub$difflog, data=inc.sub)
reg_presvote_difflog
summary(reg_presvote_difflog)

#####################
# Problem 2, Part 2
#####################

#Make a scatterplot of the two variables and add the regression line.

#Just the scatterplot:
library(ggplot2)
ggplot(inc.sub, aes(x=difflog, y=presvote)) +
  geom_point() +
  labs(title = "Vote share of presidential candidate of incumbent's party by 
       difference in campaign spending",
       x = "Log of difference in campaign spending between incumbent and 
       challenger",
       y = "Vote share of presidential candidate of the incumbent's party")

#With regression line:
ggplot(inc.sub, aes(x=difflog, y=presvote)) +
  geom_point() +
  labs(title = "Vote share of presidential candidate of incumbent's party by 
       difference in campaign spending",
       x = "Log of difference in campaign spending between incumbent and 
       challenger",
       y = "Vote share of presidential candidate of the incumbent's party") +
  geom_smooth(
    aes(x=difflog, y=presvote),
    method = "lm",
    formula = y~x,
    se=FALSE)

#####################
# Problem 2, Part 3
#####################

#Save the residuals of the model in a separate object.

residuals_presvote_difflog <- resid(reg_presvote_difflog)

#####################
# Problem 2, Part 4
#####################

#Write the prediction equation.

summary(reg_presvote_difflog)
#Produces presvote_hat = 0.507583 + 0.023837(difflog) + error

#####################
# Problem 3, Part 1
#####################

#We are interested in knowing how the vote share of the presidential candidate 
#of the incumbent’s party is associated with the incumbent’s electoral success.

#Run a regression where the outcome variable is voteshare and the explanatory 
#variable is presvote.

reg_voteshare_presvote <- lm(inc.sub$voteshare ~ inc.sub$presvote, data=inc.sub)
reg_voteshare_presvote
summary(reg_voteshare_presvote)

#####################
# Problem 3, Part 2
#####################

#Make a scatterplot of the two variables and add the regression line.

#Just the scatterplot:
library(ggplot2)
ggplot(inc.sub, aes(x=presvote, y=voteshare)) +
  geom_point() +
  labs(title = "Incumbent's vote share by vote share of presidential candidate 
       of incumbent's party",
       x = "Vote share of presidential candidate of the incumbent's party",
       y = "Incumbent's share of vote")

#With regression line:
ggplot(inc.sub, aes(x=presvote, y=voteshare)) +
  geom_point() +
  labs(title = "Incumbent's vote share by vote share of presidential candidate 
       of incumbent's party",
       x = "Vote share of presidential candidate of the incumbent's party",
       y = "Incumbent's share of vote") +
  geom_smooth(
    aes(x=presvote, y=voteshare),
    method = "lm",
    formula = y~x,
    se=FALSE)

#####################
# Problem 3, Part 3
#####################

#Write the prediction equation.

summary(reg_voteshare_presvote)
#Produces voteshare_hat = 0.441330 + 0.388018(presvote) + error

#####################
# Problem 4, Part 1
#####################

#The residuals from part (a) tell us how much of the variation in voteshare is 
#not explained by the diﬀerence in spending between incumbent and challenger. 
#The residuals in part (b) tell us how much of the variation in presvote is not 
#explained by the diﬀerence in spending between incumbent and challenger in the 
#district.

#Run a regression where the outcome variable is the residuals from Question 1 
#and the explanatory variable is the residuals from Question 2.

reg_residuals <- lm(residuals_voteshare_difflog ~ residuals_presvote_difflog)
reg_residuals
summary(reg_residuals)

#####################
# Problem 4, Part 2
#####################

#Make a scatterplot of the two residuals and add the regression line.

#Just the scatterplot:
library(ggplot2)
ggplot(inc.sub, aes(x=residuals_presvote_difflog, y=residuals_voteshare_difflog)) +
  geom_point() +
  labs(title = "Residuals from regressing incumbent vote share on log of 
  difference in spending by residuals from regressing presidential candidate 
       vote share on log of difference in spending",
       x = "Residuals from regressing presidential candidate vote share on 
       log of difference in spending",
       y = "Residuals from regressing incumbent vote share on 
       log of difference in spending")

#With regression line:
ggplot(inc.sub, aes(x=residuals_presvote_difflog, y=residuals_voteshare_difflog)) +
  geom_point() +
  labs(title = "Residuals from regressing incumbent vote share on log of 
  difference in spending by residuals from regressing presidential candidate 
       vote share on log of difference in spending",
       x = "Residuals from regressing presidential candidate vote share on 
       log of difference in spending",
       y = "Residuals from regressing incumbent vote share on 
       log of difference in spending") +
  geom_smooth(
    aes(x=residuals_presvote_difflog, y=residuals_voteshare_difflog),
    method = "lm",
    formula = y~x,
    se=FALSE)

#####################
# Problem 4, Part 3
#####################

#Write the prediction equation.

summary(reg_residuals)
#Produces residuals_voteshare_difflog_hat = 
#-1.942e-18 + 2.569e-01(residuals_presvote_difflog) + error

#####################
# Problem 5, Part 1
#####################

#What if the incumbent’s vote share is aﬀected by both the president’s 
#popularity and the diﬀerence in spending between incumbent and challenger?

#Run a regression where the outcome variable is the incumbent’s voteshare and 
#the explanatory variables are difflog and presvote.

reg_voteshare_difflog_presvote <- lm(inc.sub$voteshare ~ inc.sub$difflog + inc.sub$presvote, data = inc.sub)
reg_voteshare_difflog_presvote
summary(reg_voteshare_difflog_presvote)

#####################
# Problem 5, Part 2
#####################

#Write the prediction equation.

summary(reg_voteshare_difflog_presvote)
#Produces voteshare_hat =  0.4486442 + 0.0355431(difflog) + 0.2568770(presvote) + error

#####################
# Problem 5, Part 3
#####################

#What is it in this output that is identical to the output in Question 4? 
#Why do you think this is the case?

summary(reg_residuals) #output of problem 4

#Reading the output tables, I see that the residual standard error is 0.07339 for
#both this multivariate regression and the bivariate residual regression from
#problem 4. The residuals are also identical:
#   Min        1Q    Median        3Q       Max 
#-0.25928  -0.04737  -0.00121   0.04618   0.33126 

#These results may be identical because __________________________?????????????