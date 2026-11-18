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
lapply(c("car"),  pkgTest)

install.packages("tidyverse")
install.packages("ggplot2")

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

install.packages("car")
library("car")
data(Prestige)
help(Prestige)

####################
# Problem 1, Part A
####################

# Creating a copy of the data set to avoid overwriting original data:
prestigecopy <- data.frame(Prestige)
# Coding "prof" as 1; "bc" and "wc" as 0:
prestigecopy$professional <- ifelse(prestigecopy$type %in% "prof", 1, 0)

####################
# Problem 1, Part B
####################

reg_prestige_income_prof_incomeprofinxn <- lm(prestigecopy$prestige ~ 
                                                prestigecopy$income +
                                                prestigecopy$professional +
                                                (prestigecopy$income * 
                                                   prestigecopy$professional),
                                              data = prestigecopy)
summary(reg_prestige_income_prof_incomeprofinxn)

lapply(c("stargazer", "vioplot", "arm"),  pkgTest)
library(stargazer)
stargazer(reg_prestige_income_prof_incomeprofinxn) #for LaTeX
# !!!!!!!!!!!!!!!! REMEMBER TO WRITE LENGTHY INTERPRETATION !!!!!!!!!!!!!!!! #

####################
# Problem 1, Part C
####################

summary(reg_prestige_income_prof_incomeprofinxn) #intercept & coefficients
 
####################
# Problem 1, Part D
####################

# no code needed

####################
# Problem 1, Part E
####################

# no code needed

####################
# Problem 1, Part F
####################

# no code needed

####################
# Problem 1, Part G
####################

# no code needed

####################
# Problem 2, Part A
####################

2*pt(-abs(2.625), df = 29)

####################
# Problem 2, Part B
####################

2*pt(-abs(3.23), df = 75)

####################
# Problem 2, Part C
####################

# no code needed

####################
# Problem 2, Part D
####################

# no code needed