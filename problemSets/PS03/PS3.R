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
lapply(c("stringr"),  pkgTest)

lapply(c("nnet", "MASS"),  pkgTest)
library(dplyr)
if (!requireNamespace("nnet", quietly = TRUE)) install.packages("nnet")
if (!requireNamespace("MASS", quietly = TRUE)) install.packages("MASS")
library(nnet)  
library(MASS)  

# set wd for current folder
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


#####################
# Problem 1
#####################

# loading data
gdp_data <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/gdpChange.csv")

# inspect
View(gdp_data)
str(gdp_data)
head(gdp_data)
colnames(gdp_data)


# Creating factor variables for outcome 
gdp_data <- gdp_data %>%
  mutate(GDPcat = case_when(
    GDPWdiff == 0 ~ "no change",
    GDPWdiff > 0  ~ "positive",
    GDPWdiff < 0  ~ "negative"
  )) %>%
  mutate(GDPcat = factor(GDPcat, levels = c("no change", "positive", "negative")))

# check the updated dataframe 
str(gdp_data)


# Running Unordered Multinomial Logistic Regression
unordered_logit <- multinom(GDPcat ~ REG + OIL, data = gdp_data)
summary(unordered_logit)


# Adjusting to specify an ordered factor
gdp_data <- gdp_data %>%
  mutate(GDPcat = case_when(
    GDPWdiff < 0 ~ "negative",
    GDPWdiff == 0 ~ "no change",
    GDPWdiff > 0 ~ "positive"
  )) %>%
  mutate(GDPcat = factor(GDPcat, levels = c("negative", "no change", "positive"), ordered = TRUE))

# check
str(gdp_data$GDPcat)

# Running Ordered Multinomial Logistic Regression
ordered_logit <- polr(GDPcat ~ REG + OIL, data=gdp_data)
summary(ordered_logit)



#####################
# Problem 2
#####################

# loading data
mexico_elections <- read.csv("https://raw.githubusercontent.com/ASDS-TCD/StatsII_Spring2024/main/datasets/MexicoMuniData.csv")
View(mexico_elections)
str(mexico_elections)
head(mexico_elections)
colnames(mexico_elections)


# Estimating Poisson Regression Model
mexico_poisson <- glm(PAN.visits.06 ~ competitive.district + marginality.06 + PAN.governor.06, data = mexico_elections, family=poisson)
summary(mexico_poisson)




# Calculating mean number of visits from winning PAN candidate
# Given the coefficients:
# Intercept: -3.81
# competitive.district: -0.08 (for a competitive district, competitive.district=1)
# marginality.06: -2.08 (for an average poverty level, marginality.06 = 0)
# PAN.governor.06: -0.31 (for a district with a PAN governor, PAN.governor.06 = 1)

mean_visits <- exp(-3.81 + (-0.08 * 1) + (-2.08 * 0) + (-0.31 * 1))
mean_visits


