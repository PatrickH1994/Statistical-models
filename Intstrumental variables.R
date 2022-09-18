# IV-Method tutorial
# in this file I create a tutorial on how to use the IV method in R.
# 
# What is the instrumental variable method?
# In the instrumental variable method we try to correct for endogeneity by regressing
# an instrument - i.e. a exogeneous variable z that affects y only through the endogenous
# variable x - and then use the predicted values from the first regression as the 
# treatment in the second regression. The main idea is that the part of the error
# term that correlates with the treatment will be removed from the first equations predicted 
# value as it is included in the error term
#
# Key steps:
# 1) Get data from AER
# 2) Calculate variables
# 3) Analysis using the IV model
# 4) Test if the treatment is endogenous


# Prepare packages
install.packages("AER")
install.packages("stargazer")
library(AER)
library(stargazer)
library(dplyr)

print("1. Get data-----------------------------------------------------")
# As the dataset we use a panel dataset of cigarette demand in US states
# in years 1985 and 1995. Our aim is to study the effect of prices on demand. 
# But we cannot simply regress demand on prices because prices are endogenous.
# Therfore, we use sales tax as an instrument. The idea here is that sales tax is
# exogenous because it only affects demand through price.

data("CigarettesSW")
stargazer(CigarettesSW, type='text')
head(CigarettesSW, 10)

print("2. Calculate Variables ----------------------------------")

# compute real per capita prices
CigarettesSW$rprice <- with(CigarettesSW, price / cpi)

#  compute the sales tax
CigarettesSW$salestax <- with(CigarettesSW, (taxs - tax) / cpi)


print("3) Analysis using the IV model ------------------------")

# First we want to make sure the sales tax correlates with the price
cor(CigarettesSW$salestax, CigarettesSW$price)
# --> Cor = 0.6 as expected the tax affects the price and thus can function as a IV

# generate a subset for the year 1995
c1995 <- subset(CigarettesSW, year == "1995")

print("In the first section we use 2SLS, in which we first regress
      price on salestax and then the predicted values on demand.
      Note that this method gives us the wrong standard errors and
      thus we should use the in-built IV-method. But here we use
      first 2SLS to illustrate how IVs work.")

# perform the first stage regression
cig_s1 <- lm(log(rprice) ~ salestax, data = c1995)
coeftest(cig_s1, vcov = vcovHC, type = "HC1")

# store the predicted values
lcigp_pred <- cig_s1$fitted.values

# run the stage 2 regression
cig_s2 <- lm(log(c1995$packs) ~ lcigp_pred)
coeftest(cig_s2, vcov = vcovHC)

print("Second option is to use the ivreg method, which gives us the correct standard errors. But note that the estimate remains the same.")

cig_ivreg <- ivreg(log(packs) ~ log(rprice) | salestax, data = c1995)

coeftest(cig_ivreg, vcov = vcovHC, type = "HC1")

print("Interpresting results: the results suggests that an 1% increase in prices will result in a 1.08% drop in consumption")

# We may wan't to add more controls in which case the code looks like this

cig_ivreg <- ivreg(log(packs) ~ log(rprice) + population + income | . -log(rprice) + salestax, data = c1995)

coeftest(cig_ivreg, vcov = vcovHC, type = "HC1")

print("4) Test if the treatment is endogenous----------------------")

#Calculate first-stage model
cig_reg_1 <- lm(log(packs) ~ log(rprice) + population + income, data = c1995)

# Add residuals from first-stage model to dataset
c1995$vhat <- resid(cig_reg_1)

# Add residuals as a predictor and if the residuals' estimates are
# different from 0 then the treatment is endogeous
endo_model <- lm(log(packs) ~ vhat, data = c1995)
coeftest(endo_model, vcov = vcovHC, type = "HC1")
