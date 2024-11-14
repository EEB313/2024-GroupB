
#### EEB313 Project ####

#### calculating diversity per site ####

all_insect_data <- read.csv("arthropod_list.csv")


library(vegan)
library(plyr)
library(tidyverse)



test_data <- read.csv("final_arthro.csv")

new_test_data <- test_data[-351]

shannon_data <- data.frame(Plot = new_test_data$Plot,
           Shannon =  diversity(x = new_test_data[, -1], 
                                index = 'shannon'
           )
)


# Load necessary library
library(fitdistrplus)

# Fit a logistic distribution to the Shannon diversity data
logisticfit <- fitdist(shannon_data$Shannon, "logis")

# Generate a QQ plot for the logistic distribution fit
qqcomp(list(logisticfit), legendtext = "logistic")


# Load required libraries
library(fitdistrplus)

# Fit different distributions to the Shannon diversity data
gammafit <- fitdist(shannon_data$Shannon, "gamma")
weibullfit <- fitdist(shannon_data$Shannon, "weibull")
logisticfit <- fitdist(shannon_data$Shannon, "logis")
lnormfit <- fitdist(shannon_data$Shannon, "lnorm")
gengammafit <- fitdist(shannon_data$Shannon, "gengamma", start = list(mu = mean(shannon_data$Shannon), sigma = sd(shannon_data$Shannon), Q = 0))

# Extract AIC and BIC for each distribution
aic_bic_table <- data.frame(
  Distribution = c("Gamma", "Weibull", "Logistic", "Lognormal", "Generalized Gamma"),
  AIC = c(gammafit$aic, weibullfit$aic, logisticfit$aic, lnormfit$aic, gengammafit$aic),
  BIC = c(gammafit$bic, weibullfit$bic, logisticfit$bic, lnormfit$bic, gengammafit$bic)
)

# Display AIC and BIC values for comparison
print(aic_bic_table)
















