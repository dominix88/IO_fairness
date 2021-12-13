#### Power Analysis ####
# Distributive Fairness and Legitimacy in International Organizations    
# Date: 01.12.2021
# R version 4.0.3
# Platform: x86_64-w64-mingw32/x64 (64-bit)
# Author: Dominik Schraff

library(DeclareDesign) # Version 0.26.0
library(DesignLibrary) # Version 0.1.6
library(lme4) # Version 1.1-25
library(ggplot2) # Version 3.3.2
library(sjPlot)

# Pooled

fair <- multi_arm_designer(N = 1500, m_arms = 7, 
                             outcome_means = c(0,2,1.25,1.25,1.25,-2,2),
                             outcome_sds =  c(2.5,2.5,2.5,2.5,2.5,2.5,2.5),
                             conditions = c("control", "equality", "convergence", 
                                            "redistribution", "merit", 
                                            "survival.fittest", "national.gain"))

diagnosis <- diagnose_design(fair, sims = 500)
diagnosis

pooled.report <- data.frame(EstimatorLabel = diagnosis$diagnosands_df$estimator_label,
                           power = diagnosis$diagnosands_df$power,
                           mean_estimate = diagnosis$diagnosands_df$mean_estimate)

ggplot(data=pooled.report, aes(x=EstimatorLabel, y=power, fill = EstimatorLabel)) +
  geom_bar(stat="identity") + ggtitle("Single country (N=1500)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# High support for international cooperation 

fair.high <- multi_arm_designer(N = 750, m_arms = 7, 
                                   outcome_means = c(0,2.5,2,2,0.5,-2.5,1),
                                   outcome_sds =  c(2.5,2.5,2.5,2.5,2.5,2.5,2.5),
                                   conditions = c("control", "equality", "convergence", 
                                                  "redistribution", "merit", 
                                                  "survival.fittest", "national.gain"))

diagnosis.high <- diagnose_design(fair.high, sims = 500)
diagnosis.high

high.report <- data.frame(EstimatorLabel = diagnosis.high$diagnosands_df$estimator_label,
          power = diagnosis.high$diagnosands_df$power,
          mean_estimate = diagnosis.high$diagnosands_df$mean_estimate)

ggplot(data=high.report, aes(x=EstimatorLabel, y=power, fill = EstimatorLabel)) +
  geom_bar(stat="identity") + ggtitle("High support (N=750)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Low support for international cooperation 

fair.low <- multi_arm_designer(N = 750, m_arms = 7, 
                               outcome_means = c(0,1.5,1,-1.5,2,0.5,2.5),
                               outcome_sds =  c(2.5,2.5,2.5,2.5,2.5,2.5,2.5),
                               conditions = c("control", "equality", "convergence", 
                                              "redistribution", "merit", 
                                              "survival.fittest", "national.gain"))

diagnosis.low <- diagnose_design(fair.low, sims = 500)
diagnosis.low

low.report <- data.frame(EstimatorLabel = diagnosis.low$diagnosands_df$estimator_label,
                           power = diagnosis.low$diagnosands_df$power,
                           mean_estimate = diagnosis.low$diagnosands_df$mean_estimate)

ggplot(data=low.report, aes(x=EstimatorLabel, y=power, fill = EstimatorLabel)) +
  geom_bar(stat="identity") + ggtitle("Low support (N=750)") +
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# Simulate data

high.dat <- draw_data(fair.high)
high.dat$high.support <- 1

low.dat <- draw_data(fair.low)
low.dat$high.support <- 0

pooled.dat <- rbind(high.dat, low.dat)

# Analysis

lm0 <- lm(Y ~ Z, data = pooled.dat)
summary(lm0)

lm1 <- lm(Y ~ Z*high.support, data = pooled.dat)
summary(lm1)
plot_model(lm1, type = "int")

