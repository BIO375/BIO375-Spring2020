#### ANOVA for Lecture Video #### 

# Clean up the working environment
rm(list = ls())
# Verify working directory, should be ~/Documents/Analyses/lastname_first
getwd()

### Install and load packages ####

# install.packages("ggfortify")
library("ggfortify")
# multcomp is used for contrasts and multiple comparisons
# install.packages("multcomp")
library("multcomp")
# nlme is used for random effects ANOVA
# install.packages("nlme")
library("nlme")

# Load tidyverse
library("tidyverse")
# Check for updates
tidyverse_update()

# Check for updates
tidyverse_update()

### General ANOVA workflow ####

# The general workflow as you do analyses in R should be as follows:
#   Step 1.  Plot your data (boxplots, histograms, Q-Q plots)
#   Step 2.  Use the function lm() to fit a model, specifying equation & data
#     e.g., y ~ x, data = data
#   Step 3.  Check assumptions again, using residuals plot
#   Step 4.  If assumptions are met, use the functions anova() and summary() 
#     to interpret statistical results.  If assumptions are not met, try 
#     data transformation and/or a non-parametric or robust version of the test

### Fixed effects ANOVA ####

### Step 1.  Read in and plot data ####

# It is important to read in the predictor as a factor

diatoms <- read_csv("zinc,diversity
back,2.27
back,1.7
back,2.05
back,1.98
back,2.2
back,1.53
back,0.76
back,1.89
high,1.25
high,1.15
high,0.63
high,1.04
high,1.9
high,1.88
high,0.85
high,1.43
high,1.37
low,1.4
low,2.18
low,1.83
low,1.88
low,2.1
low,2.38
low,2.83
low,1.66
medium,1.62
medium,2.19
medium,2.1
medium,2.06
medium,2.02
medium,1.94
medium,1.75
medium,0.8
medium,0.98", 
                    col_types = cols(
  zinc = col_factor() ))


ggplot(diatoms, aes(x = zinc, y = diversity))+
  geom_boxplot() +
  theme_bw()
ggplot(diatoms) +
  geom_histogram(aes(diversity), binwidth = 0.2)+
  facet_wrap(~zinc)
ggplot(diatoms)+
  geom_qq(aes(sample = diversity, color = zinc))

### Step 2.  Construct your ANOVA model ####

model_div <- lm(diversity~zinc, data = diatoms)

### Step 3.  Check the assumptions, again ####

summ_div <- diatoms %>%
  group_by(zinc) %>%
  summarise(mean_div = mean(diversity),
            n_div = n(),
            sd_div = sd(diversity),
            se_div = sd_div/sqrt(n()))
grand_mean_div = mean(diatoms$diversity)

ratio <-(max(summ_div$sd_div))/(min(summ_div$sd_div))

# The function autoplot will give you a residuals by predicte plot, which is 
# called "Residuals vs. Fitted" here.  It also gives you a Q-Q plot of the RESIDUALS.

autoplot(model_div)

### Step 4. Interpret results ####

anova(model_div)
# summary(model_div)


### Multiple Comparisons ####

# Planned comparisons

planned <- glht(model_div, linfct = 
                  mcp(zinc = c("low - back = 0",
                                   "medium - back = 0",
                                   "high - back = 0")))
confint(planned)
summary(planned)

# Unplanned comparisons
# The key things you need to specify here are the model name and the factor name

tukey <- glht(model_div, linfct = mcp(zinc = "Tukey"))
summary(tukey)

### Multiple Comparisons if package "multcomp" fails ####
model_div_b <- aov(diversity ~ zinc, diatoms)
TukeyHSD(model_div_b)


### Non-parametric Kruskal-Wallis test ####
# This is a very simple test output, it gives you a test statistic, df, and p

kruskal.test(diversity ~ zinc, data = diatoms)


### Robust Welch's ANOVA ####

# I cannot tell you why the function for this is called oneway.test()
# Regardless this is how you do it:
oneway.test(diversity ~ zinc, data = diatoms)


### Plot using base R plotting, adapted from abd script

par(bty="l")
adjustAmount <- 0.15
stripchart(diversity ~ zinc, data = diatoms, method = "jitter",
           vertical = TRUE, las = 1, pch = 1, xlab = "Zinc level",
           ylab = "Diatom diversity", col = "firebrick", 
           cex = 1.2, ylim = c(0, 3))
segments( c(1,2,3,4) + adjustAmount, summ_div$mean_div - summ_div$se_div, 
          c(1,2,3,4) + adjustAmount, summ_div$mean_div + summ_div$se_div )
points(summ_div$mean_div ~ c( c(1,2,3,4) + adjustAmount ), pch = 1, col = "blue")

# The function autoplot will give you a residuals by predicte plot, which is 
# called "Residuals vs. Fitted" here.  It also gives you a Q-Q plot of the RESIDUALS.

autoplot(model01)

# If it looks hideous, try clicking on the little Zoom magnifying glass to open
# the plot in a larger window.

### Step 4. Interpret results ####

# Use the function anova() to answer our first research question: is there an effect
# of parasite treatment on Daphnia growth rate?

anova(model01)

# the row beginning with our predictor variable name (parasite) shows a significant
# p<0.0001, so there is an effect of parasite treatment on growth rate.

# To address the second question, which parasites are different from the control, we
# can choose among approaches.

# Start with a summary of the model results
summary(model01)

# There are 4 rows in the table of coefficients and the first row is labeled intercept.
# Do not be fooled!  This is not the grand mean as a reasonable human might expect!
# Instead, because R alphabetized the names of your parasite treatment groups, it 
# is the mean of the control (C comes before M and P in the alphabet) group. 
# So going forward, just assume that the word '(Intercept)' represents the first 
# level of the alphabetically ordered treatment levels.  Treatment contrasts
# report differences between the reference level (in this lucky case the control)
# and the other levels.  So in the summary table the numbers associated with each
# parasite are differences between growth rates associated with that parasite and
# the control.

# Because the control ended up as the reference group, the p-values associated 
# with the contrasts are actually useful.  That said, they are totally at risk
# of elevated type I error, so you'd be better off using Tukey HSD to evaluate 
# pairwise differences!

### Multiple Comparisons ####

# Earlier you installed and loaded the package multcomp.  To do planned comparisons
# we will use a function in multcomp called glht, short for general linear
# hypotheses.
# linfct specifies the linear hypotheses to be tested, I find the easiest way
# is to specify by name

# Planned comparisons

planned <- glht(model01, linfct = 
                  mcp(parasite = c("Metschnikowia - control = 0",
                                   "Pansporella - control = 0",
                                   "Pasteuria - control = 0")))
confint(planned)
summary(planned)

# Unplanned comparisons
# The key things you need to specify here are the model name and the factor name

tukey <- glht(model01, linfct = mcp(parasite = "Tukey"))
summary(tukey)

### Multiple Comparisons if package "multcomp" fails ####
model01_b <- aov(growth.rate ~ parasite, daphnia)
TukeyHSD(model01_b)


### Non-parametric Kruskal-Wallis test ####
# This is a very simple test output, it gives you a test statistic, df, and p

kruskal.test(growth.rate ~ parasite, data = daphnia)


### Robust Welch's ANOVA ####

# I cannot tell you why the function for this is called oneway.test()
# Regardless this is how you do it:
oneway.test(growth.rate ~ parasite, data = daphnia)

### Random effects ANOVA ####
# For this, we will use the example in your book examining the repeatibility of
# measurements on walking stick limbs
stick <- read_csv("datasets/abd/chapter15/chap15e6WalkingStickFemurs.csv",
                  col_types = cols(specimen = col_factor() ) )

# To include a random effect, we no longer use the linear model function lm(),
# instead we use lme()

# The random effects ANOVA function requires two formulas, rather than just one. 
# The first formula (beginning with "fixed =") is for the fixed effect. The walking
# stick insect example doesn't include a fixed-effect variable, so we just provide
# a symbol for a constant in the formula ("~ 1"), representing the grand mean. The
# second formula (beginning with "random =") is for the random effect. In this 
# example, the individual specimens are the random groups, and the second formula
# indicates this (the "~ 1" in the formula below indicates that each specimen has 
# its own mean). You will need to have loaded the nlme library.

model02 <- lme(fixed = femurLength ~ 1,
               random = ~1|specimen, data = stick)

# Obtain the variance components for the random effects using VarCorr. The output includes
# the standard deviation and variance for both components of random variation in the random
# effects model for this example. The first is the variance among the specimen means. This 
# is the variance among groups, and is confusingly labeled "Intercept" in the output. The
# second component is the variance among measurements made on the same individuals. This 
# is the within group variance, also known as the error mean square, and is labeled 
# "Residual" in the output.

model02_varcomp <- VarCorr(model02)
model02_varcomp

# This gives us the estimates of the variance components for groups/Intercept and 
# error/Residual

# To get repeatibility, tell R to do some math by extracting the first entry in the first
# column and calling it VarAmong

varAmong  <- as.numeric( model02_varcomp[1,1] )

# And then extracting the second entry in the first column and calling it VarWithin
varWithin <- as.numeric( model02_varcomp[2,1] )

# And then doing the math
repeatability <- varAmong / (varAmong + varWithin)
repeatability

# End of story, 74% of walking stick femur length is due to variability among actual
# insects, not picture analysis issues.








