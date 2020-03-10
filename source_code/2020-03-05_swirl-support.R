# library(swirl)
# install_course("Statistical_Inference")
# swirl()
# 9, 10, 11


library(tidyverse)

### Confidence interval of the mean step by step ####
# To show an example of code for a confidence interval (similar to swirl), I will use the calcium dataset

coelomic <- tribble(
  ~calcium,
  28,
  27,
  29,
  29,
  30,
  30,
  31,
  30,
  33,
  27,
  30,
  32,
  31
)

# In swirl or certain problems, you will be given the mean, standard deviation and sample size.  Here I calculate them
# directly and name them, mean <- mean(coelomic$calcium), but you can also just type in the number, mean <- 29.76923 .
data <- coelomic
summary <- coelomic %>%
  summarise(mean = mean(calcium),
            sd = sd(calcium),
            n = n(),
            se = sd(calcium)/sqrt(n()))

alpha <- 0.05

# In words, the confidence interval equation is 
# mean plus or minus the product of the critical value of t, given alpha and df, and the standard error of the mean.
# the mean you can calculate
# the "plus or minus" is accomplished with a short vector c(-1,-2) that you multiply by...
# the critical value of t using the function qt():  qt(1-alpha, df = n-1) which is also multiplied by...
# the standard error of the mean

# NOTE that in the swirl text it refers to the critical value of t as t_(n-1) which I think is bogus, but whatever.

# If you used summarise to calculate the descriptive statistics, then the code is
summary$mean + c(-1,1)*qt(1-alpha, df = summary$n -1 )*summary$se

# If you just entered in the numbers directly then the code is shown below in comments
# mean <- 29.76923	
# sd <- 1.786703	
# n <- 13	
# se <- 0.4955423
# df <- n-1
# alpha = 0.05

# mean + c(-1,1)*qt(1-alpha, df)*se

# swirl uses different names
# the mean is mn
# the standard deviation is s
# and you have to type the numbers in for df and n
mn <- 29.76923
s <- 1.786703
mn + c(-1,1)*qt(.975, 12)*s/sqrt(13)

### Confidence interval for the difference between two means ####

# swirl abbreviations to know

# mu_y - mu_x is the WORST.  What it means in English is the true mean of group 2 minus the true mean of group 1.
# I find the fact that they name the groups y and x to be an abomination.

# X' and Y' refer to the sample mean from group 1 and the sample mean from group2
# (Y'-X') is sample mean group 2 minus sample mean group 1

# n_x is the sample size for group 1, n_y is the sample size for group 2

# THIS MAKES MY HEAD EXPLODE
# > (127.44-132.86) + c(-1,1)*qt(0.975,27)*sqrt(1/8+1/21)*sp
# [1] -20.361097   9.521097
# 
# | Not quite right, but keep trying. Or, type info() for more options.
# 
# | Type 132.86-127.44+c(-1,1)*qt(.975,ns)*sp*sqrt(1/8+1/21) at the command prompt.

