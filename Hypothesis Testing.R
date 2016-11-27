##########################################################################
#                                   T-test
##########################################################################
# Assumptions:
# 1. The data is normally distributed
# 2. Samples are iid

# Data to perform the test on
data_vector <- c(63, 75, 84, 58, 52, 96, 63, 55, 76, 83)

# Null Hypothesis
# H0: mu = mu0

#Alternative Hypothesis
# (Left tail) Ha: mu < mu0
# (Right tail) Ha: mu > mu0
# (Two tail) Ha: mu != mu0

t_test <- function(data, mu0, alpha, alt){
  x_bar <- mean(data)
  var <- var(data)
  n <- length(data)
  tstat <- (x_bar - mu0)/sqrt(var/n)
  dof <- length(data) - 1
  if(alt == "less than"){
    t_critical <- qt(alpha, df=dof)
    p_val <- pt(tstat, df=dof)
  }else if (alt == "greater than"){
    t_critical <- qt(1-alpha, df=dof)
    p_val <- 1 - pt(tstat, df=dof)
  }
  else if (alt == "does not equal"){
    t_critical <- qt(1-alpha/2, df=dof)
    p_val <- 2*pt(tstat, df=dof)
  }
  print(c("Population Mean" = mu0, 
          "Sample Man" = x_bar, 
          "Sample Variance" = var, 
          "Sample Size" = n, 
          "Degrees of Freedom" = dof, 
          "t-statistic" = tstat, 
          "t-critical" = t_critical, 
          "p-value" = p_val))
}
t_test(data_vector, 73, 0.05, alt = "less than")
t_test(data_vector, 73, 0.05, alt = "greater than")
t_test(data_vector, 73, 0.05, alt = "does not equal")

#Out of box functions - default: alpha= 0.05, mu = 0
t.test(data_vector, mu = 73, alternative = "less")
t.test(data_vector, mu = 73, alternative = "greater")
t.test(data_vector, mu = 73, alternative = "two.sided")

##########################################################################
#                               ANOVA
##########################################################################
#Hypothesis
#H0: mu_1 = mu_2 = ...= mu_p
#Ha: at least 1 mean is signficantly different 

# 3 groups with 7 observations
y1 <- c(18.2, 20.1, 17.6, 16.8, 18.8, 19.7, 19.1)
y2 <- c(17.4, 18.7, 19.1, 16.4, 15.9, 18.4, 17.7)
y3 <- c(15.2, 18.8, 17.7, 16.5, 15.9, 17.1, 16.7)

#create matrix with group identification
y <- c(y1,y2,y3)
n <- rep(7,3)
group <- rep(1:3, n)
group

#summaries by group for combined data, applies stem function across y by group
tmp <- tapply(y, group, stem) #apply stem plot function to y-values by group number
plot(y~as.factor(group), data=data) #boxplot

tmpfn <- function(x){c(sum = sum(x), mean = mean(x), var=var(x), n = length(x))}
tapply(y, group, tmpfn) #applies function tmpfn over y by group

data <- data.frame(y=y, group=factor(group))
data #data frame of y-values with group indicators
fit <- lm(y~group,data=data) #fit a model y by group
anova(fit) #statistical differences between groups, p-value = 0.03735
df <- anova(fit)[, "Df"] #Df_treatment
names(df) <- c('trt', 'err')#residual_df; alt use anova(fit)$Df
df

#alternative method to anova(fit)
anova_model <- aov(y~group, data=data)
summary(anova_model)

alpha <- c(.05, .01)
qf(alpha, df["trt"], df["err"], lower.tail = FALSE) #quantile values for ANOVA at 95% and 99% - F-distribution

#confidence interval on pooled variance 
RSS <- anova(fit)["Residuals", "Sum Sq"] 
RSS/qchisq(c(0.025, 0.975), 18, lower.tail = FALSE)

#POST-HOC tests
#pairwise t-test using pooled SD
pairwise.t.test(y, group, p.adjust = "bonferroni") #Bonferroni correction 
#signficant differences between mean group 1 and group 3

#TukeyHSD CI
Tukey <- TukeyHSD(aov(fit),conf.level =0.95) #significant differences group1 with group3 (doesn't contain 0, differencs > 0)
plot(Tukey)

