
library(openxlsx); #load the Excel Library
library(stats)
InputData = read.xlsx("/Users/Tiffany/Desktop/Project Files/AB Testing/Continuous Consumption.xlsx", sheet = "R data set")

str(InputData)
ControlGroup <- InputData[InputData$Variation.Title == "Control Group",]
TreatmentGroup <- InputData[InputData$Variation.Title == "No Continuous Consumption",]

#Assuming normal distribution and large sample size & using a z-test
#Assume variances are equal for both populations
#Determine maximum pooled SD to conclude statistical differences between 2 population means
Std_Approx <- function(m1,m2,n1,n2,m0=0, conf_level=.95){
  qt <- qnorm((1-conf_level)/2)
  n_pooled <- sqrt((1/n1) + (1/n2))
  s_pooled <- (m1-m2-m0)/(n_pooled*qt)
  return(abs(s_pooled)) 
}

m1 <- TreatmentGroup[,"PV/V"]
m2 <- ControlGroup[,"PV/V"]

n1 <- TreatmentGroup[,"new_session_start.(hits)"]
n2 <- ControlGroup[,"new_session_start.(hits)"]

Results <- Std_Approx(m1,m2,n1,n2)
names(Results) <- paste("comp", 1:length(Results), sep = "")
Results

#Simulation
n_1 = 140491
n_2 = 140861

set.seed(100)
x = rnorm(n_1, mean = 3.47, sd= 92)
set.seed(100)
y = rnorm(n_2, mean = 2.81, sd= 92)

hist(y, col=rgb(1,0,0,0.5))
hist(x, col=rgb(0,0,1,0.5), add=T)

## H0: The null hypothesis is that the 2 populations from which
##     the data was drawn have the same true mean
## A:  The alternative is that this mean is different in at
##     least one of the populations.

t.test(x,y)
