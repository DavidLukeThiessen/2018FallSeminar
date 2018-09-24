# This script generates the data files to be used in the presenation
library(MASS)


# Example 3 from Inference and Missing Data - Rubin, discussion
# Column 1 is X, Column 2 is Y
Sig = matrix(c(1,0.7,0.7,1),2,2)
ExData1 = mvrnorm(n=50,mu=c(0,0),Sigma=Sig)

cor(ExData1[,1],ExData1[,2])

# In method A, Y is missing if X>0
# In method B, Y is missing if Y>0
# M=1 means observed
MisMatA = matrix(c(rep(1,100)),50,2)
MisMatA[,2] = ifelse(ExData1[,1]>0,0,1)
MisMatB = matrix(c(rep(1,100)),50,2)
MisMatB[,2] = ifelse(ExData1[,2]>0,0,1)



dist(ExData1[,2])
