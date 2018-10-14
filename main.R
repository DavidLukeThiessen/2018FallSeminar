# This script generates the data files to be used in the presenation
library(MASS)


# Example 3 from Inference and Missing Data - Rubin, discussion
# Column 1 is X, Column 2 is Y
rm(list=ls())


Sig = matrix(c(1,0.5,0.5,1),2,2)
Mu = c(1,1)
n = 30
m = 500

mylogit=function(x){
  1/(1+exp(x))
}

mydatagen=function(){
  ExData1 = mvrnorm(n=n,mu=Mu,Sigma=Sig)
  MCARPattern = rbinom(n=n,size=1,p=0.7)
  MARPattern = rbinom(n=n,size=1,p=1-sapply(ExData1[,1],mylogit))
  MNARPattern = rbinom(n=n,size=1,p=1-sapply(ExData1[,2],mylogit))
  mydata = cbind(ExData1,MCARPattern,MARPattern,MNARPattern)
  
  # mean(sapply(ExData1[,1],mylogit))
  # mean(sapply(ExData1[,2],mylogit))
  
  return(mydata)
}


SimResults = matrix(nrow = m,ncol = 6)
colnames(SimResults)=c("CC MCAR","CC MAR","CC MNAR","IPW MCAR","IPW MAR","IPW MNAR")
# SimResults

for(i in 1:m){
  mydata = mydatagen()
  # mydata
  
  c(sum(mydata[,3]),sum(mydata[,4]),sum(mydata[,5]))
  
  # mydata[which(mydata[,3]==0),]
  # mydata[which(mydata[,4]==0),]
  # mydata[which(mydata[,5]==0),]
  
  
  ######### CC Analysis
  SimResults[i,1] = mean(mydata[which(mydata[,3]==1),2])
  SimResults[i,2] = mean(mydata[which(mydata[,4]==1),2])
  SimResults[i,3] = mean(mydata[which(mydata[,5]==1),2])
  
  
  ############# IPW Analysis
  # Probability of it being 1 vs 0  ?
  
  model0 = glm(mydata[,3]~mydata[,1],family=binomial(link="logit"))
  probs0 = predict.glm(model0,type='response')
  # probs0 
  weight0 = n*probs0/sum(probs0)
  # weight0
  # sum(weight0)
  
  model1 = glm(mydata[,4]~mydata[,1],family=binomial(link="logit"))
  probs1 = predict.glm(model1,type='response')
  # probs1 
  weight1 = n*probs1/sum(probs1)
  # weight1
  # sum(weight1)
  
  model2 = glm(mydata[,5]~mydata[,1],family=binomial(link="logit"))
  probs2 = predict.glm(model2,type='response')
  # probs2 
  weight2 = n*probs2/sum(probs2)
  # weight2
  # sum(weight2)
  
  SimResults[i,4] = mean(mydata[which(mydata[,3]==1),2]/weight0[which(mydata[,3]==1)])
  SimResults[i,5] = mean(mydata[which(mydata[,4]==1),2]/weight1[which(mydata[,4]==1)])
  SimResults[i,6] = mean(mydata[which(mydata[,5]==1),2]/weight2[which(mydata[,5]==1)])
  
  
}

head(SimResults)

colMeans(SimResults)
apply(X=SimResults, MARGIN=2, FUN=sd)











