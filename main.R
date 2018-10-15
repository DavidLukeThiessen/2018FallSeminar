# This script generates the data files to be used in the presenation
library(MASS)


# Example 3 from Inference and Missing Data - Rubin, discussion
# Column 1 is X, Column 2 is Y

rm(list=ls())

Mu = c(1,1)
Sig = matrix(c(1,0.5,0.5,1),2,2)
n = 30
m = 1000

mylogit=function(x){
  1/(1+exp(x))
}

mydatagen=function(){
  ExData1 = mvrnorm(n=n,mu=Mu,Sigma=Sig)
  MCARPattern = rbinom(n=n,size=1,p=0.7)
  MARPattern = rbinom(n=n,size=1,p=1-sapply(ExData1[,1],mylogit))
  MNARPattern = rbinom(n=n,size=1,p=1-sapply(ExData1[,2],mylogit))
  mydata = cbind(ExData1,MCARPattern,MARPattern,MNARPattern)
  colnames(mydata)=c("X1","X2","X3","X4","X5")
  # mean(sapply(ExData1[,1],mylogit))
  # mean(sapply(ExData1[,2],mylogit))
  mydata = data.frame(mydata)
  return(mydata)
}


SimResults = matrix(nrow = m,ncol = 12)
colnames(SimResults)=c("CC MCAR","CC MAR","CC MNAR","IPW MCAR","IPW MAR","IPW MNAR",
                       "SI Mean MCAR","SI Mean MAR","SI Mean MNAR",
                       "SI Reg MCAR", "SI Reg MAR", "SI Reg MNAR")
# SimResults

set.seed(4)
for(i in 1:m){
  mydata = mydatagen()
  # mydata
  
  # c(sum(mydata[,3]),sum(mydata[,4]),sum(mydata[,5]))
  
  # mydata[which(mydata[,3]==0),]
  # mydata[which(mydata[,4]==0),]
  # mydata[which(mydata[,5]==0),]
  
  
  ######### CC Analysis
  SimResults[i,1] = mean(mydata[which(mydata[,3]==1),2])
  SimResults[i,2] = mean(mydata[which(mydata[,4]==1),2])
  SimResults[i,3] = mean(mydata[which(mydata[,5]==1),2])
  
  
  ############# IPW Analysis
  # model0 = glm(mydata[,3]~mydata[,1],family=binomial(link="logit"))
  # probs0 = predict.glm(model0,type='response')
  # # probs0 
  # weight0 = n*probs0/sum(probs0)
  # # weight0
  # # sum(weight0)
  # 
  # model1 = glm(mydata[,4]~mydata[,1],family=binomial(link="logit"))
  # probs1 = predict.glm(model1,type='response')
  # # probs1 
  # weight1 = n*probs1/sum(probs1)
  # # weight1
  # # sum(weight1)
  # 
  # model2 = glm(mydata[,5]~mydata[,1],family=binomial(link="logit"))
  # probs2 = predict.glm(model2,type='response')
  # # probs2 
  # weight2 = n*probs2/sum(probs2)
  # # weight2
  # # sum(weight2)
  # 
  # SimResults[i,4] = mean(mydata[which(mydata[,3]==1),2]/weight0[which(mydata[,3]==1)])
  # SimResults[i,5] = mean(mydata[which(mydata[,4]==1),2]/weight1[which(mydata[,4]==1)])
  # SimResults[i,6] = mean(mydata[which(mydata[,5]==1),2]/weight2[which(mydata[,5]==1)])
  
  # This tries the other weighting equation after (3.4) in SA with MD
  model0 = glm(X3 ~ X1,data=mydata,family=binomial(link="logit"))
  probs0 = predict.glm(model0,type='response')
  # probs0
  # 1/probs0
  # sum(1/probs0[which(mydata[,3]==1)])
  weight0 = sum(mydata[,3])*(1/probs0)/sum((1/probs0[which(mydata[,3]==1)]))
  # weight0
  # sum(weight0)
  
  model1 = glm(X4~X1,data=mydata,family=binomial(link="logit"))
  probs1 = predict.glm(model1,type='response')
  # probs1
  weight1 = sum(mydata[,4])*(1/probs1)/sum((1/probs1[which(mydata[,4]==1)]))
  # weight1
  # sum(weight1)
  
  model2 = glm(X5~X1,data=mydata,family=binomial(link="logit"))
  probs2 = predict.glm(model2,type='response')
  # probs2 
  weight2 = sum(mydata[,5])*(1/probs2)/sum((1/probs2[which(mydata[,5]==1)]))
  # weight2
  # sum(weight2)
  
  SimResults[i,4] = mean(mydata[which(mydata[,3]==1),2]*weight0[which(mydata[,3]==1)])
  SimResults[i,5] = mean(mydata[which(mydata[,4]==1),2]*weight1[which(mydata[,4]==1)])
  SimResults[i,6] = mean(mydata[which(mydata[,5]==1),2]*weight2[which(mydata[,5]==1)])
  
  
  # Single Imputation of sample average
  completed1 = ifelse(mydata[,3]==1,mydata[,2],mean(mydata[which(mydata[,3]==1),2]))
  # completed1
  # mydata[,c(2,3)]
  # mean(completed1)
  # mean(mydata[which(mydata[,3]==1),2])
  # sd(completed1)
  # sd(mydata[which(mydata[,3]==1),2])
  completed2 = ifelse(mydata[,4]==1,mydata[,2],mean(mydata[which(mydata[,4]==1),2]))
  completed3 = ifelse(mydata[,5]==1,mydata[,2],mean(mydata[which(mydata[,5]==1),2]))
  SimResults[i,7]=mean(completed1)
  SimResults[i,8]=mean(completed2)
  SimResults[i,9]=mean(completed3)
  
  
  
  # Single Imputation of Conditional Mean
  model4 = lm(X2~X1,data=mydata[which(mydata[,3]==1),])
  predicted4 = predict(model4,newdata=data.frame(X1=mydata[,1]))
  completed4 = ifelse(mydata[,3]==1,mydata[,2],predicted4)
  # completed4
  # mydata[,c(2,3)]
  SimResults[i,10]=mean(completed4)
  
  model5 = lm(X2~X1,data=mydata[which(mydata[,4]==1),])
  predicted5 = predict(model5,newdata=data.frame(X1=mydata[,1]))
  completed5 = ifelse(mydata[,3]==1,mydata[,2],predicted5)
  # completed4
  # mydata[,c(2,3)]
  SimResults[i,11]=mean(completed5)
  
  model6 = lm(X2~X1,data=mydata[which(mydata[,5]==1),])
  predicted6 = predict(model6,newdata=data.frame(X1=mydata[,1]))
  completed6 = ifelse(mydata[,3]==1,mydata[,2],predicted6)
  # completed4
  # mydata[,c(2,3)]
  SimResults[i,12]=mean(completed6)
  
}
save.image()
head(SimResults)

colMeans(SimResults)
apply(X=SimResults, MARGIN=2, FUN=sd)


plot(mydata[,1],mydata[,4],xlab="X1",ylab="Missing Indicator")
curve(predict(model1,data.frame(X1=x),type="resp"),add=TRUE)
# model1


# head(mydata)







