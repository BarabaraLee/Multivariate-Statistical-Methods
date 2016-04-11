library(MASS)
library(tree)
crabs

crabs$one<-as.factor(with(crabs, paste0(sp, sex)))
levels(crabs$one)
#[1] "BF" "BM" "OF" "OM"
crabs<-crabs[-c(1,2)]

#-------------Prob1------------------
tree.model1<-tree(one~.,data=crabs)
plot(tree.model1)
text(tree.model1,cex=0.5)
summary(tree.model1)
# Classification tree:
#   tree(formula = one ~ ., data = crabs)
# Variables actually used in tree construction:
#   [1] "FL"    "index" "BD"    "RW"   
# [5] "CW"   
# Number of terminal nodes:  21 
# Residual mean deviance:  0.4003 = 71.65 / 179 
# Misclassification error rate: 0.09 = 18 / 200 

#-------------Prob2------------------
#LDA:
MCR_a=0
for(i in 1:500){
train<-sample(1:200,150)
z=lda(one~.,data=crabs,subset=train)
pred=predict(z,crabs[-train,])
c1=pred$class
c2=crabs[-train,7]
MCR=sum(ifelse(c1==c2,0,ifelse(c1!=c2,1,NA)))/length(c1)
MCR_a=MCR_a+MCR
}
MCR_f=MCR_a/500 #=0.03092
#QDA:
MCR_a=0
for(i in 1:500){
  train<-sample(1:200,150)
  z=qda(one~.,data=crabs,subset=train)
  pred=predict(z,crabs[-train,])
  c1=pred$class
  c2=crabs[-train,7]
  MCR=sum(ifelse(c1==c2,0,ifelse(c1!=c2,1,NA)))/length(c1)
  MCR_a=MCR_a+MCR
}
MCR_f=MCR_a/500 #0.04144

#-------------Prob3------------------Implementation of K-means
#Part1, K=2
data=crabs[,4:8]
label<-rep(0,200)
data=scale(data)
center1=c(-1.5,-1.5,-1.5,-1.5,-1.5)
center2=c(1.5,1.5,1.5,1.5,1.5)
center=rbind(center1,center2)

dis<-function(record,center){
  sqrt(sum((record-center)^2))
}
K=2
c1=c()
c2=c()

c=rbind(c1,c2)
for(t in 1:50){
  #find new labels
  for(i in 1:200){
    dist=100000
    kk=0
    for (j in 1:K){
      if (dis(data[i,],center[j,])<dist){
        dist=dis(data[i,],center[j,])
        kk=j
      }
    }
    label[i]=kk
  }
  #find new centers
  Fouravg1=sapply(split(data[,1],label),mean)
  Fouravg2=sapply(split(data[,2],label),mean)
  Fouravg3=sapply(split(data[,3],label),mean)
  Fouravg4=sapply(split(data[,4],label),mean)
  Fouravg5=sapply(split(data[,5],label),mean)
  for (j in 1:K){
    center[j,]=c(Fouravg1[j],Fouravg2[j],Fouravg3[j],Fouravg4[j],Fouravg5[j])
  }
  print (label)
}
# [1] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2
# [34] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1
# [67] 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [100] 2 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 2 1 2 2 2 2 2 2 2
# [133] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 2 2 2 2
# [166] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [199] 2 2
#Part2, K=4
data=crabs[,4:8]
label<-rep(0,200)
data=scale(data)
center1=c(-2,-2,-2,-2,-2)
center2=c(2,2,2,2,2)
center3=c(1,1,1,1,1)
center4=c(-1,-1,-1,-1,-1)
center=rbind(center1,center2,center3,center4)

dis<-function(record,center){
  sqrt(sum((record-center)^2))
}
K=4
c1=c()
c2=c()
c3=c()
c4=c()
c=rbind(c1,c2,c3,c4)
for(t in 1:50){
  #find new labels
  for(i in 1:200){
    dist=100000
    kk=0
    for (j in 1:K){
      if (dis(data[i,],center[j,])<dist){
        dist=dis(data[i,],center[j,])
        kk=j
      }
    }
    label[i]=kk
  }
  #find new centers
  Fouravg1=sapply(split(data[,1],label),mean)
  Fouravg2=sapply(split(data[,2],label),mean)
  Fouravg3=sapply(split(data[,3],label),mean)
  Fouravg4=sapply(split(data[,4],label),mean)
  Fouravg5=sapply(split(data[,5],label),mean)
  for (j in 1:K){
  center[j,]=c(Fouravg1[j],Fouravg2[j],Fouravg3[j],Fouravg4[j],Fouravg5[j])
  }
  print (label)
}
# [1] 1 1 1 1 1 1 1 1 1 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3 3 3 3
# [34] 3 3 3 3 3 3 3 3 3 3 2 2 2 2 2 2 2 1 1 1 1 1 1 1 1 1 1 1 1 4 4 4 4
# [67] 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 3 2 2
# [100] 2 1 1 1 1 1 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 3 3 3 3 3 3 3 3 3 3
# [133] 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 1 1 4 4 4 4 4 4 4 4 4 3 3 3 3
# [166] 3 3 3 3 3 3 3 3 3 3 2 3 3 3 3 3 3 3 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [199] 2 2


