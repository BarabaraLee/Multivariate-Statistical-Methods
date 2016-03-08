#Final Exam ------------------------------------ Prob 1
Y=read.csv('Prob1_Y')
X=read.csv('Prob1_X')

#PCA dimension reduction, scaling is not necessary since 
#all values have the same range of [0,255]-----------
Y_pos=function(x,y)320*(x-1)+y
y_c=1+Y_pos(c(90,120,150,180,210,90,120,180,90,120,180),c(160,160,160,160,160,120,120,120,200,200,200))
Y_n=as.matrix(Y[,y_c])

# > Y_n
#         X28639 X38239 X47839 X57439 X67039 X28599 X38199 X57399 X28679 X38279 X57479
# [1,]    220     41    112     26    189     44     48    165    182     29     15
# [2,]    132     11    152     69     83    217    255    255    157    117     25
# [3,]    172    174    199     39     61     26    123    102     19     68     53
# [4,]     44     90    111     83     54     10     45    255    143     91     25
# [5,]    171     57     41     78    101     97    111    255    154     89     72
# [6,]     78    119     80     68     60    168     98     79     24     19    206
# [7,]    181     32    132     76     43     31    135     25    105     55     16
# [8,]    194     97    164     81     67    119     76     80    122     69     76
# [9,]    231    187    105     66     98    146     41     85    102     64     75
# [10,]   215     90     72    160     54     48     69     69    161     13     79
# [11,]    36     79    245    145    179     17     13     18     16     65    123
# [12,]    47    158     75     97     61     43     22    255     59    137     80
# [13,]    73     55    110     30     85     32     26    255    131    136     20
# [14,]   102     10    190     46     74    164     85     36     12     11    255
# [15,]   223    123    158    107     94    102    119    126    180     39     83

X_=prcomp(X[,-1])# X_1 out of PCA 
plot(X_)
X_n=X_$x[,1:10]# Dimension Reduction
# > summary(X_)
# Importance of components:
#                            PC1       PC2       PC3       PC4       PC5       PC6       PC7       PC8
# Standard deviation     1.463e+04 9621.7395 7.054e+03 6486.9965 6.177e+03 5.399e+03 5.052e+03 4.543e+03
# Proportion of Variance 3.585e-01    0.1551 8.336e-02    0.0705 6.393e-02 4.883e-02 4.277e-02 3.458e-02
# Cumulative Proportion  3.585e-01    0.5136 5.969e-01    0.6674 7.314e-01 7.802e-01 8.230e-01 8.575e-01
#                           PC9      PC10      PC11      PC12      PC13      PC14      PC15
# Standard deviation     4.265e+03 4.142e+03 3.791e+03 3681.0416 3.331e+03 3.265e+03 2.669e-10
# Proportion of Variance 3.048e-02 2.874e-02 2.408e-02    0.0227 1.859e-02 1.786e-02 0.000e+00
# Cumulative Proportion  8.880e-01 9.168e-01 9.408e-01    0.9636 9.821e-01 1.000e+00 1.000e+00
result=lm(Y_n~X_n)# Multiple regression
summary(result)[5]# (210,160) chin position regression result
# Response X67039 :
#   
#   Call:
#   lm(formula = X67039 ~ X_n)
# 
# Residuals:
#   1        2        3        4        5        6        7        8        9       10       11       12       13       14       15 
# 14.7881 -13.4603 -16.5053   2.8978   6.1164  -0.8095  18.4742   4.4816  -3.9652  -8.9358  -1.1253  -0.3596  -9.6602  -1.0626   9.1257 
# 
# Coefficients:
#                Estimate Std. Error t value Pr(>|t|)    
# (Intercept) 86.8666667  4.7518907  18.280 5.27e-05 ***
# X_nPC1      -0.0007739  0.0003363  -2.301  0.08280 .  
# X_nPC2       0.0011281  0.0005112   2.207  0.09196 .  
# X_nPC3       0.0014583  0.0006973   2.091  0.10466    
# X_nPC4       0.0030938  0.0007582   4.080  0.01509 *  
# X_nPC5      -0.0009132  0.0007963  -1.147  0.31538    
# X_nPC6       0.0016490  0.0009110   1.810  0.14454    
# X_nPC7       0.0013891  0.0009735   1.427  0.22678    
# X_nPC8       0.0062613  0.0010827   5.783  0.00444 ** 
# X_nPC9       0.0004654  0.0011532   0.404  0.70718    
# X_nPC10     -0.0014008  0.0011875  -1.180  0.30350    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 18.4 on 4 degrees of freedom
# Multiple R-squared:  0.9479,	Adjusted R-squared:  0.8177 
# F-statistic: 7.281 on 10 and 4 DF,  p-value: 0.03535

#make a new data for prediction:
x_new=(X_n[1,]+X_n[3,]+X_n[5,]+X_n[7,]+X_n[8,])/5.0
#x_new=c(1,colSums(rbind(x_new,c(2,-20,10,-10,5,-5,10,-10,20,-20))))
x_new=c(1,x_new)
X_n1=cbind(1,X_n)
coeff=result$coefficients[,5]
t(x_new)%*%result$coefficients
#        X28639   X38239   X47839   X57439   X67039   X28599   X38199   X57399   X28679   X38279   X57479
# [1,] 176.3837 84.75026 129.8396 59.86544 86.72901 69.55481 100.2671 132.6298 107.7541 70.61056 45.55909
y_new_est=t(x_new)%*%coeff #=86.72901
RSD=18.4 #Residual Standard Error
sde_y_new=RSD*sqrt(t(x_new)%*%solve(t(X_n1)%*%X_n1)%*%(x_new)+1)#=19.82849
t_quantile=qt(p=0.975,df=4)#=2.776445
PI=c(y_new_est-t_quantile*sde_y_new,y_new_est+t_quantile*sde_y_new)
#PI=[31.67629, 141.78173]

#Hypothesis test:
m1<-manova(Y_n~X_n[,1])# test of model with only one predictor variable
summary(m1,test='Wilks')
#            Df    Wilks approx F num Df den Df  Pr(>F)  
# X_n[, 1]   1 0.021101   12.652     11      3 0.02996 *
# Residuals 13 
m1<-manova(Y_n~X_n[,1:2])# test of model with only two predictor variable
summary(m1,test='Wilks')
#             Df      Wilks approx F num Df den Df  Pr(>F)  
# X_n[, 1:2]  2 0.00046025   8.2932     22      4 0.02633 *
# Residuals  12  
m1<-manova(Y_n~X_n[,1:3])# test of model with only three predictor variable
summary(m1,test='Wilks')
#             Df     Wilks approx F num Df den Df  Pr(>F)  
# X_n[, 1:3]  3 4.886e-06   6.9119     33 3.6502 0.04462 *
#   Residuals  11      

#Final Exam ------------------------------------ Prob 2
X_happy=read.csv('_happy')
X_sad=read.csv('_sad')
X_wink=read.csv('_wink')

dis <- vegdist(all,method='bray')
#pl <- ordiplot(cmdscale(dis), main="cmdscale")
ord <- isomap(dis, k=3,ndim=10)
plot(ord, main="Isomap k=3")
all_pca=ord$points
labels=c(rep(1,15),rep(2,15),rep(3,15))
all_p=as.data.frame(cbind(all_pca,labels))
all_p$labels=as.factor(all_p$labels)
all_p=all_p[sample(1:nrow(all_p)),]
trainData=all_p[1:30,]
testData=all_p[31:45,]
library(e1071)
# k1='linear'
# k2='polynomial'
# k3='radial'
# k4='sigmoid'
obj <- tune(svm, labels~.,data=trainData,kernel='sigmoid',
            ranges = list( cost = c(0.005,0.01,0.1,0.3,0.5,1,2,4,6,8) ), 
            tunecontrol = tune.control(cross=5)   )
summary(obj)
plot(obj)
svmModel<-svm(labels~.,data=trainData,kernel='sigmoid',cost=obj$best.parameters)
result=predict(svmModel,testData[,1:10])
compare=cbind(result,testData[,11])
sum=0
for (i in 1:15){
  if(compare[i,1]!=compare[i,2])sum=sum+1
}
print(t(compare))
# 40 12 22 41 34 39 30 6 7 21 44 43 18 11 26
# result  3  2  2  2  2  3  3 1 2  1  3  3  2  3  2
#         3  1  2  3  3  3  2 1 1  2  3  3  2  1  2
print(sum/15.0) #print the error rate
#[1] 0.4666667

#Final Exam ------------------------------------ Prob 4
library(e1071)
setwd("/media/intel/KINGSTON/yalefaces")
X_centerlight=as.matrix(read.csv('_centerlight'))
X_glasses=as.matrix(read.csv('_glasses'))
X_happy=as.matrix(read.csv('_happy'))
X_leftlight=as.matrix(read.csv('_leftlight'))
X_noglasses=as.matrix(read.csv('_noglasses'))
X_normal=as.matrix(read.csv('_normal'))
X_rightlight=as.matrix(read.csv('_rightlight'))
X_sad=as.matrix(read.csv('_sad'))
X_sleepy=as.matrix(read.csv('_sleepy'))
X_surprised=as.matrix(read.csv('_surprised'))
X_wink=as.matrix(read.csv('_wink'))
all=rbind(X_centerlight,X_glasses,X_happy,X_leftlight, X_noglasses,X_normal,X_rightlight,X_sad, X_sleepy,X_surprised,X_wink)
all_pca=prcomp(all[,-1])
plot(all_pca)
# > summary(all_pca)
# Importance of components:
#                             PC1       PC2       PC3       PC4       PC5       PC6       PC7       PC8       PC9
# Standard deviation     1.024e+04 6942.0245 5.517e+03 5.055e+03 4.023e+03 3.762e+03 3.081e+03 2.812e+03 2.771e+03
# Proportion of Variance 3.054e-01    0.1404 8.869e-02 7.444e-02 4.715e-02 4.123e-02 2.766e-02 2.304e-02 2.237e-02 =>0.77038
# Cumulative Proportion  3.054e-01    0.4458 5.345e-01 6.089e-01 6.561e-01 6.973e-01 7.250e-01 7.480e-01 7.704e-01
#                            PC10      PC11      PC12      PC13      PC14      PC15      PC16      PC17      PC18
# Standard deviation     2.523e+03 2.150e+03 1.998e+03 1.795e+03 1.760e+03 1.731e+03 1657.4044 1.586e+03 1.502e+03
# Proportion of Variance 1.855e-02 1.346e-02 1.163e-02 9.390e-03 9.030e-03 8.730e-03    0.0080 7.320e-03 6.570e-03 =>0.86306
# Cumulative Proportion  7.889e-01 8.024e-01 8.140e-01 8.234e-01 8.325e-01 8.412e-01    0.8492 8.565e-01 8.631e-01
#                            PC19      PC20      PC21      PC22      PC23      PC24      PC25      PC26      PC27
# Standard deviation     1.463e+03 1.432e+03 1.343e+03 1.291e+03 1.235e+03 1.205e+03 1.147e+03 1.144e+03 1.084e+03
# Proportion of Variance 6.240e-03 5.970e-03 5.260e-03 4.850e-03 4.450e-03 4.230e-03 3.840e-03 3.810e-03 3.420e-03 =>0.90513
# Cumulative Proportion  8.693e-01 8.753e-01 8.806e-01 8.854e-01 8.899e-01 8.941e-01 8.979e-01 9.017e-01 9.052e-01
Cumulative_Proportion=c(3.054e-01,0.4458,5.345e-01,6.089e-01,6.561e-01,6.973e-01,7.250e-01,7.480e-01,7.704e-01,7.889e-01,8.024e-01,8.140e-01,8.234e-01,8.325e-01,8.412e-01,0.8492,8.565e-01,8.631e-01,8.693e-01,8.753e-01,8.806e-01,8.854e-01,8.899e-01,8.941e-01,8.979e-01,9.017e-01)
plot(Cumulative_Proportion)
all_p=all_pca$x[,1:6] #cu

#pairs(~all_p[,1]+all_p[,2]+all_p[,3]+all_p[,4]+all_p[,5])

#Final Exam ------------------------------------ Prob 3
library(kernlab)
SC=specc(scale(all_p),centers=11,kernel ='laplacedot',kpar =list(sigma=7.5),iterations=50000)
plot(scale(all_p),col=SC)
print(SC)

# Spectral Clustering object of class "specc" 
# Cluster memberships: 
#   4 4 9 4 4 8 9 7 9 4 11 4 5 8 7 4 4 9 4 4 10 7 7 9 4 11 4 5 8 9 4 4 9 4 4 10 9 7 1 9 11 4 5 8 9 6 6 6 6 6 6 6 3 6 6 3 6 3 3 6 4 4 9 4 4 10 9 7 1 9 11 6 5 8 9 4 4 9 4 4 10 9 7 1 9 11 6 5 8 7 2 5 2 2 2 10 2 2 2 5 11 4 3 10 2 4 4 9 4 4 10 9 9 1 9 11 4 5 8 9 4 4 9 4 4 10 9 1 1 9 11 4 5 8 9 4 4 7 4 4 10 9 9 1 9 11 4 5 8 9 4 4 9 4 4 10 9 9 1 9 11 4 5 8 9 
# Laplace kernel function. 
# Hyperparameter : sigma =  7.5 
# Centers:  
#   [,1]        [,2]        [,3]        [,4]         [,5]       [,6]
# [1,]  0.6678677  0.96538788 -0.06190962 -1.14616038 -0.326007486 -0.6045286
# [2,]  1.4127423 -1.62200517  1.46986638  2.00022338  0.812460818 -0.6306745
# [3,] -0.4090628 -1.41286443  1.26762034 -1.08748609  0.878683766  0.3090773
# [4,] -0.7453242  0.07737694 -0.83427719  0.53665793 -0.324755227 -0.2163078
# [5,] -0.2796854  0.49151014 -0.48312542  0.79204993  1.902509579  0.6260486
# [6,] -1.3654063 -1.57821939  0.47237266 -1.67295552  0.566593963 -0.6877674
# [7,]  0.4737479  1.10040388  0.76549838 -0.50462665 -0.173919293 -0.4935667
# [8,]  1.5734180  0.02907292 -0.83612418 -1.55428693 -0.502038538  1.1820232
# [9,]  0.1206100  0.98752670  0.79254820  0.05631518 -0.001721027 -0.2944560
# [10,]  2.2063500 -1.47308804 -1.23145327  0.08152264 -0.199894005 -0.4585494
# [11,] -0.5770591 -0.61587567  0.59956321  0.42527858 -1.299479691  2.7854162
# Cluster size:  
#   [1]  8  8  5 46 11 13  9 10 35 10 10
# Within-cluster sum of squares:  
#   [1]  53.28955 181.07577  68.17774 180.11255  96.37523 143.88464  43.08548 146.86978 167.48242 169.36351 190.01196
#all_pl=scale(all_p)[1:165,]

all_pl=scale(all_p)
wi_ss=sum(53.28955, 181.07577,  68.17774, 180.11255,  96.37523, 143.88464,  43.08548, 146.86978, 167.48242, 169.36351, 190.01196)
#wi_ss=1439.729

d<-dist(all_pl,method='euclidean')
fit<-hclust(d,method='ward')
plot(fit,cex=.75)
groups<-cutree(fit,k=11)
rect.hclust(fit,k=11,border='red')
groups
# [1]  1  1  2  1  1  3  2  4  4  1  5  1  6  3  2  1  1  2  1  7  8  4  4  1  1  5  1
# [28]  6  3  9  1  1  2  1  7  8  2  4  4  9  5  1  6  3  2 10 10 10 10 10 10 10 10 10
# [55] 10 10 10 10 10 10  1  1  2  1  7  8  2  4  4  9  5 10  6  3  9  1  1  2  1  7  8
# [82]  2  4  4  9  5 10  6  3  4 11  1 11 11 11  8 11 11 11  1  5  1 11  8 11  1  1  2
# [109]  1  7  8  2  1  4  9  5  1  6  3  2  1  1  2  1  7  8  2  4  4  9  5  1  6  3  9
# [136]  1  1  4  1  7  8  2  2  4  9  5  1  6  3  9  1  1  2  1  7  8  2  4  4  9  5  1
# [163]  6  3  2

var1=split(all_pl[,1],groups)
var2=split(all_pl[,2],groups)
var3=split(all_pl[,3],groups)
var4=split(all_pl[,4],groups)
var5=split(all_pl[,5],groups)
var6=split(all_pl[,6],groups)
cluster1=cbind(var1$`1`,var2$`1`,var3$`1`,var4$`1`,var5$`1`,var6$`1`)
cluster2=cbind(var1$`2`,var2$`2`,var3$`2`,var4$`2`,var5$`2`,var6$`2`)
cluster3=cbind(var1$`3`,var2$`3`,var3$`3`,var4$`3`,var5$`3`,var6$`3`)
cluster4=cbind(var1$`4`,var2$`4`,var3$`4`,var4$`4`,var5$`4`,var6$`4`)
cluster5=cbind(var1$`5`,var2$`5`,var3$`5`,var4$`5`,var5$`5`,var6$`5`)
cluster6=cbind(var1$`6`,var2$`6`,var3$`6`,var4$`6`,var5$`6`,var6$`6`)
cluster7=cbind(var1$`7`,var2$`7`,var3$`7`,var4$`7`,var5$`7`,var6$`7`)
cluster8=cbind(var1$`8`,var2$`8`,var3$`8`,var4$`8`,var5$`8`,var6$`8`)
cluster9=cbind(var1$`9`,var2$`9`,var3$`9`,var4$`9`,var5$`9`,var6$`9`)
cluster10=cbind(var1$`10`,var2$`10`,var3$`10`,var4$`10`,var5$`10`,var6$`10`)
cluster11=cbind(var1$`11`,var2$`11`,var3$`11`,var4$`11`,var5$`11`,var6$`11`)
wiss_1=sum((cluster1-colMeans(cluster1))^2)
wiss_2=sum((cluster2-colMeans(cluster2))^2)
wiss_3=sum((cluster3-colMeans(cluster3))^2)
wiss_4=sum((cluster4-colMeans(cluster4))^2)
wiss_5=sum((cluster5-colMeans(cluster5))^2)
wiss_6=sum((cluster6-colMeans(cluster6))^2)
wiss_7=sum((cluster7-colMeans(cluster7))^2)
wiss_8=sum((cluster8-colMeans(cluster8))^2)
wiss_9=sum((cluster9-colMeans(cluster9))^2)
wiss_10=sum((cluster10-colMeans(cluster10))^2)
wiss_11=sum((cluster11-colMeans(cluster11))^2)
wiss_hc=cbind(wiss_1,wiss_2,wiss_3,wiss_4,wiss_5,wiss_6,wiss_7,wiss_8,wiss_9,wiss_10,wiss_11)
#        wiss_1   wiss_2   wiss_3   wiss_4  wiss_5   wiss_6   wiss_7   wiss_8   wiss_9  wiss_10 wiss_11
# [1,] 163.3609 55.84344 146.8698 97.13051 190.012 100.9495 56.63406 169.3635 114.8533 225.2949 196.288
wi_ss2=sum(wiss_hc)#1516.6

#Final Exam ------------------------------------ Prob 5
library(CCA)
X_happy1=prcomp(X_happy[,-1])# X_1 out of PCA 
plot(X_happy1)
X_h=X_happy1$x[,1:6]
X_sleepy1=prcomp(X_sleepy[,-1])# X_1 out of PCA 
plot(X_sleepy1)
X_s=X_sleepy1$x[,1:6]

ccl<-cc(X_h,X_s)#Canonical Correlation Analysis
ccl$cor
# [1] 0.9969858 0.9954081 0.9776235 0.9705297 0.8906162 0.6941896
cor(ccl$scores$xscores,ccl$scores$yscores)
#           [,1]          [,2]          [,3]          [,4]          [,5]          [,6]
# [1,]  9.969858e-01 -8.173166e-17 -1.020989e-16  1.423499e-16  1.365620e-16  1.984438e-16
# [2,]  5.791188e-17  9.954081e-01 -5.356714e-16 -1.312853e-16 -1.374403e-16  1.802728e-16
# [3,]  4.105060e-17 -1.684400e-16  9.776235e-01  1.593777e-17 -4.621412e-17  3.567025e-17
# [4,] -1.211247e-16  2.083653e-16  5.339696e-17  9.705297e-01  3.217196e-16 -1.856619e-16
# [5,]  2.348024e-16  2.923592e-16  1.208053e-16  1.309639e-16  8.906162e-01  1.035800e-17
# [6,] -1.087203e-17  3.034813e-16 -6.317027e-17  3.726713e-16  3.332470e-17  6.941896e-01
ccl$xcoef
#          [,1]          [,2]          [,3]          [,4]          [,5]          [,6]
# PC1 -9.018805e-05  2.336406e-05 -1.203454e-05 -1.429606e-05 -3.076575e-05  1.369199e-05
# PC2  3.021849e-05  6.605923e-05  1.441669e-05 -1.181193e-04  1.774393e-05  1.553452e-05
# PC3 -3.752026e-05 -1.571664e-04 -3.920744e-05 -9.971860e-05  4.042683e-05 -2.669433e-05
# PC4 -1.029635e-05 -4.568464e-05  6.742929e-05  1.568610e-05  5.464790e-05  2.085734e-04
# PC5 -9.585554e-06 -4.806061e-05  2.007840e-04 -2.573979e-05 -9.663229e-05 -4.865693e-05
# PC6 -8.560564e-05  4.264114e-05  8.740372e-05  3.234244e-05  1.994713e-04 -7.783813e-05

ccl$ycoef
#          [,1]          [,2]          [,3]          [,4]          [,5]          [,6]
# PC1 -9.133539e-05  2.690728e-05 -5.745785e-06 -2.581993e-05 -2.388294e-05  1.810427e-05
# PC2 -4.066385e-05 -4.372407e-05  2.331921e-05  1.235190e-04 -3.580240e-05 -3.832255e-06
# PC3 -3.210862e-05 -1.437134e-04  3.937445e-05 -4.769514e-05  6.440233e-05  8.103907e-05
# PC4 -5.867591e-05 -5.919880e-05 -1.864479e-05 -2.627764e-05  5.524930e-05 -1.785440e-04
# PC5  1.477952e-05 -7.762694e-05 -1.961595e-04 -6.725493e-06 -7.500086e-05  1.914687e-05
# PC6  3.838458e-05 -6.416380e-05  9.816486e-05 -8.434131e-05 -1.872341e-04 -4.711644e-05
X_ht=ccl$scores$xscores
X_st=ccl$scores$yscores
man<-manova(X_ht~X_st)# test of X_ht~X_st
summary(man,test='Wilks')
#            Df      Wilks approx F num Df den Df   Pr(>F)    
# X_st       6 1.5186e-08   26.257     36 15.935 5.29e-09 ***
# Residuals  8          
plot(ccl$cor,type="b",ylab='Correlation')


