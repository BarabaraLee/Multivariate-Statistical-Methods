# Author Linjun Li, Virginia Tech
# Code of the iterative reweighted least squares (IWLS) algorithm
import numpy as np
from numpy.linalg import inv
from sklearn import datasets

def p(x,b):
    expo=np.exp(sum(x*b))
    return 1.0/(1+1/expo)
def X_tuda(X,b):
    X_t=[]
    for i in range(len(X)):
        X_t.append(p(X[i],b)*(1-p(X[i],b))*X[i])
    return X_t
def new_beta(X,b,y):
    X_t=X_tuda(X,b)
    pp=map((lambda x: p(x,b)), X)
    left=inv(np.dot(X.T,X_t))  
    right=np.dot(X.T,(y-pp))
    return b+np.dot(left,right)
def IWLS(bnew,b,h):
    while sum((bnew-b)**2)/len(b)>h:
        b=bnew
        bnew=new_beta(X,b,y)
    return bnew
def ErrorRate(y_actual, y_hat):
    errorNumber=0
    for i in range(len(y_hat)): 
        if y_hat[i] != y_actual[i]:
            errorNumber=errorNumber+1
    return errorNumber*1.0/len(y_hat)
    
iris = datasets.load_iris()
X=iris.data[0:100]
a=np.ones(len(X)).reshape(len(X),1)
X=np.append(a,X,axis=1)
# The columns in the feature matrix are normalized
Xmax=X.max(axis=0)
X=X/Xmax
y=iris.target[0:100]

b=np.ones(5)*0.01
bnew=new_beta(X,b,y)
b_est=IWLS(bnew,b,1)
#Estimation of parameters for normalized X.
#13.9304908 , -126.32102131,  -48.57057898,  166.42321302, 71.61746806
b_est_orig=b_est/Xmax
#Estimation of parameters for the original X.
#13.9304908 , -18.04586019, -11.03876795,  32.63200255,  39.78748226
p_est=map((lambda x: p(x,b_est)), X)
Decision=np.array([int(round(i)) for i in p_est])
'''
y=iris.target[0:100]:
array([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
       1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
       1, 1, 1, 1, 1, 1, 1, 1])
Decision:
array([0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
       0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
       1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
       1, 1, 1, 1, 1, 1, 1, 1])
'''
print ErrorRate(y,Decision)#=0