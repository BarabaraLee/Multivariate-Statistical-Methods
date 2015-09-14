import numpy as np
import matplotlib.pyplot as plt

'''
This is an demonstration that two normal RVs 
form a distribution that is not bivariate 
normal distributed.
'''
X1=np.random.randn(500)

def X2(X1,c):
    X2=[]
    for i in range(len(X1)):
        if X1[i]>=c or X1[i]<=-c:
            X2.append(X1[i])
        elif X1[i]>-c and X1[i]<c:
            X2.append(-X1[i])
    return np.array(X2)
    
X2=X2(X1,1.0)
plt.plot(X1,X2,'o',c='r',)
plt.xlabel('X1')
plt.ylabel('X2')
plt.show()