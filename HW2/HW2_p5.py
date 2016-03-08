from numpy.random import multivariate_normal
import matplotlib.pyplot as plt
import pandas as pd
import numpy as np
import scipy.stats as ss

mean=[0,0,0]
cov=[[6,3,2],[3,10,3],[2,3,5]]
x,y,z = multivariate_normal(mean,cov,1000000).T
Sample=pd.DataFrame(x,columns=['X'])
Sample['Y']=y
Sample['Z']=z
xx=Sample[map(lambda y,z:y>=-0.05 and y<=0.05 and z>=-0.05 and z<=0.05,Sample['Y'],Sample['Z'])]['X']
xx.hist()
print np.mean(np.array(xx))#=-0.183756247228
print np.var(np.array(xx))#=4.84656080454
plt.show()
ss.shapiro(xx)
#(0.9962435960769653, 0.8010374903678894)


'''
#plt.plot(x,x,x,'x'); plt.axis('equal'); 
#plt.show()
import matplotlib as mpl
from mpl_toolkits.mplot3d import Axes3D
import numpy as np

mpl.rcParams['legend.fontsize'] = 10
#fig = plt.figure()
#ax = fig.gca(projection='3d')
#ax.scatter(x, y, z)
#ax.legend()
'''

