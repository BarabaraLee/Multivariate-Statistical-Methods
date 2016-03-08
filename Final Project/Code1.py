from PIL import Image
import numpy as np
from numpy import array,ndarray
import pandas as pd
#---------------------Prob 1 Data-----------------------
y=[]
x=[]
for i in range(1,16):
    im = Image.open('subject'+str(i).zfill(2)+'.happy.gif')
    y.append( list(array(im).flatten()) )

    im2 = Image.open('subject'+str(i).zfill(2)+'.wink.gif')
    im3 = Image.open('subject'+str(i).zfill(2)+'.surprised.gif')
    x.append(list(array(im2).flatten())+list(array(im3).flatten()))
    
pd.DataFrame(y).to_csv('Prob1_Y') #--image data
pd.DataFrame(x).to_csv('Prob1_X') #--image data
#---------------------Prob 2,3,4,5 Data-----------------------
pd.DataFrame(y).to_csv('_happy') #--image data
y_wink=[]
y_sad=[]
for i in range(1,16):
    im2 = Image.open('subject'+str(i).zfill(2)+'.wink.gif')
    im3 = Image.open('subject'+str(i).zfill(2)+'.sad.gif')
    y_wink.append( list(array(im2).flatten()) )
    y_sad.append( list(array(im3).flatten()) )
    
pd.DataFrame(y_wink).to_csv('_wink') #--image data
pd.DataFrame(y_sad).to_csv('_sad') #--image data

y_centerlight=[]
y_glasses=[]
y_leftlight=[]
y_noglasses=[]
y_normal=[]
y_rightlight=[]
y_sleepy=[]
y_surprised=[]
for i in range(1,16):
    im_centerlight = Image.open('subject'+str(i).zfill(2)+'.centerlight.gif')
    im_glasses = Image.open('subject'+str(i).zfill(2)+'.glasses.gif')
    im_leftlight = Image.open('subject'+str(i).zfill(2)+'.leftlight.gif')
    im_noglasses = Image.open('subject'+str(i).zfill(2)+'.noglasses.gif')
    im_normal = Image.open('subject'+str(i).zfill(2)+'.normal.gif')
    im_rightlight = Image.open('subject'+str(i).zfill(2)+'.rightlight.gif')
    im_sleepy = Image.open('subject'+str(i).zfill(2)+'.sleepy.gif')
    im_surprised = Image.open('subject'+str(i).zfill(2)+'.surprised.gif')
    
    y_centerlight.append( list(array(im_centerlight).flatten()) )
    y_glasses.append( list(array(im_glasses).flatten()) )
    y_leftlight.append( list(array(im_leftlight).flatten()) )
    y_noglasses.append( list(array(im_noglasses).flatten()) )
    y_normal.append( list(array(im_normal).flatten()) )
    y_rightlight.append( list(array(im_rightlight).flatten()) )
    y_sleepy.append( list(array(im_sleepy).flatten()) )
    y_surprised.append( list(array(im_surprised).flatten()) )

pd.DataFrame(y_centerlight).to_csv('_centerlight') #--image data
pd.DataFrame(y_glasses).to_csv('_glasses') #--image data
pd.DataFrame(y_leftlight).to_csv('_leftlight') #--image data
pd.DataFrame(y_noglasses).to_csv('_noglasses') #--image data
pd.DataFrame(y_normal).to_csv('_normal') #--image data
pd.DataFrame(y_rightlight).to_csv('_rightlight') #--image data
pd.DataFrame(y_sleepy).to_csv('_sleepy') #--image data
pd.DataFrame(y_surprised).to_csv('_surprised') #--image data