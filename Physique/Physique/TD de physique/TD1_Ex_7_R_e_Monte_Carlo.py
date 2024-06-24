import numpy as np
import numpy.random as rd
from matplotlib import pyplot as plt
###### Données numériques ######
N=int(1000)
E=1.00
alpha=0.01
n=100


###### Construction des tableaux de tirages ######
X=np.exp(np.linspace(np.log(0.1),np.log(10),n))

Tabecarttype=[]

for x in X:
   tabRe=x / ( (1+rd.normal(0,alpha,1000))/(1/(1+x)+rd.normal(0,alpha,1000))-1) 
   Tabecarttype.append(np.std(tabRe,ddof=1))


###### Tracé ######

plt.figure("Tracé de la résistance d'entrée")
plt.plot(X,Tabecarttype,'o',color="red")
plt.xscale('log')
#plt.xticks([0.1,10], fontweight='bold',fontsize=13)
#plt.yticks( [0.04,0.18],fontweight='bold',fontsize=18)
plt.show()
plt.xlabel("x",color="black",fontsize=14)
plt.ylabel(r"$u\left(\dfrac{R_{e_{exp}}}{R_e}\right)$",color="black",fontsize=14,rotation=0,loc="top")