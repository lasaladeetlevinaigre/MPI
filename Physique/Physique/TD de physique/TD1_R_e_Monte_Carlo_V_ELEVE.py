import numpy as np
import numpy.random as rd
from matplotlib import pyplot as plt
###### Données numériques ######
N=int(1000)
E=1.00
alpha=0.01
n=100

###### Construction des tableaux de tirages ######
X=np.exp(np.linspace(np.log(0.1),np.log(10),n)) # on constitue le tableau
Tabecarttype=[]
for x in ####A COMPLETER####: # on itère sur toutes les valeurs de x
   tabRe= ####A COMPLETER#### # on réalise le tirage de 1000 valeurs pour x fixé
   Tabecarttype.append(####A COMPLETER####) # on constitue le tableau des écarts-type

###### Tracé ######
plt.figure("Tracé de la résistance d'entrée")
plt.plot(X,Tabecarttype,'o',color="orange")
plt.xscale('log')
plt.show()
plt.xlabel("x",color="r",fontsize=14)
plt.ylabel(r"$u\left(\dfrac{R_{e_{exp}}}{R_e}\right)$",color="r",fontsize=14,rotation=45)