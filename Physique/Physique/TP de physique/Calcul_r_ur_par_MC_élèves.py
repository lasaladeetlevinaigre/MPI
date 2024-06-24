# -*- coding: utf-8 -*-

import numpy as np
import numpy.random as rd
from matplotlib import pyplot as plt


###### Données numériques ######
N=          # à compléter
R=          # à compléter
uR=         # à compléter
C=          # à compléter
uC=         # à compléter
f0=         # à compléter
deltaf0=    # à compléter
fc1=        # à compléter
deltafc1=   # à compléter
fc2=        # à compléter
deltafc2=   # à compléter

pi=np.pi



###### Construction des tableaux de tirages de f0, C, fc1, fc2, et R ######
##########   (choix de distributions uniformes)   ##########
tabf0=rd.uniform(f0-deltaf0,f0+deltaf0,N)
tabC=rd.normal(C,uC,N)
tabfc1=rd.uniform(fc1-deltafc1,fc1+deltafc1,N)
tabfc2=rd.uniform(fc2-deltafc2,fc2+deltafc2,N)
tabR=rd.normal(R,uR,N)

###### Construction du tableau de L ######
tabr=  # à compléter


###### Estimateurs de Lmoy et de son incertitude uL ######
rmoy=np.mean(tabr)
ur=np.std(tabr,ddof=1)

plt.hist(tabr,bins=100,color="blue",fill=False)
plt.xlabel(r"Valeurs de $r$ obtenues",color="blue")
plt.ylabel("Fréquence des tirages $r$",color="blue")
plt.title(u"Résultats: r = {0:.2f} $\Omega$, u(r) = {1:.2f} $\Omega$".format(rmoy,ur),color="red",fontsize=16)
plt.show()


print(r"La moyenne empirique de r est", rmoy,r"$\Omega$")
print(r"L'incertitude-type sur r est", ur,r"$\Omega$")