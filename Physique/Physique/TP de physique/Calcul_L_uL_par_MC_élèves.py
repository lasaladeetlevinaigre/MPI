# -*- coding: utf-8 -*-

import numpy as np
import numpy.random as rd
from matplotlib import pyplot as plt

###### Données numériques ######
N=        # à compléter
C=        # à compléter
uC=       # à compléter
f0=       # à compléter
deltaf0=  # à compléter
uf0=      # à compléter
pi=np.pi

###### Construction des tableaux de tirages de f0 et C ######
##########   (choix de distributions uniformes)   ##########
tabf0=rd.uniform(f0-deltaf0,f0+deltaf0,N)
tabC=rd.normal(C,uC,N)

###### Calcul du tableau de L ######
tabL=1/(4*pi**2*tabC*tabf0**2)

###### Estimateurs de Lmoy et de son incertitude uL ######
Lmoy=np.mean(tabL)
uL=np.std(tabL,ddof=1)

############ Tracé de l'histogramme de la distribution ##########
plt.hist(tabL,bins=100,color="blue",fill=False)
plt.xlabel(r"Valeurs de $L$ obtenues")
plt.ylabel("Fréquence des tirages $L$",color="blue")
plt.title(u'Résultats: L = {0:.2f} mH, u(L) = {1:.2f} mH'.format(Lmoy*1e3,uL*1e3),color="red",fontsize=16)
plt.show()