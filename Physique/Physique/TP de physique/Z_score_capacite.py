# -*- coding: utf-8 -*-
"""
Created on Tue Oct 10 23:30:30 2023

@author: JL GRAYE
"""

import numpy as np
from matplotlib import pyplot as plt
n_mes=np.arange(1,11,1)

Cref= #### à compléter
tabR=np.array(.....) #### à compléter 
tabf0=np.array(.....) #### à compléter
N=len(R)

tabC= #### à compléter
Cemp=np.mean(tabC)
uC=round(np.std(tabC,ddof=1),2) # on arrondit uC à 2 décimales
Z=(tabC-Cref)/uc

##### Représentation graphique du z-score #####
plt.plot(n_mes,Z,c="red",marker='+',linestyle=' ')
plt.plot([0, 10], [0, 0], c='orange', linestyle = '--')
plt.title(u"z-score des valeurs de C - valeur expérimentale: C = {0:.2f} nF, u(C) = {1:.2f} nF".format(Cemp*1e9,uC*1e9))
plt.xlabel("mesure n")
plt.ylabel("z-score")
plt.errorbar(n_mes,Z,2,linestyle=' ')
plt.show()