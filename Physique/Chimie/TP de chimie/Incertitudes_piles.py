# -*- coding: utf-8 -*-
"""
Created on Wed Mar 22 19:17:49 2023

@author: JL GRAYE
"""


import numpy as np
from numpy import random as rd

lnQ=np.log10(np.array([..........])) # à compléter (tableau des valeurs de log(Q))
DeltaE=np.array([.........]) #à compléter (tableau des valeurs de DeltaE)
uDeltaE=np.array([...........])  #à compléter (tableau des valeurs des incertitudes sur DeltaE)
N=len(DeltaE)

nbsim=int(1e5)
tab_a=np.zeros((nbsim),dtype=float)
tab_b=np.zeros((nbsim),dtype=float)

######### Lancement des nbsim simulations ##########
for i in range(nbsim):
	mDeltaE=rd.normal(DeltaE,uDeltaE)
	a,b=np.polyfit(lnQ,mDeltaE,1)
	tab_a[i],tab_b[i]=a,b

a_sim,b_sim=np.mean(tab_a),np.mean(tab_b)
u_a,u_b=np.std(tab_a,ddof=1),np.std(tab_b,ddof=1)
print("Valeur de la pente simulée: a_sim={0} V".format(a_sim))
print(r"L'incertitude type sur la pente vaut: u(a)={0} V".format(u_a))
print("Valeur de l'ordonnée à l'origine simulée: b_sim={0} V ".format(b_sim))
print(r"L'incertitude type sur l'ordonnée à l'origine vaut: u(b)={0} V".format(u_b))
