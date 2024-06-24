# -*- coding: utf-8 -*-
"""
Created on Wed Jan 18 17:26:49 2023

@author: JL GRAYE
"""

import numpy as np
from numpy import random as rd

t=np.arange(0,....,....) #à compléter
f=np.array([.........]) #à compléter
lnf=np.log(f)
uf=np.array([...........])  #à compléter
ulnf=(1/f)*uf # tableau des incertitudes sur ln(f)
N=len(f)

nbsim=int(1e5)
tab_a=np.zeros((nbsim),dtype=float)
tab_b=np.zeros((nbsim),dtype=float)

######### Lancement des nbsim simulations ##########
for i in range(nbsim):
	mlnf=rd.normal(lnf,ulnf) # grace aux tableaux numpy on traite les N tirages de la simulation i en une seule commande
	a,b=np.polyfit(t,mlnf,1) # on réalise la régression linéaire sur les données de la simulation i en cours
	tab_a[i],tab_b[i]=a,b #on stocke les valeurs de a et b pour la simulation i en cours

a_sim,b_sim=np.mean(tab_a),np.mean(tab_b)
u_a,u_b=np.std(tab_a,ddof=1),np.std(tab_b,ddof=1) # on calcule l'incertitude type sur a et b
print("Valeur de la pente simulée: a_sim={0} F".format(a_sim))
print(r"L'incertitude type sur la pente vaut: u(a)={0} F".format(u_a))
print("Valeur de l'ordonnée à l'origine simulée: b_sim={0} s".format(b_sim))
print(r"L'incertitude type sur l'ordonnée à l'origine vaut: u(b)={0} s".format(u_b))