# -*- coding: utf-8 -*-
"""
Created on Sun Mar 26 17:02:18 2023

@author: WenKroy2
"""

import numpy as np
from numpy import random as rd
from matplotlib import pyplot as plt

T=np.array([298,300,303,306,308,312]) 
e=np.array([1.420, 1.418, 1.414, 1.411, 1.409, 1.405])
ue=np.array([5e-4]*6)
N=len(e)

nbsim=int(1e5)
tab_a=np.zeros((nbsim),dtype=float)
tab_b=np.zeros((nbsim),dtype=float)

######### Lancement des nbsim simulations ##########
for i in range(nbsim):
	me=rd.normal(e,ue)
	a,b=np.polyfit(T,me,1)
	tab_a[i],tab_b[i]=a,b

a_sim,b_sim=np.mean(tab_a),np.mean(tab_b)
u_a,u_b=np.std(tab_a,ddof=1),np.std(tab_b,ddof=1)
print("Valeur de la pente simulée: a_sim={0} V/K".format(a_sim))
print(r"L'incertitude type sur la pente vaut: u(a)={0} V/K".format(u_a))
print("Valeur de l'ordonnée à l'origine simulée: b_sim={0} V ".format(b_sim))
print(r"L'incertitude type sur l'ordonnée à l'origine vaut: u(b)={0} V".format(u_b))
ereg=a_sim*T+b_sim
plt.plot(T,ereg)
plt.scatter(T,e)
plt.title("$e(V)=f(T(K))$", color="Purple",fontsize=18)
plt.xlabel("T(K)",fontsize=14)
plt.ylabel("$e(V)$",fontsize=14)
plt.text(302,1.408, r"Pente $a$=({0}$\pm${1}) V/K".format(round(a_sim,5),round(u_a,5)), horizontalalignment = 'center', verticalalignment = 'center', color="r", fontsize=13)
plt.text(302,1.405, r"Ordonnée à l'origine $b$=({0}$\pm${1}) V".format(round(b_sim,3),round(u_b,3)), horizontalalignment = 'center', verticalalignment = 'center', color="r", fontsize=13)
plt.ylim(1.4,1.425)
plt.xlim(295)
plt.show()