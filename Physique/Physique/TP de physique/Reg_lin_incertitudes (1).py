
import numpy as np
from numpy import random as rd

x=np.arange(0,....,....) #à compléter
f=np.array([.........]) #à compléter
uf=np.array([...........])  #à compléter
N=len(f)

nbsim=int(1e5)
tab_a=np.zeros((nbsim),dtype=float)
tab_b=np.zeros((nbsim),dtype=float)

######### Lancement des nbsim simulations ##########
for i in range(nbsim):
	mf=rd.normal(f,uf)
	a,b=np.polyfit(x,mf,1)
	tab_a[i],tab_b[i]=a,b

a_sim,b_sim=np.mean(tab_a),np.mean(tab_b)
u_a,u_b=np.std(tab_a,ddof=1),np.std(tab_b,ddof=1)
print("Valeur de la pente simulée: a_sim={0} F".format(a_sim))
print(r"L'incertitude type sur la pente vaut: u(a)={0} F".format(u_a))
print("Valeur de l'ordonnée à l'origine simulée: b_sim={0} s".format(b_sim))
print(r"L'incertitude type sur l'ordonnée à l'origine vaut: u(b)={0} s".format(u_b))