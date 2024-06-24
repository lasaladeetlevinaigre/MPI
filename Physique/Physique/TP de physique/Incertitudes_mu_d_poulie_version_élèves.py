import numpy as np
import matplotlib.pyplot as plt
import numpy.random as rd
from math import pi

############################################################################
############# Données numériques et calcul des tableaux de tirages #########
############################################################################
N=int(1E5)
m=  # à compléter
u_m= # à compléter

T2=m*9.81
u_T2=u_m*9.81
tabT2=rd.normal(T2,u_T2,N)

pi=np.pi
alpha=pi/2
delta_alpha=2.0*pi/180 #demi-largeur de distribution estimée à 2 degrés)
tabalpha=rd.uniform(alpha-delta_alpha,alpha+delta_alpha,N)


T1= # à compléter
delta_T1= # à compléter
tabT1=rd.uniform(T1-delta_T1,T1+delta_T1,N)


###############################################################################################
############# Estimation du coefficient de frottement dynamique et son incertitude#############
###############################################################################################

tab_mud=(1/tabalpha)*np.log(tabT1/tabT2)

mud_emp=np.mean(tab_mud)
u_mud=np.std(tab_mud,ddof=1)


plt.figure()
plt.hist(tab_mud,bins=100)
plt.title(r"$\mu_{d_{emp}}=$ {0:.2f}, et $u(\mu_d)=$ {1:.2f}".format(mud_emp,u_mud))
plt.xlabel(r'Coefficient de frottement dynamique $\mu_d$')
plt.ylabel('Distribution des tirages')
plt.show()



