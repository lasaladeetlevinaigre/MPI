import numpy as np
import matplotlib.pyplot as plt
import numpy.random as rd
from math import *

tab_alpha= ## à compléter (tableau numpy) avec les valeurs mesurées de l'angle limite de glissement


alpha_emp=np.mean(tab_alpha)
u_alpha=np.std(tab_alpha)

plt.figure()
plt.hist(tab_alpha,bins=10)
plt.title(r"$\alpha_{emp}=$ {0:.2f}{2}, et $u(\alpha)=$ {1:.2f}{2}".format(alpha_emp,u_alpha,"°"))
plt.xlabel(r"Angle limite $\alpha$"))
plt.ylabel('fréquence des tirages')
plt.show()

#############################################################################
############# Evaluation du coefficient de frottement statique ##############
#############################################################################
tab_mus=np.tan(tab_alpha*pi/180) #calcul du tableau des valeurs de mus

mus_emp=np.mean(tab_mus)
u_mus=np.std(tab_mus,ddof=1)


plt.figure()
plt.hist(tab_mus,bins=100)
plt.title(r"$\mu_{s_{emp}}=$ {0:.2f}, et $u(\mu_s)=$ {1:.2f}".format(mus_emp,u_mus))
plt.xlabel(r'Coefficient de frottement statique $\mu_s$')
plt.ylabel('Fréquence des tirages')
plt.show()