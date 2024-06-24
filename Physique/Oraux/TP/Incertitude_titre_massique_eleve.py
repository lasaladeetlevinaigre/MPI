import numpy as np
import numpy.random as rd
from matplotlib import pyplot as plt


###### Données numériques ######
Mac=98
Cb=5e-2
uCb=0.5e-4
Ve= #### à compléter 
DeltaVe= #### à compléter
msol= #### à compléter
umsol= #### à compléter
N=10000

###### Construction des tableaux de tirages ######
tabCb=rd.normal(Cb,uCb,N)
tabVe=rd.uniform(Ve-DeltaVe,Ve+DeltaVe,N)
tabmsol=rd.normal(msol,umsol,N)

###### Construction du tableau de valeurs de C%acide ######
tabCpourcentacide= #### à compléter

###### Estimateurs de n et de l'incertitude u(n)######
Cpourcentacide_emp=np.mean(tabCpourcentacide)
u_Cpourcentacide=np.std(tabCpourcentacide,ddof=1)
print(r"Estimateur concentration en acide: C\%(acide)=",Cpourcentacide_emp)
print(r"L'incertitude-type concentration en acide u(C\%(acide))=",u_Cpourcentacide)

##### Tracé de l'histogramme de la distribution Monte Carlo #####
plt.hist(tabCpourcentacide,bins=100,color="orange")
plt.plot([Cpourcentacide_emp,Cpourcentacide_emp],[0,180],c="red")
plt.text(Cpourcentacide_emp,25,r"$C\%_{emp}$",c='blue')
plt.xlabel(r'Concentration en acide ($C_{\%\ acide}$)')
plt.ylabel(r'Fréquence')
plt.show()

plt.savefig("Histogramme_C_massique.eps", orientation='landscape') 