import numpy as np
import numpy.random as rd

###### Données numériques ######
Ca=.... # A COMPLETER Concentration de l'acide titrant (après dilution) 
Vb=.... # A COMPLETER prise d'essai pour le titrage
Ve=.... # A COMPLETER Volume d'acide versé à l'équivalence
DeltaVe=....  # A COMPLETER Demi-intervalle des graduations de la burette
uVb=.... # A COMPLETER Incertitude sur le volume de la prise d'essai (fiole jaugée), à ajuster en fonction de la verrerie employée

 
N=10000 #nombre d'échantillons de la simulation MC

###### Construction des tableaux de tirages des grandeurs possédant une incertitude ######
#tabVe=np.random.normal(Ve, uVe, N)
tabVe=np.random.uniform(Ve-DeltaVe,Ve+DeltaVe, N) #lecture du volume sur la burette
tabVb=np.random.normal(Vb, uVb, N)

###### Construction du tableau de valeurs de Cb ######
tabCb=..... # A COMPLETER

###### Estimateurs de Cb et de son incertitude u(Cb)######
Cb_emp=np.mean(tabCb)
u_Cb=np.std(tabCb,ddof=1)
print( u"La moyenne empirique de la concentration en bicarbonate est {0:.6f} mol/L".format(Cb_emp))
print(u"L'incertitude-type sur la concentration en bicarbonate est {0:.6f} mol/L".format(u_Cb))
