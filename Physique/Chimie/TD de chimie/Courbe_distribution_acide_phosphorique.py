# -*- coding: utf-8 -*-
"""
Created on Fri Feb 10 22:14:45 2023

@author: WenKroy2
"""

from matplotlib import pyplot as plt
import numpy as np
K1,K2,K3=10**(-2.2),10**(-7.2),10**(-12.3)
C=0.1
pH=np.linspace(0,14,300)
h=10**(-pH)

C1=C/(1+K1/h+K1*K2/h**2+K1*K2*K3/h**3)
C2=C/(1+h/K1+K2/h+K2*K3/h**2)
C3=C/(h**2/(K1*K2)+h/K2+1+K3/h)

"""
C1=C/(1+10**(-pK1+pH)+10**(-pK1-pK2+2*pH)+10^(-pK1-pK2+3*pH))
C2=C/(1+10**(-pH+pK1)+10**(-pK2+pH)+10**(-pK2-pK3+2*pH))
C3=C/(10**(+pK1+pK2-2*pH)+10**(-pH+pK2)+1+10**(-pK3+pH))
"""
C4=C-(C1+C2+C3)

plt.plot(pH,100*C1/C,'r', label=r"% acide $H_3PO_4$") 
plt.plot(pH,100*C2/C,'b' , label="% base $H_2PO_4^-$") 
plt.plot(pH,100*C3/C,'y' , label="% base $HPO_4^{2-}$") 
plt.plot(pH,100*C4/C,'g' , label="% base $PO_4^{3-}$") 
plt.xlabel("pH",fontsize=18)
plt.ylabel("% des 4 espèces",fontsize=18)
plt.ylim(0)
plt.xlim(0)
#plt.plot([4.75, 4.75], [-10, 50], color="green", linestyle="--")
#plt.text(4.75, -4, r"$pK_a$", horizontalalignment = 'center', verticalalignment = 'center', color="green", fontsize=13)

plt.text(1.5, 95, r"$1$", horizontalalignment = 'center', verticalalignment = 'center', color="r", fontsize=13)
plt.text(6.5, 95, r"$2$", horizontalalignment = 'center', verticalalignment = 'center', color="b", fontsize=13)
plt.text(11.6, 95, r"$3$", horizontalalignment = 'center', verticalalignment = 'center', color="y", fontsize=13)
plt.text(14.2, 95, r"$4$", horizontalalignment = 'center', verticalalignment = 'center', color="g", fontsize=13)
plt.legend(loc="center right")
plt.title("Diagramme de distribution des espèces", color="Purple",fontsize=24)
plt.show()