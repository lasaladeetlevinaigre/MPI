# -*- coding: utf-8 -*-
"""
Created on Tue Jan 23 22:57:14 2024

@author: JL GRAYE
"""

import numpy as np
import numpy.random as rd

###### Données numériques ######
N=int(1e5)
xA=----- #### A COMPLETER
xO=----- #### A COMPLETER
deltaxA=----- #### A COMPLETER
deltaxO=----- #### A COMPLETER

###### Construction des tableaux de tirages ######
tabxA=rd.uniform(xA-deltaxA,xA+deltaxA,N)
tabxO=rd.uniform(xO-deltaxO,xO+deltaxO,N)

###### Construction du tableau de OA ######
OA=tabxA-tabxO

###### Estimateurs de OA et de l'incertitude ######
OAemp=np.mean(OA)
uOA=np.std(OA,ddof=1)


print("La moyenne empirique est: $OAemp=${0} {1}".format(OAemp,"cm"))
print("L'incertitude-type est: $u(OA)=${0} {1}".format(uOA,"cm"))
