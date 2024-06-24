# -*- coding: utf-8 -*-
"""
Created on Tue Jan 20 07:02:51 2015

@author: WK

Programme :
    - Lisant un fichier de données csv 
    - Traçant le diagramme de rayonnement en coordonnées polaires
"""
from __future__ import division 

import csv
import numpy as np
import matplotlib.pyplot as plt

###############################################################################
######################   Importation des données  #############################
###############################################################################

alpha = np.array([])   # tableau des angles
U = np.array([])        # tableau des valeurs mesurées


f = open("Donnees.csv","r")
dialect = csv.Sniffer().sniff(f.readline())   # lecture des paramètres d'enregistrement
f.readline()         # On saute la ligne d'en tête
donnees = csv.reader(f, dialect)  # mise en forme automatique
for row in donnees:
    alpha = np.append(alpha,[float(row[0].replace(',','.'))]) # modification du délimiteur numérique
    U = np.append(U,[float(row[1].replace(',','.'))])
f.close()

theta=(180 - alpha)/2  #calcul pour obtenir l'angle entre l'émetteur et le récepteur

#print ("theta=", alpha)
#print ("U=", U)


###############################################################################
##########  Tracé du  diagramme polaire  #########################
###############################################################################

plt.polar(theta*np.pi/180, U/max(U), 'o', color='g',label=u"points expérimentaux")

plt.ylim(0, 1.1) 

plt.polar([0,0], [-2,2], "r") #axe de l'émetteur

plt.thetagrids([ i for i in range(0,190,10)]) #grille pour la lecture

plt.title(u"Diagramme de rayonnement du cornet de l'émetteur", fontsize=16) #titre du graphique

plt.legend()
plt.show()