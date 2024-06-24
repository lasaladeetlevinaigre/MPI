 # -*- coding: utf-8 -*-

import numpy as np
import matplotlib.pyplot as plt
import scipy.fftpack as fftp
from math import pi

###############################################################################
######################     Lecture du fichier.txt    ##########################
###############################################################################
fichier = open('FiltrageNumeriquePointsEntree.txt','r')
lu = fichier.readlines() #liste des chaines de caractère de chaque liqne
fichier.close()

# traitement des chaines de caractère
tableau=[]
for line in lu[1:]:
    tL,eL=line.rstrip('\n').split(";")
    tableau.append([float(tL),float(eL)])
    #tableau.append([map(float,line.rstrip('\n').split(';'))])

tableau = np.array(tableau)

T,E = tableau[:,0], tableau[:,1] #array des instants et des valeurs d'entrée
n = len(T)

###############################################################################
############################    Affichage    ##################################
###############################################################################
def affiche(T,E,S,titre):
    '''affiche les signaux temporels d'entree et de sortie avec
    T liste des instants :array, E entrée : array, S sortie :array, titre :str'''
    plt.figure()
    plt.plot(T,E,label=u'entrée')
    plt.plot(T,S,'b--',label=u'sortie')
    plt.legend()
    plt.title(titre)
    plt.show()

###############################################################################
################  Filtrage numérique temporel des données  ####################
###############################################################################

###Filtrage passe bas
te = T[1]  #pas temporel d'echantillonnage
f01 = 1000 #fréquence de coupure choisie
a = 2*pi*f01*te/2 #paramètre alpha théorique
s1 = 0  # valeur initiale du signal de sortie
S1 = [] # tableau des valeurs de sortie
S1.append(s1)
for i in range(n-1):
    s1=((1-a)/(1+a))*S1[i]+(a/(1+a))*(E[i+1]+E[i])
    S1.append(s1)


S1 = np.array(S1)
affiche(T,E,S1,'Temporel : passe bas f01 = {:d}'.format(f01))


#Filtrage passe Haut
te = T[1]  #pas temporel d'echantillonnage
f02=1000 #fréquence de coupure choisie
a = 2*pi*f02*te/2 #paramètre alpha théorique
s2 = 0
S2 = []
S2.append(s2)
for i in range(n-1):
    s2=((1-a)/(1+a))*S2[i]+(1/(1+a))*(E[i+1]-E[i])
    S2.append(s2)


S2 = np.array(S2)
affiche(T,E,S2,'Temporel : passe haut f02 = {:d}'.format(f02))

###############################################################################
#################   Filtrage numérique spectral des données   #################
###############################################################################

F = fftp.fftfreq(n,te)                 # Liste des fréquences d'échantillonnage
TFE = fftp.fft(E)                   # transformée de Fourier du signal d'entrée

f01=1000
def H1(f):       # passe bas ordre 1
    return 1/(1+1j*(f/f01))

f02=1000

def H2(f):      # passe haut ordre 1
    return 1j*(f/f01)/(1+1j*(f/f01))

TFS1 = H1(F)*TFE
S1 = fftp.ifft(TFS1).real
TFS2 = H2(F)*TFE
S2 = fftp.ifft(TFS2).real

affiche(T,E,S1,u'Fréquentiel : passe bas f01 = {:d}'.format(f01))
affiche(T,E,S2,u'Fréquentiel : passe haut f02 = {:d}'.format(f02))

###############################################################################
########################      Fichier de sortie     ###########################
###############################################################################
def ecrit(T,S,nom):
    '''crée le fichier de sortie nom.txt contenant la liste des instants et la
    liste des tensionsde sortie'''
    fichier = open(nom+'.txt','w')
    fichier.write('T;S\n')
    for k in range(n):
        m = str(T[k])+';'+str(S[k])+'\n'
        fichier.write(m)
    fichier.close()

ecrit(T,S1,'FiltrageNumeriquePointsSortie')