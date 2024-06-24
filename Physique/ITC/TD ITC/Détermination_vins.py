# -*- coding: utf-8 -*-
"""
Created on Sat Jan 14 23:35:39 2023

@author: JL GRAYE
"""


#######################################
###### Description des notations ######
#######################################

#X matrice des données
#DictX dictionnaire des étiquettes
#SplA échantillon de valeurs de X prises au hasard pour confectionner le set d'apprentissage
#A matrice d'apprentissage 
#DictA dictionnaire des étiquettes de l'ensemble d'apprentissage


import numpy as np
import random as rd
from sklearn import datasets
vin=datasets.load_wine()

#print(vin['data'])
#print(type(vin['data']))
#print(vin['target_names'])
print(vin['target'])
print(vin.keys())

#print(vin['data'].shape)
#print((vin['DESCR']))
#print(type(vin['data']))
print(((vin['feature_names'])))
print(vin['data'].shape)





##### Construction de la matrice X et du dictionnaire DictX du set de données à partir de nbdonnées données du set complet 
N,n=vin['data'].shape
print('taille du set N=',N)
print(type(vin['data']))
X=vin['data']


DictX={}
for i in range(N):
    DictX[i]=vin['target'][i]




##### Construction de la matrice A et du dictionnaire DictA du set d'apprentissage à partir du set de données
NA=3*N//4
SplA=rd.sample([i for i in range(N)],NA) # Constitution du set d'apprentissage (valeur prises au hasard)
A=np.zeros((NA,13), dtype=float)
DictA={}
for i,j in enumerate(SplA):
    A[i]=X[j]
    DictA[i]=DictX[j]



######################################################
######## Algorithme des k plus proches voisins #######
###################################################### 


#### Choix du nombre de plus proches voisins ####

k=50

### Fonctions nécessaires au tri ###

def tri(L):
    if len(L)<=1:
        return L
    else:
        m=len(L)//2
        return fct(tri(L[:m]),tri(L[m:]))
        
def fct(L1,L2):
    if L1==[]:
        return L2
    if L2==[]:
        return L1
    if L1[0][1]<L2[0][1]:
        return [L1[0]]+fct(L1[1:],L2)
    else:
        return [L2[0]]+fct(L1,L2[1:])



### Fonction de calcul de la liste des k plus proches voisins
def kppv(A,DictA,k,x):
    L=[]
    N,n=A.shape
    for i in range(N):
        dist2=0.0
        for j in range(n):
            dist2=dist2+(A[i,j]-x[j])**2
        dist=np.sqrt(dist2)
        L.append((DictA[i],dist))
    return Etiqu_maj(tri(L)[:k],10**(-4))





######### Fonction de recherche de l'étiquette majoritaire ########## 
def Etiqu_maj(L,epsilon):
    if L[0][1]<=epsilon:
        return L[0][0]
    dicfreq={}
    freqmax=0
    for el in L: #itération sur la liste
        if el[0] in dicfreq: #on vérifie la présence de la clé dans le dictionnaire
            dicfreq[el[0]]=dicfreq[el[0]]+1 # incrémente de 1 fréquence si clé déjà présente
        else:
            dicfreq[el[0]]=1 #sinon on inscrit cette nouvelle clé avec 1 comme valeur (fréquence)
        if dicfreq[el[0]]>=freqmax: #si cette dernière clé inscrite est la plus fréquente
            clefreqmax=el[0] #on la valide comme la plus fréquente le cas échéant
            freqmax=dicfreq[el[0]]
    return clefreqmax #on renvoie enfin la clé de plus forte fréquence


#i=110
#print(u"Essai pour le vin n°",i)
#print("étiquette vraie:",vin['target'][i]) # affiche l'étiquette de la donnée n°i
#print("étiquette estimée:",kppv(A,DictA,k,X[i])) # lance l'algorithme avec le candidat x  


##### Construction de la matrice de confusion #####
def MatConf(X,DictX,SplA,A,DictA,k):
    N,n=X.shape
    nb_etiqu=len(vin['target_names'])
    M=np.zeros((nb_etiqu,nb_etiqu),dtype=int)
    compt=0
    liste_i=[]
    while compt<N//4: # on realise les tests sur 25% du nb total de données
        i=rd.randint(0,N-1)
        if i not in SplA and i not in liste_i: # si le tiré pas dans le set d'apprentissage et non déjà tiré
            x=X[i,:] #alors on extrait ce nouveau candidat
            etiqu_vraie=DictX[i] # son étiquette vraie
            etiqu_estim= kppv(A,DictA,k,x) # son etiquette estimée
            M[int(etiqu_vraie),int(etiqu_estim)]+=1 #incrémentation d'une unité de la valeur de M[i,j]
            liste_i.append(i)
            compt+=1
    return M

#print("Matrice de confusion:\n",MatConf(X,DictX,SplA,A,DictA,k))

def perf(X,DictX,SplA,A,DictA,k):
    M=MatConf(X,DictX,SplA,A,DictA,k)
    print(M)
    n=M.shape[0]
    somvrai,somtot=0.,0.
    for i in range(n):
        for j in range(n):
            if i==j:
                somvrai+=M[i,j]
            somtot+=M[i,j]
    #print(somvrai,somtot)
    return somvrai/somtot*100


print('matrice de confusion et pourcentrage de bonnes prédictions:\n',perf(X,DictX,SplA,A,DictA,k),'%')
"""