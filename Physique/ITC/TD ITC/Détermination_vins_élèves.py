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

type(vin)



#### Choix du nombre de plus proches voisins ####
k=15


##### Construction de la matrice X et du dictionnaire DictX du set de données à partir de nbdonnées données du set complet (qui en comporte 70000 donc trop gros!)
N,n=vin['data'].shape
X=vin['data']
DictX={}
for i in range(N):
    DictX[i]=vin['target'][i]

##### Construction de la matrice A et du dictionnaire DictA du set d'apprentissage à partir du set de données
NA=3*N//4
SplA=rd.sample([i for i in range(N)],NA)
A=np.zeros((NA,13), dtype=float)
DictA={}
for i,j in enumerate(SplA):
    A[i]=X[j]
    DictA[i]=DictX[j]




######################################################
######## Algorithme des k plus proches voisins #######
###################################################### 

### Fonctions nécessaires au tri ###

def tri(L):
    if len(L)<=1:
        .....
    else:
        m=.....
        return fct(tri(L[:m]),tri(L[m:]))
        
def fct(L1,L2):
    if L1==[]:
        .....
    if L2==[]:
        .....
    if L1[0][1]<L2[0][1]:
        return [L1[0]]+fct(L1[1:],L2)
    else:
        return .....



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
    return Etiqu_maj(tri(L)[:k],1e-4)





######### Fonction de recherche de l'étiquette majoritaire ########## 
def Etiqu_maj(L,epsilon):
    if L[0][1]<=epsilon:
        return L[0][0]
    dicfreq={}
    freqmax=0
    for el in L: 
        if el[0] in dicfreq: 
            .............
        else:
            ............. 
        if dicfreq[el[0]]>=freqmax: 
            clefreqmax=el[0] #on la valide comme la plus fréquente le cas échéant
            freqmax=dicfreq[el[0]]
    return clefreqmax #on renvoie enfin la clé de plus forte fréquence


i=158
print(u"Essai pour le vin n°",i)
print(vin['target'][i]) # affiche l'étiquette de la donnée n°i
print(kppv(A,DictA,k,X[i])) # lance l'algorithme avec le candidat x  




##### Construction de la matrice de confusion #####
def MatConf(X,DictX,SplA,A,DictA,k):
    N,n=X.shape
    nb_etiqu=len(vin['target_names'])
    M=np.zeros((....,....),dtype=int)
    compt=0
    liste_i=[]
    while compt<N//4: # on realise les tests sur 25% du nb total de données
        i=rd.randint(0,N-1)
        if i not in .... and i not in ....: # si le tiré pas dans le set d'apprentissage et non déjà tiré
            x=X[i,:] #alors on extrait ce nouveau candidat
            etiqu_vraie=.... # son étiquette vraie
            etiqu_estim= .... # son etiquette estimée
            M[int(etiqu_vraie),int(etiqu_estim)]+=1 #incrémentation d'une unité de la valeur de M[i,j]
            ....
            ....
    return M

print('matrice de confusion:\n',MatConf(X,DictX,SplA,A,DictA,k))
"""