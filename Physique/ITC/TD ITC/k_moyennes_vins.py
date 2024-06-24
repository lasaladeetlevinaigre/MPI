import math
import copy
import random as rd
import numpy as np
from sklearn import datasets
vin=datasets.load_wine()


def transpose(X):
    N,n=X.shape
    trX=np.empty((n,N))
    for i in range(N):
        for j in range(n):
            trX[j,i]=X[i,j]
    return trX
      

def aleat(X):
    N=X.shape[0]
    d={}#clé: indice i dans le tableau final de 178 elts, valeur: indice élément de X1
    X1=transpose(X)
    Xf=np.empty((13,N), dtype=float)
    i=0
    while i<N:
        numelt=rd.randint(0,N-1)
        if numelt not in d.values():
            d[i]=numelt
            Xf[:,i]=X1[:,numelt]
            i+=1
    return (Xf,d)



def barycentre(X):
	n,N=X.shape
	G=np.zeros((n,1),dtype=float)
	for i in range(n):
		G[i,0]=np.sum(X[i,:])/N
	return G



def dist(Xj,Cp):
    n=Cp.shape[0]
    dst2=0.0
    for i in range(n):
        dst2=dst2+(Xj[i]-Cp[i])**2
    return np.sqrt(dst2)



def k_moyennes(Xf,k):
    n,N=Xf.shape
    ##### Initialisation #####
    P=[[] for i in range(k)]
    C=[] #initialisation liste vide des barycentres de classes
    assert N>=k # vérification que le nombre de classes est inférieur au nombre d'éléments du dataset
    indC=rd.sample(range(0,N),k) #indice des k éléments choisis au hasard comme barycentres
    for p in indC:
        C.append(Xf[:,p])
	
    ##### Début de la procédure itérative #####
    modifclasses=True # on initialise un booleen à True pour vérifier la stabilité des classes
    i=0
    while modifclasses:
        i+=1
        print(i)
        Pancien=P
        P=[[] for i in range(k)] #réinitialisation des classes
        for j in range(N):
            dmin=math.inf #on initialise la distance minimale (infinie)			
            for p in range(k): # p indice d'itération sur les k classes
                dp=dist(Xf[:,j],C[p]) #calcul de la distance avec le barycentre de la classe p
                if dp<dmin:
                    dmin=dp
                    pmin=p
            P[pmin].append(j)
        #### Recalcul des k barycentres ####
        C=[] #réinitialisation de la liste des barycentres
        for p in range(k):
            Xp=Xf[:,P[p]] # on forme un tableau de données Xp constituée uniquement de la classe p
            Gp=barycentre(Xp)
            C.append(Gp)
        if Pancien==P:
            modifclasses=False
    return (P,C) 


def Etiqu_maj(L,d,classes):
    freq=[0,0,0]
    for i in L:
        freq[classes[d[i]]]+=1
    if freq[0]>freq[1] and freq[0]>freq[2]:
        return 0,freq
    elif freq[1]>freq[0] and freq[1]>freq[2]:
        return 1,freq
    else:
        return 2,freq

X=vin['data']
Xf,d=aleat(X)
print(d)        
classif=k_moyennes(Xf,3)

print(Etiqu_maj(classif[0][0],d,vin['target']))
print(Etiqu_maj(classif[0][1],d,vin['target']))
print(Etiqu_maj(classif[0][2],d,vin['target']))





 
