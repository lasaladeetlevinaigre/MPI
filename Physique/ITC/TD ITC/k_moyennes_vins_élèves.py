import math
import random as rd
import numpy as np
from sklearn import datasets
vin=datasets.load_wine()


def transpose(X):
    # fonction à compléter

      

def aleat(X):
    # fonction à compléter



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
    while modifclasses:
        Pancien=P
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
    # fonction à compléter






X=vin['data']
Xf,d=aleat(X)
print(d)        
classif=k_moyennes(Xf,3)

print(Etiqu_maj(classif[0][0],d,vin['target']))
print(Etiqu_maj(classif[0][1],d,vin['target']))
print(Etiqu_maj(classif[0][2],d,vin['target']))




 
