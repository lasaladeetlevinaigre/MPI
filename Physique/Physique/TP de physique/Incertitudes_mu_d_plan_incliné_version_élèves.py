import numpy as np
import matplotlib.pyplot as plt
import numpy.random as rd
from math import pi

############################################################################
############# Données numériques et calcul des tableaux de tirages #########
############################################################################

alpha=......*pi /180 # à compléter
g=9.81

fichier = open('Raw Data.csv','r')
fichier_lu = fichier.readlines() #liste des chaines de caractères de chaque liqne
fichier.close()

###############################################################################################################
##### Lecture de chaque ligne, implémentation des données en liste de listes, puis conversion en tableau ######
###############################################################################################################

liste=[]
for line in fichier_lu[1:]: #slicing éliminant les intitulés de colonne
    tps,ax,ay,az,a=line.rstrip('\n').split(";")
    liste.append([float(tps),float(ax),float(ay),float(az),float(a)])
    
tableau_donnees = np.array(liste) # conversion en  tableau numpy, toujours préférable pour 
                            # permettre des calculs termes à termes si nécessaire
                            # la structure est une ligne de 5 données tps,ax,ay,az,a


############################################################################################################
####### Extraction du par slicing du tableau des dates et de la composante pertinente d'accélération #######
############################################################################################################
t=tableau_donnees[:,0]
a_ch=........ #  à compléter

#### Tracé pour choisir par slicing les bonnes valeurs du tableau "tableau_cpste_accel"
plt.figure()
plt.xlabel(r'temps (s)')
plt.ylabel("Accélération selon l'axe choisi")
plt.plot(t,........) # à compléter

####### Choix de l'intervalle des valeurs à conserver ######
t_min=........ #à compléter
t_max=........ #à compléter

#### Détermination des indices du tableau correspondant à t_min et t_max
while t[i]<t_min:
        i+=1
imin=i
while t[i]<t_max:
        i+=1
imax=i

t=t[imin:imax]
a_ch=a_ch[imin:imax]


plt.hist(a_ch,bins=50)
plt.xlabel(r"Composante choisie de l'accélération $a_{ch}")
plt.ylabel('Fréquence des tirages')
plt.show()

a_ch_emp=np.mean(a_ch)
u_a_ch=np.std(a_ch,ddof=1)

plt.figure()
plt.hist(a_ch,bins=100)
plt.title(r"$a_{ch_{emp}}=$ {0:.2f}, et $u(a_{ch})=$ {1:.2f}".format(a_ch_emp,u_a_ch))
plt.xlabel(r"composante d'accélération choisie $a_{ch}$")
plt.ylabel('Fréquence des tirages')
plt.show()

#########################################################################
##### Calcul du coefficient de frottement dynamique et son incertitude #####
#########################################################################
tab_mu_d= np.tan(alpha)-abs(a_ch)/(g*np.cos(alpha))

mu_d_emp=np.mean(tab_mu_d)
u_mu_d=np.std(tab_mu_d,ddof=1)


plt.figure()
plt.hist(tab_mu_d,bins=100)
plt.title(r"$\mu_{d_{emp}}=$ {0:.2f}, et $u(\mu_d)=$ {1:.2f}".format(mu_d_emp,u_mu_d))
plt.xlabel(r'Coefficient de frottement dynamique $\mu_d$')
plt.ylabel('Distribution des tirages')
plt.show()
