def dist(p1, p2):
#    return np.sqrt(np.sum( (p1 - p2)**2 ))

def nearest(tab, p):
    d = float("inf")
    i = 0
    for j in range(len(tab)):
        d_j = dist(tab[j], p)
        if d_j < d:
            d = d_j
            i = j
    return i

# fonction d'affichage des points
def disp(tab):
    '''
    entrée : tableau tab = [ [x_0, y_0], [x_1, y_1], ... ]
    sortie : affichage des points associés aux couples
    '''
    n_points = len(tab)
    t_x = tab[:,0]
    t_y = tab[:,1]
    plt.plot(t_x, t_y, "o")

# calcul du barycentre d'un nuage de points
def center(tab):
    n = len(tab)
    x_c = 0.0
    y_c = 0.0
    for i in range(n):
        x_c += tab[i][0]
        y_c += tab[i][1]
    return np.array([x_c, y_c]) / n

def center(tab):
    return sum(tab) / len(tab)

def shuffle(tab):
    n = len(tab)
    for i in range(1, n):
        j = rd.randint(0,i)
        tab[i], tab[j] = tab[j], tab[i]

def mu_init(tab_points, k):
    n_points = len(tab_points)
    tab_idx = [i for i in range(n_points)]
    shuffle(tab_idx)
    tab_mu = [tab_points[tab_idx[i]] for i in range(k)]
    return np.array(tab_mu)

def make_class(tab_points, tab_mu):
    n_points = len(tab_points)
    k = len(tab_mu)
    tab_class = np.zeros(n_points, dtype=int)
    for i in range(n_points):
        tab_class[i] = nearest(tab_mu, tab_points[i])
    return tab_class


# nombre de points
n_points = 8
tab_points = rd.rand(n_points, 2)
#print(tab_points)

# nombre de classes choisi
k = 3
tab_mu = mu_init(tab_points, k)
tab_class = make_class(tab_points, tab_mu)
#print(tab_mu)
print(tab_class)