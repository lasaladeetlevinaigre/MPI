
/******************************************************/
/* Concours commun INP                                */
/* https://www.concours-commun-inp.fr                 */
/* CC BY-NC-SA, Novembre 2023                         */
/* https://creativecommons.org/licenses/by-nc-sa/4.0/ */
/******************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <time.h>

/*
La directive #define est utilisée pour définir des valeurs pour
des constantes qui serviront à déclarer des tableaux de taille fixe.
*/
#define m 4
#define n 4

/* Macro de calcul du maximum entre i et j */
int max(int i,int j)
{
    if (i<j)
        return j;
    else
        return i;
}


int recolte(int champ[m][n], int i, int j){
         /* Cas triviaux */
         if ( (i == 0) && (j == 0) )
          return champ[0][0];
         if (i == 0)
           return champ[0][j] + recolte(champ,0, j - 1);
         if (j == 0)
           return champ[i][0] + recolte(champ,i - 1, 0);

         /* Cas général */
         return champ[i][j] + max(recolte(champ,i - 1, j),recolte(champ,i, j - 1));
}


void deplacements(int fleurs[m][n], int i, int j){
if (i == 0 && j == 0) {
     printf("Case A, ");
     return;
}
if (i == 0) {
     deplacements(fleurs,0, j - 1);
     printf("Aller à droite, ");
     return;
}
if (j == 0) {
     deplacements(fleurs,i - 1, 0);
     printf("Descendre, ");
     return;
}
if (fleurs[i - 1][j] > fleurs[i][j - 1]) {
   deplacements(fleurs,i - 1, j);
   printf("Descendre, ");
}
else {
   deplacements(fleurs,i, j - 1);
   printf("Aller à droite, ");
}
}


int recolte_iterative(int champ[m][n], int i, int j,int fleurs[m][n]){
    int x, y;

    fleurs[0][0] = champ[0][0];

    /* Bord haut */
    for (x = 1; x <= i; x++) {
         fleurs[x][0] = champ[x][0] + fleurs[x - 1][0];
    }
    /* Bord gauche */
    for (y = 1; y <= j; y++) {
         fleurs[0][y] = champ[0][y] + fleurs[0][y - 1];
    }
    /* Autres cases */
    for (y = 1; y <= j; y++) {
        for (x = 1; x <= i; x++) {
            fleurs[x][y] = champ[x][y] + max(fleurs[x - 1][y], fleurs[x][y - 1]);
} }
    deplacements(fleurs,i, j);
    return fleurs[i][j];
}






int main(){
     int champ[m][n],fleurs[m][n];

     int i,j;

    /* Exemple du champ de fleurs : le nombre de fleurs par case est un entier
    aléatoire entre 0 et 10. On utilise la fonction int rand() de stdlib. Le générateur
    de nombre pseudo-aléatoires est tout d'abord initialisé.*/
     srand(time(NULL));
     for (i=0;i<m;i++) for(j=0;j<n;j++)
          champ[i][j] = rand() % 11;

    for (i=0;i<m;i++)
    {
        for(j=0;j<n;j++)
            printf("%d\t",champ[i][j]);
        printf("\n");
    }

     printf("\n Nombre de fleurs maximum cueillies : %d\n",recolte_iterative(champ,3,3,fleurs));

     return 0;
}

