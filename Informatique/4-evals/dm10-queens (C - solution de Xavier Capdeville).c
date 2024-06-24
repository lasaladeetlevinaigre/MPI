#include <stdio.h>
#include <stdlib.h>

void
printBoard (int **board, int N)
{
  for (int i = 0; i < N; i++)
	{
	  for (int j = 0; j < N; j++)
		{
		  if (board[i][j])
			printf ("R");
		  else
			printf ("0");
		}
	  printf ("\n");
	}
}

int
cgood (int row, int col, int **board, int N)
{
  for (int i = 0; i < col; i++)
	{
	  if (board[row][i])
		return 0;
	}

  //diag gauche
  for (int i = row, j = col; i >= 0 && j >= 0; i--, j--)
	{
	  if (board[i][j])
		return 0;
	}

  //diag droite
  for (int i = row, j = col; j >= 0 && i < N; i++, j--)
	{
	  if (board[i][j])
		return 0;
	}

  return 1;
}

int
backtrack (int col, int N, int **board)
{

  if (col >= N)
	return 1;

  for (int i = 0; i < N; i++)
	{
	  if (cgood (i, col, board, N))
		{
		  board[i][col] = 1;

		  if (backtrack ((col + 1), N, board))
			return 1;

		  board[i][col] = 0;
		}
	}
  return 0;
}

int
main (int argc, char **argv)
{
  int N = 0;
  if (argc != 2)
	{
	  printf ("Usage: %s <taille>\n", argv[0]);
	  return 1;
	}
  N = atoi (argv[1]);

  //echiquier vide

  printf ("Taille de l'C)chiquier %d ", N);

  int **board = (int **) malloc (N * sizeof (int *));
  for (int i = 0; i < N; i++)
	{
	  board[i] = (int *) malloc (N * sizeof (int));
	}

  for (int i = 0; i < N; i++)
	{
	  for (int j = 0; j < N; j++)
		{
		  board[i][j] = 0;
		}
	}

  if (backtrack (0, N, board))
	{
	  printf ("Solution trouvC)e\n");
	  printBoard (board, N);
	}
  else
	{
	  printf ("Pas de solution trouvC)e");
	}

  for (int i = 0; i < N; i++)
	{
	  free (board[i]);
	}
  free (board);

  return 0;
}
