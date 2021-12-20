/* -----------------------------------------------------------------------------
 * Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer       
 * For further details please refer to the file LICENCE.original which you      
 * should have received as part of this distribution.                           
 *----------------------------------------------------------------------------- 
 */
#include "array_alloc.h"

int * ialloc_1d(int dim){
  int *array;
  array = (int *) malloc( dim * sizeof( int ) );
  return array;
}

int ** ialloc_2d(int dim1, int dim2){
  int **array;
  int i;
  array = (int **) malloc(dim1 * sizeof(int *));
  for(i = 0; i<dim1; i++){
    array[i] = (int *) malloc( dim2 * sizeof(int) );
  }
  return array;
}

double * dalloc_1d(int dim){
  double *array;
  array = (double *) malloc( dim * sizeof( double ) );
  return array;
}

double *** dalloc_3d(int dim1, int dim2, int dim3){
  double ***array;
  int i,j;
  
  array = (double ***) malloc(dim1 * sizeof(double **));
  for(i = 0; i<dim1; i++){
    array[i] = (double **) malloc( dim2 * sizeof(double *) );
  }
  
  for(i = 0; i<dim1; i++){
    for(j = 0; j<dim2; j++){
      array[i][j] = (double *) malloc( dim3 * sizeof(double) );
    }
  }
  return array;
}
