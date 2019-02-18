/* -----------------------------------------------------------------------------
 * Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer       
 * For further details please refer to the file LICENCE.original which you      
 * should have received as part of this distribution.                           
 *----------------------------------------------------------------------------- 
 */
#include "dino_type.h"
#include "matrix_vector_kernel.h"
int main(){

  /* The file reader */
  dino_type dino;
  
  /* Mesh sizes */
  int ncell, ncell_3d, nlayers;

  /* Mesh coluring */
  int ncolours;
  int *ncells_per_colour;
  int **cmap;

  /* dof-maps for space 1 */
  int ndf1, undf1;
  int **map1;

  /* dof-maps for space 2 */
  int ndf2, undf2;
  int **map2;

  /* The data */
  double *data1, *data2, *answer;
  double ***op_data;

  /* scalar loop tempories */
  int i,j;
  int cell;
  
  /* Open the file */
  dino_open(&dino);
  
  /* Read the mesh sizes */
  input_scalar(&dino, &ncell);
  input_scalar(&dino, &ncell_3d);
  input_scalar(&dino, &ncolours);  
  input_scalar(&dino, &nlayers);

  /* colouring */
  ncells_per_colour = (int *) malloc(ncolours*sizeof(int));
  input_1d_int(&dino, ncells_per_colour, ncolours);
  cmap = (int **) malloc( ncolours *sizeof(int *) );
  for(i=0;i<ncolours; i++){
    cmap[i] = (int *) malloc( ncells_per_colour[i]*sizeof(int));
    }
  input_2d_int(&dino, cmap, ncolours, ncells_per_colour[0]);

  /* sizes and map for space 1 */
  input_scalar(&dino, &ndf1);
  input_scalar(&dino, &undf1);
  map1 = (int **) malloc(ndf1 * sizeof(int *));
  for(i = 0; i<ndf1; i++){
    map1[i] = (int *) malloc( ncell * sizeof(int) );
  }
  input_2d_int(&dino, map1, ndf1, ncell);

  /* sizes and map for space 2 */
  input_scalar(&dino, &ndf2);
  input_scalar(&dino, &undf2);
  map2 = (int **) malloc(ndf2 * sizeof(int *));
  for(i = 0; i<ndf2; i++){
    map2[i] = (int *) malloc( ncell * sizeof(int) );
  }
  input_2d_int(&dino, map2, ndf2, ncell);

  /* allocate data memory */
  data1 = (double *) malloc( undf1 * sizeof(double) );
  data2 = (double *) malloc( undf2 * sizeof(double) );
  answer = (double *) malloc( undf1 * sizeof(double) );
  op_data = (double ***) malloc( ndf1 * sizeof(double **));
  for(i = 0; i<ndf1; i++){
    op_data[i] = (double **) malloc( ndf2 * sizeof(double *) );
  }
  for(i = 0; i<ndf1; i++){
    for(j = 0; j<ndf2; j++){
      op_data[i][j] = (double *) malloc( ncell_3d * sizeof(double) );
    }
  }

  /* now read, read, read */
  input_3d_double(&dino, op_data, ndf1, ndf2, ncell_3d);
  input_1d_double(&dino, data1, undf1);
  input_1d_double(&dino, data2, undf2);
  input_1d_double(&dino, answer, undf1);
  printf("eaten the data\n");

  for(i = 0; i < ncolours; i++){
    for(j = 0; j < ncells_per_colour[i]; j++){
      matrix_vector_code(cmap[i][j], nlayers, data1, data2, ncell_3d, op_data, ndf1, undf1, map1, ndf2, undf2, map2);
    }
  }
  
  /* Free the crispy bits */
  dino_close(&dino);
  free(ncells_per_colour);
  free(cmap);
  free(map1);
  free(map2);
  free(op_data);
  free(data1);
  free(data2);
  free(answer);
  return 0;
}
