/* -----------------------------------------------------------------------------
 * Copyright (c) 2022,  Met Office, on behalf of HMSO and Queen's Printer       
 * For further details please refer to the file LICENCE.original which you      
 * should have received as part of this distribution.                           
 *----------------------------------------------------------------------------- 
 */

#include "dino_type.h"
#include "array_alloc.h"

int main(){
  /* The file reader */
  dino_type dino;
  /* Mesh sizes */
  int ncell_2d, nlayers;

  /* Mesh coluring */
  int ncolours;
  int *ncells_per_colour;
  int **cmap;

  /* cma sizes */
  int cma_bandwidth, cma_alpha, cma_beta, cma_gamma_m, cma_gamma_p;

  /* dofs and maps for space 1 */
  /* dof-maps for space 1 */
  int ndf, undf;
  int **map;
  int *indirection_map;

  /* The actual data */
  double *data1, *data2, *answer, ***cma_matrix;
  char fname[128];

  /*open the data file */
  sprintf(fname,"%s","../../CMA_appinv/dinodump.dat");
  dino_open_fn(&dino,fname);

  /* ingest the scalars */
  input_scalar(&dino, &ncolours);
  input_scalar(&dino, &ncell_2d);
  input_scalar(&dino, &nlayers);    
  input_scalar(&dino, &cma_bandwidth);
  input_scalar(&dino, &cma_alpha);  
  input_scalar(&dino, &cma_beta);
  input_scalar(&dino, &cma_gamma_m);  
  input_scalar(&dino, &cma_gamma_p);
  input_scalar(&dino, &ndf);
  input_scalar(&dino, &undf);    
  printf("cma HW:%d %d\n",ncolours, undf);

  /* allocate the arrays and read the data */

  /* colouring */ 
  ncells_per_colour = ialloc_1d(ncolours);
  input_1d_int(&dino, ncells_per_colour,ncolours);
  cmap = ialloc_2d(ncolours, ncells_per_colour[0]);
  input_2d_int(&dino, cmap, ncolours, ncells_per_colour[0]);

  /* allocate the data arrays */
  data1 = dalloc_1d(undf);
  data2 = dalloc_1d(undf);
  answer = dalloc_1d(undf);
  cma_matrix = dalloc_3d(cma_bandwidth, nlayers, ncell_2d);

  /* allocate the dof map data */
  map = ialloc_2d(ndf, ncell_2d);
  indirection_map = ialloc_1d(nlayers);

  /* read the data arrays */
  input_1d_double(&dino, data1, undf);
  input_1d_double(&dino, data2, undf);  
  printf("read some data:%lf\n",data2[0]);
  input_3d_double(&dino, cma_matrix, cma_bandwidth, nlayers, ncell_2d);
  printf("read the matrix:%lf\n",cma_matrix[1][1][1]);

  /* read the maps */
  input_2d_int(&dino, map, ndf, ncell_2d);
  input_1d_int(&dino, indirection_map, nlayers);

  /* read the answer */
  input_1d_double(&dino, answer, undf);
  printf("final answer: %lf\n",answer[undf-1]);
  
  
  return 0;
}
