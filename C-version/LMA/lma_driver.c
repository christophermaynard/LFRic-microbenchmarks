/* -----------------------------------------------------------------------------
 * Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer       
 * For further details please refer to the file LICENCE.original which you      
 * should have received as part of this distribution.                           
 *----------------------------------------------------------------------------- 
 */
#include "dino_type.h"
#include "matrix_vector_kernel.h"
#include "array_alloc.h"

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
  ncells_per_colour = ialloc_1d(ncolours);
  input_1d_int(&dino, ncells_per_colour, ncolours);

  cmap = ialloc_2d(ncolours, ncells_per_colour[0]);
  input_2d_int(&dino, cmap, ncolours, ncells_per_colour[0]);

  /* sizes and map for space 1 */
  input_scalar(&dino, &ndf1);
  input_scalar(&dino, &undf1);

  map1 = ialloc_2d(ndf1, ncell);
  input_2d_int(&dino, map1, ndf1, ncell);
  
  /* sizes and map for space 2 */
  input_scalar(&dino, &ndf2);
  input_scalar(&dino, &undf2);

  map2 = ialloc_2d(ndf2, ncell);
  input_2d_int(&dino, map2, ndf2, ncell);

  /* allocate data memory */
  data1 = dalloc_1d(undf1);
  data2 = dalloc_1d(undf2);
  answer = dalloc_1d(undf1);
  
  op_data = dalloc_3d( ndf1, ndf2, ncell_3d );

  /* now read, read, read */
  input_3d_double(&dino, op_data, ndf1, ndf2, ncell_3d);
  input_1d_double(&dino, data1, undf1);
  input_1d_double(&dino, data2, undf2);
  input_1d_double(&dino, answer, undf1);
  printf("Ingested the data\n");

#pragma omp target map(tofrom: data1[0:undf1)			    \
	map(to: op_data[0:ndf1, 0:ndf2, 0:ncell_d], data2[0:undf2], \
	    cmap[0:ncolours, 0:ncells_per_colour[0] ], map1[0:ndf1,0:ncell], \
	    map2[0:ncf2,0:ncell] )
  for(i = 0; i < ncolours; i++){
#pragma omp temas distribute parallel for
    for(j = 0; j < ncells_per_colour[i]; j++){
      matrix_vector_code((cmap[i][j]-1), nlayers, data1, data2, ncell_3d, op_data, ndf1, undf1, map1, ndf2, undf2, map2);
    }
  }
    


  /* random check */
  i = 56;
  printf("%d %.16e %.16e\n",i, data1[i],answer[i]);
  i = 57;
  printf("%d %.16e %.16e\n",i, data1[i],answer[i]);
  i = 58;
  printf("%d %.16e %.16e\n",i, data1[i],answer[i]);
 

  
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
