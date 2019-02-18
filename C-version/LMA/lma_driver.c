/* -----------------------------------------------------------------------------
 * Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer       
 * For further details please refer to the file LICENCE.original which you      
 * should have received as part of this distribution.                           
 *----------------------------------------------------------------------------- 
 */
#include "dino_type.h"

int main(){

  /* The file reader */
  dino_type dino;
  
  /* Mesh sizes */
  int ncell, ncell_3d, nlayers;

  /* Mesh coluring */
  int ncolours;
  int *ncells_per_colour;
  int **cmap;

  /* scalar loop tempories */
  int i;
  
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
  input_2d_int(&dino, cmap, ncolours, max_ncell_per_colour);
  printf("%d %d\n",cmap[0][0], cmap[0][2]);
  
  dino_close(&dino);
  free(ncells_per_colour);
  free(cmap);
  return 0;
}
