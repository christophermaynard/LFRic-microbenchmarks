/* -----------------------------------------------------------------------------*/
/* Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer       */
/* For further details please refer to the file LICENCE.original which you      */
/* should have received as part of this distribution.                           */
/*----------------------------------------------------------------------------- */
#include "dino_type.h"

void dino_open(dino_type *dino){
  int fp_s;
  strcpy(dino->fn,"../../LMA/dinodump.dat");
  dino->fp = fopen(dino->fn,"r");
  if(!dino->fp){
    fprintf(stderr,"Error: file not opened ...\n");
    fprintf(stderr,"%s \n",dino->fn);
    fprintf(stderr,"%d\n",dino->fp);
    exit(1);
  }
  return;
}

void dino_close(dino_type *dino){
  fclose(dino->fp);
}

void input_scalar(dino_type *dino, int *scalar){
  fscanf(dino->fp,"%d",scalar);
  return;
}

void input_1d_int(dino_type *dino, int *array, int dim){
  int i;
  for(i = 0; i<dim; i++){
    fscanf(dino->fp,"%d",&array[i]);
  }
  return;
}

void input_2d_int(dino_type *dino, int **array, int dim1, int dim2){
  int i,j;
  int scalar;
  for(i = 0; i < dim1; i++){
    for(j = 0; j < dim2; j++){
      fscanf(dino->fp,"%d",&array[i][j]);
    }
  }
  return;
}

void input_1d_double(dino_type *dino, double *array, int dim){
  int i;
  for(i = 0; i<dim; i++){
    fscanf(dino->fp,"%d",&array[i]);
  }
  return;
}

void input_3d_double(dino_type *dino, double ***array, int dim1, int dim2, int dim3){
  int i,j,k;
  int scalar;
  for(i = 0; i < dim1; i++){
    for(j = 0; j < dim2; j++){
      for(k = 0; k < dim3; k++){
	fscanf(dino->fp,"%d",&array[i][j][k]);
      }
    }
  }
  return;
}

  
