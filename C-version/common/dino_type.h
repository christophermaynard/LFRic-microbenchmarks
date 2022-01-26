/* -----------------------------------------------------------------------------
 * Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer       
 * For further details please refer to the file LICENCE.original which you      
 * should have received as part of this distribution.                           
 *----------------------------------------------------------------------------- 
 */

#ifndef DINO_H
#define DINO_H
#include <stdlib.h>
#include <stdio.h>
#include <stdarg.h>
#include <string.h>


#define MAX_FILE_LEN 128

typedef struct{
  FILE *fp;
  char fn[MAX_FILE_LEN];
  
} dino_type;

void dino_open(dino_type *dino);
void dino_open_fn(dino_type *dino, char *fn);
void dino_close(dino_type *dino);

void input_scalar(dino_type *dino, int *scalar);
void input_1d_int(dino_type *dino, int *array, int dim);
void input_2d_int(dino_type *dino, int **array, int dim1, int dim2);

void input_1d_double(dino_type *dino, double *array, int dim);
void input_3d_double(dino_type *dino, double ***array, int dim1, int dim2, int dim3);

#endif    
