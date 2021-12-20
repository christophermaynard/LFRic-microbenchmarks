/* -----------------------------------------------------------------------------
 * Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer       
 * For further details please refer to the file LICENCE.original which you      
 * should have received as part of this distribution.                           
 *----------------------------------------------------------------------------- 
 */
#ifndef H_ARRAY_ALLOC
#define H_ARRAY_ALLOC
#include <stdlib.h>

int * ialloc_1d(int dim);
int ** ialloc_2d(int dim1, int dim2);

double * dalloc_1d(int dim);
double *** dalloc_3d(int dim1, int dim2, int dim3);

#endif
