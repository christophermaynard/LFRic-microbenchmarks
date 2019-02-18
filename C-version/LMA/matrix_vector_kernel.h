/* -----------------------------------------------------------------------------*/
/* Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer       */
/* For further details please refer to the file LICENCE.original which you      */
/* should have received as part of this distribution.                           */
/*----------------------------------------------------------------------------- */
#ifndef H_MATRIX_VECTOR_KERNEL
#define H_MATRIX_VECTOR_KERNEL
#include <stdlib.h>
#include <stdio.h>

void matrix_vector_code(int cell, int nlayers, double *lhs, double *x, int ncell_3d, double ***matrix, int ndf1, int undf, int **map1, int ndf2, int undf2, int **map2);
#endif
