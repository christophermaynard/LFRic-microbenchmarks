/* -----------------------------------------------------------------------------*/
/* Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer       */
/* For further details please refer to the file LICENCE.original which you      */
/* should have received as part of this distribution.                           */
/*----------------------------------------------------------------------------- */

#include "matrix_vector_kernel.h"

void matrix_vector_code(int cell, int nlayers, double *lhs, double *x, int ncell_3d, double ***matrix, int ndf1, int undf, int **map1, int ndf2, int undf2, int **map2){

  int df1,df2,k,ik;
  for( k=0; k<nlayers; k++){
    ik = cell*nlayers+k;
    for( df1 = 0; df1<ndf1; df1++){
      for( df2 = 0; df2<ndf2; df2++){
	lhs[ map1[df1][cell] + k ] = lhs[ map1[df1][cell] + k ] + matrix[df1][df2][ik]*x[ map2[df2][cell] + k];
      }
    }
  }
  
  return;
}
