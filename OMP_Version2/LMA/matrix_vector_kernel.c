/* -----------------------------------------------------------------------------*/
/* Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer       */
/* For further details please refer to the file LICENCE.original which you      */
/* should have received as part of this distribution.                           */
/*----------------------------------------------------------------------------- */

#include "matrix_vector_kernel.h"
#include "omp.h"

void matrix_vector_code(int cell, int nlayers, double *lhs, double *x, int ncell_3d, double ***matrix, int ndf1, int undf, int **map1, int ndf2, int undf2, int **map2){

  int df1,df2,k,ik,kk;
  for( k=0; k<nlayers; k++){
    ik = cell*nlayers+k;
    kk=k-1;
    for( df1 = 0; df1<ndf1; df1++){
      for( df2 = 0; df2<ndf2; df2++){
		
	lhs[ map1[df1][cell] + kk ] +=  matrix[df1][df2][ik]*x[ map2[df2][cell] + kk];
      }
    }
  }
  
  return;
}


void matrix_vector_code_1D(int cell, int nlayers, double* lhs, double *x, int ncell_3d, double *matrix, int ndf1, int undf, int **map1, int ndf2, int undf2, int **map2){

	int df1, df2, k ,ik, kk;

	for(df1 = 0; df1<ndf1; df1++){
	
	       for(df2=0; df2<ndf2; df2++){
		   #pragma omp simd
	           for(k = 0; k < nlayers; k++){				
		
			
		  ik = cell*nlayers+k;
		  kk = k-1;
	          lhs[ map1[df1][cell] + kk] += matrix[ (df1 * ndf2 * ncell_3d) + (df2 * ncell_3d) + ik] * x[ map2[df2][cell] + kk];
	      
		}
	      }
	   }
	
   return;
}
