/* -----------------------------------------------------------------------------*/
/* Copyright (c) 2017,  Met Office, on behalf of HMSO and Queen's Printer       */
/* For further details please refer to the file LICENCE.original which you      */
/* should have received as part of this distribution.                           */
/*----------------------------------------------------------------------------- */

#include "matrix_vector_kernel.h"
#include <immintrin.h>

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
		for(k = 0; k < nlayers; k+= 4){
		  /*ik = cell*nlayers+k;
		  kk = k-1;
	          lhs[map1[df1][cell] + kk] += matrix[ (df1 * ndf2 * ncell_3d) + (df2 * ncell_3d) + ik] * x[ map2[df2][cell] + kk];
	      	  */
		   double mat_values[4] = { matrix[(df1 * ndf2 * ncell_3d) + (df2 * ncell_3d) + ik],
			  		    matrix[(df1 * ndf2 * ncell_3d) + (df2 * ncell_3d) + ik + 1], 
		                            matrix[(df1 * ndf2 * ncell_3d) + (df2 * ncell_3d) + ik + 2],
			  		    matrix[(df1 * ndf2 * ncell_3d) + (df2 * ncell_3d) + ik + 3] 
		   };
		   double x_values[4] = { x[ map2[df2][cell] + kk],
			                  x[ map2[df2][cell] + kk + 1],
			                  x[ map2[df2][cell] + kk + 2],
			                  x[ map2[df2][cell] + kk + 3]
		   };
		   __m256d mat_vec = _mm256_set_pd(mat_values[0], mat_values[1], mat_values[2], mat_values[3]);
		   __m256d x_vec = _mm256_set_pd(x_values[0], x_values[1], x_values[2], x_values[3]);
		   __m256d lhs_vec;

		   __mm256_fmadd_pd(mat_vec, x_vec, lhs_vec);
		   lhs[map1[df1][cell] + kk] += _mm256_extract_pd(lhs, 0);
		   

		}
	      }
	   }
	
   return;
}
