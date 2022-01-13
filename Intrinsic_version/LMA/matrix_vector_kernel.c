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


oid matrix_vector_code_1D(int cell, int nlayers, double* lhs, double *x, int ncell_3d, double *matrix, int ndf1, int undf, int **map1, int ndf2, int undf2, int **map2){

        int df1, df2, k ,ik, kk;



           for(df1 = 0; df1<ndf1; df1++){
              for(df2=0; df2<ndf2; df2++){
                        for(k = 0; k < nlayers; k+= 4){
                                ik = cell*nlayers+k;
                  kk = k-1;

                   double mat_values[8] = { matrix[(df1 * ndf2 * ncell_3d) + (df2 * ncell_3d) + ik],
                                            matrix[(df1 * ndf2 * ncell_3d) + (df2 * ncell_3d) + ik + 1],
                                            matrix[(df1 * ndf2 * ncell_3d) + (df2 * ncell_3d) + ik + 2],
                                            matrix[(df1 * ndf2 * ncell_3d) + (df2 * ncell_3d) + ik + 3],
                                            matrix[(df1 * ndf2 * ncell_3d) + (df2 * ncell_3d) + ik + 4],
                                            matrix[(df1 * ndf2 * ncell_3d) + (df2 * ncell_3d) + ik + 5],
                                            matrix[(df1 * ndf2 * ncell_3d) + (df2 * ncell_3d) + ik + 6],
                                            matrix[(df1 * ndf2 * ncell_3d) + (df2 * ncell_3d) + ik + 7]
                   };

                   double x_values[4] = { x[ map2[df2][cell] + kk],
                                          x[ map2[df2][cell] + kk + 1],
                                          x[ map2[df2][cell] + kk + 2],
                                          x[ map2[df2][cell] + kk + 3],
                                          x[ map2[df2][cell] + kk + 4],
                                          x[ map2[df2][cell] + kk + 5],
                                          x[ map2[df2][cell] + kk + 6],
                                          x[ map2[df2][cell] + kk + 7]

                   };
                  double lhs_values[4] = { lhs[ map1[df1][cell] + kk ],
                                          lhs[ map1[df1][cell] + kk  + 1],
                                          lhs[ map1[df1][cell] + kk  + 2],
                                          lhs[ map1[df1][cell] + kk  + 3],
                                          lhs[ map1[df1][cell] + kk  + 4],
                                          lhs[ map1[df1][cell] + kk  + 5],
                                          lhs[ map1[df1][cell] + kk  + 6],
                                          lhs[ map1[df1][cell] + kk  + 7]
                   };

                  __m512d mat_vec = _mm512_set_pd(mat_values[0], mat_values[1], mat_values[2], mat_values[3], mat_values[4], mat_values[5], mat_values[6], mat_values[7]);
                  __m512d x_vec = _mm512_set_pd(x_values[0], x_values[1], x_values[2], x_values[3], x_values[4], x_values[5], x_values[6], x_values[7]);
                  __m512d lhs_vec = _mm512_set_pd(lhs_values[0], lhs_values[1], lhs_values[2], lhs_values[3], lhs_values[4], lhs_values[5], lhs_values[6], lhs_values[7]);
                #ifdef FMA
                   __mm512_fmadd_pd(mat_vec, x_vec, lhs_vec);
                #else

                   __m512d r;
                   r = _mm512_mul_pd(mat_vec, x_vec);

                   lhs_vec = _mm512_add_pd(lhs_vec, r);
                #endif

                   _mm512_store_pd(&lhs[map1[df1][cell] + kk], lhs_vec);

                }
              }
           }

   return;
}
