//:>------------------------------------------------------------------
//: FILE:       FTF_Utilities.cxx
//: HISTORY:
//:             28oct1996 version 1.00
//:<------------------------------------------------------------------
//:>------------------------------------------------------------------
//: FUNCTIONS:   Standalone functions
//: DESCRIPTION: Generic functions used in tracking
//: AUTHOR:      ppy - Pablo Yepes, yepes@physics.rice.edu
//:>------------------------------------------------------------------
#include <stdio.h>
#include <math.h>

//void invert_matrix ( int n, double *h ) ;
//void matrix_diagonal ( double *h, double *h11, double *h22, double *h33 ) ;
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//   Invert matrix h of dimension n
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//
//
//   Originally written in FORTRAN by Jawluen Tang, Physics department , UT-Austin 
//              modified and translated to C by Pablo Yepes, Rice U. 
//                               
//     The following routine is to invert a square symmetric matrix 
//     (in our case,it is a 3x3 matrix,so NOD=3)and calculate its 
//     determinant,substituting the inverse matrix into the same array 
//     of the original matrix H. 
//     See Philip R. Bevington,"Data reduction and error analysis for 
//     the physical science",p302 
//
void ftfInvertMatrix ( int n, double *h )  {
	static double detm, dmax_, temp;
    static int i, j, k, l;
	static int ik[3], jk[3];

    detm = 1.F ;
    
    for (k = 0 ; k < n ; ++k) {
	dmax_ = (double)0.;
	j = -1 ;
	while(j < k) {
	    i = -1 ;
	    while(i < k) {
		
		for (i = k; i < n; ++i) {
		    
		    for (j = k; j < n; ++j) {
			if (fabs(dmax_) <= fabs(h[i+j*3])) 
				{
			    dmax_ = h[i + j * 3];
			    ik[k] = i;
			    jk[k] = j;
			}
		    }
		}
		if (dmax_ == (double)0.) {
		    //printf ( "Determinant is ZERO!" );
		    return ;
		}
		i = ik[k];
	    }
	    if (i > k) {
		
		for (j = 0 ; j < n; ++j) {
		    temp = h[k + j * 3];
		    h[k + j * 3] = h[i + j * 3];
		    h[i + j * 3] = -(double)temp;
		}
	    }
	    j = jk[k];
	}
	if (j != k) {
	    
	    for (i = 0 ; i < n; ++i) {
		temp = h[i + k * 3];
		h[i + k * 3] = h[i + j * 3];
		h[i + j * 3] = -(double)temp;
	    }
	}
	
	for (i = 0 ; i < n; ++i) {
	    if (i != k) {
		h[i + k * 3] = -(double)h[i + k * 3] / dmax_;
	    }
	}
	
	for (i = 0; i < n; ++i) {
	    
	    for (j = 0; j < n; ++j) {
	    	if (i != k && j != k) {
		       h[i + j * 3] += h[i + k * 3] * h[k + j * 3];
		    }
	    }
	}
	for (j = 0; j < n; ++j) {
	    if (j != k) {
		h[k + j * 3] /= dmax_;
	    }
	}
	h[k + k * 3] = 1.F / dmax_;
	detm *= dmax_;
    }
    for (l = 0; l < n; ++l) {
    	k = n - l -1 ;
	    j = ik[k];
	    if (j > k) {
	       for (i = 0; i < n; ++i) {
		      temp = h[i + k * 3];
		      h[i + k * 3] = -(double)h[i + j * 3];
		      h[i + j * 3] = temp;
	       }
	   }
	   i = jk[k];
	   if (i > k) {
	      for (j = 0; j < n; ++j) {
	    	temp = h[k + j * 3];
		    h[k + j * 3] = -(double)h[i + j * 3];
		    h[i + j * 3] = temp;
	      }
	   }
    }
    return ;
} 
//+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
//    Function to give the diagonal elements (h11,h22,h33)
//    of the inverse symmetric 3x3 matrix of h
//    Calculation by Geary Eppley (Rice University)
//    coded by Pablo Yepes        (Rice University)
//++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
void ftfMatrixDiagonal ( double *h, double &h11, double &h22, double &h33 ){
	double f1, f2, f3 ;

	f1 = h[5]*h[6]-h[8]*h[1] ;
	f2 = h[4]*h[8]-h[5]*h[5] ;
	f3 = h[8]*h[0]-h[2]*h[2] ;
	h11 = double(h[8] / ( f3 - f1 * f1 / f2 )) ;

	f1 = h[2]*h[1]-h[0]*h[5] ;
	f2 = h[8]*h[0]-h[2]*h[2] ;
	f3 = h[0]*h[4]-h[1]*h[1] ;
	h22 = double(h[0] / ( f3 - f1 * f1 / f2 )) ;

	f1 = h[1]*h[5]-h[4]*h[2] ;
	f2 = h[0]*h[4]-h[1]*h[1] ;
	f3 = h[4]*h[8]-h[7]*h[7] ;
	h33 = double(h[4] / ( f3 - f1 * f1 / f2 )) ;
}
