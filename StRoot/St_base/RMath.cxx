#include "RMath.h"

// http://wwwinfo.cern.ch/asdoc/shortwrupsdir/f110/top.html 

ClassImp(RMath)
//___________________________________________________________________________
void RMath::mxmad_0_(int n_, float *a, float *b, float *c, int i, int j, int k)
{
    /* Local variables */
    int l, m, n, ia, ic, ib, ja, jb, iia, iib, ioa, iob;

/* CERN PROGLIB# F110    MXMAD           .VERSION KERNFOR  1.0   650809 */
/* ORIG. 01/01/64 RKB */

    /* Parameter adjustments */
    --a;  --b;  --c;
    /* Function Body */
//                      MXMAD MXMAD1 MXMAD2 MXMAD3 MXMPY MXMPY1 MXMPY2 MXMPY3 MXMUB MXMUB1 MXMUB2 MXMUB3
//  const int iandj1[] = {21,   22,    23,    24,   11,    12,    13,    14,    31,   32,   33,    34 };
    const int iandj1[] = {2,    2 ,    2 ,    2 ,   1 ,    1 ,    1 ,    1 ,    3 ,   3 ,   3 ,    3  };
    const int iandj2[] = { 1,    2,     3,     4,    1,     2,     3,     4,     1,    2,    3,     4 };
    int n1 = iandj1[n_];
    int n2 = iandj2[n_];
    if (i == 0 || k == 0) return;
    
    switch (n2) {
	case 1: iia = 1; ioa = j; iib = k; iob = 1; break;
        case 2: iia = 1; ioa = j; iib = 1; iob = j; break;
	case 3: iia = i; ioa = 1; iib = k; iob = 1; break;
        case 4: iia = i; ioa = 1; iib = 1; iob = j; break;
    };

    ia = 1; ic = 1;
    for (l = 1; l <= i; ++l) {
	ib = 1;
	for (m = 1; m <= k; ++m,++ic) {
	    switch (n1) {
		case 1:  c[ic] = (float)0.; break;
		case 3:  c[ic] = -c[ic];    break;
	    };
	    if (j == 0) continue;
	    ja = ia; jb = ib;
	    for (n = 1; n <= j; ++n, ja+=iia, jb+=iib) 
		c[ic] += a[ja] * b[jb];
	    ib += iob;
	}
	ia += ioa;
    }
    return;
} /* mxmad_ */

//___________________________________________________________________________
void RMath::mxmad_0_(int n_, double *a, double *b, double *c, int i, int j, int k)
{
    /* Local variables */
    int l, m, n, ia, ic, ib, ja, jb, iia, iib, ioa, iob;

    /* Parameter adjustments */
    --a;  --b;  --c;
    /* Function Body */
//                      MXMAD MXMAD1 MXMAD2 MXMAD3 MXMPY MXMPY1 MXMPY2 MXMPY3 MXMUB MXMUB1 MXMUB2 MXMUB3
//  const int iandj1[] = {21,   22,    23,    24,   11,    12,    13,    14,    31,   32,   33,    34 };
    const int iandj1[] = {2,    2 ,    2 ,    2 ,   1 ,    1 ,    1 ,    1 ,    3 ,   3 ,   3 ,    3  };
    const int iandj2[] = { 1,    2,     3,     4,    1,     2,     3,     4,     1,    2,    3,     4 };
    int n1 = iandj1[n_];
    int n2 = iandj2[n_];
    if (i == 0 || k == 0) return;
    
    switch (n2) {
	case 1: iia = 1; ioa = j; iib = k; iob = 1; break;
        case 2: iia = 1; ioa = j; iib = 1; iob = j; break;
	case 3: iia = i; ioa = 1; iib = k; iob = 1; break;
        case 4: iia = i; ioa = 1; iib = 1; iob = j; break;
    };

    ia = 1; ic = 1;
    for (l = 1; l <= i; ++l) {
	ib = 1;
	for (m = 1; m <= k; ++m,++ic) {
	    switch (n1) {
		case 1:  c[ic] = (double)0.; break;
		case 3:  c[ic] = -c[ic];    break;
	    };
	    if (j == 0) continue;
	    ja = ia; jb = ib;
	    for (n = 1; n <= j; ++n, ja+=iia, jb+=iib) 
		c[ic] += a[ja] * b[jb];
	    ib += iob;
	}
	ia += ioa;
    }
    return;
} /* mxmad_ */

//___________________________________________________________________________
void RMath::mxmlrt_0_(int n__, float *a, float *b, float *c, int ni,int nj)
{
  /* Local variables */
  float x;
  int ia, ib, ic, ja, kc, ii, jj, kj, ki, ia1, ib1, ic1, ja1;

/* CERN PROGLIB# F110    MXMLRT          .VERSION KERNFOR  2.00  720707 */
/* ORIG. 01/01/64 RKB */


/* --      ENTRY MXMLRT */

/* --                C = A(I,J) X B(J,J) X A*(J,I) */
/* --                A* STANDS FOR A-TRANSPOSED */

/*        CALL MXMLRT (A,B,C,NI,NJ)     IS EQUIVALENT TO */
/*             CALL MXMPY (A,B,X,NI,NJ,NJ) */
/*             CALL MXMPY1 (X,A,C,NI,NJ,NI) */

/*        OR   CALL MXMPY1 (B,A,Y,NJ,NJ,NI) */
/*             CALL MXMPY (A,Y,C,NI,NJ,NI) */

    /* Parameter adjustments to use indeces from "1" */
  --a;  --b;  --c;

  /* Function Body */
  int ipa = 1;
  int jpa = nj;
  if (n__ == 1) { ipa = ni;  jpa = 1; }

/* --                C = A*(I,J) X B(J,J) X A(J,I) */

/*        CALL MXMLTR (A,B,C,NI,NJ)     IS EQUIVALENT TO */
/*             CALL MXMPY2 (A,B,X,NI,NJ,NJ) */
/*             CALL MXMPY (X,A,C,NI,NJ,NI) */

/*        OR   CALL MXMPY (B,A,Y,NJ,NJ,NI) */
/*             CALL MXMPY2 (A,Y,C,NI,NJ,NI) */

  if (ni <= 0 || nj <= 0) return;
    
  ic1 = 1;  ia1 = 1;
  for (ii = 1; ii <= ni; ++ii, ic1+=ni, ia1+=jpa) {
    ic = ic1;
    for (kc = 1; kc <= ni; ++kc,ic++) c[ic] = (float)0.;
    ib1 = 1;  ja1 = 1;
    for (jj = 1; jj <= nj; ++jj,++ib1,ja1 += ipa) {
      ib = ib1;  ia = ia1;
      x = (float)0.;
      for (kj = 1;kj <= nj;++kj,ia+=ipa,ib += nj) 
		x += a[ia] * b[ib];	    
      ja = ja1;  ic = ic1;
      for (ki = 1; ki <= ni; ++ki,++ic,ja += jpa) 
		c[ic] += x * a[ja];	    
    }
  }
  return;
} /* mxmlrt_ */

//___________________________________________________________________________
void RMath::mxmlrt_0_(int n__, double *a, double *b, double *c, int ni,int nj)
{
  /* Local variables */
  double x;
  int ia, ib, ic, ja, kc, ii, jj, kj, ki, ia1, ib1, ic1, ja1;

    /* Parameter adjustments to use indeces from "1" */
  --a;  --b;  --c;

  /* Function Body */
  int ipa = 1;  int jpa = nj;
  if (n__ == 1) { ipa = ni;  jpa = 1; }

  if (ni <= 0 || nj <= 0) 	return;
    
  ic1 = 1;  ia1 = 1;
  for (ii = 1; ii <= ni; ++ii, ic1+=ni, ia1+=jpa) {
    ic = ic1;
    for (kc = 1; kc <= ni; ++kc,ic++) c[ic] = (double)0.;
    ib1 = 1; ja1 = 1;
    for (jj = 1; jj <= nj; ++jj,++ib1,ja1 += ipa) {
      ib = ib1;  ia = ia1;
      x = (double)0.;
      for (kj = 1;kj <= nj;++kj,ia+=ipa,ib += nj) 
		x += a[ia] * b[ib];	    
      ja = ja1; ic = ic1;
      for (ki = 1; ki <= ni; ++ki,++ic,ja += jpa) 
		c[ic] += x * a[ja];	    
    }
  }
  return;
} /* mxmlrt_ */


