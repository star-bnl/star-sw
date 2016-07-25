/* $Id: Fint.h,v 1.1 2013/06/24 23:39:59 fisyak Exp $ 
   $Log: Fint.h,v $
   Revision 1.1  2013/06/24 23:39:59  fisyak
   Freeze

   Revision 1.3  2012/08/23 19:16:00  fisyak
   Freeze

   Revision 1.2  2012/08/22 23:23:27  fisyak
   Add template version of Fint and test for it
   CERN PROGLIB# E104    FINT            .VERSION KERNFOR  4.02  820723 
   ORIG. 09/08/65 CHL. 
   INTERPOLATION ROUTINE. AUTHOR C. LETERTRE. 
   MODIFIED BY B. SCHORR, 1.07.1982. 
*/
#ifndef __FINT__
#define __FINT__
#include "Rtypes.h"
#include "TMath.h"
#include "StThreeVectorD.h"
#include "StThreeVectorF.h"
//#define __DEBUG__PRINT__
#ifdef __DEBUG__PRINT__
#include "Riostream.h"
using namespace std;
#endif /* __DEBUG__PRINT__ */
template<typename T>
Int_t FintKnots (Int_t narg,T *arg,Int_t *nent,T *ent, Int_t *index, Double_t *weight) {
  /* System generated locals */
  Int_t i__2, i__3;
  T r__1;
  /* Local variables */
  static Int_t loca, locb, locc, ndim, lmin, lmax;
  static Double_t h__;
  static Int_t k, n;
  static Double_t x;
  static Int_t istep, knots, ishift;
  static Double_t eta;
  /* Parameter adjustments */
  --ent;
  //  --table;
  /* Function Body */
  if (narg < 1 || narg > 5) return 0;
  lmax = 0;
  istep = 1;
  knots = 1;
  index[0] = 1;
  weight[0] = 1.;
  for (n = 0; n < narg; ++n) {
    x = arg[n];
    ndim = nent[n];
    loca = lmax;
    lmin = lmax + 1;
    lmax += ndim;
    if (ndim > 2)       goto L10;
    if (ndim == 1)      continue;
    h__ = x - ent[lmin];
    if (h__ == 0.)      goto L90;
    ishift = istep;
    if (x - ent[lmin + 1] == 0.)       goto L21;
    ishift = 0;
    eta = h__ / (ent[lmin + 1] - ent[lmin]);
    goto L30;
  L10:
    locb = lmax + 1;
  L11:
    locc = (loca + locb) / 2;
    if     ((r__1 = x - ent[locc]) < 0.)       goto L12;
    else if (r__1 == 0)                        goto L20;
    else                                       goto L13;
  L12:
    locb = locc;
    goto L14;
  L13:
    loca = locc;
  L14:
    if (locb - loca > 1)                      goto L11;
    /* Computing MIN */
    i__2 = TMath::Max(loca,lmin);
    i__3 = lmax - 1;
    loca = TMath::Min(i__2,i__3);
    ishift = (loca - lmin) * istep;
    eta = (x - ent[loca]) / (ent[loca + 1] - ent[loca]);
    goto L30;
  L20:
    ishift = (locc - lmin) * istep;
  L21:
    for (k = 1; k <= knots; ++k) {
      index[k - 1] += ishift;
      /* L22: */
    }
    goto L90;
  L30:
#ifdef __DEBUG__PRINT__
    cout << n << " x = " << x << " loca " << loca << " eta " << eta << " knots " << knots << endl;
#endif /* __DEBUG__PRINT__ */
    for (k = 1; k <= knots; ++k) {
      index[ k         - 1] += ishift;
      index[ k + knots - 1]  = index [k - 1]          + istep;
      weight[k + knots - 1]  = weight[k - 1]          * eta;
      weight[k         - 1] -= weight[k + knots - 1];
#ifdef __DEBUG__PRINT__
      cout << " index[ k         - 1] = " << index[ k         - 1]
	   << " index[ k + knots - 1] = " << index[ k + knots - 1]
	   << " weight[k + knots - 1] = " << weight[k + knots - 1]
	   << " weight[k         - 1] = " << weight[k         - 1] << endl;
#endif /* __DEBUG__PRINT__ */
    }
    knots <<= 1;
  L90:
    istep *= ndim;
  }
  return knots;
} 
//________________________________________________________________________________
template<typename T,typename D>
D Fint (Int_t narg, T *arg, Int_t *nent, T *ent, D *table) {
  if (narg < 1 || narg > 5) return D();
  Int_t index[32];
  Double_t weight[32];
  Int_t knots = FintKnots(narg,arg,nent,ent,index,weight);
  D ret_val = 0;
  for (Int_t k = 0; k < knots; ++k) {
    Int_t i = index[k]-1;
    D v =  table[i];
    v *= weight[k];
    ret_val += weight[k]*table[i];
  }
  return ret_val;
}
//________________________________________________________________________________
StThreeVectorD Fint (Int_t narg, Double_t *arg, Int_t *nent, Double_t *ent, StThreeVectorD *table) {
  if (narg < 1 || narg > 5) return StThreeVectorD();
  Int_t index[32];
  Double_t weight[32];
  Int_t knots = FintKnots(narg,arg,nent,ent,index,weight);
  StThreeVectorD val(0,0,0);
  for (Int_t k = 0; k < knots; ++k) {
    Int_t i = index[k]-1;
    val += weight[k]*table[i];
  }
  return val;
}
//________________________________________________________________________________
StThreeVectorF Fint (Int_t narg, Double_t *arg, Int_t *nent, Double_t *ent, StThreeVectorF *table) {
  if (narg < 1 || narg > 5) return StThreeVectorF();
  Int_t index[32];
  Double_t weight[32];
  Int_t knots = FintKnots(narg,arg,nent,ent,index,weight);
  StThreeVectorF val(0,0,0);
  for (Int_t k = 0; k < knots; ++k) {
    Int_t i = index[k]-1;
    val += weight[k]*table[i];
  }
  return val;
}
#endif /* __FINT__ */
