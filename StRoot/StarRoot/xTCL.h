/*!
 * \class xTCL 
 * \author Victor Perev, Feb 2007
 */
/***************************************************************************
 *
 * $Id: xTCL.h,v 1.1 2007/04/26 04:22:36 perev Exp $
 *
 * Author: Victor Perev, Feb 2007
 ***************************************************************************
 *
 * Description:
 *
 * Set of useful routines in style of TCL, but not in TCL
 *
 *
 **************************************************************************/
#ifndef xTCL_h
#define xTCL_h
#include "TCL.h"
#include "TMatrixD.h"
#include "TVectorD.h"


class xTCL {
public:
static double vmaxa  (const double   *a, int na);
static double vmaxa  (const TVectorD &a);
static int    lvmaxa (const double *v,int n);
static int    lvmina (const double *v,int n);
static void   vfill  (      double *a,double f,int na);
static void   mxmlrt (const TMatrixD &A,const TMatrixD &B,TMatrixD &X);   
static void   mxmlrtS(const TMatrixD &A,const TMatrixD &B,TMatrixD &X);   
static void   mxmlrtS(const double *A,const double *B,double *X,int nra,int nca);  
static TMatrixD T(const TMatrixD &mx);  
static void   eigen2 (const double err[3], double lam[2], double eig[2][2]);
static double simpson(const double *F,double A,double B,int NP);
static double vasum  (const double *a, int na);
static double vasum  (const TVectorD &a);
static int SqProgSimple(      TVectorD &x
                       ,const TVectorD &g,const TMatrixD &G 
		       ,const TVectorD &Min		   
		       ,const TVectorD &Max,int iAkt);
static double **makeMatrixD(int m,int n);
};
#endif// xTCL_h
