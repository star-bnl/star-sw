// $Id: StEmcFit.h,v 1.6 2001/12/29 20:33:50 suaide Exp $
// $Log: StEmcFit.h,v $
// Revision 1.6  2001/12/29 20:33:50  suaide
// added documentation
//
// Revision 1.5  2001/12/28 21:31:09  suaide
// Added documentation
//
// Revision 1.4  2001/12/04 18:23:55  suaide
// small fix
//
// Revision 1.3  2001/11/07 17:54:10  suaide
// some modifications for real data
//
// Revision 1.1  2001/10/17 13:51:31  suaide
// new modifications to work with real data
//
// Revision 1.12  1999/09/24 22:03:09  perev
// Add InitRun & FinishRun to template maker
//

/*!\class StEmcFit
\author Alexandre A. P. Suaide

This class hols some fit method that are used during EMC calibration.
It is an adaption of non-linear method of fit from Numerical Recipes on C
*/
#ifndef STAR_StEmcFit
#define STAR_StEmcFit
#include "TObject.h"
#include <math.h>
class StEmcFit : public TObject 
{
  private: 
            Float_t   temp;
            Float_t   atry[2000];
            Float_t   beta[5000];
            Float_t   da[5000];    
            Float_t   oneda[5000][5];
            Float_t   a[2000];
            Int_t     ia[2000];
            Float_t   alpha[5000][5000],covar[5000][5000],chisq;
            Float_t   x[5000],y[5000],sig[5000];
            Int_t     ma,ndata;
            Float_t   chisqr,alamda;
            Int_t     mfit;
            Float_t   ochisq;
            Float_t   dyda[2000];
            Int_t     functype;
            
            void      gaussians(Float_t,Float_t*);
            void      gaussPlusExp(Float_t,Float_t*);
            void      exponential(Float_t,Float_t*);
            void      gaussj(Int_t,Int_t);
            void      covsrt(Int_t);
            void      mrqcof();
            void      mrqmin();



  protected:
    
  public: 
 
    
                      StEmcFit();///< Default constructor
   virtual            ~StEmcFit();///< Default destructor
   
            void      SetFuncType(Int_t t)                         { functype=t; } ///< Set function type for fit
            void      SetNParms(Int_t n)                           { ma=n; } ///< Set number of parameters
            void      SetParm(Int_t i,Float_t xx,Int_t st)         { a[i]=xx; ia[i]=st;} ///< Set parameter initial guess
            void      ClearAll(); ///< Clear all
            void      AddPoint(Float_t xx,Float_t yy,Float_t sy)   { ndata++; x[ndata]=xx; y[ndata]=yy; sig[ndata]=sy;} ///< Add fit point
            void      Fit(); ///< Fit
            void      Fit(Int_t); ///< Fit using a fixed number of iterations
            Float_t   GetParameter(Int_t i)                        { return a[i]; } ///< Get parameter value
            Float_t   GetParameterError(Int_t i)                   { return sqrt(covar[i][i]); } ///< Get parameter error
            Float_t   GetCovariance(Int_t i,Int_t j)               { return covar[i][j]; } ///< Get covariance matrix element
            Float_t   GetChiSquare()                               { return chisq; } ///< Get chi square / sqrt (Npoints - Nparam)
            Int_t     GetNPoints()                                 { return ndata; } ///< Get number of points
            Int_t     GetNParam()                                  { return ma; } ///< Get number of parameters
            Float_t   Y(Float_t); ///< Calculates fitted function for a given X

   ClassDef(StEmcFit, 1)  
};

#endif
