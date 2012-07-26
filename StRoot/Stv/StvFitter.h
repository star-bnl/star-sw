#ifndef __StvFitter_h_
#define __StvFitter_h_
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "TNamed.h"
#include "StvUtil/StvNodePars.h"

class StvHit;
class StvNodePars;
class StvHitErrs;
class StHitPlane;
class StvHitErrCalculator;

class StvFitter : public TNamed {
public:	

StvFitter(const char *name="DefaultFitter");
      void    Set(const StvNodePars *inPars, const StvFitErrs *inErrs
                 ,      StvNodePars *otPars=0,     StvFitErrs *otErrs=0);
      void    Set(const StvNodePars *inPars, const StvFitErrs *inErrs
                 ,const StvNodePars *jnPars, const StvFitErrs *jnErrs
                 ,      StvNodePars *otPars,       StvFitErrs *otErrs);
const double *GetHitErrs() const {return mHitErrs;}        
  void Prep();

double Xi2(const StvHit *hit);
double Xi2();
int  Update();
static StvFitter *Inst() {return mgFitter;}	

private:
int IsTooBig(StvFitPars &fp) const;
private:
int  Jpdate();
int  Vpdate();
static double JoinTwo(int nP1,const double *P1,const double *E1
                     ,int nP2,const double *P2,const double *E2
	             ,              double *PJ,      double *EJ
                     ,int mode=0);	
static void Test();
protected:
      char         mBeg[1];
      int          mKase; 	//0=fit to hit,1=refit,2=fit to vertex
const StvNodePars *mInPars;
const StvFitErrs  *mInErrs;
const StvNodePars *mJnPars;
const StvFitErrs  *mJnErrs;
      StvNodePars *mOtPars;
      StvFitErrs  *mOtErrs;
const StvHit      *mHit;
const StHitPlane  *mHitPlane;
      StvHitErrCalculator *mHitErrCalc;
      StvNodePars  mTkPars;
      StvFitErrs   mTkErrs;
      StvFitPars   mQQPars;
      StvFitErrs   mQQErrs;
      StvFitPars   mDelta;
      double       mHitErrs[3];
      double	   mCos2L,mCosL,mSinL,mCosP,mSinP,mXi2,mDeltaL;
      double       mDcaT,mDcaP,mDcaL;
      double       mDcaFrame[3][3];
      char         mEnd[1];
static StvFitter *mgFitter;
};
#endif //__StvFitter_h_


