#ifndef __StvFitter_h_
#define __StvFitter_h_
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "TNamed.h"
#include "StvNodePars.h"

class StvHit;
class StvNodePars;
class StvHitErrs;
class StHitPlane;
class StvHitErrCalculator;

class StvFitter : public TNamed {
public:	

StvFitter(const char *name="DefaultFitter");
      void    Set(const StvNodePars *inPars, const StvFitErrs *inErrs
          ,             StvNodePars *otPars,       StvFitErrs *otErrs);
const double *GetHitErrs() const {return mHitErrs;}        
  void Prep();

double Xi2(const StvHit *hit);
int  Update();
static StvFitter *Inst() {return mgFitter;}	

private:
static double JoinTwo(int nP1,const double *P1,const double *E1
                     ,int nP2,const double *P2,const double *E2
	             ,              double *PJ,      double *EJ);
	
protected:
      char         mBeg[1];
const StvNodePars *mInPars;
const StvFitErrs  *mInErrs;
      StvNodePars *mOtPars;
      StvFitErrs  *mOtErrs;
const StvHit      *mHit;
const StHitPlane  *mHitPlane;
      StvHitErrCalculator *mHitErrCalc;
      StvNodePars  mTkPars;
      StvFitErrs   mTkErrs;
      double       mHitErrs[3];
      double	   mCos2L,mCosL,mSinL,mCosP,mSinP,mXi2,mDeltaL;
      double       mDcaP,mDcaL,mDist;
      double       mDcaFrame[3][3];
      char         mEnd[1];
static StvFitter *mgFitter;
};
#endif //__StvFitter_h_


