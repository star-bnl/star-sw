#ifndef __StiFitter_h_
#define __StiFitter_h_
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "TNamed.h"
#include "StiNodePars.h"

class StiHit;
class StiNodePars;
class StiHitErrs;
class StHitPlane;
class StiHitErrCalculator;

class StiFitter : public TNamed {
public:	

StiFitter(const char *name="DefaultFitter");
  void Set(const StiNodePars *inPars, const StiFitErrs *inErrs
          ,      StiNodePars *otPars,       StiFitErrs *otErrs);
  void Prep();

double Xi2(const StiHit *hit);
int  Update();
static StiFitter *Inst() {return mgFitter;}	

private:
static double JoinTwo(int nP1,const double *P1,const double *E1
                     ,int nP2,const double *P2,const double *E2
	             ,              double *PJ,      double *EJ);
	
protected:
      char         mBeg[1];
const StiNodePars *mInPars;
const StiFitErrs  *mInErrs;
      StiNodePars *mOtPars;
      StiFitErrs  *mOtErrs;
const StiHit      *mHit;
const StHitPlane  *mHitPlane;
      StiHitErrCalculator *mHitErrCalc;
      StiNodePars  mTkPars;
      StiFitErrs   mTkErrs;
      StiHitErrs   mHitErrs;
      double	   mCos2L,mCosL,mSinL,mCosP,mSinP,mXi2,mDeltaL;
      double       mDcaP,mDcaL,mDist;
      double       mDcaFrame[3][3];
      char         mEnd[1];
static StiFitter *mgFitter;
};
#endif //__StiFitter_h_


