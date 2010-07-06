#ifndef __StvHitErrCalculator_h_
#define __StvHitErrCalculator_h_
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "TNamed.h"

class StvHit;
class StvNodePars;
class StvHitErrs;
class StvHitErrCalculator : public TNamed {
public:	

StvHitErrCalculator(const char *name="DefaultHitErr");
virtual void CalcDetErrs(const StvHit *hit, const StvNodePars *node, StvHitErrs *hrr);
virtual void CalcDCAErrs(const StvHit *hit, const StvNodePars *node, StvHitErrs *hrr);
        void SetPars(const double *par,int nPar);
static void Test(double phiG=0,double lamG=0);
protected:
enum {kMaxPars=10};
enum {kYErr=0,kZErr=1,kThkDet=2,kWidTrk=3};
int mNPar;
double mPar[kMaxPars];		// mPar[0]=
float mCp,mSp,mCl,mSl;
ClassDef(StvHitErrCalculator,0)
};

class StvTpcHitErrCalculator : public StvHitErrCalculator {

public:	
StvTpcHitErrCalculator(const char *name="TpcHitErr"):StvHitErrCalculator(name){};
virtual void CalcDetErrs(const StvHit *hit, const StvNodePars *node, StvHitErrs *hrr);
virtual void CalcDCAErrs(const StvHit *hit, const StvNodePars *node, StvHitErrs *hrr);

protected:
enum {kYDiff=4,kZDiff=5};
ClassDef(StvTpcHitErrCalculator,0)
};
#endif
