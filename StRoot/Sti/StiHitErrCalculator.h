#ifndef __StiHitErrCalculator_h_
#define __StiHitErrCalculator_h_
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <math.h>
#include "TNamed.h"

class StiHit;
class StiNodePars;
class StiHitErrs;
class StiHitErrCalculator : public TNamed {
public:	

StiHitErrCalculator(const char *name="DefaultHitErr");
virtual void CalcDetErrs(const StiHit *hit, const StiNodePars *node, StiHitErrs *hrr);
virtual void CalcDCAErrs(const StiHit *hit, const StiNodePars *node, StiHitErrs *hrr);
        void SetPars(const double *par,int nPar);
static void Test(double phiG=0,double lamG=0);
protected:
enum {kMaxPars=10};
enum {kYErr=0,kZErr=1,kThkDet=2,kWidTrk=3};
int mNPar;
double mPar[kMaxPars];		// mPar[0]=
float mCp,mSp,mCl,mSl;
ClassDef(StiHitErrCalculator,0)
};

class StiTpcHitErrCalculator : public StiHitErrCalculator {

public:	
StiTpcHitErrCalculator(const char *name="TpcHitErr"):StiHitErrCalculator(name){};
virtual void CalcDetErrs(const StiHit *hit, const StiNodePars *node, StiHitErrs *hrr);
virtual void CalcDCAErrs(const StiHit *hit, const StiNodePars *node, StiHitErrs *hrr);

protected:
enum {kYDiff=4,kZDiff=5};
ClassDef(StiTpcHitErrCalculator,0)
};
#endif
