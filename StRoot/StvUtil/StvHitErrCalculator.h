#ifndef __StvHitErrCalculatorulator_h_
#define __StvHitErrCalculatorulator_h_
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
enum {kMaxPars=10};

StvHitErrCalculator(const char *name="");
        void SetPars(const double *par);
        void SetTrack(const double tkDir[3]);
        void SetTrack(const float  tkDir[3]);
virtual void CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3]);
virtual void CalcDcaErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3]);
virtual void CalcDcaDers(double dRR[kMaxPars][3]);
virtual double Trace(const float hiPos[3]);
virtual  int GetNPars() const 			{return 4;}
const double *GetPars() const 			{return mPar ;}
static StvHitErrCalculator *Inst(const char *name);
static void Test(double phiG=33,double lamG=33);
static void Dest(double phiG=33,double lamG=33);
protected:
enum {kYErr=0,kZErr=1,kWidTrk=2,kThkDet=3};
double mPar[kMaxPars];		// mPar[0]=
float mNG[3][3];
float mCp,mSp,mCl,mSl;
float mTT[2][2],mDD[4][3];
ClassDef(StvHitErrCalculator,0)
};

class StvTpcHitErrCalculator : public StvHitErrCalculator {

public:	
  StvTpcHitErrCalculator(const char *name="TpcHitErr"):StvHitErrCalculator(name){};
virtual void CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3]);
virtual void CalcDcaErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3]);
virtual void CalcDcaDers(double dRR[kMaxPars][3]);
virtual double Trace(const float hiPos[3]);
virtual  int GetNPars() const 			{return 6;}
static void Dest(double phiG=33,double lamG=33);

protected:
enum {kYDiff=4,kZDiff=5};
  float mZSpan;
ClassDef(StvTpcHitErrCalculator,0)
};
#endif
