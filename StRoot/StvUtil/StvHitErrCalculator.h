#ifndef __StvHitErrCalculatorulator_h_
#define __StvHitErrCalculatorulator_h_
#include "TNamed.h"

class StvHitErrCalculator : public TNamed {
public:	
enum {kMaxPars=10};

StvHitErrCalculator(const char *name,int nPars=2);
        void SetPars(const double *par);
        void SetTrack(const double tkDir[3]);
        void SetTrack(const float  tkDir[3]);
virtual void CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3]);
virtual void CalcDcaErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3]);
virtual void CalcDcaDers(double dRR[kMaxPars][3]);
virtual double Trace(const float hiPos[3]);
virtual  int GetNPars() const 			{return mNPar;}
const double *GetPars() const 			{return mPar  ;}
static StvHitErrCalculator *Inst(const char *name);
protected:
void CalcLocals(const float hiDir[3][3]);
protected:
enum {kYErr=0,kZErr=1,kWidTrk=2};
char mBeg[1];
int mNPar;			//Size of mPar
int mFailed;
double mPar[kMaxPars];		// mPar
double mTG[3];		// track direction in global system
double mTL[3];		// track direction in local hit plane system
double mCp ,mSp ,mCl ,mSl;
double mCp2,mSp2,mCl2,mSl2;
double mTT[2][2]; 	//matrix converting from detector to track(dca) system
double mDD[kMaxPars][3];
char mEnd[1];
ClassDef(StvHitErrCalculator,0)
};

class StvTpcHitErrCalculator : public StvHitErrCalculator {

public:	
  StvTpcHitErrCalculator(const char *name="TpcHitErr"):StvHitErrCalculator(name,7){};
virtual void CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3]);
virtual double Trace(const float hiPos[3]);
static void Dest(double phiG=33,double lamG=33);

protected:
enum {kYDiff  =2	//Diffusion in XY direction
     ,kYThkDet=4	//Effective detectot thickness for Y err 
     ,kZDiff  =3	//Diffusion in Z direction
     ,kZThkDet=5	//Effective detectot thickness for Z err
     ,kZAB2   =6	//Constant member in Z direction (a*b)**2
     };
double mZSpan;
ClassDef(StvTpcHitErrCalculator,0)
};
#endif
