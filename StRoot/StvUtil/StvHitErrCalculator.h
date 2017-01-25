#ifndef __StvHitErrCalculatorulator_h_
#define __StvHitErrCalculatorulator_h_
#include "assert.h"
#include "TNamed.h"
class StvHit;
typedef float Mtx33F_t[3][3];

class StvHitErrCalculator : public TNamed {
public:	
enum {kMaxPars=10};

StvHitErrCalculator(const char *name,int nPars=2);
        void SetPars(const double *par);
        void SetTrack(const double tkDir[3]);
        void SetTrack(const float  tkDir[3]);
virtual  int CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3]);
virtual  int CalcDcaErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3]);
virtual  int CalcDcaErrs(const StvHit *hit,double hRR[3]);
virtual void CalcDetDers(double dRR[kMaxPars][3]);
virtual void CalcDcaDers(double dRR[kMaxPars][3]);
virtual double Trace(const float hiPos[3]);
virtual  int GetNPars() const 			{return mNPar;}
const double *GetPars() const 			{return mPar ;}
      double *GetPars()  			{return mPar ;}
static StvHitErrCalculator *Inst(const char *name);
static void Test(double phiG=33,double lamG=66);
static void Dest(double phiG=33,double lamG=66);
protected:
int CalcLocals(const float hiDir[3][3]);
protected:
enum {kYErr=0,kZErr=1};
enum {kXX=0,kYX=1,kXY=1,kYY=2,kZX=3,kXZ=3,kZY=4,kYZ=4,kZZ=5};

char mBeg[1];
int mFailed;
int mNPar;			//Size of mPar
double mPar[kMaxPars];		// mPar
double mDRr[6];		        // Full hitErr Matrix in detecor system
double mTRr[6];		        // Full hitErr Matrix in track   system
double mTG[3][3];		// track direction in global system
double mTL[3];		// track direction in local hit plane system
double mCp ,mSp ,mCl ,mSl;
double mCp2,mSp2,mCl2,mSl2,mCpCl;
double mTT[3][3]; 	//matrix converting from detector to track(dca) system
double mDD[kMaxPars][6];
char mEnd[1];
ClassDef(StvHitErrCalculator,0)
};

class StvHitRrCalculator : public StvHitErrCalculator {

public:	
  StvHitRrCalculator(const char *name="HitRr"):StvHitErrCalculator(name,0){};
virtual int CalcDcaErrs(const StvHit *stvHit,double hRR[3]);
virtual int CalcDcaErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3]){assert(0); return 999;};
virtual int CalcDetErrs(const float*, const float (*)[3], double*);                       
ClassDef(StvHitRrCalculator,0)
};



class StvTpcHitErrCalculator : public StvHitErrCalculator {

public:	
  StvTpcHitErrCalculator(const char *name="TpcHitErr"):StvHitErrCalculator(name,8){};
virtual int CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3]);
static void Dest(double phiG=33,double lamG=33);

protected:
enum {
kYErr  	=0, 	/* Intrinsic resolution, padrow or Y direction		*/
kZErr  	=1, 	/* Intrinsic resolution, z direction			*/
kThkDet	=2,	/* detector thickness**2 , not fitted			*/
kYYDiff	=3,  	/* Diffusion in XY direction *yFactor			*/
kZZDiff	=4,  	/* Diffusion in Z direction  *ZFactor			*/
kYFact 	=5, 	/*	Error factor in Y-direction 			*/
kZFact 	=6, 	/*	Error factor in Z-direction 			*/
kZAB2  	=7};	/* Constant member in Z direction (a*b)**2		*/
double mZSpan;
ClassDef(StvTpcHitErrCalculator,0)
};


class StvTpcGeoErrCalculator : public StvHitErrCalculator {

public:	
  StvTpcGeoErrCalculator(const char *name="TpcHitGeo"):StvHitErrCalculator(name,7){};
virtual int CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3]);

protected:
enum {kYDiff  =2	//Diffusion in XY direction
     ,kYThkDet=4	//Effective detectot thickness for Y err 
     ,kZDiff  =3	//Diffusion in Z direction
     ,kZThkDet=5	//Effective detectot thickness for Z err
     ,kZAB2   =6	//Constant member in Z direction (a*b)**2
     };
double mZSpan;
ClassDef(StvTpcGeoErrCalculator,0)
};

class StvTpcStiErrCalculator : public StvHitErrCalculator {

public:	
  StvTpcStiErrCalculator(const char *name="StiHitErr"):StvHitErrCalculator(name,6){};
virtual int CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRR[3]);

protected:
enum {kYErr   =0	//Diffusion in XY direction
     ,kYThkDet=1	//Effective detectot thickness**2/12 for Y err 
     ,kYDiff  =2	//Diffusion in Z direction
     ,kZErr   =3	//electronics Z err
     ,kZDiff  =4	//Diffusion in Z direction
     ,kZThkDet=5};	//Effective detectot thickness**2/12 for Z err 
double mZSpan;
/*ClassDef(StvTpcStiErrCalculator,0)*/
};

#endif
