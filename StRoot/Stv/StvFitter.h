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
enum E_Failed {kBigVari=-1,kBigErrs=-99};

public:	
StvFitter(const char *name="DefaultFitter");
      void    Set(int idir) 		{ mDir = idir; }
      void    Set(const StvNodePars *inPars, const StvFitErrs *inErrs
                 ,      StvNodePars *otPars=0,     StvFitErrs *otErrs=0);
      void    Set(const StvNodePars *inPars, const StvFitErrs *inErrs
                 ,const StvNodePars *jnPars, const StvFitErrs *jnErrs
                 ,      StvNodePars *otPars,       StvFitErrs *otErrs);
         int  IsFailed() const 		{return mFailed;}
const double *GetHitErrs() const 	{return mHitErrs;}        
  void Prep();

double Xi2(const StvHit *hit);	//Xi2 for hit or vertex
double Xi2();			//Xi2 for 2 subtracks joining
double Xi2Join(const StvHit *hit);	//Xi2 for 5nd line+hit 
double GetXi2()			{return mXi2;}	//evaluated Xi2
int  Update();
static StvFitter *Inst() {return mgFitter;}	

private:
double TooBig(StvFitPars &fp, int *mask) const;
private:
int  Hpdate();		//Update Hit fit
int  Jpdate();		//Updatejoin fit
int  Vpdate();		//Update vertex fit
// static double JoinTwo(int nP1,const double *P1,const double *E1
//                      ,int nP2,const double *P2,const double *E2
// 	             ,              double *PJ,      double *EJ
//                      ,int mode=0);	
static void Test();
protected:
      char         mBeg[1];
      char         mFailed; 	//Fail flag. Something completely wrong
      char         mKase; 	//0=fit to hit,1=refit,2=fit to vertex
      char         mDir;	//Direction of moving (0=Outside==>inside)
const StvNodePars *mInPars;	//1st input params
const StvFitErrs  *mInErrs;	//1st input params errors
      StvNodePars *mOtPars;	//Output params
      StvFitErrs  *mOtErrs;	//Output errors
const StvHit      *mHit;
const StHitPlane  *mHitPlane;
      StvHitErrCalculator *mHitErrCalc;
      StvFitPars   mQQPars;
      StvFitErrs   mQQErrs;
      StvFitPars   mDelta;	//typical deltas for parameters in current env
      double       mHitErrs[3];
      double	   mXi2,mDeltaL;
      double       mDcaT,mDcaP,mDcaL;
      double       mDcaFrame[3][3];
      char         mEnd[1];
      StvNodePars *mJnPars;	//2nd input params
      StvFitErrs  *mJnErrs;	//2nd input params errors
static StvFitter *mgFitter;
};
#endif //__StvFitter_h_

class TCLx 
{
public:
static int trsinv2x2(const double *pM,double *pMi);
static int trsinv3x3(const double *pM,double *pMi);
static int trsinv4x4(const double *pM,double *pMi);
static int trsinv5x5(const double *pM,double *pMi);
static double sign(const double *a,int n);
static void Test();
};

