#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TMath.h"
#include "TCernLib.h"
#include "StvFitter.h"
#include "StvUtil/StvNodePars.h"
#include "StvHit.h"
#include "StvUtil/StvHitErrCalculator.h"
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"

StvFitter *StvFitter::mgFitter=0;
#define VDOT(a,b)   ( a[0]*b[0]+a[1]*b[1]+a[2]*b[2])
#define DIST(a,b)   ((a[0]-b[0])*(a[0]-b[0])+(a[1]-b[1])*(a[1]-b[1])+(a[2]-b[2])*(a[2]-b[2]))
#define DDOT(a,b,c) ((a[0]-b[0])*c[0]+(a[1]-b[1])*c[1]+(a[2]-b[2])*c[2])
#define VADD(a,b)   { a[0]+=b[0];a[1]+=b[1];a[2]+=b[2];}

static inline double MyXi2(const double G[3],double dA,double dB)  
{
  double Gdet = G[0]*G[2]-G[1]*G[1];
  return  (G[2]*dA*dA-2*G[1]*dA*dB+G[0]*dB*dB)/Gdet;
}


//______________________________________________________________________________
StvFitter::StvFitter(const char *name):TNamed(name,"")
{
  memset(mBeg,0,mEnd-mBeg+1);
  assert(!mgFitter);
  mgFitter = this;
}
//______________________________________________________________________________
void StvFitter::Set(const StvNodePars *inPars, const StvFitErrs *inErrs
                   ,      StvNodePars *otPars,       StvFitErrs *otErrs)
{
  memset(mBeg,'@',mEnd-mBeg+1);
  mKase = 0;
  mInPars = inPars; mInErrs = inErrs;
  mOtPars = otPars; mOtErrs = otErrs;
  mJnPars =      0; mJnErrs =      0;
}
//______________________________________________________________________________
void StvFitter::Set(const StvNodePars *inPars, const StvFitErrs *inErrs
                   ,const StvNodePars *jnPars, const StvFitErrs *jnErrs
                   ,      StvNodePars *otPars,       StvFitErrs *otErrs)
{
  memset(mBeg,'@',mEnd-mBeg+1);
  mKase = 1;
  mInPars = inPars; mInErrs = inErrs;
  mOtPars = otPars; mOtErrs = otErrs;
  mJnPars = jnPars; mJnErrs = jnErrs;
}
//______________________________________________________________________________
void StvFitter::Prep()
{
  mHit   = 0; mHitPlane = 0;
  double myTan = mInPars->_tanl;
  mCos2L = 1./(1+myTan*myTan);
  mCosL = sqrt(mCos2L);
  mSinL = myTan*mCosL;
  mCosP = mInPars->_cosCA;
  mSinP = mInPars->_sinCA;

  mTkPars = *mInPars;
//		Track Frame
  mDcaFrame[0][0] =  mCosL*mCosP;
  mDcaFrame[0][1] =  mCosL*mSinP;
  mDcaFrame[0][2] =  mSinL;

  mDcaFrame[1][0] = -mSinP;
  mDcaFrame[1][1] =  mCosP;
  mDcaFrame[1][2] =  0;

  mDcaFrame[2][0] = -mSinL*mCosP;
  mDcaFrame[2][1] = -mSinL*mSinP;
  mDcaFrame[2][2] =  mCosL;

}
//______________________________________________________________________________
double StvFitter::Xi2(const StvHit *hit)
{
  if (mHit == hit) return mXi2;
  mHit = hit;
  const float *errMtx=mHit->errMtx();
  if (errMtx) mKase=2;

  mHitPlane = mHit->detector();

//	restore old parameters for nhits>1  
  mTkPars._x = mInPars->_x; mTkPars._y = mInPars->_y; mTkPars._z = mInPars->_z;

//		Hit position
  const float *hP = mHit->x();

//		Track direction
  double *tD = mDcaFrame[0];
//		Start track position
  double *tP = &mTkPars._x;


//		Distance to DCA along track in xy
//mDeltaL = DDOT(hP,tP,tD);  
//		DCA track position
  switch (mKase) {
    case 0: {
    mHitErrCalc = (StvHitErrCalculator*)mHitPlane->GetHitErrCalc();
    assert(mHitErrCalc);
    mHitErrCalc->SetTrack(tD);
    const StHitPlane *hp = hit->detector(); 
    const Mtx33F_t &hD = hp->GetDir(hit->x());
    mHitErrCalc->CalcDcaErrs(hit->x(),hD,mHitErrs);
    }; break;
    case 1: assert(0 && "Wrong case 1");

    case 2: {
      double d[6]={errMtx[0],errMtx[1],errMtx[2]
                  ,errMtx[3],errMtx[4],errMtx[5]};
      TCL::trasat(mDcaFrame[1],d,mHitErrs,2,3); }
  }
  assert(mHitErrs[0]>0);
  assert(mHitErrs[2]>0);
  assert(mHitErrs[2]*mHitErrs[0]>mHitErrs[1]*mHitErrs[1]);

//		Hit position wrt track 
  double dca[3] = {hP[0]-tP[0],hP[1]-tP[1],hP[2]-tP[2]};
  double dis2=VDOT(dca,dca); mDist = sqrt(dis2);

  mDcaP=VDOT(mDcaFrame[1],dca);
  mDcaL=VDOT(mDcaFrame[2],dca);

  double G[3] = {mHitErrs[0]+mInErrs->mHH
                ,mHitErrs[1]+mInErrs->mHZ
                ,mHitErrs[2]+mInErrs->mZZ};
//  (BB*dX*dX-2*BA*dX*dY+AAdY*dY)/det 
  mXi2 = MyXi2(G,mDcaP,mDcaL);
  return mXi2 ; 
}  
//______________________________________________________________________________
double StvFitter::Xi2()
{
  StvFitPars F   = (*mInPars-*mJnPars);
  double     Zero[5]= {0};
  double myXi2 = JoinTwo(5,F.Arr()    ,mInErrs->Arr()
                        ,5,Zero       ,mJnErrs->Arr()
		        ,mQQPars.Arr(),mQQErrs.Arr());
  return myXi2;
}  
//______________________________________________________________________________
int StvFitter::Update()
{
static int nCall=0; nCall++;

  switch (mKase) {
    case 0: break;
    case 1: return Jpdate();
    case 2: return Vpdate();
  }
		
  mTkErrs = *mInErrs;


//		New Z ortogonal to X (track direction)
  StvFitPars myHitPars(mDcaP, mDcaL );
  StvFitErrs myHitErrs(mHitErrs[0],mHitErrs[1],mHitErrs[2]);
  StvFitPars myTrkPars;
  StvFitPars myJrkPars;
  assert(mTkErrs.Sign()>0);

  double myXi2 = JoinTwo(2,myHitPars.Arr(),myHitErrs.Arr()
                        ,5,myTrkPars.Arr(),mTkErrs.Arr()
		        ,  myJrkPars.Arr(),mOtErrs->Arr());
  assert(fabs(myXi2-mXi2)<0.01*(myXi2+mXi2));

  *mOtPars = mTkPars;
  *mOtPars+= myJrkPars;
  mOtErrs->SetHz(mOtPars->_hz);

  return 0;
}  
//______________________________________________________________________________
int StvFitter::Vpdate()
{
static int nCall=0; nCall++;

  mTkErrs = *mInErrs;
//???????????????  for (int i=0;i<3;i++) {mTkErrs[i]+=mHitErrs[i];}

//		New Z ortogonal to X (track direction)
  StvFitPars myHitPars(mDcaP, mDcaL );
  StvFitErrs myHitErrs(0,0,0);
  StvFitPars myTrkPars;
  StvFitPars myJrkPars;
  assert(mTkErrs.Sign()>0);

  double myXi2 = JoinTwo(2,myHitPars.Arr(),myHitErrs.Arr()
                        ,5,myTrkPars.Arr(),mTkErrs.Arr()
		        ,  myJrkPars.Arr(),mOtErrs->Arr());
//??????  assert(fabs(myXi2-mXi2)<0.01*(myXi2+mXi2));
  assert(fabs(myJrkPars[0]-myHitPars[0])<1e-6);
  assert(fabs(myJrkPars[1]-myHitPars[1])<1e-6);
  *mOtPars = mTkPars;
  *mOtPars+= myJrkPars;
  mOtErrs->SetHz(mOtPars->_hz);
  return 0;
}  
//______________________________________________________________________________
int StvFitter::Jpdate()
{
  *mOtPars = *mJnPars; (*mOtPars)+=mQQPars;
  assert(!mOtPars->check());
  *mOtErrs =  mQQErrs;   
   mOtErrs->SetHz(mOtPars->_hz);
  return 0;
}
//______________________________________________________________________________
double StvFitter::JoinTwo(int nP1,const double *P1,const double *E1
                         ,int nP2,const double *P2,const double *E2
	                 ,              double *PJ,      double *EJ)
{

  assert(nP1<=nP2);
  int nE1 = nP1*(nP1+1)/2;
  int nE2 = nP2*(nP2+1)/2;
  TArrayD ard(nE2*6);
  double *a = ard.GetArray();  
  double *sumE 		= (a);
  double *sumEI 	= (a+=nE2);
  double *e2sumEIe2 	= (a+=nE2);
  double *subP 		= (a+=nE2);
  double *sumEIsubP	= (a+=nE2);
  double chi2=3e33;


  do {//empty loop
//  	Join errors
    TCL::vadd(E2,E1,sumE,nE1);
    TCL::trsinv(sumE,sumEI,nP1);
    TCL::vsub  (P1  ,P2   ,subP   ,nP1);
    TCL::trasat(subP,sumEI,&chi2,1,nP1); 
    if (!EJ) break;

    TCL::trqsq (E2  ,sumEI,e2sumEIe2,nP2); 
    TCL::vsub(E2,e2sumEIe2,EJ,nE2);
  } while(0);
//  	Join params
  if (PJ) {
    TCL::tras(subP     ,sumEI,sumEIsubP,1,nP1);
    TCL::tras(sumEIsubP,E2   ,PJ       ,1,nP2);
    TCL::vadd(PJ       ,P2   ,PJ         ,nP2);
  }
  return chi2;
}
