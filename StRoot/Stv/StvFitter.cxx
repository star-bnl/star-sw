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
  mInPars = inPars; mInErrs = inErrs;
  mOtPars = otPars; mOtErrs = otErrs;
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
  mHitPlane = mHit->detector();
  mHitErrCalc = (StvHitErrCalculator*)mHitPlane->GetHitErrCalc();
  assert(mHitErrCalc);

//	restore old parameters for nhits>1  
  mTkPars._x = mInPars->_x; mTkPars._y = mInPars->_y; mTkPars._z = mInPars->_z;

//		Hit position
  const float *hP = mHit->x_g();

//		Track direction
  double *tD = mDcaFrame[0];
//		Start track position
  double *tP = &mTkPars._x;


//		Distance to DCA along track in xy
  mDeltaL = DDOT(hP,tP,tD);  

//		DCA track position
  VADD(tP,mDeltaL*tD);
  double tkDir[3]={mTkPars._cosCA,mTkPars._sinCA,mTkPars._tanl};
  mHitErrCalc->SetTrack(tkDir);
  const StHitPlane *hp = hit->detector(); 
  const Mtx33F_t &hD = hp->GetDir(hit->x_g());
  mHitErrCalc->CalcDcaErrs(hit->x_g(),hD,mHitErrs);


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
int StvFitter::Update()
{
static int nCall=0; nCall++;
//const double *Nt = mDcaFrame[0];
  const double *Np = mDcaFrame[1];
  const double *Nl = mDcaFrame[2];

		
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

  double befXi2 = MyXi2(myHitErrs.Arr(),myHitPars.mH,myHitPars.mZ);
  double aftXi2 = MyXi2(myHitErrs.Arr(),myJrkPars.mH-myHitPars.mH
                                       ,myJrkPars.mZ-myHitPars.mZ);
  assert(befXi2>aftXi2);

  *mOtPars = mTkPars;
  for (int i=0;i<3;i++) {
   (&(mOtPars->_x))[i] += Np[i]*myJrkPars.mH +Nl[i]*myJrkPars.mZ;} 
  double dA = myJrkPars.mA, dL = myJrkPars.mL;
  mOtPars->_psi   += dA;
  mOtPars->_ptin  += myJrkPars.mC;
//   mOtPars->_cosCA -= mOtPars->_sinCA*dA;
//   mOtPars->_sinCA += mOtPars->_cosCA*dA;
  mOtPars->ready();
  if (mOtPars->_tanl < 1) { mOtPars->_tanl = (mSinL+mCosL*dL)/(mCosL-mSinL*dL);}
  else 		  	  { mOtPars->_tanl = tan(atan(mOtPars->_tanl)+dL);}

  mOtPars->_curv   = mOtPars->_hz * mOtPars->_ptin;

  mOtPars->move(-mDeltaL*mCosL);

  return 0;
}  
  
#if 1
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
  double *e1sumEIe1 	= (a+=nE2);
  double *subP 		= (a+=nE2);
  double *sumEIsubP	= (a+=nE2);
  double chi2=3e33;

// Choose the smalest errors
  const double *p1 = P2, *p2 = P1, *e1 = E2, *e2 = E1;

  do {//empty loop
//  	Join errors
    TCL::vadd(e1,e2,sumE,nE1);
    TCL::trsinv(sumE,sumEI,nP1);
    TCL::vsub  (p2  ,p1   ,subP   ,nP1);
    TCL::trasat(subP,sumEI,&chi2,1,nP1); 
    if (!EJ) break;

    TCL::trqsq (e1  ,sumEI,e1sumEIe1,nP2); 
    TCL::vsub(e1,e1sumEIe1,EJ,nE2);
  } while(0);
//  	Join params
  if (PJ) {
    TCL::tras(subP     ,sumEI,sumEIsubP,1,nP1);
    TCL::tras(sumEIsubP,e1   ,PJ       ,1,nP2);
    TCL::vadd(PJ       ,p1   ,PJ         ,nP2);
  }
  return chi2;
}
#endif
#if 0
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
  double *e1sumEIe1 	= (a+=nE2);
  double *subP 		= (a+=nE2);
  double *sumEIsubP	= (a+=nE2);
  double chi2=3e33,p,q;

// Choose the smalest errors
  const double *p1 = P1, *p2 = P2, *e1 = E1, *e2 = E2, *t;
  double choice = (nP1==nP2)? 0:1;
  if (!choice   ) {
    for (int i=0,n=1;i<nE2;i+=(++n)) {
    p=fabs(e1[i]);q=fabs(e2[i]);choice += (p-q)/(p+q+1e-10);
  }}
  if ( choice >0) {t = p2; p2 = p1; p1 = t; t = e2; e2 = e1; e1 = t;}

  do {//empty loop
//  	Join errors
    TCL::vadd(e1,e2,sumE,nE1);
    int negati = sumE[2]<0;
    if (negati) TCL::vcopyn(sumE,sumE,nE1);
    int ign0re = sumE[0]<=0;
    if (ign0re) sumE[0] = 1;
    TCL::trsinv(sumE,sumEI,nP1);
    if (ign0re) {sumE[0]  = 0; sumEI[0] = 0;}
    if (negati) {TCL::vcopyn(sumE,sumE,nE1);TCL::vcopyn(sumEI,sumEI,nE1);}
    TCL::vsub(p2       ,p1   ,subP       ,nP1);
    TCL::trasat(subP,sumEI,&chi2,1,nP1); 
    if (!EJ) break;
    TCL::trqsq (e1  ,sumEI,e1sumEIe1,nP2); 
    TCL::vsub(e1,e1sumEIe1,EJ,nE2);
  } while(0);
//  	Join params
  if (PJ) {
    TCL::tras(subP     ,sumEI,sumEIsubP,1,nP1);
    TCL::tras(sumEIsubP,e1   ,PJ       ,1,nP2);
    TCL::vadd(PJ       ,p1   ,PJ         ,nP2);
  }
  return chi2;
}
#endif
