#include <stdio.h>
#include <stdlib.h>
#include <assert.h>
#include "TMath.h"
#include "TCernLib.h"
#include "StiFitter.h"
#include "StiNodePars.h"
#include "StiHit.h"
#include "StiHitErrCalculator.h"
#include "StarVMC/GeoTestMaker/StTGeoHelper.h"

StiFitter *StiFitter::mgFitter=0;
#define VDOT(a,b)   ( a[0]*b[0]+a[1]*b[1]+a[2]*b[2])
#define DIST(a,b)   ((a[0]-b[0])*(a[0]-b[0])+(a[1]-b[1])*(a[1]-b[1])+(a[2]-b[2])*(a[2]-b[2]))
#define DDOT(a,b,c) ((a[0]-b[0])*c[0]+(a[1]-b[1])*c[1]+(a[2]-b[2])*c[2])
#define VADD(a,b)   { a[0]+=b[0];a[1]+=b[1];a[2]+=b[2];}



//______________________________________________________________________________
StiFitter::StiFitter(const char *name):TNamed(name,"")
{
  memset(mBeg,0,mEnd-mBeg+1);
  assert(!mgFitter);
  mgFitter = this;
}
//______________________________________________________________________________
void StiFitter::Set(const StiNodePars *inPars, const StiFitErrs *inErrs
                   ,      StiNodePars *otPars,       StiFitErrs *otErrs)
{
  memset(mBeg,'@',mEnd-mBeg+1);
  mInPars = inPars; mInErrs = inErrs;
  mOtPars = otPars; mOtErrs = otErrs;
}
//______________________________________________________________________________
void StiFitter::Prep()
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
double StiFitter::Xi2(const StiHit *hit)
{
  if (mHit == hit) return mXi2;
  mHit = hit;
  mHitPlane = mHit->detector();
  mHitErrCalc = (StiHitErrCalculator*)mHitPlane->GetHitErrCalc();
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

  mHitErrCalc->CalcDCAErrs(mHit, &mTkPars,&mHitErrs);


//		Hit position wrt track 
  double dca[3] = {hP[0]-tP[0],hP[1]-tP[1],hP[2]-tP[2]};
  double dis2=VDOT(dca,dca); mDist = sqrt(dis2);

  mDcaP=VDOT(mDcaFrame[1],dca);
  mDcaL=VDOT(mDcaFrame[2],dca);

  double G[3] = {mHitErrs.hYY+mInErrs->mHH
                ,mHitErrs.hYZ+mInErrs->mHZ*mCosL
                ,mHitErrs.hZZ+mInErrs->mZZ*mCos2L};
  double Gdet = G[0]*G[2]-G[1]*G[1];
//  (BB*dX*dX-2*BA*dX*dY+AAdY*dY)/det 
   
  mXi2 = (G[2]*mDcaP*mDcaP-2*G[1]*mDcaP*mDcaL+G[0]*mDcaL*mDcaL)/Gdet;
  return mXi2 ; 
}  
  
//______________________________________________________________________________
int StiFitter::Update()
{

//const double *Nt = mDcaFrame[0];
  const double *Np = mDcaFrame[1];
  const double *Nl = mDcaFrame[2];

		
  mTkErrs = *mInErrs;

//		New Z ortogonal to X (track direction)
static const StiFitErrs &Q = mTkErrs;	
static const int idx[]={&Q.mHZ-&Q.mHH,&Q.mZZ-&Q.mHH,&Q.mZZ-&Q.mHH
                       ,&Q.mZA-&Q.mHH,&Q.mZL-&Q.mHH,&Q.mZC-&Q.mHH,-1};
  double *d = mTkErrs.Arr();
  for (int j=0;idx[j]>=0;j++) {*(d+idx[j]) *= mCosL;}
  StiFitPars myHitPars(mDcaP, mDcaL );
  StiFitErrs myHitErrs(mHitErrs.hYY,mHitErrs.hYZ,mHitErrs.hZZ);
  StiFitPars myTrkPars;
  StiFitPars myJrkPars;

  double myXi2 = JoinTwo(2,myHitPars.Arr(),myHitErrs.Arr()
                        ,5,myTrkPars.Arr(),mTkErrs.Arr()
		        ,  myJrkPars.Arr(),mOtErrs->Arr());
  assert(fabs(myXi2-mXi2)<0.01*(myXi2+mXi2));

  d = mOtErrs->Arr();
  for (int j=0;idx[j]>=0;j++) {*(d+idx[j]) /= mCosL;}

  *mOtPars = mTkPars;
  for (int i=0;i<3;i++) {
   (&(mOtPars->_x))[i] += Np[i]*myJrkPars.mH +Nl[i]*myJrkPars.mZ;} 
  double dA = myJrkPars.mA, dL = myJrkPars.mL;
  mOtPars->_psi   += dA;
  mOtPars->_ptin  += myJrkPars.mC;
//   mOtPars->_cosCA -= mOtPars->_sinCA*dA;
//   mOtPars->_sinCA += mOtPars->_cosCA*dA;
  mOtPars->ready();
  if (mOtPars->_tanl < 1) { mOtPars->_tanl+= dL/mCos2L;}
  else 		  	  { mOtPars->_tanl = tan(atan(mOtPars->_tanl)+dL);}

  mOtPars->_curv   = mOtPars->_hz * mOtPars->_ptin;
  double myDist = DIST((mHit->x_g()),(&mOtPars->_x)); myDist=sqrt(myDist);
  if (myDist>mDist) printf("StiFitter::Update *** %g > %g ***\n",myDist,mDist);
//  assert(myDist<mDist);

  mOtPars->move(-mDeltaL*mCosL);

  
  return 0;
}  
  
//______________________________________________________________________________
double StiFitter::JoinTwo(int nP1,const double *P1,const double *E1
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

