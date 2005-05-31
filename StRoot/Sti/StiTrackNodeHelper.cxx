#include <stdio.h>
#include <stdlib.h>
#include "StiTrackNodeHelper.h"
#include "TCL.h"

#define NICE(a) ( ((a) <= -M_PI)? ((a)+2*M_PI) :\
                  ((a) >   M_PI)? ((a)-2*M_PI) : (a))

#define sinX(a) StiTrackNode::sinX(a)
//______________________________________________________________________________
StiTrackNodeHelper::StiTrackNodeHelper(double chi2Max)
{
  memset(this,'A',sizeof(*this));
  mChi2Max = chi2Max;
}
//______________________________________________________________________________
int StiTrackNodeHelper::propagate(StiNodePars *ipars)
{
  if (ipars) mAP = *ipars;
  mBP = mAP;
  alpha = mSNode->_alpha - mPNode->_alpha;
  ca=1;sa=0;
  if (fabs(alpha) > 1.e-6) { //rotation part

    double xt1=mAP._x; 
    double yt1=mAP._y; 
    double cosCA0 = mAP._cosCA;
    double sinCA0 = mAP._sinCA;

    ca = cos(alpha);
    sa = sin(alpha);

    mBP._x = xt1*ca + yt1*sa;
    mBP._y= -xt1*sa + yt1*ca;
    mBP._cosCA =  cosCA0*ca+sinCA0*sa;
    mBP._sinCA = -cosCA0*sa+sinCA0*ca;
    double nor = 0.5*(mBP._sinCA*mBP._sinCA+mBP._cosCA*mBP._cosCA +1);
    mBP._cosCA /= nor;
    mBP._sinCA /= nor;
    mBP._eta= NICE(mAP._eta-alpha); 
  }// end of rotation part

//  	Propagation 
  x1 = mBP._x;
  x2 = mSNode->mFP._x;
  dx = x2-x1;
  rho = mBP._curv;
  dsin = rho*dx;
  sinCA2=mBP._sinCA + dsin; 
  if (sinCA2> 0.95) sinCA2= 0.95;
  if (sinCA2<-0.95) sinCA2=-0.95;
  cosCA2 = ::sqrt((1.-sinCA2)*(1.+sinCA2));
  sumSin   = mBP._sinCA+sinCA2;
  sumCos   = mBP._cosCA+cosCA2;
  dy = dx*(sumSin/sumCos);
  y2 = mBP._y+dy;
  dl0 = mBP._cosCA*dx+mBP._sinCA*dy;
  sind = dl0*rho;
  if (fabs(dsin) < 0.02 && mBP._cosCA >0) { //tiny angle
    dl = dl0*(1.+sind*sind/6);
  } else {
    double cosd = cosCA2*mBP._cosCA+sinCA2*mBP._sinCA;
    dl = atan2(sind,cosd)/rho;
  }

  mCP._x = x2;
  mCP._y = y2;
  mCP._z= mBP._z + dl*mBP._tanl;
  mCP._eta = (mBP._eta+rho*dl);  					
  mCP._eta = NICE(mCP._eta);  					
  mCP._curv = mBP._curv;
  mCP._tanl = mBP._tanl;
  mCP._sinCA   = sinCA2;
  mCP._cosCA   = cosCA2;
  return propagateMtx();
} 
//______________________________________________________________________________
int StiTrackNodeHelper::propagateMtx()
{
//  	fYE == dY/dEta
  double fYE= dx*(1.+mBP._cosCA*cosCA2+mBP._sinCA*sinCA2)/(sumCos*cosCA2);
//	fEC == dEta/dRho
  double fEC = dx/cosCA2;
//	fYC == dY/dRho
  double fYC=(dl0)/sumCos*fEC;
//	fZE == dZ/dEta
  double dLdEta = dy/cosCA2;
  double fZE =  mCP._tanl*dLdEta;

// 	fZC == dZ/dRho
  double dang = dl*rho;
  double C2LDX = dl*dl*(
               0.5*sinCA2*pow((1+pow(dang/2,2)*sinX(dang/2)),2) +
                   cosCA2*dang*sinX(dang));

  double fZC = mCP._tanl*C2LDX/cosCA2;

//  	fZT == dZ/dTanL; 
  double fZT= dl; 

  
  mMtx.reset();
//  X related derivatives
  mMtx.A[0][0] = -1;
  mMtx.A[1][0] = -sinCA2/cosCA2; 
  mMtx.A[2][0] = -mCP._tanl/cosCA2 ;
  mMtx.A[3][0] = -mCP._curv/cosCA2 ;       ;

  mMtx.A[1][3]=fYE; mMtx.A[1][4]=fYC; mMtx.A[2][3]=fZE;
  mMtx.A[2][4]=fZC; mMtx.A[2][5]=fZT; mMtx.A[3][4]=fEC;
  double fYX = mMtx.A[1][0]; 
  mMtx.A[1][0] = fYX*ca-sa;
  mMtx.A[1][1] = fYX*sa+ca-1;
  return 0;
}
//______________________________________________________________________________
int StiTrackNodeHelper::propagateError(StiNodeErrs &lastFE)
{
  mPE = lastFE;
  mFE = mPE;
  StiTrackNode::errPropag6(mPE.A,mMtx.A,kNPars);
  StiNodeErrs fe = mSNode->mFE;
  mSNode->mFE = mPE;
  const StiDetector *detS = mSNode->getDetector();
  StiNodePars savePars = mSNode->mFP;
  if (detS)  mSNode->propagateMCS(mPNode,detS);
  mSNode->mFP = savePars;
  mSNode->mPP = savePars;
  mPE = mSNode->mFE;
  mFE = mPE;
  mSNode->mFE=fe;
  return 0;
}

//______________________________________________________________________________
int StiTrackNodeHelper::fake1Fit()
{
  int ians=0;
  if (!mPNode) {
    mSNode->resetError(0.);
    mBigE = mSNode->mFE;
  } else {
    propagate(&mPNode->mFP);
    propagateError(mPNode->mFE);
    mSNode->mFE = mFE;
    mSNode->mPE = mFE;
  }
  StiHit *hit = mSNode->getHit();  
  if (!hit) 	return 0;
  double chi2 = mSNode->getChi2();
  if (chi2>1e3) return 0;
  StiNodePars savePars = mSNode->mFP;
  int state = mSNode->_state;
  mSNode->updateNode();
  mSNode->_state=state;
  mSNode->mFP = savePars;
  return ians;
}  
  
//______________________________________________________________________________
int StiTrackNodeHelper::fake2Fit()
{
  int ians=0;
  if (!mPNode) {
    mLE = mBigE ;
    return 0;
  } else {
    propagate(&mPNode->mFP);
    propagateError(mLE);
    mLE = mPE;
  }
  StiHit *hit = mSNode->getHit();  
//  	refit it
  if (hit && mSNode->getChi2()<1e3)  {
    StiNodePars savePars = mSNode->mFP;
    StiNodeErrs saveErrs = mSNode->mFE;
    mSNode->mFE = mLE;
    int state = mSNode->_state;
    mSNode->updateNode();
    mSNode->_state=state;
    mLE = mSNode->mFE;
    mSNode->mFP = savePars;
    mSNode->mFE = saveErrs;
  }
//  	combine errors
  join(mPE,mSNode->mFE);
  mSNode->mFE=mJE;
  if (hit) {
    double chi2 = mSNode->evaluateChi2(hit);
    if (chi2> mChi2Max) chi2 = 1e+33;
    mSNode->setChi2(chi2);
  }
  
  return ians;
}  
//______________________________________________________________________________
int StiTrackNodeHelper::join(StiNodeErrs &a,StiNodeErrs &b)
{

  double sum[kNErrs],sumI[kNErrs],sub[kNErrs];
  TCL::vadd(a.A,b.A,sum,kNErrs);
  TCL::vsub(a.A,b.A,sub,kNErrs);
  sum[0] = 1;
  TCL::trsinv(sum,sumI,kNPars);
  sum[0]  = 0; sumI[0] = 0;
  TCL::trqsq(sub,sumI,mJE.A,kNPars); 
  TCL::vlinco(sum,0.5,mJE.A,-0.5,mJE.A,kNErrs);
  return 0;
}

  
