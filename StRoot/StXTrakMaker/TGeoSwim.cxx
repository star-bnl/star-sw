
// $Id: TGeoSwim.cxx,v 1.8 2017/11/06 20:49:19 perev Exp $
//
//
// Class StTGeoHelper
// ------------------



#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string>
#include <map>
#include <assert.h>
#include "TString.h"
#include "TMath.h"
#include "TGeoManager.h"
#include "TGeoNavigator.h"
#include "TGeoNode.h"
#include "TGeoVolume.h"
#include "TGeoShape.h"
#include "TGeoBBox.h"
#include "TGeoNode.h"
#include "TGeoMedium.h"
#include "TGeoMaterial.h"
#include "TGeoSwim.h"
#include "THelixTrack.h"
ClassImp(TGeoSwim)
//! TGeoSwim Constructor.
/*!
      \param Name of instance
*/
//_____________________________________________________________________________
TGeoSwim::TGeoSwim(const char *name):TNamed(name,"TGeoSwim")
{
  fSmax = 5;
  fRmax = 1000;
  fZmin =-1000;
  fZmax = 1000;
  fHelx[0] = new THelixTrack();
  fHelx[1] = new THelixTrack();
  fNode[0] = 0;
  fNode[1] = 0;
  fMag = 0;  
  fLoss= 0;
  fEnd = 0;  
  fInOutLen[0] = 0;  
  fInOutLen[1] = 0;  
  fC  = 0;
  fP  = 0;  			//momentum  loss(GeV) 
  fPt = 0;  			//momentum  loss(GeV) 
  fPLoss = 0;  			//momentum  loss(GeV) 
  fTimeFly = 0;  		//time in seconds 
  memset(fB,0,sizeof(fB));
}

/// Setting of tracking region.
/*!
      \param Rmax - maxiimal xy Radius
      \param Zmin - min Z
      \param Zmax - max Z
      \param sMax - max step
*/
//_____________________________________________________________________________
void TGeoSwim::Set(double Rmax,double Zmin,double zMax,double sMax)
{
  fRmax = Rmax;
  fZmin = Zmin;
  fZmax = zMax;
  fSmax = sMax;
}

/// Setting of tracking starting point.
/*!
      \param pos - 3d track start point position
      \param dir - 3d track direction 
      \param curv - curvatore(signed)
*/
//_____________________________________________________________________________
int TGeoSwim::Set(const double* pos,const double* dir, double curv)
{
THelixTrack **helx = fHelx;
double *B = fB;
double *inOut = fInOutLen;
  if (fEnd) fEnd->Reset();
  fPt=0;fP=0;fPLoss=0;fTimeFly=0; memset(fB,0,sizeof(fB));
  fC = curv;
  fHelx[0]->Set(pos,dir,curv);
  if (fMag) {
    (*fMag)(pos,fB);
    fPti = (fabs(fB[2])>1e-6) ? fC/fB[2]:1e-3;
    fPt  = fabs(1./fPti);
    fP   = fabs(fPt/fHelx[0]->GetCos());
  }
  fStartSign = dir[0]*pos[0]+dir[1]*pos[1];

  gGeoManager->SetCurrentPoint    (fHelx[0]->Pos());
  gGeoManager->SetCurrentDirection(fHelx[0]->Dir());
  fNode[0] = gGeoManager->FindNode();
  if (!fNode[0]) return 13;
  return 0;
}
//_____________________________________________________________________________
const double *TGeoSwim::GetPos  (int idx) const      {return fHelx[idx]->Pos();}
//_____________________________________________________________________________
const double *TGeoSwim::GetDir  (int idx) const      {return fHelx[idx]->Dir();}
//_____________________________________________________________________________
const char   *TGeoSwim::GetPath ()        const      {return gGeoManager->GetPath();}
//_____________________________________________________________________________
double        TGeoSwim::GetTime ()        const      
{
  return fTimeFly/TMath::C();
}
//_____________________________________________________________________________
int TGeoSwim::OutScene(const double *x) const
{
  if (x[2]<fZmin  || x[2] > fZmax) 	return 1;
  if (x[0]*x[0]+x[1]*x[1] > fRmax*fRmax)return 2;
  return 0;
}
//_____________________________________________________________________________
const TGeoMaterial *TGeoSwim::GetMate () const      
{
  if (!fNode[0]) return 0;
  return fNode[0]->GetMedium()->GetMaterial();
}
//_____________________________________________________________________________
/// Start tracking..
/*!
      \param maxLenP - Max allowed tracking length
    returns flag, see enum SwimExit
*/
//_____________________________________________________________________________
int TGeoSwim::Swim(double maxLenP)
{
static const double kMaxLoss = 0.1;// Max momentum loss 

enum {kMaxIter=100};
enum {kInside  = 1, kOutside  = 0};
THelixTrack **helx = fHelx;
double *B = fB;
double *inOut = fInOutLen;

  *fHelx[1] = *fHelx[0];
  double maxLen = maxLenP;
  double cutLen = 1e11;
  fInOutLen[2]=0; fTimeFly=0;
  while(1) {
    const double *poz = fHelx[0]->Pos();
    double range = fabs(poz[0])+fabs(poz[1])+fabs(poz[2]);
    double myMicron = 1e-4+range*1e-4;

    double dP,dC,dPt,pos[3],dir[3],cosTh2,cosTh,lenxy2,lenxy;

    fNode[0] = gGeoManager->GetCurrentNode();
    double myRad =1./(fabs(fHelx[0]->GetRho())+1e-10)/fHelx[0]->GetCos();
    double maxStep = fSmax;
    if (maxStep>cutLen) maxStep=cutLen; cutLen = 1e11;
    if (maxStep>maxLen) maxStep=maxLen;
    if (maxStep>myRad) maxStep = myRad;
    fInOutLen[0]=0; fInOutLen[1]=maxStep;
    double step=0,myLen=0;
    const TGeoMaterial *gmate = GetMate();
    int kase = kInside;  	
    for (int iter=0;iter<kMaxIter; iter++)  {
      switch (kase) {

	case kInside: {		//Inside 
	  if (step>0) {
            fHelx[1]->Move(step); fInOutLen[0]+=step;
            gGeoManager->SetCurrentPoint    (fHelx[1]->Pos());
            gGeoManager->SetCurrentDirection(fHelx[1]->Dir());
            fNode[1] = gGeoManager->FindNode();
          }
          if ( fInOutLen[0]+maxStep > maxLen) maxStep = maxLen-fInOutLen[0];
          if (maxStep<=0) break;
	  gGeoManager->FindNextBoundary(maxStep);
	  step = gGeoManager->GetStep()*0.99;
	  break;}

	case kOutside : {		// outside & quality bad
	  if (fInOutLen[1]>fInOutLen[0]+step){ fInOutLen[1]=fInOutLen[0]+step;}
          if (fInOutLen[1]>maxLen) fInOutLen[1]=maxLen; 
	  if (fInOutLen[0]< fInOutLen[1]/2)  { step = (fInOutLen[1]-fInOutLen[0])*0.9;}
	  else 				   { step = (fInOutLen[1]-fInOutLen[0])*0.5;}
	  break;}

	default: assert(0 && "Wrong case");

      }
      range = fInOutLen[1]-fInOutLen[0];
      if (range<myMicron)	break;
      if (step <myMicron) 	{ 	//if step is tiny try to change upper limit
	step = myMicron;
	if (step> range/2) step = range/2;			
      }
      fHelx[1]->Eval(step,pos,dir); 
      kase = (gGeoManager->IsSameLocation(pos[0],pos[1],pos[2]))? kInside:kOutside;
    }

    range = fInOutLen[1]-fInOutLen[0];
    if (range>myMicron) 		return kNoConv; 	//no convergency at all
    fHelx[1]->Move(range);

    if (OutScene(fHelx[1]->Pos()))	return kOutScene;


    myLen = fInOutLen[1];
    fInOutLen[2]+=fInOutLen[1];
    if (fLoss) { // Account of energy loss

      dP = -(*fLoss)(gmate,fP,myLen,0);
//    ================================

      if (fabs(dP) > kMaxLoss*fP) { // Cut step, too big eloss
         cutLen = fInOutLen[1]/2;
         *fHelx[1]=*fHelx[0];
         gGeoManager->SetCurrentPoint    (fHelx[1]->Pos());
         gGeoManager->SetCurrentDirection(fHelx[1]->Dir());
         fNode[1] = gGeoManager->FindNode();
	 continue;
      }
      memcpy(pos,fHelx[1]->Pos(),sizeof(pos));
      memcpy(dir,fHelx[1]->Dir(),sizeof(dir));
      fCurrSign = dir[0]*pos[0]+dir[1]*pos[1];
      if (fCurrSign*fStartSign<=0) return kApogee;

      cosTh2 = (1.-dir[2])*(1.+dir[2]);
      cosTh  = sqrt(cosTh2);
      for (int j=0;j<3;j++){ dir[j]/=cosTh;}
      dPt = fPt/fP*dP;
      double dPti = -fPti*(dPt/fPt);
      fP += dP; fPt+= dPt; fPti+= dPti;
      if (fabs(fPt)<kMinMom) { return kBadMom; }
      fPLoss -= dP;
      lenxy2 = cosTh2*myLen*myLen;
      lenxy  = cosTh*myLen;

      double B2old = fB[2];//????
      if (fMag) { //We have mag field 
	(*fMag)(pos,fB);
//      ===============
        dC  = fPti*fB[2] - fC;
      } else {
	dC = fC*dPti/fPti;
      }
      fC +=dC;
      double dPhi = dC*lenxy /2.;
      double dH   = dC*lenxy2/6.;
      pos[0]+= -dir[1]*dH;
      pos[1]+=  dir[0]*dH;
      dir[0]+= -dir[1]*dPhi;
      dir[1]+=  dir[0]*dPhi;
      fHelx[1]->Set(pos,dir,fC);       
//		Time of flight calculation
      double m = fLoss->GetMass();
      double p = fP-dP;
      double betIn0 = sqrt((p*p+m*m)/(p*p));
      p = fP;
      double betIn1 = sqrt((p*p+m*m)/(p*p));
      fTimeFly += 0.5*(betIn0+betIn1)*fInOutLen[1];
//
    }
    gGeoManager->SetCurrentPoint    ((double*)fHelx[1]->Pos());
    gGeoManager->SetCurrentDirection((double*)fHelx[1]->Dir());
    fNode[1] = gGeoManager->FindNode();
    if (!fNode[1]) 		return kOutScene;
    if (!fEnd || (*fEnd)())	return kNormal;
    fNode[0] = fNode[1];
    *fHelx[0] = *fHelx[1];
    maxLen -= fInOutLen[1];
    if (maxLen<=0) 		return kEndRange;
  }
  return kFailed;
}
//_____________________________________________________________________________
//_____________________________________________________________________________
int TGeoSwimDefaultEnd::operator()()
{
  mTit = 0;
  mEnd = mKase;
  switch(mKase) {
  
    case kDefault: return kDefault;		

    case kNewVolu: {
      TString myPath(gGeoManager->GetPath());
      if (myPath == mPrevPath) {mEnd = kUndef;return kUndef;}
      mPrevPath = myPath;      return kNewVolu;
    }

    case kNameVolu:{
      mEnd = kUndef;
      TString myPath(gGeoManager->GetPath());
      if (myPath == mPrevPath) {return kUndef;};
      mPrevPath = myPath;
      
      for (int j=0;mTits[j];j++) {
        if (myPath.Index(mTits[j])<0) 	continue;
        if (j == mJit) 			break;
        mJit = j;
        mTit = mTits[j];
        mEnd = kNameVolu; break;
      }
      return mEnd;
    }

    default: assert(0 && "Wrong TGeoSwimEnd kase");

  }
  return kUndef;
}
