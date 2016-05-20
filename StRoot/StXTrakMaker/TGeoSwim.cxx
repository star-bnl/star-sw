
// $Id: TGeoSwim.cxx,v 1.1 2016/05/20 18:40:41 perev Exp $
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
//_____________________________________________________________________________
TGeoSwim::TGeoSwim(const char *name):TNamed(name,"TGeoSwim")
{
  fRmax = 1000;
  fZmin =-1000;
  fZmax = 1000;
  fHelx[0] = 0;
  fHelx[1] = 0;
  fNode[0] = 0;
  fNode[1] = 0;
}
//_____________________________________________________________________________
void TGeoSwim::Set(double Rmax,double Zmin,double zMax)
{
  fRmax = Rmax;
  fZmin = Zmin;
  fZmax = zMax;
}
//_____________________________________________________________________________
int TGeoSwim::Set(THelixTrack *inHelx,THelixTrack *otHelx)
{
 fHelx[0]=inHelx; fHelx[1]=otHelx;
 gGeoManager->SetCurrentPoint    (fHelx[0]->Pos());
 gGeoManager->SetCurrentDirection(fHelx[0]->Dir());
 fNode[0] = gGeoManager->FindNode();
 if (!fNode[0]) return 1;
 return 0;
}

//_____________________________________________________________________________
int TGeoSwim::Set(const double* pos,const double* dir, double curv)
{
 return Set(new THelixTrack(pos,dir,curv),new THelixTrack()); 
}
//_____________________________________________________________________________
const double *TGeoSwim::GetPos  (int idx) const      {return fHelx[idx]->Pos();}
//_____________________________________________________________________________
const double *TGeoSwim::GetDir  (int idx) const      {return fHelx[idx]->Dir();}
//_____________________________________________________________________________
const char *TGeoSwim::GetPath  () const      {return gGeoManager->GetPath();}
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
//_____________________________________________________________________________
int TGeoSwim::Swim(double maxLenP)
{
enum {kMaxIter=100};
enum {kInside  = 1, kOutside  = 0};

  const TGeoNode **nodes=fNode;
  double   *inout= fInOutLen;
  THelixTrack** th=fHelx;

  const double *poz = fHelx[0]->Pos();
  double range = fabs(poz[0])+fabs(poz[1])+fabs(poz[2]);
  double myMicron = 1e-4+range*1e-4;


  *fHelx[1] = *fHelx[0];
  fNode[0] = gGeoManager->GetCurrentNode();
  double myRad =1./(fabs(fHelx[0]->GetRho())+1e-10)/fHelx[0]->GetCos();
  double maxLen = (maxLenP>3*myRad)? 3*myRad: maxLenP;
  fInOutLen[0]=0; fInOutLen[1]=maxLen;
  double pos[3],dir[3],step=0,maxStep=0;
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
        maxStep = fInOutLen[1]-fInOutLen[0];
	if (maxStep>0.3*myRad) maxStep=0.3*myRad;
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

      default: assert(0 || "Wrong case");
    
    }
    if (maxStep<=0) return 1;
    range = fInOutLen[1]-fInOutLen[0];
    if (range<myMicron)	break;
    if (step<myMicron) 	{ 	//if step is tiny try to change upper limit
      step = myMicron;
      if (step> range/2) step = range/2;			
    }
    fHelx[1]->Eval(step,pos,dir); 
    kase = (gGeoManager->IsSameLocation(pos[0],pos[1],pos[2]))? kInside:kOutside;
  }

  range = fInOutLen[1]-fInOutLen[0];
  if (range>myMicron) return 13; 	//no convergency at all
  fHelx[1]->Move(range);

  if (OutScene(fHelx[1]->Pos()))	return 2;


  gGeoManager->SetCurrentPoint    ((double*)fHelx[1]->Pos());
  gGeoManager->SetCurrentDirection((double*)fHelx[1]->Dir());
  fNode[1] = gGeoManager->FindNode();

  return (fNode[1])? 0:99;
}
