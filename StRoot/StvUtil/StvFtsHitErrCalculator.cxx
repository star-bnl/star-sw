#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "StvFtsHitErrCalculator.h"
#include "StvUtil/StvDebug.h"
#include "StThreeVectorF.hh"
#include <string>

ClassImp(StvFtsHitErrCalculator)
double StvFtsHitErrCalculator::mgRPhiErrs[2]={0,0}; 	//these errors are used only for CalcDetErrs

//______________________________________________________________________________
int StvFtsHitErrCalculator::CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRr[3])
{
   double Rxy2 = (hiPos[0]*hiPos[0]+hiPos[1]*hiPos[1]);
   mPar[kYErr] = mgRPhiErrs[kYErr]*Rxy2;
   mPar[kZErr] = mgRPhiErrs[kZErr];
   return StvHitErrCalculator::CalcDetErrs(hiPos,hiDir,hRr);
}
//______________________________________________________________________________


#include "StHit.h"
#include "Stv/StvHit.h"
#include "StarVMC/GeoTestMaker/StTGeoProxy.h"
//______________________________________________________________________________
int StvFtsHitErrCalculator::CalcDcaErrs(const StvHit *hit,double hRr[3])
{
   StHit *stHit = (StHit*)hit->stHit();
   StThreeVectorF dRdPdZ = stHit->positionError();
   float const *fx = hit->x();
   double Rxy2 = (fx[0]*fx[0]+fx[1]*fx[1]);
   mPar[kYErr] = dRdPdZ[1];
   mPar[kZErr] = dRdPdZ[0];

   mPar[kYErr] *= mPar[kYErr];
   if (mPar[kYErr]>mgRPhiErrs[kYErr]) mgRPhiErrs[kYErr]=mPar[kYErr];
   mPar[kYErr] *= Rxy2;
   mPar[kZErr] *= mPar[kZErr];
   if (mPar[kZErr]>mgRPhiErrs[kZErr]) mgRPhiErrs[kZErr]=mPar[kZErr];
 
   const StHitPlane *hp = hit->detector(); 
   const Mtx33F_t &hD = hp->GetDir(fx);
   return CalcDcaErrs(fx,hD,hRr);
}

