#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include <assert.h>
#include "StEvent/StEnumerations.h"
#include "StvFtsHitErrCalculator.h"
#include "StvUtil/StvDebug.h"
#include "StThreeVectorF.hh"
#include <string>

ClassImp(StvFtsHitErrCalculator)
double StvFtsHitErrCalculator::mgRPhiErrs[2]={0,0}; 	//these errors are used only for CalcDetErrs

//______________________________________________________________________________
int StvFtsHitErrCalculator::CalcDetErrs(const float hiPos[3],const float hiDir[3][3],double hRr[3])
{
//  X = R*cos(Fi), Y=R*sin(Fi), Z = z   
//   dX/dR  = (    cos(Fi)  ,sin(Fi),0)
//   dX/dFi = (-R*sin(Fi), R*cos(Fi),0)
//   U & V coordinate in detector plane
//   U =  hiDir[1][0]*X+hiDir[1][1]*Y + hiDir[1][2]*Z
//   V =  hiDir[2][0]*X+hiDir[2][1]*Y + hiDir[2][2]*Z
//
//  dU/dR =   hiDir[1][0]*cos(Fi)+hiDir[1][1]*sin(Fi) 
//  dV/dR =   hiDir[2][0]*cos(Fi)+hiDir[2][1]*sin(Fi) 
//  dU/dFi =(-hiDir[1][0]*sin(Fi)+hiDir[1][1]*cos(Fi))*R 
//  dV/dFi =(-hiDir[2][0]*sin(Fi)+hiDir[2][1]*cos(Fi))*R 
// But our case is more simple:
// hiDir {{0, 0, 1}, {0, -1, 0}, {1, 0, 0}}

//  dU/dR = -sin(Fi) 
//  dV/dR =  cos(Fi) 
//  dU/dFi =-cos(Fi)*R 
//  dV/dFi =-sin(Fi)*R 

//  UU = sin(Fi)**2*dR**2 +  cos(Fi)**2*R**2*dFi**2
//  VV = cos(Fi)**2*dR**2 +  sin(Fi)**2*R**2*dFi**2
//  UV = -sin(Fi)*cos(Fi)*dR**2 + cos(Fi)*sin(Fi)*(R*dFi)**2
//  UV = sin(Fi)*cos(Fi)*(-dR**2 + (R*dFi)**2)
   if (mPar[kZErr]<1e-11) { hRr[0]=1;hRr[1]=0;hRr[2]=1; return 0;}

   double Rxy2 = (hiPos[0]*hiPos[0]+hiPos[1]*hiPos[1]);
   mPar[kPhiErr] = mgRPhiErrs[kPhiErr];
   mPar[kRxyErr] = mgRPhiErrs[kRxyErr];
   double myRxyErr2 = mPar[kRxyErr]*mPar[kRxyErr];
   double myPhiErr2 = mPar[kPhiErr]*mPar[kPhiErr];
   double cosFi2 = hiPos[0]*hiPos[0]/Rxy2;
   double sinFi2 = hiPos[1]*hiPos[1]/Rxy2;
   double sicoFi = hiPos[0]*hiPos[1]/Rxy2;
   memset(mDRr,0,sizeof(mDRr));
   mDRr[kYY] = sinFi2*myRxyErr2+cosFi2*myPhiErr2*Rxy2;
   mDRr[kZZ] = cosFi2*myRxyErr2+sinFi2*myPhiErr2*Rxy2;
   mDRr[kZY] = sicoFi*(-myRxyErr2+myPhiErr2*Rxy2);
   if (!hRr) return 0;
   hRr[0] =  mDRr[kYY];
   hRr[1] =  mDRr[kYZ];
   hRr[2] =  mDRr[kZZ];

   assert(hRr[0]*hRr[2]>hRr[1]*hRr[1]);

//    double tst[4] = {-hiPos[1],hiPos[0],tst[1],-tst[0]};
//    double nor = sqrt(hiPos[0]*hiPos[0]+hiPos[1]*hiPos[1]);
//    for (int i=0;i<4;i++) {tst[i]/=nor;}
//    double dR2 = hRr[0]*tst[0]*tst[0]+2*hRr[1]*tst[0]*tst[1]+hRr[2]*tst[1]*tst[1];
//    double dF2 = hRr[0]*tst[2]*tst[2]+2*hRr[1]*tst[2]*tst[3]+hRr[2]*tst[3]*tst[3];
//    assert(fabs(dR2-pow(mPar[kRxyErr],2))/dR2      <1e-3);
//    assert(fabs(dF2-pow(mPar[kPhiErr],2)*Rxy2)/dF2 <1e-3);

   return 0;
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
   mPar[kRxyErr] = dRdPdZ[0];
   mPar[kPhiErr] = dRdPdZ[1];

   if (mgRPhiErrs[kRxyErr]<mPar[kRxyErr]) mgRPhiErrs[kRxyErr]=mPar[kRxyErr];
   if (mgRPhiErrs[kPhiErr]<mPar[kPhiErr]) mgRPhiErrs[kPhiErr]=mPar[kPhiErr];
 
   const StHitPlane *hp = hit->detector(); 
   const Mtx33F_t &hD = hp->GetDir(fx);
   int ians = CalcDcaErrs(fx,hD,hRr);

//    double Rxy2 = (fx[0]*fx[0]+fx[1]*fx[1]);
//    double tst[4] = {-fx[1],fx[0],tst[1],-tst[0]};
//    double nor = sqrt(fx[0]*fx[0]+fx[1]*fx[1]);
//    for (int i=0;i<4;i++) {tst[i]/=nor;}
//    double dR2 = mDRr[0]*tst[0]*tst[0]+2*mDRr[1]*tst[0]*tst[1]+mDRr[2]*tst[1]*tst[1];
//    double dF2 = mDRr[0]*tst[2]*tst[2]+2*mDRr[1]*tst[2]*tst[3]+mDRr[2]*tst[3]*tst[3];
//    assert(fabs(dR2-pow(mPar[kRxyErr],2))/dR2      <1e-3);
//    assert(fabs(dF2-pow(mPar[kPhiErr],2)*Rxy2)/dF2 <1e-3);
// 
//    tst[0] = fx[0]*mTG[1][0]+fx[1]*mTG[1][1];
//    tst[1] = fx[0]*mTG[2][0]+fx[1]*mTG[2][1];
//    nor = sqrt(tst[0]*tst[0]+tst[1]*tst[1]);
//    tst[0]/=nor; tst[1]/=nor; 
//    tst[2] = -tst[1]; tst[3] = tst[0];
//    double ddR2 = mTRr[0]*tst[0]*tst[0]+2*mTRr[1]*tst[0]*tst[1]+mTRr[2]*tst[1]*tst[1];
//    double ddF2 = mTRr[0]*tst[2]*tst[2]+2*mTRr[1]*tst[2]*tst[3]+mTRr[2]*tst[3]*tst[3];
//    assert(fabs(ddR2-pow(mPar[kRxyErr],2))/ddR2      <1e-1);
//    assert(fabs(ddF2-pow(mPar[kPhiErr],2)*Rxy2)/ddF2 <1e-1);

   return ians;
}

