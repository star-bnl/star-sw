/**********************************************************************
 *
 * $Id: StEStructTrack.cxx,v 1.1 2003/10/15 18:20:51 porter Exp $
 *
 * Author: Jeff Porter merge of code from Aya Ishihara and Jeff Reid
 *
 **********************************************************************
 *
 * Description:  Persistent track information
 *
 **********************************************************************/

#include "StEStructTrack.h"
#include "StPhysicalHelix.hh"
#include "SystemOfUnits.h"

#ifndef ST_NO_NAMESPACES
using namespace units;
#endif

ClassImp(StEStructTrack)

StEStructTrack::StEStructTrack(StEStructTrack *track) : TObject() {
  mPx = track->Px();
  mPy = track->Py();
  mPz = track->Pz();

  mEta = track->Eta();
  mPhi = track->Phi();

  mBxPrimary = track->BxPrimary();
  mByPrimary = track->ByPrimary();
  mBzPrimary = track->BzPrimary();

  mBxGlobal = track->BxGlobal();
  mByGlobal = track->ByGlobal();
  mBzGlobal = track->BzGlobal();

  mPIDe = track->PIDe();
  mPIDpi = track->PIDpi();
  mPIDp = track->PIDp();
  mPIDk = track->PIDk();
  mPIDd = track->PIDd();

  mChi2 = track->Chi2();

  mDedx = track->Dedx();

  mNFitPoints = track->NFitPoints();
  mNFoundPoints = track->NFoundPoints();
  mNMaxPoints = track->NMaxPoints();

  mDetectorID = track->DetectorID();
  mFlag = track->Flag();

  mCharge = track->Charge();

  mMap[0] = track->TopologyMapData(0);
  mMap[1] = track->TopologyMapData(1);
  mTPCNHits = track->TopologyMapTPCNHits();
  
  //
  // check to see if one can complete ... requires event level information
  // such as bfield. If so, complete and set, if not set incomplete.
  //
 if(track->isComplete()){
    FillTransientData();
    mStartPos=track->StartPos();
    mHelix = track->Helix();
    FillTpcReferencePoints();
    mIsComplete=true;
 } else {
   mIsComplete=false;
 }
}

//----------------------------------------------------------
void StEStructTrack::FillTransientData(){

  evalPt();
  evalYt();
  evalXt();
  evalFourMomentum();
  
};

//----------------------------------------------------------
void StEStructTrack::evalYt(){

  float _r=mPt/0.139;
  mYt = log(sqrt(1+_r*_r)+_r);

  mytbin=(int) floor((mYt-1.0)/0.5);
  if(mytbin>6)mytbin=6;
  if(mytbin<0)mytbin=0;


};

//----------------------------------------------------------
void StEStructTrack::evalXt(){
  //
  // cut and paste from Aya's code
  //
  float PionMass = 0.139;
  float Temperature = 0.25;
  float Minimum = (1+(PionMass/Temperature))*exp(-PionMass/Temperature);
  float mtOnly = sqrt((mPx*mPx)+(mPy*mPy)+PionMass*PionMass);
  mXt=1-(1+(mtOnly/Temperature))*exp(-mtOnly/Temperature)/Minimum;

};


//----------------------------------------------------------
void StEStructTrack::evalFourMomentum(const float mass){

  float lMass=mass;
  // assume pion mass for now !
  if(lMass==0)lMass=0.139;

  mFourMomentum.setPx(mPx);
  mFourMomentum.setPy(mPy);
  mFourMomentum.setPx(mPx);
  mFourMomentum.setE(sqrt(mPt*mPt+mPz*mPz+lMass*lMass));

}

//----------------------------------------------------------
void StEStructTrack::evalTrajectory(float pvx, float pvy, float pvz, double bField){

  //
  // this is taken verbatum from Aya's code
  // I don't understand the various varients of 
  // 3-vectors and helices.... but will just use them

  StThreeVector<double> p(mPx*GeV,mPy*GeV,mPz*GeV);

  StThreeVector<double> dcap(mBxPrimary,mByPrimary,mBzPrimary);
  StThreeVector<double> vertexp(pvx,pvy,pvz);
  StThreeVector<double> originp=dcap+vertexp;

  StThreeVectorF dca(mBxPrimary,mByPrimary,mBzPrimary);
  StThreeVectorF vertex(pvx,pvy,pvz);
  mStartPos=dca+vertex;

  const double q = (double)mCharge;
  // const double B = 2.0*0.249117*tesla;
  

  StPhysicalHelix thisHelix(p,originp,q,bField*kilogauss);  
  StHelixD helix(thisHelix.curvature(), thisHelix.dipAngle(), thisHelix.phase(), mStartPos, thisHelix.h());
  mHelix = helix;

  FillTpcReferencePoints();
  mIsComplete=true;

}

//----------------------------------------------------------
void StEStructTrack::FillTpcReferencePoints(){
  //
  // now eval the nominal points at Exit, Entrance, & MidTpc
  //

  static StThreeVectorF WestEnd(0.,0.,200.);
  static StThreeVectorF EastEnd(0.,0.,-200.);
  static StThreeVectorF EndCapNormal(0.,0.,1.0);

  pairD candidates = mHelix.pathLength(200.0);  //
  double sideLength = (candidates.first > 0) ? candidates.first : candidates.second;
  double endLength = mHelix.pathLength(WestEnd,EndCapNormal);
  if (endLength < 0.0) endLength = mHelix.pathLength(EastEnd,EndCapNormal);
  double firstExitLength = (endLength < sideLength) ? endLength : sideLength;
  mNominalTpcExitPoint = mHelix.at(firstExitLength);


  candidates = mHelix.pathLength(50.0);
  sideLength = (candidates.first > 0) ? candidates.first : candidates.second;
  mNominalTpcEntrancePoint = mHelix.at(sideLength);


  candidates = mHelix.pathLength(127.0);  
  sideLength = (candidates.first > 0) ? candidates.first : candidates.second;
  mMidTpcPoint = mHelix.at(sideLength);

}


//----------------------------------------------------------
//  older stuff ... should look at some time in the future ...
//----------------------------------------------------------

Float_t StEStructTrack::Pt() { return mPt; };

Float_t StEStructTrack::Mt(Float_t mass) { 
  return sqrt((mPt*mPt)+(mass*mass)); 
}

Float_t StEStructTrack::E(Float_t mass) { 
  return ((mPt*mPt)+(mPz*mPz)+(mass*mass)); 
}

Float_t StEStructTrack::Rapidity(Float_t mass) { 
  Float_t E = this->E(mass);
  return 0.5*log((E+mPz)/(E-mPz)); 
}

Float_t StEStructTrack::Dca() { 
  return (sqrt((mBxPrimary*mBxPrimary)+(mByPrimary*mByPrimary)+(mBzPrimary*mBzPrimary))); 
}

Float_t StEStructTrack::DcaPrimary() { 
  return (sqrt((mBxPrimary*mBxPrimary)+(mByPrimary*mByPrimary)+(mBzPrimary*mBzPrimary))); 
}

Float_t StEStructTrack::DcaGlobal() { 
  return (sqrt((mBxGlobal*mBxGlobal)+(mByGlobal*mByGlobal)+(mBzGlobal*mBzGlobal))); 
}

Float_t StEStructTrack::PIDpiPlus() { 
  return ((mCharge == 1) ? mPIDpi : 0); 
}

Float_t StEStructTrack::PIDpiMinus() { 
  return ((mCharge == -1) ? mPIDpi : 0); 
}

/**********************************************************************
 *
 * $Log: StEStructTrack.cxx,v $
 * Revision 1.1  2003/10/15 18:20:51  porter
 * initial check in of Estruct Analysis maker codes.
 *
 *
 *********************************************************************/











