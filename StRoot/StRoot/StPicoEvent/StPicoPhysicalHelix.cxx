//
// StPicoPhysicalHelix is a parametrization of a particle that moves along the helix
//

// C++ headers
#include <math.h>

// PicoDst headers
#include "StPicoHelix.h"
#include "StPicoPhysicalHelix.h"
#ifdef _VANILLA_ROOT_
#include "PhysicalConstants.h" 
#include "SystemOfUnits.h"
#else
#include "StarClassLibrary/PhysicalConstants.h" 
#include "StarClassLibrary/SystemOfUnits.h"
#endif

ClassImpT(StPicoPhysicalHelix, Double_t);

//_________________
StPicoPhysicalHelix::StPicoPhysicalHelix(){
  /* no-op */
}

//_________________
StPicoPhysicalHelix::~StPicoPhysicalHelix() {
  /* no-op */
}

//_________________
StPicoPhysicalHelix::StPicoPhysicalHelix(const TVector3& p,
					 const TVector3& o,
					 Double_t B, Double_t q) {
  mH = (q*B <= 0) ? 1 : -1;
  if(p.y() == 0 && p.x() == 0) {
    setPhase((M_PI/4)*(1-2.*mH));
  }
  else {
    setPhase(atan2(p.y(),p.x())-mH*M_PI/2);
  }
  setDipAngle(atan2(p.z(),p.Perp()));
  mOrigin = o;
  
#ifndef ST_NO_NAMESPACES
  {
    using namespace units;
#endif
    setCurvature( ::fabs( (c_light*nanosecond/meter*q*B/tesla) /
			  ( p.Mag()/GeV*mCosDipAngle) / meter) );
#ifndef ST_NO_NAMESPACES
  }
#endif
}

//_________________
StPicoPhysicalHelix::StPicoPhysicalHelix(Double_t c, Double_t d,
					 Double_t phase, const TVector3& o,
					 Int_t h) : StPicoHelix(c, d, phase, o, h) {
  /* no-op */
}

//_________________
TVector3 StPicoPhysicalHelix::momentum(Double_t B) const {
  
  if (mSingularity) {
    return(TVector3(0,0,0));
  }
  else {
#ifndef ST_NO_NAMESPACES
    {
      using namespace units;
#endif
      Double_t pt = GeV*fabs(c_light*nanosecond/meter*B/tesla)/(fabs(mCurvature)*meter);
    
      return ( TVector3( pt*cos(mPhase+mH*M_PI/2),   // pos part pos field
			 pt*sin(mPhase+mH*M_PI/2),
			 pt*tan(mDipAngle) ) );
#ifndef ST_NO_NAMESPACES
    }
#endif
  } //else
}

//_________________
TVector3 StPicoPhysicalHelix::momentumAt(Double_t S, Double_t B) const {
  // Obtain phase-shifted momentum from phase-shift of origin
  StPicoPhysicalHelix tmp(*this);
  tmp.moveOrigin(S);
  return tmp.momentum(B);
}

//_________________
Int_t StPicoPhysicalHelix::charge(Double_t B) const {
  return (B > 0 ? -mH : mH);
}

//_________________
Double_t StPicoPhysicalHelix::geometricSignedDistance(Double_t x, Double_t y) {
  // Geometric signed distance
  Double_t thePath = this->pathLength(x,y);
  TVector3 DCA2dPosition = this->at(thePath);
  DCA2dPosition.SetZ(0);
  TVector3 position(x,y,0);
  TVector3 DCAVec = (DCA2dPosition-position);
  TVector3 momVec;
  // Deal with straight tracks
  if (this->mSingularity) {
    momVec = this->at(1)- this->at(0);
    momVec.SetZ(0);
  }
  else {
    momVec = this->momentumAt(thePath,1./tesla); // Don't care about Bmag.  Helicity is what matters.
    momVec.SetZ(0);
  }
  
  Double_t cross = DCAVec.x()*momVec.y() - DCAVec.y()*momVec.x();
  Double_t theSign = (cross>=0) ? 1. : -1.;
  return theSign*DCAVec.Perp();
}

//_________________
Double_t StPicoPhysicalHelix::curvatureSignedDistance(Double_t x, Double_t y) {
  // Protect against mH = 0 or zero field
  if (this->mSingularity || abs(this->mH)<=0) {
    return (this->geometricSignedDistance(x,y));
  }
  else {
    return (this->geometricSignedDistance(x,y))/(this->mH);
  }
}

//_________________
Double_t StPicoPhysicalHelix::geometricSignedDistance(const TVector3& pos) {
  Double_t sdca2d = this->geometricSignedDistance(pos.x(),pos.y());
  Double_t theSign = (sdca2d>=0) ? 1. : -1.;
  return (this->distance(pos))*theSign;
}

//_________________
Double_t StPicoPhysicalHelix::curvatureSignedDistance(const TVector3& pos) {
  Double_t sdca2d = this->curvatureSignedDistance(pos.x(),pos.y());
  Double_t theSign = (sdca2d>=0) ? 1. : -1.;
  return (this->distance(pos))*theSign;
}
