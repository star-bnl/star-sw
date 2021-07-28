/*******************************************************************
 * The StHbtShiftedHiddenInfo class
 * Author: Adam Kisiel
 *******************************************************************/
#include "StHbtMaker/ThCorrFctn/StHbtShiftedHiddenInfo.h"

#define DEGTORAD 0.017453293

StHbtShiftedHiddenInfo::StHbtShiftedHiddenInfo() {/* no-op */};
StHbtShiftedHiddenInfo::StHbtShiftedHiddenInfo(const StHbtLorentzVector& aInitialMom, 
					       const int& aPid,
					       TRandom* aRand,
					       const StHbtMomRes* aMomRes,
					       const double momShift,
					       const ShiftType aShiftType)
  :
  mPid(aPid), mMomShift(momShift), mShiftType(aShiftType)
{
  setInitialMom(&aInitialMom, aRand, aMomRes);
}

StHbtShiftedHiddenInfo::StHbtShiftedHiddenInfo(const StHbtShiftedHiddenInfo& aHiddenInfo)
  :
  mShiftedMom(aHiddenInfo.getShiftedMom()),
  mPid(aHiddenInfo.getPid())
{ /* no-op */ };

StHbtShiftedHiddenInfo::StHbtShiftedHiddenInfo(const StHbtLorentzVector& aShiftedMom,
					       const int& aPid):
  mShiftedMom(aShiftedMom),
  mPid(aPid)
{ /* no-op */ };

StHbtShiftedHiddenInfo::~StHbtShiftedHiddenInfo()
{/* no-op */};


inline const StHbtLorentzVector StHbtShiftedHiddenInfo::getShiftedMom() const {
  return mShiftedMom;
}

inline int StHbtShiftedHiddenInfo::getPid() const
 {return  mPid;}

inline  void  StHbtShiftedHiddenInfo::setInitialMom(const StHbtLorentzVector* aP, TRandom* aRand, const StHbtMomRes* aMomRes) { 
  Double_t px = aP->x();
  Double_t py = aP->y();
  Double_t pz = aP->z();
  Double_t shift;

  /* Shift the momentum based on the sign of the particle */
  switch (mShiftType) {
  case PSHIFT:
    shift = (mPid > 0) ? 1.0 + mMomShift : shift = 1.0 - mMomShift;
    px *= shift;
    py *= shift;
    pz *= shift;
    break;
  case PTSHIFT:
    shift = (mPid > 0) ? 1.0 + mMomShift : shift = 1.0 - mMomShift;
    px *= shift;
    py *= shift;
    break;
  case PHISHIFT:
    Double_t thephi = atan(py/px);
    Double_t pt = hypot(px, py);

    if (px < 0)
      thephi += 3.14159;
    
    //    thephi += (1 - pt) * mMomShift * pt * (abs(mPid) / mPid);
    if (abs(mPid) == 321)
      thephi += mMomShift;
    px = pt * cos(thephi);
    py = pt * sin(thephi);
    break;
  }
    
  Double_t ptmom = hypot(px, py);
  Double_t pter = aMomRes->getPtError(ptmom);
  Double_t totmom = sqrt ((px * px) + (py * py) + (pz * pz));
  Double_t phier = aMomRes->getPhiError(totmom);
  Double_t thetaer = aMomRes->getThetaError(totmom);
  Double_t Deltapx = px * pter - py * phier * DEGTORAD;
  Double_t Deltapy = py * pter + px * phier * DEGTORAD;
  Double_t Deltapz = pz * pter + ptmom * thetaer * DEGTORAD / (::pow((ptmom/pz),2));
  mShiftedMom.setX(px + aRand->Gaus(0,fabs(Deltapx)));
  mShiftedMom.setY(py + aRand->Gaus(0,fabs(Deltapy)));
  mShiftedMom.setZ(pz + aRand->Gaus(0,fabs(Deltapz)));
  switch (abs(mPid)) {
  case 211:
    mShiftedMom.setT(::sqrt((0.139*0.139 + mShiftedMom.x() * mShiftedMom.x() + mShiftedMom.y() * mShiftedMom.y() + mShiftedMom.z() * mShiftedMom.z())));
    break;
  case 321:
    mShiftedMom.setT(::sqrt((0.493*0.493 + mShiftedMom.x() * mShiftedMom.x() + mShiftedMom.y() * mShiftedMom.y() + mShiftedMom.z() * mShiftedMom.z())));
    break;
  case 2212:
    mShiftedMom.setT(::sqrt((0.938*0.938 + mShiftedMom.x() * mShiftedMom.x() + mShiftedMom.y() * mShiftedMom.y() + mShiftedMom.z() * mShiftedMom.z())));
    break;

  }
}

inline  void StHbtShiftedHiddenInfo:: setPid(int aPid)
{ mPid=aPid;}

inline  void StHbtShiftedHiddenInfo::setShiftType(ShiftType aShiftType)
{ mShiftType = aShiftType; }

inline StHbtHiddenInfo* StHbtShiftedHiddenInfo::getParticleHiddenInfo()
 const
{return new StHbtShiftedHiddenInfo(mShiftedMom, mPid);}

inline StHbtLorentzVector StHbtShiftedHiddenInfo::getMomentum() const 
{
  return mShiftedMom;
}

inline void StHbtShiftedHiddenInfo::setShift(double momShift) 
{
  mMomShift = momShift;
}

