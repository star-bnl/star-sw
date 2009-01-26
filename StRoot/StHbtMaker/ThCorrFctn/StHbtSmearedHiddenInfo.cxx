/*******************************************************************
 * The StHbtSmearedHiddenInfo class
 * Author: Adam Kisiel
 *******************************************************************/
#include "StHbtMaker/ThCorrFctn/StHbtSmearedHiddenInfo.h"
#include "TMath.h"
#define DEGTORAD 0.017453293

StHbtSmearedHiddenInfo::StHbtSmearedHiddenInfo() 
{/* no-op */};

StHbtSmearedHiddenInfo::StHbtSmearedHiddenInfo(const StHbtLorentzVector& aInitialMom, 
					       const StHbtLorentzVector& aFreezeOut,
					       const int& aPid,
					       TRandom* aRand,
					       const StHbtMomRes* aMomRes)
  :
  mPid(aPid)
{
  setInitialMom(&aInitialMom, aRand, aMomRes);
  setFreezeOut(&aFreezeOut);
}

StHbtSmearedHiddenInfo::StHbtSmearedHiddenInfo(const StHbtSmearedHiddenInfo& aHiddenInfo)
  :
  mSmearedMom(aHiddenInfo.getSmearedMom()),
  mPid(aHiddenInfo.getPid())
{ 
  mFreezeOut = new StHbtLorentzVector(aHiddenInfo.getFreezeOut());
};

StHbtSmearedHiddenInfo::StHbtSmearedHiddenInfo(const StHbtLorentzVector& aSmearedMom,
					       const StHbtLorentzVector& aFreezeOut,
					       const int& aPid):
  mSmearedMom(aSmearedMom),
  mPid(aPid)
{
  mFreezeOut = new StHbtLorentzVector(aFreezeOut);
};

StHbtSmearedHiddenInfo::~StHbtSmearedHiddenInfo()
{
  if (mFreezeOut) delete mFreezeOut;
};

inline const StHbtLorentzVector& StHbtSmearedHiddenInfo::getSmearedMom() const {
  return mSmearedMom;
}

inline StHbtLorentzVector& StHbtSmearedHiddenInfo::getFreezeOut() const 
{
  return *mFreezeOut;
}

inline int StHbtSmearedHiddenInfo::getPid() const
 {return  mPid;}

inline  void  StHbtSmearedHiddenInfo::setInitialMom(const StHbtLorentzVector* aP, TRandom* aRand, const StHbtMomRes* aMomRes) { 
  /* Smears the initial momentum according to the
   * power-law parametrization and stores the 
   * smeared momentum as a hidden information */

  /* Getting the initial momentum */
  Float_t px = aP->x();
  Float_t py = aP->y();
  Float_t pz = aP->z();

  /* Calculating helper veriables */
  Float_t totmom = sqrt ((px * px) + (py * py) + (pz * pz));
  Float_t per = aMomRes->getPtError(totmom);
  Float_t thetaan = TMath::ATan2(hypot(px,py),pz);
  Float_t phier = aMomRes->getPhiError(totmom);
  Float_t thetaer = aMomRes->getThetaError(totmom);
  Float_t pshift = aMomRes->getPShift(totmom);

  /* Rescale the momnentum components according to the P shift */
  //  Float_t rescale = (totmom - (pshift * totmom)) / totmom;
  // pP shift now an absolute value
  Float_t rescale = (totmom - pshift) / totmom;
  //  cout << "Rescale:   " << rescale << endl;

  /* Getting the error distribution widths */
  //  Float_t Deltapx = px * pter - py * phier * DEGTORAD;
  //  Float_t Deltapy = py * pter + px * phier * DEGTORAD;
  //  Float_t Deltapz = pz * pter + ptmom * thetaer * DEGTORAD / (::pow((ptmom/pz),2));
  // Angles now in readians - do not recalculate them!
  Float_t Deltapx = TMath::Abs(px) * per + TMath::Abs(py) * phier + TMath::Abs(px * (1/TMath::Tan(thetaan))) * thetaer;
  Float_t Deltapy = TMath::Abs(py) * per + TMath::Abs(px) * phier + TMath::Abs(py * (1/TMath::Tan(thetaan))) * thetaer;
  Float_t Deltapz = TMath::Abs(pz) * per + TMath::Abs(pz * TMath::Tan(thetaan)) * thetaer;


  /* storing the smeared momentum in the hidden info */
  mSmearedMom.setX((px + aRand->Gaus(0,fabs(Deltapx))) * rescale);
  mSmearedMom.setY((py + aRand->Gaus(0,fabs(Deltapy))) * rescale);
  mSmearedMom.setZ((pz + aRand->Gaus(0,fabs(Deltapz))) * rescale);

  /* Calclating the energy */
  switch (abs(mPid)) {
  case 211:
    mSmearedMom.setT(::sqrt((0.139*0.139 + mSmearedMom.x() * mSmearedMom.x() + mSmearedMom.y() * mSmearedMom.y() + mSmearedMom.z() * mSmearedMom.z())));
    break;
  case 321:
    mSmearedMom.setT(::sqrt((0.493*0.493 + mSmearedMom.x() * mSmearedMom.x() + mSmearedMom.y() * mSmearedMom.y() + mSmearedMom.z() * mSmearedMom.z())));
    break;
  case 2212:
    mSmearedMom.setT(::sqrt((0.938*0.938 + mSmearedMom.x() * mSmearedMom.x() + mSmearedMom.y() * mSmearedMom.y() + mSmearedMom.z() * mSmearedMom.z())));
    break;

  }
}

inline  void StHbtSmearedHiddenInfo:: setPid(int aPid)
{ mPid=aPid; }

inline  void StHbtSmearedHiddenInfo:: setFreezeOut(const StHbtLorentzVector* aFreezeOut)
{ mFreezeOut = new StHbtLorentzVector(*aFreezeOut); } 

inline StHbtHiddenInfo* StHbtSmearedHiddenInfo::getParticleHiddenInfo()
 const
{return new StHbtSmearedHiddenInfo(mSmearedMom, *mFreezeOut, mPid);}

inline StHbtLorentzVector& StHbtSmearedHiddenInfo::getMomentum()
{
  return mSmearedMom;
}

