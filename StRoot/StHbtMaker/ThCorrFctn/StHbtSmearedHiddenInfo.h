/***************************************************************************
 *
 * $Id: 
 *
 * Author: Adam Kisiel, Warsaw University of Technology, Poland
 ***************************************************************************
 *
 * Description : Stores the hidden information for the particle
 * Momentum of the particle is smeared according to the momentum resolution
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef StHbtSmearedHiddenInfo_hh
#define StHbtSmearedHiddenInfo_hh

#include "StHbtMaker/Base/StHbtHiddenInfo.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "TRandom.h"
#include "StHbtMaker/ThCorrFctn/StHbtMomRes.hh"

class StHbtSmearedHiddenInfo : public StHbtHiddenInfo{

public:

// --- Constructors
  StHbtSmearedHiddenInfo();
  StHbtSmearedHiddenInfo(const StHbtLorentzVector& aInitialMom, 
			 const StHbtLorentzVector& aFreezeOut,
			 const int& aPid,
			 TRandom* aRand,
			 const StHbtMomRes* aMomRes);
  StHbtSmearedHiddenInfo(const StHbtSmearedHiddenInfo& aHiddenInfo);
  StHbtSmearedHiddenInfo(const StHbtLorentzVector& aSmearedMom,
			 const StHbtLorentzVector& aFreezeOut,
			 const int& aPid);
// --- Destructor
  virtual ~StHbtSmearedHiddenInfo();

// --- Return hidden info content
  const StHbtLorentzVector& getSmearedMom() const;
  StHbtLorentzVector& getMomentum();
  StHbtLorentzVector& getFreezeOut() const;
  int getPid() const;

// ---Change hidden info content
  void setInitialMom(const StHbtLorentzVector*, TRandom*, const StHbtMomRes*);
  void setFreezeOut(const StHbtLorentzVector*);
  void setPid(int);

// !!! MANDATORY !!!
// --- Copy the hidden info from StHbtTrack to StHbtParticle
  virtual StHbtHiddenInfo* getParticleHiddenInfo() const;

  StHbtLorentzVector mSmearedMom;
  StHbtLorentzVector *mFreezeOut;

 private:
  int mPid;
};

#endif
