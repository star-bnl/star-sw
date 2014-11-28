/***************************************************************************
 *
 * $Id: 
 *
 * Author: Adam Kisiel, Warsaw University of Technology, Poland
 ***************************************************************************
 *
 * Description : Stores the hidden information for the particle
 * The Momentum of the particle is first shifted according to
 * ShiftType and then smeared according to the momentum resolution
 * ShiftTypes:
 * PSHIFT: Particle momentum is multiplied by (1 + x)
 * PTSHIFT: Particle transverse momentum is multiplied by (1 + x)
 * PHISHIFT: The direction (in transverse plane) of the momentum 
 * of one particle is shifted by x (in rad).
 *
 ***************************************************************************
 *
 * $Log: 
 *
 ***************************************************************************/

#ifndef StHbtShiftedHiddenInfo_hh
#define StHbtShiftedHiddenInfo_hh

#include "StHbtMaker/Base/StHbtHiddenInfo.hh"
#include "StHbtMaker/Infrastructure/StHbtTypes.hh"
#include "TRandom.h"
#include "StHbtMaker/ThCorrFctn/StHbtMomRes.hh"

enum ShiftType{PSHIFT, PTSHIFT, PHISHIFT};

class StHbtShiftedHiddenInfo : public StHbtHiddenInfo{

public:

// --- Constructors
  StHbtShiftedHiddenInfo();
  StHbtShiftedHiddenInfo(const StHbtLorentzVector& aInitialMom, 
			 const int& aPid,
			  TRandom* aRand,
			 const StHbtMomRes* aMomRes,
			 const double momShift,
			 const ShiftType aShiftType);

  StHbtShiftedHiddenInfo(const StHbtShiftedHiddenInfo& aHiddenInfo);
  StHbtShiftedHiddenInfo(const StHbtLorentzVector& aShiftedMom,
			 const int& aPid);
// --- Destructor
  virtual ~StHbtShiftedHiddenInfo();


// --- Return hidden info content

  const StHbtLorentzVector getShiftedMom() const;
  StHbtLorentzVector getMomentum() const;
  int getPid() const;

// ---Change hidden info content
  void setInitialMom(const StHbtLorentzVector*, TRandom*, const StHbtMomRes*);
  void setPid(int);
  void setShift(double momShift);
  void setShiftType(ShiftType aShiftType);

// !!! MANDATORY !!!
// --- Copy the hidden info from StHbtTrack to StHbtParticle
  virtual StHbtHiddenInfo* getParticleHiddenInfo() const;

  StHbtLorentzVector mShiftedMom;
 protected:
  //  StHbtLorentzVector mEmPoint;
  int mPid;
  double mMomShift;
  ShiftType mShiftType;

};

#endif
