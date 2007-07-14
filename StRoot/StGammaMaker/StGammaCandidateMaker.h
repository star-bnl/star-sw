#ifndef __StGammaCandidateMaker_h__
#define __StGammaCandidateMaker_h__

class TVector3;
class StGammaTrack;

#include "StMaker.h"

#include "StGammaCandidate.h"

class StGammaCandidateMaker : public StMaker
{

 public:
  StGammaCandidateMaker( const Char_t *name="gcmaker" );
  ~StGammaCandidateMaker(){ /* nada */ };

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");
  
  void SetMinimumET( Float_t et ){ mMinimumET = et; }
  void SetRadius( Float_t r ) { mRadius = r; }
  void SetSmdRange( Float_t r ){ mSmdRange = r; }

 private:
 protected:

  Int_t mId;
  Int_t nextId(){ return mId++; }

  Int_t MakeEndcap();
  Int_t MakeBarrel();

  bool getPositionMomentumAtBarrel(StGammaTrack* track, double magneticField, TVector3& position, TVector3& momentum);
  
  Float_t mMinimumET; // in GeV
  Float_t mRadius;    // in sqrt( deta**2 + dphi**2 )
  Float_t mSmdRange;  // in cm

  ClassDef(StGammaCandidateMaker,1);

};

#endif
