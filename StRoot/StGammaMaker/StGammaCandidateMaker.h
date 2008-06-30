// -*- mode: C++ -*- Put Emacs in C++ mode

#ifndef __StGammaCandidateMaker_h__
#define __StGammaCandidateMaker_h__

class TClonesArray;
class StGammaTrack;
class StEEmcCluster;

#include "TVector3.h"
#include "StMaker.h"

class StGammaCandidateMaker : public StMaker
{

 public:
  StGammaCandidateMaker( const Char_t *name="gcmaker" );
  ~StGammaCandidateMaker(){ /* nada */ }

  Int_t Init();
  Int_t Make();
  void  Clear(Option_t *opts="");
  
  void  SetMinimumET( Float_t et ){ mMinimumET = et; }
  void  SetRadius( Float_t r ) { mRadius = r; }
  void  SetSmdRange( Float_t r ){ mSmdRange = r; }
  Int_t Compress();

  enum { kNoCompress, kCompressSmd, kCompressAll  };
  void  SetCompressLevel(Int_t level = kCompressSmd ) { mCompressLevel = level; } // 0=No compression, 1=SMD only, 2=All

  virtual const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StGammaCandidateMaker.h,v 1.8 2008/06/30 14:58:38 jwebb Exp $ built "__DATE__" "__TIME__; return cvs;}


 private:
 protected:

  Int_t mId;
  Int_t nextId(){ return mId++; }
  Int_t MakeEndcap();
  Int_t MakeBarrel();
  template<class T> void Compress(TClonesArray* clones);

  // Calculate position of EEMC cluster using intersection of max ESMD strips under
  // cluster seed tower. Returns (0,0,0) for failure.
  TVector3 getEEmcClusterPosition(const StEEmcCluster& cluster);

  Float_t mMinimumET; // in GeV
  Float_t mRadius;    // in sqrt( deta**2 + dphi**2 )
  Float_t mSmdRange;  // in cm
  Int_t   mCompressLevel; // 0=No compression, 1=SMD only, 2=All


  ClassDef(StGammaCandidateMaker,2);

};

#endif
