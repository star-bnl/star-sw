/***************************************************************************
 *
 * $Id: StRichPIDTraits.h,v 2.1 2001/03/27 03:33:05 perev Exp $
 *
 * Author: Matt Horsley, March 29, 2000
 ***************************************************************************
 *
 * Description: patterened after StDedxPidTraits.h
 **************************************************************************/
#ifndef StRichPIDTraits_hh
#define StRichPIDTraits_hh

#include "StEvent/StTrackPidTraits.h"
#include "StParticleDefinition.hh"
#include "TArrayD.h"


class StParticleDefinition;

class StRichPIDTraits : public StTrackPidTraits {

public:
  StRichPIDTraits();
  StRichPIDTraits(StDetectorId, StParticleDefinition*);
  // StRichPIDTraits& operator=(const StRichPIDTraits&); use default

  virtual ~StRichPIDTraits();
  StParticleDefinition*  particle() const;
  void                   addAreaArray(TArrayD&);
  void                   addHitArray(TArrayD&);
  TArrayD                 getDensityArray(); 
  TArrayD&                getHitArray();
  TArrayD&                getAreaArray();
  void                   addNewRingArea(double);
  double                getNewRingArea();
  void                  addRHits(int);
  int                   getRHits();
  void                  setCut(double);
  double                getCut();
  

protected:
  StParticleDefinition*  mParticle;   // !
  TArrayD                mAreaArray; //!
  TArrayD                mHitArray; //!
  
  int mRichHits;
  double mCut;
  double mNewRingArea;

  StObject* clone() const;
  ClassDef(StRichPIDTraits,1)
};
#endif
