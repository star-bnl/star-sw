#ifndef StEfficiency_hh
#define StEfficiency_hh
#include "StParticleDefinition.hh"
#include "StParticleTable.hh"
#include "StEvent.h"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"

class StEfficiency {

 private:
  StParticleDefinition* mParticle;
  double mScale;
  double mMomentumTerm;
  int mNhitCut;
  double mChiSquaredCut ;
  double mDcaCut ; 

 protected:

 public:
  StEfficiency();
  ~StEfficiency();

  void setNhitCut(int nhit);
  int nhitCut();
  void setDcaCut(double dca);
  double dcaCut();
  void init(StParticleDefinition* particle);
  double efficiency(StTrack* track);

};

#endif
