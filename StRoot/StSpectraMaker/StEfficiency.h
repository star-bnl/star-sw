#ifndef StEfficiency_hh
#define StEfficiency_hh
#include "StSpectraCut.h"
#include "StParticleDefinition.hh"
#include "StParticleTable.hh"
#include "StEventTypes.h"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "TH2.h"
class TH2D;

enum efficiencyType {function, histogram};
istream &operator>>(istream &is, efficiencyType &efficType);
ostream &operator<<(ostream &os, const efficiencyType &efficType);

class StEfficiency {

 private:

  efficiencyType mEfficType;
  StParticleDefinition* mParticle;

  double mScale;
  double mMomentumTerm;
  TH2D* mHistoEffic;

 protected:

 public:
  StEfficiency();
  StEfficiency(efficiencyType efficType, char* efficFile);
  ~StEfficiency();
  friend istream &operator>>(istream &is, StEfficiency &effic);
  friend ostream &operator<<(ostream &os, const StEfficiency &effic);
  //
  // could not get this as private
  //
  #ifdef ST_NO_TEMPLATE_DEF_ARGS
  vector<StSpectraCut*, allocator<StSpectraCut*> > mSpectraCutContainer;//!
  #else
  vector<StSpectraCut*> mSpectraCutContainer;//!
  #endif
  //
  void setParticle(string particle);
  double efficiency(StTrack* track);

};

#endif
