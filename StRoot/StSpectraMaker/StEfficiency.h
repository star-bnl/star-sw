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

class StEfficiency {

 private:

  StParticleDefinition* mParticle;

  double mScale;
  double mMomentumTerm;
  TH2D mEfficHistogram;

 protected:

 public:
  StEfficiency();
  StEfficiency(char* efficFile);
  ~StEfficiency();
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
  TH2D getEfficHistogram();

  double getLowEdge(char axis);
  double getUpEdge(char axis);
  int getNbin(char axis);

};

#endif
