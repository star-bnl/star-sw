#ifndef StEfficiency_hh
#define StEfficiency_hh
#include "StSpectraCut.h"
#include "StSpectraAxesEnumeration.h"
#include "StParticleDefinition.hh"
#include "StEventTypes.h"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "TH2.h"

class TH2D;

#include <vector>
#if !defined(ST_NO_NAMESPACES)
using std::vector;
#endif

class StEfficiency {

 private:

  StParticleDefinition* mParticle;
  TH2D mEfficHistogram;
  StSpectraOrdinate mOrdinate ;
  StSpectraAbscissa mAbscissa ;

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

  StSpectraOrdinate getOrdinate();
  StSpectraAbscissa getAbscissa();

  double getLowEdge(char axis);
  double getUpEdge(char axis);
  int getNbin(char axis);

};

#endif
