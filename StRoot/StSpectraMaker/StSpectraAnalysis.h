#ifndef StSpectraAnalysis_hh
#define StSpectraAnalysis_hh

#include "StParticleDefinition.hh"
#include "StParticleTable.hh"
#include "StEfficiency.h"
#include "StEventTypes.h"
//
// a base class
// for all derived StSpectraAnalysis classes
//
class StSpectraAnalysis {

 private:

 protected:

  string mTitle;
  StParticleDefinition* mParticle;
  StEfficiency mEffic;
  int mNumEvent;

 public:
  virtual ~StSpectraAnalysis();

  void setTitle(string title);
  string getTitle();

  void setParticle(string particle);
  StParticleDefinition* getParticle();

  void setEfficiency(StEfficiency effic);
  StEfficiency* getEfficiency();

  virtual void bookHistograms()=0;
  virtual void fillHistograms(StEvent& event)=0;
  virtual void projectHistograms()=0;
};

#endif

