#ifndef StSpectraAnalysis_hh
#define StSpectraAnalysis_hh

#include "StParticleDefinition.hh"
#include "StParticleTable.hh"
#include "StEfficiency.h"
#include "StEvent.h"

class StSpectraAnalysis {

 private:

 protected:

  string mTitle;
  StParticleDefinition* mParticle;
  StEfficiency mEffic;
  int mNumEvent;

 public:
  StSpectraAnalysis();
  ~StSpectraAnalysis();

  void setTitle(string title);
  string getTitle();

  void setParticle(string particle);
  StParticleDefinition* getParticle();

  void setEfficiencyParam(StEfficiency effic);
  StEfficiency* getEfficiencyParam();

  void bookHistograms();
  void fillHistograms(StEvent& event);
  void projectHistograms();
};

#endif

