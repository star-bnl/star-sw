#ifndef StSpectraAnalysis_hh
#define StSpectraAnalysis_hh

#include "StParticleDefinition.hh"
#include "StParticleTable.hh"
#include "StEfficiency.h"
#include "StEvent.h"

class StSpectraAnalysis {

 private:

 protected:

  char* mTitle;
  StParticleDefinition* mParticle;
  double* mYBinSize;
  double* mMtBinSize;
  StEfficiency* mEffic;
  int* mNumEvent;

 public:
  StSpectraAnalysis();
  ~StSpectraAnalysis();

  void setTitle(char title[20]);
  char getTitle();

  void setParticle(char particle[20]);
  StParticleDefinition* getParticle();

  void setYBinSize(double ybin);
  double getYBinSize();
  void setMtBinSize(double mtbin);
  double getMtBinSize();

  void setEfficiencyParam(StEfficiency* effic);
  StEfficiency* getEfficiencyParam();

  void bookHistograms();
  void fillHistograms(StEvent& event);
  void projectHistograms();
};

#endif

