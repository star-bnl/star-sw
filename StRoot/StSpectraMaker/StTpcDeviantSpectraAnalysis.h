#ifndef StTpcDeviantSpectraAnalysis_hh
#define StTpcDeviantSpectraAnalysis_hh

#include "TH1.h"
#include "TH2.h"
#include "TH3.h"
class TH1D;
class TH2D;
class TH3D;

#include "StSpectraAnalysis.h"
#include "StEventTypes.h"
#include "SystemOfUnits.h"
#include "PhysicalConstants.h"
#include "StTpcDedxPidAlgorithm.h"

class StTpcDeviantSpectraAnalysis : public  StSpectraAnalysis {

 private:

  TH3D* mYMtDeviant;
  TH2D* mYMt;
  TH1D* mPIDDeviant;  
  TH2D* mDedxvsP;

  float mlYbin;
  float muYbin;
  int   mnYbin;
  float mlMtbin;
  float muMtbin;
  int   mnMtbin;

 protected:

 public:

  StTpcDeviantSpectraAnalysis();
  ~StTpcDeviantSpectraAnalysis();
  void bookHistograms();
  void fillHistograms(StEvent& event);
  void projectHistograms();

  void setYAxis(float lYbin, float uYbin, int nYbin);
  void setMtAxis(float lMtbin, float uMtbin, int nMtbin);


};

#endif







