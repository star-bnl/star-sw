#ifndef StNoPidSpectraAnalysis_hh
#define StNoPidSpectraAnalysis_hh

#include "TH2.h"
class TH2D;

#include "StSpectraAnalysis.h"

class StEvent;

class StNoPidSpectraAnalysis : public StSpectraAnalysis {

 private:

  TH2D* m2DSpectra;
  
 public:

  StNoPidSpectraAnalysis();
  ~StNoPidSpectraAnalysis();

  void bookHistograms();
  void fillHistograms(StEvent& event);
  void projectHistograms();
  void writeHistograms();
};

#endif







