#ifndef LaserTpcPadChargeHistogramGroup_h
#define LaserTpcPadChargeHistogramGroup_h


#include "Rtypes.h"
#include "HistogramGroup.h"


class LaserTpcPadChargeHistogramGroup : public HistogramGroup {
 public:
  LaserTpcPadChargeHistogramGroup(const char* group="laser", const char* subGroup="pad charge", const char* trigger="laser", const char* detector="tpc");
  ~LaserTpcPadChargeHistogramGroup();
  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 
 private:
  static unsigned int mSec[6]; // sector indices start at 0
  TH2* hTpcSec[6];
  ClassDef(LaserTpcPadChargeHistogramGroup,1) ;
};


#endif
