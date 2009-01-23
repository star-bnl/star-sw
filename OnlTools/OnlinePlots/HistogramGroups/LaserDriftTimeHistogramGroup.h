#ifndef LaserDriftTimeHistogramGroup_h
#define LaserDriftTimeHistogramGroup_h


#include "Rtypes.h"
#include "HistogramGroup.h"

class StReadLaserEvent;

class LaserDriftTimeHistogramGroup : public HistogramGroup {
 public:
  LaserDriftTimeHistogramGroup(const char* group="laser", const char* subGroup="drift time", const char* trigger="laser", const char* detector="tpc");
  ~LaserDriftTimeHistogramGroup();
  virtual void reset();
  virtual bool fill(evpReader* evp, char* datap);
  virtual void draw(TCanvas* cc); 
 private:
  TH1* hDriftTime;
  StReadLaserEvent* mLaser; //! don't create a streamer for this
  ClassDef(LaserDriftTimeHistogramGroup,1) ;
};


#endif
