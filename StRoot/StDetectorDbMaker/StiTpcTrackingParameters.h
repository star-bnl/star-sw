#ifndef StiTpcTrackingParameters_h
#define StiTpcTrackingParameters_h
#include "StDetectorDbMaker/StiTrackingParameters.h"
class StiTpcTrackingParameters : public StiTrackingParameters {
 public:
  static StiTpcTrackingParameters* 	instance();
 protected:
  StiTpcTrackingParameters(St_TrackingParameters *table=0) : StiTrackingParameters(table) {}
  virtual ~StiTpcTrackingParameters() {fgInstance = 0;}
 private:
  static StiTpcTrackingParameters* fgInstance;
  ClassDef(StiTpcTrackingParameters,1) //C++ TChair for TpcTrackingParameters table class
};
#endif
