#ifndef StiSsdTrackingParameters_h
#define StiSsdTrackingParameters_h
#include "StDetectorDbMaker/StiTrackingParameters.h"
class StiSsdTrackingParameters : public StiTrackingParameters {
 public:
  static StiSsdTrackingParameters* 	instance();
 protected:
  StiSsdTrackingParameters(St_TrackingParameters *table=0) : StiTrackingParameters(table) {}
  virtual ~StiSsdTrackingParameters() {fgInstance = 0;}
 private:
  static StiSsdTrackingParameters* fgInstance;
  ClassDef(StiSsdTrackingParameters,1) //C++ TChair for SsdTrackingParameters table class
};
#endif
