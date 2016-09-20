#ifndef StiSstTrackingParameters_h
#define StiSstTrackingParameters_h
#include "StDetectorDbMaker/StiTrackingParameters.h"
class StiSstTrackingParameters : public StiTrackingParameters {
 public:
  static StiSstTrackingParameters* 	instance();
 protected:
  StiSstTrackingParameters(St_TrackingParameters *table=0) : StiTrackingParameters(table) {}
  virtual ~StiSstTrackingParameters() {fgInstance = 0;}
 private:
  static StiSstTrackingParameters* fgInstance;
  ClassDef(StiSstTrackingParameters,1) //C++ TChair for SstTrackingParameters table class
};
#endif
