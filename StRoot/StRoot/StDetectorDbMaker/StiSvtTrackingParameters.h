#ifndef StiSvtTrackingParameters_h
#define StiSvtTrackingParameters_h
#include "StDetectorDbMaker/StiTrackingParameters.h"
class StiSvtTrackingParameters : public StiTrackingParameters {
 public:
  static StiSvtTrackingParameters* 	instance();
 protected:
  StiSvtTrackingParameters(St_TrackingParameters *table=0) : StiTrackingParameters(table) {}
  virtual ~StiSvtTrackingParameters() {fgInstance = 0;}
 private:
  static StiSvtTrackingParameters* fgInstance;
  ClassDef(StiSvtTrackingParameters,1) //C++ TChair for SvtTrackingParameters table class
};
#endif
