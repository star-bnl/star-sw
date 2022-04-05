#ifndef StiDefaultTrackingParameters_h
#define StiDefaultTrackingParameters_h
#include "StiTrackingParameters.h"
class StiDefaultTrackingParameters : public StiTrackingParameters {
 public:
  static StiDefaultTrackingParameters* 	instance();
 protected:
  StiDefaultTrackingParameters(St_TrackingParameters *table=0) : StiTrackingParameters(table) {}
  virtual ~StiDefaultTrackingParameters() {fgInstance = 0;}
 private:
  static StiDefaultTrackingParameters* fgInstance;
  ClassDef(StiDefaultTrackingParameters,1) //C++ TChair for DefaultTrackingParameters table class
};
#endif
