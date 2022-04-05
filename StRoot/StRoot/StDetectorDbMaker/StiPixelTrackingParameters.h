#ifndef StiPixelTrackingParameters_h
#define StiPixelTrackingParameters_h
#include "StDetectorDbMaker/StiTrackingParameters.h"
class StiPixelTrackingParameters : public StiTrackingParameters {
 public:
  static StiPixelTrackingParameters* 	instance();
 protected:
  StiPixelTrackingParameters(St_TrackingParameters *table=0) : StiTrackingParameters(table) {}
  virtual ~StiPixelTrackingParameters() {fgInstance = 0;}
 private:
  static StiPixelTrackingParameters* fgInstance;
  ClassDef(StiPixelTrackingParameters,1) //C++ TChair for PixelTrackingParameters table class
};
#endif
