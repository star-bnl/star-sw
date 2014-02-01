#ifndef StiPxlTrackingParameters_h
#define StiPxlTrackingParameters_h
#include "StDetectorDbMaker/StiTrackingParameters.h"
class StiPxlTrackingParameters : public StiTrackingParameters {
 public:
  static StiPxlTrackingParameters* 	instance();
 protected:
  StiPxlTrackingParameters(St_TrackingParameters *table=0) : StiTrackingParameters(table) {}
  virtual ~StiPxlTrackingParameters() {fgInstance = 0;}
 private:
  static StiPxlTrackingParameters* fgInstance;
  ClassDef(StiPxlTrackingParameters,1) //C++ TChair for PixelTrackingParameters table class
};
#endif
