/*
 * StiDetector represents a detector for the purposes of ITTF tracking.
 * It contains all information about the geometry of the detector and
 * the necessary physical properties for incorporating it in tracking.
 */

#ifndef STI_DETECTOR_HH
#define STI_DETECTOR_HH

#include "TObject.h"

class StiDetector : public TObject{

  public:
  
  // con/destructor
  StiDetector();
  virtual ~StiDetector();

  Bool_t isActive() const { return active; }
  Bool_t isContinuousMedium() const { return continuousMedium; }
  Bool_t isDiscreteScatterer() const { return discreteScatterer; }
  Float_t getDensity() const { return density; }
  Float_t getThickness() const { return thickness; }
  Float_t getHalfWidth() const { return halfWidth; }
  Float_t getHalfDepth() const { return halfDepth; }
  Float_t getMaterialRadLength() const { return radLength; }
  Float_t getRadLengthThickness() const { return thickness/radLength; }
  Float_t getPosition() const { return position; }
  Float_t getRefAngle() const { return refAngle; }
  Int_t getShapeCode() const { return shapeCode; }

  enum {kPlanar = 1, kCircular}; // shapeCode constants

  protected:

  Bool_t active;              // does the object provide hit information?
  Bool_t continuousMedium;    // is this a continuous scatterer?
  Bool_t discreteScatterer;   // is this a discrete scatterer?
  Float_t density;            // g/cm^3 [STAR units]
  Float_t thickness;          // extent in local x (global r) in cm
  Float_t halfWidth;          // 1/2 extent in local y (global phi) in cm
  Float_t halfDepth;          // 1/2 extent in z in cm
  Float_t radLength;          // cm
  Float_t position;           // perpendicular distance to global origin
  Float_t refAngle;           // angle of normal to object in global coords
  Int_t shapeCode;            // 1 if planar, 2 if circular

};


#endif
