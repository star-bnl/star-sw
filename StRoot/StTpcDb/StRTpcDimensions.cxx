/***************************************************************************
 *
 * $Id: StRTpcDimensions.cxx,v 1.7.6.1 2007/08/13 01:04:41 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description:  Root Implementation of Tpc Dimensions Class
 *
 ***************************************************************************
 *
 * $Log: StRTpcDimensions.cxx,v $
 * Revision 1.7.6.1  2007/08/13 01:04:41  jeromel
 * Patches for SL07a, SL44
 *
 * Revision 1.7.4.1  2007/08/12 23:27:40  jeromel
 * Further fixes for SL06g built for SL44
 *
 * Revision 1.8  2007/08/04 00:38:03  jeromel
 * SL4 issue: Removal of the inline func, moved to class implementation.
 *     Symbols may otherwise be hidden.
 *
 * Revision 1.7  2000/11/14 22:00:05  genevb
 * Switched several functions from float to double
 *
 * Revision 1.6  2000/02/15 22:21:47  hardtke
 * Add effective drift distances
 *
 * Revision 1.5  1999/12/16 21:34:37  hardtke
 * add CVS tag
 *
 **************************************************************************/
#include "StRTpcDimensions.h"

ClassImp(StRTpcDimensions)

double StRTpcDimensions::gatingGridZ() const {
  double distance;
  if (wp && pp){
  distance = pp->outerSectorPadPlaneZ() - wp->outerSectorGatingGridPadPlaneSeparation();
  }
  else {
    distance = -999;
  }
  return distance;
}

double StRTpcDimensions::innerEffectiveDriftDistance() const {
  return gatingGridZ() + zInnerOffset();
}

double StRTpcDimensions::outerEffectiveDriftDistance() const {
  return gatingGridZ() + zOuterOffset();
}

void StRTpcDimensions::AddData( St_tpcDimensions* TpcIn){ mTpc = TpcIn;}

void StRTpcDimensions::AddData( St_tpcEffectiveGeom* GeoIn){ mEffGeo = GeoIn;}

int StRTpcDimensions::numberOfSectors() const {
return (*mTpc)[0].numberOfSectors;
}

double StRTpcDimensions::ifcRadius() const {
return (*mTpc)[0].tpcInnerRadius;
}
    
double StRTpcDimensions::ofcRadius() const {
return (*mTpc)[0].tpcOuterRadius;
}
    
double StRTpcDimensions::tpcTotalLength() const {
return (*mTpc)[0].tpcTotalLength;
}

double StRTpcDimensions::wheelInnerRadius() const {
return (*mTpc)[0].wheelInnerRadius;
}

double StRTpcDimensions::wheelOuterRadius() const {
return (*mTpc)[0].wheelOuterRadius;
}

double StRTpcDimensions::wheelThickness() const {
return (*mTpc)[0].wheelThickness;
}

double StRTpcDimensions::senseGasOuterRadius() const {
return (*mTpc)[0].senseGasOuterRadius;
}
    
double StRTpcDimensions::tpeaThickness() const {
return (*mTpc)[0].tpeaThickness; 
}

double StRTpcDimensions::cathodeInnerRadius() const {
return (*mTpc)[0].cathodeInnerRadius;
}
    
double StRTpcDimensions::cathodeOuterRadius() const {
return (*mTpc)[0].cathodeOuterRadius;
}
    
double StRTpcDimensions::cathodeThickness() const {
return (*mTpc)[0].cathodeThickness;
} 

double StRTpcDimensions::zInnerOffset() const {
  return (*mEffGeo)[0].z_inner_offset;
}

double StRTpcDimensions::zOuterOffset() const {
  return (*mEffGeo)[0].z_outer_offset;
}

