/***************************************************************************
 *
 * $Id: StRTpcDimensions.cxx,v 1.7 2000/11/14 22:00:05 genevb Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description:  Root Implementation of Tpc Dimensions Class
 *
 ***************************************************************************
 *
 * $Log: StRTpcDimensions.cxx,v $
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


ClassImp(StRTpcDimensions)

