/***************************************************************************
 *
 * $Id: StRTpcWirePlane.cxx,v 1.5.4.1 2007/08/12 23:27:43 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Wire Plane Geometry interface  
 *
 ***************************************************************************
 *
 * $Log: StRTpcWirePlane.cxx,v $
 * Revision 1.5.4.1  2007/08/12 23:27:43  jeromel
 * Further fixes for SL06g built for SL44
 *
 * Revision 1.6  2007/08/04 00:38:04  jeromel
 * SL4 issue: Removal of the inline func, moved to class implementation.
 *     Symbols may otherwise be hidden.
 *
 * Revision 1.5  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#include "StRTpcWirePlane.h"

ClassImp(StRTpcWirePlane)


double  StRTpcWirePlane::anodeWireRadius() const {
   return (*mWirePlane)[0].anodeWireRadius;
}

double  StRTpcWirePlane::frischGridWireRadius() const {
   return (*mWirePlane)[0].frischGridWireRadius;
}
   
double  StRTpcWirePlane::gateWireRadius() const {
   return (*mWirePlane)[0].gatingGridWireRadius;
}
  
double  StRTpcWirePlane::anodeWirePitch() const {
   return (*mWirePlane)[0].anodeWirePitch;
}

double  StRTpcWirePlane::frischGridPitch() const {
   return (*mWirePlane)[0].frischGridWirePitch;
}

double  StRTpcWirePlane::gatePitch() const {
   return (*mWirePlane)[0].gatingGridWirePitch;
}
  
double  StRTpcWirePlane::innerSectorAnodeWirePadPlaneSeparation() const {
   return (*mWirePlane)[0].innerSectorAnodeWirePadSep;
}

double  StRTpcWirePlane::innerSectorFrischGridPadPlaneSeparation() const {
   return (*mWirePlane)[0].innerSectorFrischGridPadSep;
}

double  StRTpcWirePlane::innerSectorGatingGridPadPlaneSeparation() const {
   return (*mWirePlane)[0].innerSectorGatingGridPadSep;
}

double  StRTpcWirePlane::outerSectorAnodeWirePadPlaneSeparation() const {
   return (*mWirePlane)[0].outerSectorAnodeWirePadSep;
}

double  StRTpcWirePlane::outerSectorFrischGridPadPlaneSeparation() const {
   return (*mWirePlane)[0].outerSectorFrischGridPadSep;
}

double  StRTpcWirePlane::outerSectorGatingGridPadPlaneSeparation() const {
   return (*mWirePlane)[0].outerSectorGatingGridPadSep;
}

int StRTpcWirePlane::numberOfInnerSectorAnodeWires() const {
   return (*mWirePlane)[0].numInnerSectorAnodeWires;
}

int  StRTpcWirePlane::numberOfInnerSectorFrischGridWires() const {
   return (*mWirePlane)[0].numInnerSectorFrischGridWires;
}

int StRTpcWirePlane::numberOfInnerSectorGatingGridWires() const {
   return (*mWirePlane)[0].numInnerSectorGatingGridWires;
}

double  StRTpcWirePlane::firstInnerSectorAnodeWire() const {
   return (*mWirePlane)[0].firstInnerSectorAnodeWire;
}

double  StRTpcWirePlane::firstInnerSectorFrischGridWire() const {
   return (*mWirePlane)[0].firstInnerSectorFrischGridWire;
}

double  StRTpcWirePlane::firstInnerSectorGatingGridWire() const {
   return (*mWirePlane)[0].firstInnerSectorGatingGridWire;
}

double  StRTpcWirePlane::lastInnerSectorAnodeWire() const {
   return (*mWirePlane)[0].lastInnerSectorAnodeWire;
}

int  StRTpcWirePlane::numberOfOuterSectorAnodeWires() const {
   return (*mWirePlane)[0].numOuterSectorAnodeWires;
}

int StRTpcWirePlane::numberOfOuterSectorFrischGridWires() const {
   return (*mWirePlane)[0].numOuterSectorFrischGridWires;
}

int StRTpcWirePlane::numberOfOuterSectorGatingGridWires() const {
   return (*mWirePlane)[0].numOuterSectorGatingGridWires;
}

double  StRTpcWirePlane::firstOuterSectorAnodeWire() const {
   return (*mWirePlane)[0].firstOuterSectorAnodeWire;
}

double  StRTpcWirePlane::firstOuterSectorFrischGridWire() const {
   return (*mWirePlane)[0].firstOuterSectorFrischGridWire;
}

double  StRTpcWirePlane::firstOuterSectorGatingGridWire() const {
   return (*mWirePlane)[0].firstOuterSectorGatingGridWire;
}

double  StRTpcWirePlane::lastOuterSectorAnodeWire() const {
   return (*mWirePlane)[0].lastOuterSectorAnodeWire;
}


