#include <StRTpcWirePlane.h>

ClassImp(StRTpcWirePlane)
float  StRTpcWirePlane::anodeWireRadius() const {
   return mWirePlane.anodeWireRadius;
}

float  StRTpcWirePlane::frischGridWireRadius() const {
   return mWirePlane.frischGridWireRadius;
}
   
float  StRTpcWirePlane::gateWireRadius() const {
   return mWirePlane.gatingGridWireRadius;
}
  
float  StRTpcWirePlane::anodeWirePitch() const {
   return mWirePlane.anodeWirePitch;
}

float  StRTpcWirePlane::frischGridPitch() const {
   return mWirePlane.frischGridWirePitch;
}

float  StRTpcWirePlane::gatePitch() const {
   return mWirePlane.gatingGridWirePitch;
}
  
float  StRTpcWirePlane::innerSectorAnodeWirePadPlaneSeparation() const {
   return mWirePlane.innerSectorAnodeWirePadPlaneSeparation;
}

float  StRTpcWirePlane::innerSectorFrischGridPadPlaneSeparation() const {
   return mWirePlane.innerSectorFrischGridPadPlaneSeparation;
}

float  StRTpcWirePlane::innerSectorGatingGridPadPlaneSeparation() const {
   return mWirePlane.innerSectorGatingGridPadPlaneSeparation;
}

float  StRTpcWirePlane::outerSectorAnodeWirePadPlaneSeparation() const {
   return mWirePlane.outerSectorAnodeWirePadPlaneSeparation;
}

float  StRTpcWirePlane::outerSectorFrischGridPadPlaneSeparation() const {
   return mWirePlane.outerSectorFrischGridPadPlaneSeparation;
}

float  StRTpcWirePlane::outerSectorGatingGridPadPlaneSeparation() const {
   return mWirePlane.outerSectorGatingGridPadPlaneSeparation;
}

int    StRTpcWirePlane::numberOfInnerSectorAnodeWires() const {
   return mWirePlane.numberOfInnerSectorAnodeWires;
}

int    StRTpcWirePlane::numberOfInnerSectorFrischGridWires() const {
   return mWirePlane.numberOfInnerSectorFrischGridWires;
}

int    StRTpcWirePlane::numberOfInnerSectorGatingGridWires() const {
   return mWirePlane.numberOfInnerSectorGatingGridWires;
}

float  StRTpcWirePlane::firstInnerSectorAnodeWire() const {
   return mWirePlane.firstInnerSectorAnodeWire;
}

float  StRTpcWirePlane::firstInnerSectorFrischGridWire() const {
   return mWirePlane.firstInnerSectorFrischGridWire;
}

float  StRTpcWirePlane::firstInnerSectorGatingGridWire() const {
   return mWirePlane.firstInnerSectorGatingGridWire;
}

float  StRTpcWirePlane::lastInnerSectorAnodeWire() const {
   return mWirePlane.lastInnerSectorAnodeWire;
}

int    StRTpcWirePlane::numberOfOuterSectorAnodeWires() const {
   return mWirePlane.numberOfOuterSectorAnodeWires;
}

int    StRTpcWirePlane::numberOfOuterSectorFrischGridWires() const {
   return mWirePlane.numberOfOuterSectorFrischGridWires;
}

int    StRTpcWirePlane::numberOfOuterSectorGatingGridWires() const {
   return mWirePlane.numberOfOuterSectorGatingGridWires;
}

float  StRTpcWirePlane::firstOuterSectorAnodeWire() const {
   return mWirePlane.firstOuterSectorAnodeWire;
}

float  StRTpcWirePlane::firstOuterSectorFrischGridWire() const {
   return mWirePlane.firstOuterSectorFrischGridWire;
}

float  StRTpcWirePlane::firstOuterSectorGatingGridWire() const {
   return mWirePlane.firstOuterSectorGatingGridWire;
}

float  StRTpcWirePlane::lastOuterSectorAnodeWire() const {
   return mWirePlane.lastOuterSectorAnodeWire;
}


















