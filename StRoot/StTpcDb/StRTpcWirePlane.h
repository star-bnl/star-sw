#ifndef __STRTPCWIREPLANE__
#define __STRPCWIREPLANE__
#include "StTpcWirePlaneI.h"
#include "StDbLib/Geometry/tpcWirePlanes.h"

class StRTpcWirePlane : public StTpcWirePlaneI {

 private:

   tpcWirePlanes* mWirePlane;

public:
  StRTpcWirePlane(){}
  ~StRTpcWirePlane(){}
  void AddData(tpcWirePlanes* WireIn){
    mWirePlane = WireIn;
  }

  //Implements Abstract Interface
  float  anodeWireRadius()                           const;
  float  frischGridWireRadius()                      const;
  float  gateWireRadius()                            const;
  
  float  anodeWirePitch()                            const;
  float  frischGridPitch()                           const;
  float  gatePitch()                                 const;
  
  float  innerSectorAnodeWirePadPlaneSeparation()    const;
  float  innerSectorFrischGridPadPlaneSeparation()   const;
  float  innerSectorGatingGridPadPlaneSeparation()   const;

  float  outerSectorAnodeWirePadPlaneSeparation()    const;
  float  outerSectorFrischGridPadPlaneSeparation()   const;
  float  outerSectorGatingGridPadPlaneSeparation()   const;

  int    numberOfInnerSectorAnodeWires()             const;
  int    numberOfInnerSectorFrischGridWires()        const;
  int    numberOfInnerSectorGatingGridWires()        const;
  float  firstInnerSectorAnodeWire()                 const;
  float  firstInnerSectorFrischGridWire()            const;
  float  firstInnerSectorGatingGridWire()            const;
  float  lastInnerSectorAnodeWire()                  const;

  int    numberOfOuterSectorAnodeWires()             const;
  int    numberOfOuterSectorFrischGridWires()        const;
  int    numberOfOuterSectorGatingGridWires()        const;
  float  firstOuterSectorAnodeWire()                 const;
  float  firstOuterSectorFrischGridWire()            const;
  float  firstOuterSectorGatingGridWire()            const;
  float  lastOuterSectorAnodeWire()                  const;


ClassDef(StRTpcWirePlane,0)

};
#endif















