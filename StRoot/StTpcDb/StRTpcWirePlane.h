/***************************************************************************
 *
 * $Id: StRTpcWirePlane.h,v 1.8 2000/01/12 15:14:41 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Wire Plane Geometry Interface  
 *
 ***************************************************************************
 *
 * $Log: StRTpcWirePlane.h,v $
 * Revision 1.8  2000/01/12 15:14:41  hardtke
 * Update StTpcWirePlanes to use new variable names in tpcWirePlanes.idl/ Add Z position functions to StTpcPadPlane
 *
 * Revision 1.7  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#ifndef __STRTPCWIREPLANE__
#define __STRTPCWIREPLANE__
#include "StTpcWirePlaneI.h"
#include "tables/St_tpcWirePlanes_Table.h"

class StRTpcWirePlane : public StTpcWirePlaneI {

 private:

   St_tpcWirePlanes* mWirePlane;

public:
  StRTpcWirePlane(St_tpcWirePlanes* WireIn){ AddData(WireIn);}
  StRTpcWirePlane():mWirePlane(0) {}
  ~StRTpcWirePlane(){}
  void AddData(St_tpcWirePlanes* WireIn){
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

inline float  StRTpcWirePlane::anodeWireRadius() const {
   return (*mWirePlane)[0].anodeWireRadius;
}

inline float  StRTpcWirePlane::frischGridWireRadius() const {
   return (*mWirePlane)[0].frischGridWireRadius;
}
   
inline float  StRTpcWirePlane::gateWireRadius() const {
   return (*mWirePlane)[0].gatingGridWireRadius;
}
  
inline float  StRTpcWirePlane::anodeWirePitch() const {
   return (*mWirePlane)[0].anodeWirePitch;
}

inline float  StRTpcWirePlane::frischGridPitch() const {
   return (*mWirePlane)[0].frischGridWirePitch;
}

inline float  StRTpcWirePlane::gatePitch() const {
   return (*mWirePlane)[0].gatingGridWirePitch;
}
  
inline float  StRTpcWirePlane::innerSectorAnodeWirePadPlaneSeparation() const {
   return (*mWirePlane)[0].innerSectorAnodeWirePadSep;
}

inline float  StRTpcWirePlane::innerSectorFrischGridPadPlaneSeparation() const {
   return (*mWirePlane)[0].innerSectorFrischGridPadSep;
}

inline float  StRTpcWirePlane::innerSectorGatingGridPadPlaneSeparation() const {
   return (*mWirePlane)[0].innerSectorGatingGridPadSep;
}

inline float  StRTpcWirePlane::outerSectorAnodeWirePadPlaneSeparation() const {
   return (*mWirePlane)[0].outerSectorAnodeWirePadSep;
}

inline float  StRTpcWirePlane::outerSectorFrischGridPadPlaneSeparation() const {
   return (*mWirePlane)[0].outerSectorFrischGridPadSep;
}

inline float  StRTpcWirePlane::outerSectorGatingGridPadPlaneSeparation() const {
   return (*mWirePlane)[0].outerSectorGatingGridPadSep;
}

inline int StRTpcWirePlane::numberOfInnerSectorAnodeWires() const {
   return (*mWirePlane)[0].numInnerSectorAnodeWires;
}

inline int  StRTpcWirePlane::numberOfInnerSectorFrischGridWires() const {
   return (*mWirePlane)[0].numInnerSectorFrischGridWires;
}

inline int StRTpcWirePlane::numberOfInnerSectorGatingGridWires() const {
   return (*mWirePlane)[0].numInnerSectorGatingGridWires;
}

inline float  StRTpcWirePlane::firstInnerSectorAnodeWire() const {
   return (*mWirePlane)[0].firstInnerSectorAnodeWire;
}

inline float  StRTpcWirePlane::firstInnerSectorFrischGridWire() const {
   return (*mWirePlane)[0].firstInnerSectorFrischGridWire;
}

inline float  StRTpcWirePlane::firstInnerSectorGatingGridWire() const {
   return (*mWirePlane)[0].firstInnerSectorGatingGridWire;
}

inline float  StRTpcWirePlane::lastInnerSectorAnodeWire() const {
   return (*mWirePlane)[0].lastInnerSectorAnodeWire;
}

inline int  StRTpcWirePlane::numberOfOuterSectorAnodeWires() const {
   return (*mWirePlane)[0].numOuterSectorAnodeWires;
}

inline int StRTpcWirePlane::numberOfOuterSectorFrischGridWires() const {
   return (*mWirePlane)[0].numOuterSectorFrischGridWires;
}

inline int StRTpcWirePlane::numberOfOuterSectorGatingGridWires() const {
   return (*mWirePlane)[0].numOuterSectorGatingGridWires;
}

inline float  StRTpcWirePlane::firstOuterSectorAnodeWire() const {
   return (*mWirePlane)[0].firstOuterSectorAnodeWire;
}

inline float  StRTpcWirePlane::firstOuterSectorFrischGridWire() const {
   return (*mWirePlane)[0].firstOuterSectorFrischGridWire;
}

inline float  StRTpcWirePlane::firstOuterSectorGatingGridWire() const {
   return (*mWirePlane)[0].firstOuterSectorGatingGridWire;
}

inline float  StRTpcWirePlane::lastOuterSectorAnodeWire() const {
   return (*mWirePlane)[0].lastOuterSectorAnodeWire;
}

#endif
