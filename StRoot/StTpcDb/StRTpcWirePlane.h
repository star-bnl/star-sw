/***************************************************************************
 *
 * $Id: StRTpcWirePlane.h,v 1.9 2000/11/14 22:00:06 genevb Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Wire Plane Geometry Interface  
 *
 ***************************************************************************
 *
 * $Log: StRTpcWirePlane.h,v $
 * Revision 1.9  2000/11/14 22:00:06  genevb
 * Switched several functions from float to double
 *
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
  double  anodeWireRadius()                          const;
  double  frischGridWireRadius()                     const;
  double  gateWireRadius()                           const;
  
  double  anodeWirePitch()                           const;
  double  frischGridPitch()                          const;
  double  gatePitch()                                const;
  
  double  innerSectorAnodeWirePadPlaneSeparation()   const;
  double  innerSectorFrischGridPadPlaneSeparation()  const;
  double  innerSectorGatingGridPadPlaneSeparation()  const;

  double  outerSectorAnodeWirePadPlaneSeparation()   const;
  double  outerSectorFrischGridPadPlaneSeparation()  const;
  double  outerSectorGatingGridPadPlaneSeparation()  const;

  int    numberOfInnerSectorAnodeWires()             const;
  int    numberOfInnerSectorFrischGridWires()        const;
  int    numberOfInnerSectorGatingGridWires()        const;
  double  firstInnerSectorAnodeWire()                const;
  double  firstInnerSectorFrischGridWire()           const;
  double  firstInnerSectorGatingGridWire()           const;
  double  lastInnerSectorAnodeWire()                 const;

  int    numberOfOuterSectorAnodeWires()             const;
  int    numberOfOuterSectorFrischGridWires()        const;
  int    numberOfOuterSectorGatingGridWires()        const;
  double  firstOuterSectorAnodeWire()                const;
  double  firstOuterSectorFrischGridWire()           const;
  double  firstOuterSectorGatingGridWire()           const;
  double  lastOuterSectorAnodeWire()                 const;


ClassDef(StRTpcWirePlane,0)

};

inline double  StRTpcWirePlane::anodeWireRadius() const {
   return (*mWirePlane)[0].anodeWireRadius;
}

inline double  StRTpcWirePlane::frischGridWireRadius() const {
   return (*mWirePlane)[0].frischGridWireRadius;
}
   
inline double  StRTpcWirePlane::gateWireRadius() const {
   return (*mWirePlane)[0].gatingGridWireRadius;
}
  
inline double  StRTpcWirePlane::anodeWirePitch() const {
   return (*mWirePlane)[0].anodeWirePitch;
}

inline double  StRTpcWirePlane::frischGridPitch() const {
   return (*mWirePlane)[0].frischGridWirePitch;
}

inline double  StRTpcWirePlane::gatePitch() const {
   return (*mWirePlane)[0].gatingGridWirePitch;
}
  
inline double  StRTpcWirePlane::innerSectorAnodeWirePadPlaneSeparation() const {
   return (*mWirePlane)[0].innerSectorAnodeWirePadSep;
}

inline double  StRTpcWirePlane::innerSectorFrischGridPadPlaneSeparation() const {
   return (*mWirePlane)[0].innerSectorFrischGridPadSep;
}

inline double  StRTpcWirePlane::innerSectorGatingGridPadPlaneSeparation() const {
   return (*mWirePlane)[0].innerSectorGatingGridPadSep;
}

inline double  StRTpcWirePlane::outerSectorAnodeWirePadPlaneSeparation() const {
   return (*mWirePlane)[0].outerSectorAnodeWirePadSep;
}

inline double  StRTpcWirePlane::outerSectorFrischGridPadPlaneSeparation() const {
   return (*mWirePlane)[0].outerSectorFrischGridPadSep;
}

inline double  StRTpcWirePlane::outerSectorGatingGridPadPlaneSeparation() const {
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

inline double  StRTpcWirePlane::firstInnerSectorAnodeWire() const {
   return (*mWirePlane)[0].firstInnerSectorAnodeWire;
}

inline double  StRTpcWirePlane::firstInnerSectorFrischGridWire() const {
   return (*mWirePlane)[0].firstInnerSectorFrischGridWire;
}

inline double  StRTpcWirePlane::firstInnerSectorGatingGridWire() const {
   return (*mWirePlane)[0].firstInnerSectorGatingGridWire;
}

inline double  StRTpcWirePlane::lastInnerSectorAnodeWire() const {
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

inline double  StRTpcWirePlane::firstOuterSectorAnodeWire() const {
   return (*mWirePlane)[0].firstOuterSectorAnodeWire;
}

inline double  StRTpcWirePlane::firstOuterSectorFrischGridWire() const {
   return (*mWirePlane)[0].firstOuterSectorFrischGridWire;
}

inline double  StRTpcWirePlane::firstOuterSectorGatingGridWire() const {
   return (*mWirePlane)[0].firstOuterSectorGatingGridWire;
}

inline double  StRTpcWirePlane::lastOuterSectorAnodeWire() const {
   return (*mWirePlane)[0].lastOuterSectorAnodeWire;
}

#endif
