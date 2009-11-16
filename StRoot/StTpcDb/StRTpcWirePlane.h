/***************************************************************************
 *
 * $Id: StRTpcWirePlane.h,v 1.9.6.1 2007/08/13 01:04:43 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Wire Plane Geometry Interface  
 *
 ***************************************************************************
 *
 * $Log: StRTpcWirePlane.h,v $
 * Revision 1.9.6.1  2007/08/13 01:04:43  jeromel
 * Patches for SL07a, SL44
 *
 * Revision 1.9.4.1  2007/08/12 23:27:43  jeromel
 * Further fixes for SL06g built for SL44
 *
 * Revision 1.10  2007/08/04 00:38:04  jeromel
 * SL4 issue: Removal of the inline func, moved to class implementation.
 *     Symbols may otherwise be hidden.
 *
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


#endif
