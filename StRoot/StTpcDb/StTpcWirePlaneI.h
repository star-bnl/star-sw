/***************************************************************************
 *
 * $Id: StTpcWirePlaneI.h,v 1.2 1999/12/16 22:00:54 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Abstract Interface for TPC wire Plane geometry  
 *
 ***************************************************************************
 *
 * $Log: StTpcWirePlaneI.h,v $
 * Revision 1.2  1999/12/16 22:00:54  hardtke
 * add CVS tags
 *
 **************************************************************************/
#ifndef __STTPCWIREPLANEI__
#define __STTPCWIREPLANEI__
#include <TObject.h>

class StTpcWirePlaneI : public TObject {

  //Abstract base class defining accessors
public:

  virtual float  anodeWireRadius()                           const = 0;
  virtual float  frischGridWireRadius()                      const = 0;
  virtual float  gateWireRadius()                            const = 0;
  
  virtual float  anodeWirePitch()                            const = 0;
  virtual float  frischGridPitch()                           const = 0;
  virtual float  gatePitch()                                 const = 0;
  
  virtual float  innerSectorAnodeWirePadPlaneSeparation()    const = 0;
  virtual float  innerSectorFrischGridPadPlaneSeparation()   const = 0;
  virtual float  innerSectorGatingGridPadPlaneSeparation()   const = 0;

  virtual float  outerSectorAnodeWirePadPlaneSeparation()    const = 0;
  virtual float  outerSectorFrischGridPadPlaneSeparation()   const = 0;
  virtual float  outerSectorGatingGridPadPlaneSeparation()   const = 0;

  virtual int    numberOfInnerSectorAnodeWires()             const = 0;
  virtual int    numberOfInnerSectorFrischGridWires()        const = 0;
  virtual int    numberOfInnerSectorGatingGridWires()        const = 0;
  virtual float  firstInnerSectorAnodeWire()                 const = 0;
  virtual float  firstInnerSectorFrischGridWire()            const = 0;
  virtual float  firstInnerSectorGatingGridWire()            const = 0;
  virtual float  lastInnerSectorAnodeWire()                  const = 0;

  virtual int    numberOfOuterSectorAnodeWires()             const = 0;
  virtual int    numberOfOuterSectorFrischGridWires()        const = 0;
  virtual int    numberOfOuterSectorGatingGridWires()        const = 0;
  virtual float  firstOuterSectorAnodeWire()                 const = 0;
  virtual float  firstOuterSectorFrischGridWire()            const = 0;
  virtual float  firstOuterSectorGatingGridWire()            const = 0;
  virtual float  lastOuterSectorAnodeWire()                  const = 0;


ClassDef(StTpcWirePlaneI,0)

};
#endif















