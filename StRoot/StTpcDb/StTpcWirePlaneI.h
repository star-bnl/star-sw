/***************************************************************************
 *
 * $Id: StTpcWirePlaneI.h,v 1.3 2000/11/14 22:00:06 genevb Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Abstract Interface for TPC wire Plane geometry  
 *
 ***************************************************************************
 *
 * $Log: StTpcWirePlaneI.h,v $
 * Revision 1.3  2000/11/14 22:00:06  genevb
 * Switched several functions from float to double
 *
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

  virtual double  anodeWireRadius()                          const = 0;
  virtual double  frischGridWireRadius()                     const = 0;
  virtual double  gateWireRadius()                           const = 0;
  
  virtual double  anodeWirePitch()                           const = 0;
  virtual double  frischGridPitch()                          const = 0;
  virtual double  gatePitch()                                const = 0;
  
  virtual double  innerSectorAnodeWirePadPlaneSeparation()   const = 0;
  virtual double  innerSectorFrischGridPadPlaneSeparation()  const = 0;
  virtual double  innerSectorGatingGridPadPlaneSeparation()  const = 0;

  virtual double  outerSectorAnodeWirePadPlaneSeparation()   const = 0;
  virtual double  outerSectorFrischGridPadPlaneSeparation()  const = 0;
  virtual double  outerSectorGatingGridPadPlaneSeparation()  const = 0;

  virtual int    numberOfInnerSectorAnodeWires()             const = 0;
  virtual int    numberOfInnerSectorFrischGridWires()        const = 0;
  virtual int    numberOfInnerSectorGatingGridWires()        const = 0;
  virtual double  firstInnerSectorAnodeWire()                const = 0;
  virtual double  firstInnerSectorFrischGridWire()           const = 0;
  virtual double  firstInnerSectorGatingGridWire()           const = 0;
  virtual double  lastInnerSectorAnodeWire()                 const = 0;

  virtual int    numberOfOuterSectorAnodeWires()             const = 0;
  virtual int    numberOfOuterSectorFrischGridWires()        const = 0;
  virtual int    numberOfOuterSectorGatingGridWires()        const = 0;
  virtual double  firstOuterSectorAnodeWire()                const = 0;
  virtual double  firstOuterSectorFrischGridWire()           const = 0;
  virtual double  firstOuterSectorGatingGridWire()           const = 0;
  virtual double  lastOuterSectorAnodeWire()                 const = 0;


ClassDef(StTpcWirePlaneI,0)

};
#endif















