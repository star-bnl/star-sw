/***************************************************************************
 *
 * $Id: StTpcDimensionsI.h,v 1.4 2000/11/14 22:00:06 genevb Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Abstract Interface for getting TPC Dimensions data  
 *
 ***************************************************************************
 *
 * $Log: StTpcDimensionsI.h,v $
 * Revision 1.4  2000/11/14 22:00:06  genevb
 * Switched several functions from float to double
 *
 * Revision 1.3  2000/02/15 22:21:47  hardtke
 * Add effective drift distances
 *
 * Revision 1.2  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/

#ifndef __STTPCDIMENSIONSI__
#define __STTPCDIMENSIONSI__
#include <TObject.h>

class StTpcDimensionsI : public TObject {

  //Abstract base class defining accessors
public:

  virtual int   numberOfSectors()     const = 0;

  //TPC field cage parameters:
  virtual double ifcRadius()           const = 0;
  virtual double ofcRadius()           const = 0;
  virtual double tpcTotalLength()      const = 0;

  //TPC wheel parameters:
  virtual double wheelInnerRadius()    const = 0;
  virtual double wheelOuterRadius()    const = 0;
  virtual double wheelThickness()      const = 0;

  virtual double senseGasOuterRadius() const = 0;
  virtual double tpeaThickness()       const = 0;

  //TPC cathode parameters:
  virtual double cathodeInnerRadius()  const = 0;
  virtual double cathodeOuterRadius()  const = 0;
  virtual double cathodeThickness()    const = 0; 

  //TPC distances
  virtual double innerEffectiveDriftDistance()  const = 0;
  virtual double outerEffectiveDriftDistance()  const = 0;
  virtual double gatingGridZ()                  const = 0;
  virtual double zInnerOffset()                 const = 0;
  virtual double zOuterOffset()                 const = 0;

ClassDef(StTpcDimensionsI,0)

};
#endif















