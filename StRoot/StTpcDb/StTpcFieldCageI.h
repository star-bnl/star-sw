/***************************************************************************
 *
 * $Id: StTpcFieldCageI.h,v 1.1 2002/02/06 18:39:13 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Abstract Interface for Tpc Field Cage  
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#ifndef __STTPCFieldCageI__
#define __STTPCFieldCageI__
#include <TObject.h>

class StTpcFieldCageI : public TObject {

  //Abstract base class defining accessors
public:

  virtual double InnerFieldCageShift()    const = 0; //Shift in cm of IFC relative to OFC
  virtual double EastClockError()    const = 0; //(radians) phi rotation of east tpc wheel
  virtual double WestClockError()    const = 0; //(radians) phi rotation of west tpc wheel 


ClassDef(StTpcFieldCageI,0)

};
#endif















