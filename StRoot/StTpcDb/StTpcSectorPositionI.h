/***************************************************************************
 *
 * $Id: StTpcSectorPositionI.h,v 1.1 2001/08/14 18:18:03 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Abstract Interface for Tpc Sector Position  
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#ifndef __STTPCSectorPositionI__
#define __STTPCSectorPositionI__
#include <TObject.h>

class StTpcSectorPositionI : public TObject {

  //Abstract base class defining accessors
public:

  virtual double innerPositionOffsetX()  const = 0; //center of inner sector relative nominal position
  virtual double outerPositionOffsetX()  const = 0; //center of inner sector relative nominal position

  // now we define the rotation angles.  The angles are in degrees.
  // Positive rotation is clockwise
  virtual double innerRotation()  const = 0;
  virtual double outerRotation()  const = 0;



ClassDef(StTpcSectorPositionI,0)

};

#endif















