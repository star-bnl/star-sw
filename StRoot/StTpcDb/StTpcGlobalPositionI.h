/***************************************************************************
 *
 * $Id: StTpcGlobalPositionI.h,v 1.2 2002/02/12 22:50:36 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Abstract Interface for Tpc Global Position  
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#ifndef __STTPCGlobalPositionI__
#define __STTPCGlobalPositionI__
#include <TObject.h>

class StTpcGlobalPositionI : public TObject {

  //Abstract base class defining accessors
public:

  virtual double TpcCenterPositionX()    const = 0; //center of TPC in global
  virtual double TpcCenterPositionY()    const = 0; //coordinates (cm)
  virtual double TpcCenterPositionZ()    const = 0; // 

  // now we define the rotation angles.  The angles are in radians.
  // Positive rotation defined by right hand rule
  virtual double TpcRotationAroundGlobalAxisX()  const = 0;
  virtual double TpcRotationAroundGlobalAxisY()  const = 0;
  virtual double TpcRotationAroundGlobalAxisZ()  const = 0;

  virtual double TpcEFieldRotationX()   const = 0;  // These are used
  virtual double TpcEFieldRotationY()   const = 0;  // for ExB twist correction
  virtual double TpcEFieldRotationZ()   const = 0;



ClassDef(StTpcGlobalPositionI,0)

};
#endif















