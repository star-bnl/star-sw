#ifndef __STTPCSLOWCONTROLSIMI__
#define __STTPCSLOWCONTROLSIMI__
#include <TObject.h>
#include "StTpcSlowControlSimI.h"

class StTpcSlowControlSimI : public TObject {

  //Abstract base class defining accessors
public:

  virtual double driftVelocity()   const = 0;
  virtual double driftVoltage()   const = 0;
  virtual double innerSectorAnodeVoltage()   const = 0;
  virtual double innerSectorGatingGridV()   const = 0;
  virtual double outerSectorAnodeVoltage()   const = 0;
  virtual double outerSectorGatingGridV()   const = 0;
  virtual double innerSectorGasGain()   const = 0;
  virtual double innerSectorGasGainVzero()   const = 0;
  virtual double innerSectorGasGainb()   const = 0;
  virtual double outerSectorGasGain()   const = 0;
  virtual double outerSectorGasGainVzero()   const = 0;
  virtual double outerSectorGasGainb()   const = 0;
  virtual double hallPressure() const = 0;
  virtual double hallTemperature() const = 0;

  ClassDef(StTpcSlowControlSimI,0)

};

#endif














