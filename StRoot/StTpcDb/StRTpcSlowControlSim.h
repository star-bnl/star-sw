#ifndef __STRTPCSLOWCONTROLSIM__
#define __STRTPCSLOWCONTROLSIM__
//#include <TObject.h>
#include "StTpcSlowControlSimI.h"
#include "Calibrations/tpcSlowControlSim.h"

class StRTpcSlowControlSim : public StTpcSlowControlSimI {

private:

  tpcSlowControlSim* mSC;

public:

  StRTpcSlowControlSim(){}
  ~StRTpcSlowControlSim(){}
  void AddData(tpcSlowControlSim* SCIn) {
      mSC = SCIn;
   }

  //Implements Abstract Interface 

   double driftVelocity()   const;
   double driftVoltage()   const;
   double innerSectorAnodeVoltage()   const;
   double innerSectorGatingGridV()   const;
   double outerSectorAnodeVoltage()   const;
   double outerSectorGatingGridV()   const;
   double innerSectorGasGain()   const;
   double innerSectorGasGainVzero()   const;
   double innerSectorGasGainb()   const;
   double outerSectorGasGain()   const;
   double outerSectorGasGainVzero()   const;
   double outerSectorGasGainb()   const;
   double hallPressure() const;
   double hallTemperature() const;


 ClassDef(StRTpcSlowControlSim,0)

};
#endif









