#ifndef __STRTPCGAIN__
#define __STRTPCGAIN__
//#include <TObject.h>
#include "StTpcGainI.h"
#include "StDbLib/Calibrations/tpcGainFactors.h"
//#include <StTpcPadPlaneI.h>
//#include <iostream.h>

class StRTpcGain : public StTpcGainI {

private:

  tpcGainFactors* mGain;
  int mSector;
  StTpcPadPlaneI* padplane;

public:

  StRTpcGain(){}
  ~StRTpcGain(){}
  void AddData(tpcGainFactors* GainIn) {
      mGain = GainIn;
   }

  //Implements Abstract Interface 
  void SetPadPlanePointer(StTpcPadPlaneI* ppin);
  float getGain(int row, int pad)   const;
  float getOnlineGain(int row, int pad) const;
  float getNominalGain(int row, int pad) const;
  float getRelativeGain(int row, int pad) const;
  float getAverageGainInner(int sector) const;
  float getAverageGainOuter(int sector) const;
 

 ClassDef(StRTpcGain,0)

};
#endif









