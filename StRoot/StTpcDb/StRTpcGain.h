#ifndef __STRTPCGAIN__
#define __STRTPCGAIN__
//#include <TObject.h>
#include <StTpcGainI.h>
#include <gainFactors.time.h>
#include <StTpcPadPlaneI.h>
//#include <iostream.h>

class StRTpcGain : public StTpcGainI {

private:

  gain_factors mGain;
  int mSector;
  StTpcPadPlaneI* padplane;

public:

  StRTpcGain(){}
  ~StRTpcGain(){}
  void AddData(gain_factors GainIn) {
      mGain = GainIn;
   }
  void SetPadPlaneP57ointer(StTpcPadPlaneI* ppin){
      padplane = ppin;
  }

  //Implements Abstract Interface 
  float getGain(int row, int pad)   const;
  float getOnlineGain(int row, int pad) const;
  float getNominalGain(int row, int pad) const;
  float getRelativeGain(int row, int pad) const;
  float getAverageGainInner(int sector) const;
  float getAverageGainOuter(int sector) const;
 

 ClassDef(StRTpcGain,0)

};
#endif









