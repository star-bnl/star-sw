#ifndef __STRTPCGAIN__
#define __STRTPCGAIN__
//#include <TObject.h>
#include "StTpcGainI.h"
#include "tables/St_tpcGainFactors_Table.h"

class StRTpcGain : public StTpcGainI {

private:

  St_tpcGainFactors* mGain;
  int mSector;
  StTpcPadPlaneI* padplane;

public:

  StRTpcGain(St_tpcGainFactors* GainIn=0){AddData(GainIn);}
  ~StRTpcGain(){}
  void AddData(St_tpcGainFactors* GainIn) {
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









