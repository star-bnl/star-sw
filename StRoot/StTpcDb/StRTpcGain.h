#ifndef __STRTPCGAIN__
#define __STRTPCGAIN__
//#include <TObject.h>
#include <StTpcGainI.h>
#include <gainFactors.time.h>
#include <StTpcPadPlaneI.h>
#include <iostream.h>

class StRTpcGain : public StTpcGainI {

private:

  gain_factors* mGains[24];
  StTpcPadPlaneI* padplane;

public:

  StRTpcGain(){}
  ~StRTpcGain(){}
  void AddDataForSector(int sector, gain_factors* GainIn) {
    if (sector>=1&&sector<=24){ 
      mGains[sector-1] = GainIn;
    }
    else {
     cout << "StRTpcGain::AddDataForSector: Invalid Sector = " 
	  << sector << endl;
    }
   }
void SetPadPlanePointer(StTpcPadPlaneI* ppin){
padplane = ppin;
}
// void AddData(tpc_padplanes PadIn);

  //Implements Abstract Interface 
  float getGain(int sector, int row, int pad)   const;
  float getOnlineGain(int sector, int row, int pad) const;
  float getNominalGain(int sector, int row, int pad) const;
  float getRelativeGain(int sector, int row, int pad) const;
  float getAverageGainInner(int sector) const;
  float getAverageGainOuter(int sector) const;
 

 ClassDef(StRTpcGain,0)

};
#endif









