#include "StRTpcGain.h"

ClassImp(StRTpcGain)

  void StRTpcGain::SetPadPlanePointer(StTpcPadPlaneI* ppin){
      padplane = ppin;
  }


float StRTpcGain::getGain(int row, int pad)   const {
  if (row>0&&row<=padplane->numberOfInnerRows()){
    return mGain->innerSectorGainFactors[padplane->indexForRowPad(row,pad)];
  }
  else if (row>padplane->numberOfInnerRows()&&row<=padplane->numberOfRows()){
    return mGain->outerSectorGainFactors[padplane->indexForRowPad(row,pad)];
  }
} 
  
float StRTpcGain::getOnlineGain(int row, int pad) const {return 0;}
  
float StRTpcGain::getNominalGain(int row, int pad) const {return 0;}
  
float StRTpcGain::getRelativeGain(int row, int pad) const {return 0;}
  
float StRTpcGain::getAverageGainInner(int sector) const {return 0;}
  
float StRTpcGain::getAverageGainOuter(int sector) const {return 0;}
 









