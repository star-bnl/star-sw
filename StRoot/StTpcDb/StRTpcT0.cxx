#include "StRTpcT0.h"

ClassImp(StRTpcT0)

  void StRTpcT0::SetPadPlanePointer(StTpcPadPlaneI* ppin){
      padplane = ppin;
  }


float StRTpcT0::getT0(int row, int pad)   const {
  if (row>0&&row<=padplane->numberOfInnerRows()){
    return mT0->innerSectorTimeOffsets[padplane->indexForRowPad(row,pad)];
  }
  else if (row>padplane->numberOfInnerRows()&&row<=padplane->numberOfRows()){
    return mT0->outerSectorTimeOffsets[padplane->indexForRowPad(row,pad)];
  }
} 
  
 









