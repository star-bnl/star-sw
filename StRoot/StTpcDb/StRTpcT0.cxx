#include "StRTpcT0.h"

ClassImp(StRTpcT0)
//_____________________________________________________________________________
float StRTpcT0::getT0(int row, int pad)   const {
  float padT0 = 0;
  if (row > 0 && padplane->indexForRowPad(row,pad)>=0) {
    if (row<=padplane->numberOfInnerRows())
      padT0 =  (*mT0)[0].innerSectorTimeOffsets[padplane->indexForRowPad(row,pad)];
   else if (row>padplane->numberOfInnerRows()&&row<=padplane->numberOfRows())
      padT0 =  (*mT0)[0].outerSectorTimeOffsets[padplane->indexForRowPad(row,pad)];
  }
  return padT0;
} 
  
 









