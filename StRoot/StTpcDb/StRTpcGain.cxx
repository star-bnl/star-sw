/***************************************************************************
 *
 * $Id: StRTpcGain.cxx,v 1.8 2000/01/24 15:31:31 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description:  Root implementation of Gain Interface class
 *
 ***************************************************************************
 *
 * $Log: StRTpcGain.cxx,v $
 * Revision 1.8  2000/01/24 15:31:31  hardtke
 * change to use new gain and t0 tables
 *
 * Revision 1.7  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#include "StRTpcGain.h"

ClassImp(StRTpcGain)

//_____________________________________________________________________________
void StRTpcGain::SetPadPlanePointer(StTpcPadPlaneI* ppin){
    padplane = ppin;
}


//_____________________________________________________________________________
float StRTpcGain::getGain(int row, int pad)   const {
  float gain = 0;
  if (row > 0 && padplane->indexForRowPad(row,pad) > 0) {
    if (row<=padplane->numberOfInnerRows())
      gain =  (*mGainIS)[0].gain[padplane->indexForRowPad(row,pad)];
    else if (row>padplane->numberOfInnerRows()&&row<=padplane->numberOfRows())
      gain = (*mGainOS)[0].gain[padplane->indexForRowPad(row,pad)];
  }
  return gain;
} 
  
//_____________________________________________________________________________
float StRTpcGain::getOnlineGain(int row, int pad) const {return 0;}
  
//_____________________________________________________________________________
float StRTpcGain::getNominalGain(int row, int pad) const {return 0;}
  
//_____________________________________________________________________________
float StRTpcGain::getRelativeGain(int row, int pad) const {return 0;}
  
//_____________________________________________________________________________
float StRTpcGain::getAverageGainInner(int sector) const {return 0;}
  
//_____________________________________________________________________________
float StRTpcGain::getAverageGainOuter(int sector) const {return 0;}
 









