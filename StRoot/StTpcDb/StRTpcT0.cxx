/***************************************************************************
 *
 * $Id: StRTpcT0.cxx,v 1.7 2000/01/24 15:31:31 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Tpc T0 interface  
 *
 ***************************************************************************
 *
 * $Log: StRTpcT0.cxx,v $
 * Revision 1.7  2000/01/24 15:31:31  hardtke
 * change to use new gain and t0 tables
 *
 * Revision 1.6  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#include "StRTpcT0.h"

ClassImp(StRTpcT0)
//_____________________________________________________________________________
float StRTpcT0::getT0(int row, int pad)   const {
  float padT0 = 0;
  if (row > 0 && padplane->indexForRowPad(row,pad)>=0) {
    if (row<=padplane->numberOfInnerRows())
      padT0 =  (*mT0IS)[0].offset[padplane->indexForRowPad(row,pad)];
   else if (row>padplane->numberOfInnerRows()&&row<=padplane->numberOfRows())
      padT0 =  (*mT0OS)[0].offset[padplane->indexForRowPad(row,pad)];
  }
  return padT0;
} 
  
 









