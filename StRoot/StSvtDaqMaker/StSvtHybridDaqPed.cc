/***************************************************************************
 *
 * $Id: StSvtHybridDaqPed.cc,v 1.1 2001/07/11 23:29:48 munhoz Exp $
 *
 * Author: Marcelo Munhoz
 ***************************************************************************
 *
 * Description: SVT Hybrid Data BASE class
 *
 ***************************************************************************
 *
 * $Log: StSvtHybridDaqPed.cc,v $
 * Revision 1.1  2001/07/11 23:29:48  munhoz
 * adding capability for zero suppressed and pedestal reading
 *
 *
 **************************************************************************/
////////////////////////////////////////////////////////////////////////////
//                                                                        //
// This is the class to access the data from each hybrid.                 //
//                                                                        //
////////////////////////////////////////////////////////////////////////////

#include "StSvtHybridDaqPed.hh"
#include "StDAQMaker/StSVTReader.h"

ClassImp(StSvtHybridDaqPed)

StSvtHybridDaqPed::StSvtHybridDaqPed(int barrel, int ladder, int wafer, int hybrid, StSVTReader* reader) : 
  StSvtHybridPed(barrel, ladder, wafer, hybrid)
{
  if (reader)
    setHybridPed(reader);
}

int StSvtHybridDaqPed::setHybridPed(StSVTReader* reader)
{
  int anode, time, nAnodes = 240, n, status=0;
  float ped;
  unsigned char* array;
    
  for (anode=1;anode<=nAnodes;anode++) {
    
    status = reader->getPedestals(mBarrel, mLadder, mWafer, mHybrid, anode, n, array);
    
    if (status != 1) return status;

    for (time=0;time<n;time++) {
      ped = array[time];
      //cout << "anode = " << anode << " ,time = " << time << endl;
      addToPixel(anode, time, ped);
    }
  }
  
  return status;
}

int StSvtHybridDaqPed::setHybridRMSPed(StSVTReader* reader)
{
  int anode, time, nAnodes = 240, n, status=0;
  float ped;
  unsigned char* array;
    
  for (anode=1;anode<=nAnodes;anode++) {
    
    status = reader->getRMSPedestals(mBarrel, mLadder, mWafer, mHybrid, anode, n, array);
    
    if (status != 1) return status;

    for (time=0;time<n;time++) {
      ped = array[time];
      //cout << "anode = " << anode << " ,time = " << time << endl;
      addToPixel(anode, time, ped);
    }
  }
  
  return status;
}
