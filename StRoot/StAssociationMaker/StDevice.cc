/***************************************************************************
 *
 * $Id: StDevice.cc,v 1.3 1999/10/18 16:11:53 calderon Exp $
 * $Log: StDevice.cc,v $
 * Revision 1.3  1999/10/18 16:11:53  calderon
 * Frank found 2 leaks that these changes will correct:
 * -Delete the TrackPairInfos in the Clear() method
 * -Correct the sub detector destructors to delete all
 *  instances to StLocalHit.
 *
 * Revision 1.2  1999/09/23 21:25:20  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 *
 **************************************************************************/

#include "StDevice.hh"


//____________________________
StDevice::StDevice(const unsigned int nRows){
    for (unsigned int iRow=0; iRow<nRows; iRow++){
    StDevRow* row = new StDevRow();
    mRows.push_back(row);
    mNRows = nRows;
  };
}

//____________________________

StDevice::~StDevice(){
  for (unsigned int iRow=0; iRow<mRows.size(); iRow++) delete mRows[iRow];
  mRows.clear();
}

//____________________________

