/***************************************************************************
 *
 * $Id: StDevice.cc,v 1.2 1999/09/23 21:25:20 calderon Exp $
 * $Log: StDevice.cc,v $
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
  for (unsigned int iRow=0; iRow<mRows.size(); iRow++){
    delete mRows[iRow];
  };
}

//____________________________

