
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

