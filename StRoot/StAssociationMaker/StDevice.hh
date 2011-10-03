/* 
   Generic device class.  Each (generic) StSubDetector object
   has a set of StDevices (like the TPC StSubDetector has 24
   sector StDevices).  Each StDevice object has a set of StDevRows
   (like the TPC sector StDevice has 45 padrows, each one is a StDevRow)
*/

#ifndef StDevice_HH
#define StDevice_HH

#include <vector>

//class StDevRow;
#include "StDevRow.hh"

class StDevice{

private:
    vector<StDevRow*> mRows;
    unsigned int mNRows;
    
public:
    StDevice(const unsigned int nRows);
    ~StDevice();
    
    StDevRow*         row(const int irow){return mRows[irow];};
    unsigned int      numOfRows() { return mNRows; };
};


#endif
