#ifndef __STRTPCT0__
#define __STRTPCT0__
#include <StTpcT0I.h>
#include "StDbLib/Calibrations/tpcTimeOffsets.h"

class StRTpcT0 : public StTpcT0I {

private:

  tpcTimeOffsets* mT0;
  int mSector;
  StTpcPadPlaneI* padplane;

public:

  StRTpcT0(){}
  ~StRTpcT0(){}
  void AddData(tpcTimeOffsets* T0In) {
      mT0 = T0In;
   }

  //Implements Abstract Interface 
  void SetPadPlanePointer(StTpcPadPlaneI* ppin);
  float getT0(int row, int pad)   const;
 

 ClassDef(StRTpcT0,0)

};
#endif









