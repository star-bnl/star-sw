/***************************************************************************
 *
 * $Id: StRTpcGain.h,v 1.9 2000/01/24 15:31:31 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Root implementation of TPC gain interface  
 *
 ***************************************************************************
 *
 * $Log: StRTpcGain.h,v $
 * Revision 1.9  2000/01/24 15:31:31  hardtke
 * change to use new gain and t0 tables
 *
 * Revision 1.8  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#ifndef __STRTPCGAIN__
#define __STRTPCGAIN__
//#include <TObject.h>
#include "StTpcGainI.h"
#include "tables/St_tpcISGains_Table.h"
#include "tables/St_tpcOSGains_Table.h"

class StRTpcGain : public StTpcGainI {

private:

  St_tpcISGains* mGainIS;
  St_tpcOSGains* mGainOS;
  int mSector;
  StTpcPadPlaneI* padplane;

public:

  StRTpcGain(St_tpcISGains* GainIn=0,St_tpcOSGains* GainOut = 0){
   AddData(GainIn);
   AddData(GainOut);
  }
  ~StRTpcGain(){}
  void AddData(St_tpcISGains* GainIn) {
      mGainIS = GainIn;
   }
  void AddData(St_tpcOSGains* GainOut) {
      mGainOS = GainOut;
   }

  //Implements Abstract Interface 
  void SetPadPlanePointer(StTpcPadPlaneI* ppin);
  float getGain(int row, int pad)   const;
  float getOnlineGain(int row, int pad) const;
  float getNominalGain(int row, int pad) const;
  float getRelativeGain(int row, int pad) const;
  float getAverageGainInner(int sector) const;
  float getAverageGainOuter(int sector) const;
 

 ClassDef(StRTpcGain,0)

};
#endif









