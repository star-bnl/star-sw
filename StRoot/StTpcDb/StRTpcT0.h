/***************************************************************************
 *
 * $Id: StRTpcT0.h,v 1.7 2000/01/24 15:31:31 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC T0 interface  
 *
 ***************************************************************************
 *
 * $Log: StRTpcT0.h,v $
 * Revision 1.7  2000/01/24 15:31:31  hardtke
 * change to use new gain and t0 tables
 *
 * Revision 1.6  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#ifndef __STRTPCT0__
#define __STRTPCT0__
#include "StTpcT0I.h"
#include "tables/St_tpcISTimeOffsets_Table.h"
#include "tables/St_tpcOSTimeOffsets_Table.h"

class StRTpcT0 : public StTpcT0I {

private:

  St_tpcISTimeOffsets* mT0IS;
  St_tpcOSTimeOffsets* mT0OS;
  int mSector;
  StTpcPadPlaneI* padplane;

public:

  StRTpcT0(St_tpcISTimeOffsets* T0In=0,St_tpcOSTimeOffsets* T0Out=0){
   AddData(T0In);
   AddData(T0Out);
  }
  ~StRTpcT0(){}
  void AddData(St_tpcISTimeOffsets* T0In) {
      mT0IS = T0In;
   }
  void AddData(St_tpcOSTimeOffsets* T0Out) {
      mT0OS = T0Out;
   }

  //Implements Abstract Interface 
  void SetPadPlanePointer(StTpcPadPlaneI* ppin){ padplane = ppin;}
  float getT0(int row, int pad)   const;
 

 ClassDef(StRTpcT0,0)

};

#endif









