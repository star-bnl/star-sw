/***************************************************************************
 *
 * $Id: StRTpcT0.h,v 1.6 1999/12/16 22:00:53 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC T0 interface  
 *
 ***************************************************************************
 *
 * $Log: StRTpcT0.h,v $
 * Revision 1.6  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#ifndef __STRTPCT0__
#define __STRTPCT0__
#include "StTpcT0I.h"
#include "tables/St_tpcTimeOffsets_Table.h"

class StRTpcT0 : public StTpcT0I {

private:

  St_tpcTimeOffsets* mT0;
  int mSector;
  StTpcPadPlaneI* padplane;

public:

  StRTpcT0(St_tpcTimeOffsets* T0In=0){AddData(T0In);}
  ~StRTpcT0(){}
  void AddData(St_tpcTimeOffsets* T0In) {
      mT0 = T0In;
   }

  //Implements Abstract Interface 
  void SetPadPlanePointer(StTpcPadPlaneI* ppin){ padplane = ppin;}
  float getT0(int row, int pad)   const;
 

 ClassDef(StRTpcT0,0)

};

#endif









