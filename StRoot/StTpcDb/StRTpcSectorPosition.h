/***************************************************************************
 *
 * $Id: StRTpcSectorPosition.h,v 1.3 2007/08/04 00:38:04 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Sector Position
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#ifndef __STRTPCSectorPosition__
#define __STRTPCSectorPosition__
//#include <TObject.h>
#include "StMessMgr.h"
#include "StTpcSectorPositionI.h"
#include "tables/St_tpcSectorPosition_Table.h"

class StRTpcSectorPosition : public StTpcSectorPositionI {

private:

  tpcSectorPosition_st mSectorPosition;

public:

  StRTpcSectorPosition(St_tpcSectorPosition* SectPosIn=0){AddData(SectPosIn);}
  ~StRTpcSectorPosition(){}
   void AddData(St_tpcSectorPosition* SectPosIn) {
     if (SectPosIn) mSectorPosition = (*SectPosIn)[0];
 } 

  //Implements Abstract Interface 
 
  double innerPositionOffsetX()    const; //center of inner Sector relative nominal position
  double outerPositionOffsetX()    const; //center of outer sector relative nomainal position
  double innerRotation()    const; //
  double outerRotation()    const; //
 


 ClassDef(StRTpcSectorPosition,0)

};


#endif










