/***************************************************************************
 *
 * $Id: StRTpcSectorPosition.h,v 1.2 2004/08/26 21:05:10 genevb Exp $
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

inline double  StRTpcSectorPosition::innerPositionOffsetX() const { return mSectorPosition.innerSectorLocalxShift;}

inline double  StRTpcSectorPosition::outerPositionOffsetX() const { return mSectorPosition.outerSectorLocalxShift;}

inline double  StRTpcSectorPosition::innerRotation() const { return mSectorPosition.innerSectorRotationAngle;}

inline double  StRTpcSectorPosition::outerRotation() const { return mSectorPosition.outerSectorRotationAngle;}


#endif










