/***************************************************************************
 *
 * $Id: StRTpcSectorPosition.h,v 1.1 2001/08/14 18:18:03 hardtke Exp $
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

  St_tpcSectorPosition* mSectorPosition;

public:

  StRTpcSectorPosition(St_tpcSectorPosition* SectPosIn=0){AddData(SectPosIn);}
  ~StRTpcSectorPosition(){}
   void AddData(St_tpcSectorPosition* SectPosIn) {
   mSectorPosition = SectPosIn;
 } 

  //Implements Abstract Interface 
 
  double innerPositionOffsetX()    const; //center of inner Sector relative nominal position
  double outerPositionOffsetX()    const; //center of outer sector relative nomainal position
  double innerRotation()    const; //
  double outerRotation()    const; //
 


 ClassDef(StRTpcSectorPosition,0)

};

inline double  StRTpcSectorPosition::innerPositionOffsetX() const { return (*mSectorPosition)[0].innerSectorLocalxShift;}

inline double  StRTpcSectorPosition::outerPositionOffsetX() const { return (*mSectorPosition)[0].outerSectorLocalxShift;}

inline double  StRTpcSectorPosition::innerRotation() const { return (*mSectorPosition)[0].innerSectorRotationAngle;}

inline double  StRTpcSectorPosition::outerRotation() const { return (*mSectorPosition)[0].outerSectorRotationAngle;}


#endif










