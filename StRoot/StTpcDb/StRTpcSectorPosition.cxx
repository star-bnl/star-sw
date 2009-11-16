/***************************************************************************
 *
 * $Id: StRTpcSectorPosition.cxx,v 1.1.4.1 2007/08/12 23:27:42 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Sector Position geometry interface  
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#include "StRTpcSectorPosition.h"

ClassImp(StRTpcSectorPosition)

double  StRTpcSectorPosition::innerPositionOffsetX() const { return mSectorPosition.innerSectorLocalxShift;}

double  StRTpcSectorPosition::outerPositionOffsetX() const { return mSectorPosition.outerSectorLocalxShift;}

double  StRTpcSectorPosition::innerRotation() const { return mSectorPosition.innerSectorRotationAngle;}

double  StRTpcSectorPosition::outerRotation() const { return mSectorPosition.outerSectorRotationAngle;}









