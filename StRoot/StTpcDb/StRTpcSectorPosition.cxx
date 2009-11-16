/***************************************************************************
 *
 * $Id: StRTpcSectorPosition.cxx,v 1.2 2007/08/04 00:38:04 jeromel Exp $
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









