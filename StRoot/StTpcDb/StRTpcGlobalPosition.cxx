/***************************************************************************
 *
 * $Id: StRTpcGlobalPosition.cxx,v 1.2 2007/08/04 00:38:03 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Global Position geometry interface  
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#include "StRTpcGlobalPosition.h"

ClassImp(StRTpcGlobalPosition)

double  StRTpcGlobalPosition::TpcCenterPositionX() const { return (*mGlobalPosition)[0].LocalxShift;}

double  StRTpcGlobalPosition::TpcCenterPositionY() const { return (*mGlobalPosition)[0].LocalyShift;}

double  StRTpcGlobalPosition::TpcCenterPositionZ() const { return (*mGlobalPosition)[0].LocalzShift;}

double  StRTpcGlobalPosition::TpcRotationAroundGlobalAxisX() const { return (*mGlobalPosition)[0].PhiYZ_geom;}

double  StRTpcGlobalPosition::TpcRotationAroundGlobalAxisY() const { return (*mGlobalPosition)[0].PhiXZ_geom;}

double  StRTpcGlobalPosition::TpcRotationAroundGlobalAxisZ() const { return (*mGlobalPosition)[0].PhiXY_geom;}

double  StRTpcGlobalPosition::TpcEFieldRotationX() const { return (*mGlobalPosition)[0].PhiYZ;}

double  StRTpcGlobalPosition::TpcEFieldRotationY() const { return (*mGlobalPosition)[0].PhiXZ;}

double  StRTpcGlobalPosition::TpcEFieldRotationZ() const { return (*mGlobalPosition)[0].PhiXY;}








