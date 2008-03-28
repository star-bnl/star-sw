/***************************************************************************
 *
 * $Id: StRTpcFieldCage.cxx,v 1.2 2007/08/04 00:38:03 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: TPC Field Cage geometry interface  
 *
 ***************************************************************************
 *
 *
 **************************************************************************/
#include "StRTpcFieldCage.h"

ClassImp(StRTpcFieldCage)

double  StRTpcFieldCage::InnerFieldCageShift() const { return (*mFieldCage)[0].innerFieldCageShift;}
double  StRTpcFieldCage::EastClockError() const { return (*mFieldCage)[0].eastClockError;}
double  StRTpcFieldCage::WestClockError() const { return (*mFieldCage)[0].westClockError;}







