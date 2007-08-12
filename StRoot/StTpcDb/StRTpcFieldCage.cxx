/***************************************************************************
 *
 * $Id: StRTpcFieldCage.cxx,v 1.1.4.1 2007/08/12 23:27:41 jeromel Exp $
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







