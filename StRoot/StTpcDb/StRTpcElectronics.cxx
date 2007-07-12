/***************************************************************************
 *
 * $Id: StRTpcElectronics.cxx,v 1.6 2007/07/12 20:21:09 fisyak Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description:  
 *
 ***************************************************************************
 *
 * $Log: StRTpcElectronics.cxx,v $
 * Revision 1.6  2007/07/12 20:21:09  fisyak
 * Drift velocity depends on TPC half, use online RHIC clock
 *
 * Revision 1.5  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#include "StRTpcElectronics.h"
#include "StDetectorDbMaker/StDetectorDbClock.h"

ClassImp(StRTpcElectronics)

double StRTpcElectronics::samplingFrequency() const {
  //  return (*mElec)[0].samplingFrequency;
  return StDetectorDbClock::instance()->getCurrentFrequency()/1000000.0;
}

