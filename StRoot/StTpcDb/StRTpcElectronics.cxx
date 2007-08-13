/***************************************************************************
 *
 * $Id: StRTpcElectronics.cxx,v 1.5.6.1 2007/08/13 01:04:41 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description:  
 *
 ***************************************************************************
 *
 * $Log: StRTpcElectronics.cxx,v $
 * Revision 1.5.6.1  2007/08/13 01:04:41  jeromel
 * Patches for SL07a, SL44
 *
 * Revision 1.5.4.1  2007/08/12 23:27:40  jeromel
 * Further fixes for SL06g built for SL44
 *
 * Revision 1.7  2007/08/04 00:38:03  jeromel
 * SL4 issue: Removal of the inline func, moved to class implementation.
 *     Symbols may otherwise be hidden.
 *
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

int StRTpcElectronics::numberOfTimeBins() const {
   return (*mElec)[0].numberOfTimeBins;
}

double StRTpcElectronics::nominalGain() const {
   return (*mElec)[0].nominalGain;
}

double StRTpcElectronics::tZero() const {
   return (*mElec)[0].tZero;
}

double StRTpcElectronics::adcCharge() const {
   return (*mElec)[0].adcCharge;
}

double StRTpcElectronics::adcConversion() const {
   return (*mElec)[0].adcConversion;
}

double StRTpcElectronics::averagePedestal() const {
   return (*mElec)[0].averagePedestal;
}

double StRTpcElectronics::shapingTime() const {
   return (*mElec)[0].shapingTime;
}

double StRTpcElectronics::tau() const {
   return (*mElec)[0].tau;
}
