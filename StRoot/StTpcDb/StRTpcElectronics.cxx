#include "StRTpcElectronics.h"

ClassImp(StRTpcElectronics)

int StRTpcElectronics::numberOfTimeBins() const {
   return mElec->numberOfTimeBins;
}

double StRTpcElectronics::nominalGain() const {
   return mElec->nominalGain;
}

double StRTpcElectronics::samplingFrequency() const {
   return mElec->samplingFrequency;
}

double StRTpcElectronics::tZero() const {
   return mElec->tZero;
}

double StRTpcElectronics::adcCharge() const {
   return mElec->adcCharge;
}

double StRTpcElectronics::adcConversion() const {
   return mElec->adcConversion;
}

double StRTpcElectronics::averagePedestal() const {
   return mElec->averagePedestal;
}

double StRTpcElectronics::shapingTime() const {
   return mElec->shapingTime;
}

double StRTpcElectronics::tau() const {
   return mElec->tau;
}

