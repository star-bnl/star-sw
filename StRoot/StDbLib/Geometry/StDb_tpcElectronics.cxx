#include "StDb_tpcElectronics.h"

ClassImp(StDb_tpcElectronics)
//_______________________________________________________
void StDb_tpcElectronics::Streamer(typeAcceptor* accept){

if(!mstruct) mstruct = new tpcElectronics;

accept->pass("numberOfTimeBins",mstruct->numberOfTimeBins,sizeof(mstruct->numberOfTimeBins));
accept->pass("nominalGain",mstruct->nominalGain,sizeof(mstruct->nominalGain));
accept->pass("samplingFrequency",mstruct->samplingFrequency,sizeof(mstruct->samplingFrequency));
accept->pass("tZero",mstruct->tZero,sizeof(mstruct->tZero));
accept->pass("adcCharge",mstruct->adcCharge,sizeof(mstruct->adcCharge));
accept->pass("adcConversion",mstruct->adcConversion,sizeof(mstruct->adcConversion));
accept->pass("averagePedestal",mstruct->averagePedestal,sizeof(mstruct->averagePedestal));
accept->pass("shapingTime",mstruct->shapingTime,sizeof(mstruct->shapingTime));
accept->pass("tau",mstruct->tau,sizeof(mstruct->tau));

}

