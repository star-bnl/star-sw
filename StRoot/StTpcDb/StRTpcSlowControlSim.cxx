#include "StRTpcSlowControlSim.h"

ClassImp(StRTpcSlowControlSim)

   double StRTpcSlowControlSim::driftVelocity()   const {
      return mSC->driftVelocity;}
   double StRTpcSlowControlSim::driftVoltage()   const {
      return mSC->driftVoltage;}
   double StRTpcSlowControlSim::innerSectorAnodeVoltage()   const {
      return mSC->innerSectorAnodeVoltage;}
   double StRTpcSlowControlSim::innerSectorGatingGridV()   const {
      return mSC->innerSectorGatingGridV;}
   double StRTpcSlowControlSim::outerSectorAnodeVoltage()   const {
      return mSC->outerSectorAnodeVoltage;}
   double StRTpcSlowControlSim::outerSectorGatingGridV()   const {
      return mSC->outerSectorGatingGridV;}
   double StRTpcSlowControlSim::innerSectorGasGain()   const {
      return mSC->innerSectorGasGain;}
   double StRTpcSlowControlSim::innerSectorGasGainVzero()   const {
      return mSC->innerSectorGasGainVzero;}
   double StRTpcSlowControlSim::innerSectorGasGainb()   const {
      return mSC->innerSectorGasGainb;}
   double StRTpcSlowControlSim::outerSectorGasGain()   const {
      return mSC->outerSectorGasGain;}
   double StRTpcSlowControlSim::outerSectorGasGainVzero()   const {
      return mSC->outerSectorGasGainVzero;}
   double StRTpcSlowControlSim::outerSectorGasGainb()   const {
      return mSC->outerSectorGasGainb;}
   double StRTpcSlowControlSim::hallPressure() const {
      return mSC->hallPressure;}
   double StRTpcSlowControlSim::hallTemperature() const {
      return mSC->hallTemperature;}

 









