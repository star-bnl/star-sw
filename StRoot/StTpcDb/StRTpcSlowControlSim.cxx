/***************************************************************************
 *
 * $Id: StRTpcSlowControlSim.cxx,v 1.3.4.1 2007/08/12 23:27:43 jeromel Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Slow control parameters of TRS -- should be temporary 
 *
 ***************************************************************************
 *
 * $Log: StRTpcSlowControlSim.cxx,v $
 * Revision 1.3.4.1  2007/08/12 23:27:43  jeromel
 * Further fixes for SL06g built for SL44
 *
 * Revision 1.4  2007/08/04 00:38:04  jeromel
 * SL4 issue: Removal of the inline func, moved to class implementation.
 *     Symbols may otherwise be hidden.
 *
 * Revision 1.3  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#include "StRTpcSlowControlSim.h"

ClassImp(StRTpcSlowControlSim)

double StRTpcSlowControlSim::driftVelocity()   const {
      return (*mSC)[0].driftVelocity;}
double StRTpcSlowControlSim::driftVoltage()   const {
      return (*mSC)[0].driftVoltage;}
double StRTpcSlowControlSim::innerSectorAnodeVoltage()   const {
      return (*mSC)[0].innerSectorAnodeVoltage;}
double StRTpcSlowControlSim::innerSectorGatingGridV()   const {
      return (*mSC)[0].innerSectorGatingGridV;}
double StRTpcSlowControlSim::outerSectorAnodeVoltage()   const {
      return (*mSC)[0].outerSectorAnodeVoltage;}
double StRTpcSlowControlSim::outerSectorGatingGridV()   const {
      return (*mSC)[0].outerSectorGatingGridV;}
double StRTpcSlowControlSim::innerSectorGasGain()   const {
      return (*mTSS)[0].gain_in;}
double StRTpcSlowControlSim::innerSectorGasGainVzero()   const {
      return (*mSC)[0].innerSectorGasGainVzero;}
double StRTpcSlowControlSim::innerSectorGasGainb()   const {
      return (*mSC)[0].innerSectorGasGainb;}
double StRTpcSlowControlSim::outerSectorGasGain()   const {
      return (*mTSS)[0].gain_out;}
double StRTpcSlowControlSim::outerSectorGasGainVzero()   const {
      return (*mSC)[0].outerSectorGasGainVzero;}
double StRTpcSlowControlSim::outerSectorGasGainb()   const {
      return (*mSC)[0].outerSectorGasGainb;}
double StRTpcSlowControlSim::hallPressure() const {
      return (*mSC)[0].hallPressure;}
double StRTpcSlowControlSim::hallTemperature() const {
      return (*mSC)[0].hallTemperature;}



