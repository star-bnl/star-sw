/***************************************************************************
 *
 * $Id: StRTpcSlowControlSim.h,v 1.4 2001/06/20 22:25:26 hardtke Exp $
 *
 * Author:  David Hardtke
 ***************************************************************************
 *
 * Description: Slow control parameters for TRS  
 *
 ***************************************************************************
 *
 * $Log: StRTpcSlowControlSim.h,v $
 * Revision 1.4  2001/06/20 22:25:26  hardtke
 * Get TRS gain parameters from tsspar table
 *
 * Revision 1.3  1999/12/16 22:00:53  hardtke
 * add CVS tags
 *
 **************************************************************************/
#ifndef __STRTPCSLOWCONTROLSIM__
#define __STRTPCSLOWCONTROLSIM__
//#include <TObject.h>
#include "StTpcSlowControlSimI.h"
#include "tables/St_tpcSlowControlSim_Table.h"
#include "tables/St_tss_tsspar_Table.h"

class StRTpcSlowControlSim : public StTpcSlowControlSimI {

private:

  St_tpcSlowControlSim *mSC;
  St_tss_tsspar *mTSS;

public:

  StRTpcSlowControlSim(St_tpcSlowControlSim* SCIn=0,St_tss_tsspar* TSSIn=0){AddData(SCIn);AddData(TSSIn);}
  ~StRTpcSlowControlSim(){}
  void AddData(St_tpcSlowControlSim* SCIn) {
      mSC = SCIn;
   }
  void AddData(St_tss_tsspar* TSSIn) {
      mTSS = TSSIn;
   }

  //Implements Abstract Interface 

   double driftVelocity()   const;
   double driftVoltage()   const;
   double innerSectorAnodeVoltage()   const;
   double innerSectorGatingGridV()   const;
   double outerSectorAnodeVoltage()   const;
   double outerSectorGatingGridV()   const;
   double innerSectorGasGain()   const;
   double innerSectorGasGainVzero()   const;
   double innerSectorGasGainb()   const;
   double outerSectorGasGain()   const;
   double outerSectorGasGainVzero()   const;
   double outerSectorGasGainb()   const;
   double hallPressure() const;
   double hallTemperature() const;


 ClassDef(StRTpcSlowControlSim,0)

};

   inline double StRTpcSlowControlSim::driftVelocity()   const {
      return (*mSC)[0].driftVelocity;}
   inline double StRTpcSlowControlSim::driftVoltage()   const {
      return (*mSC)[0].driftVoltage;}
   inline double StRTpcSlowControlSim::innerSectorAnodeVoltage()   const {
      return (*mSC)[0].innerSectorAnodeVoltage;}
   inline double StRTpcSlowControlSim::innerSectorGatingGridV()   const {
      return (*mSC)[0].innerSectorGatingGridV;}
   inline double StRTpcSlowControlSim::outerSectorAnodeVoltage()   const {
      return (*mSC)[0].outerSectorAnodeVoltage;}
   inline double StRTpcSlowControlSim::outerSectorGatingGridV()   const {
      return (*mSC)[0].outerSectorGatingGridV;}
   inline double StRTpcSlowControlSim::innerSectorGasGain()   const {
      return (*mTSS)[0].gain_in;}
   inline double StRTpcSlowControlSim::innerSectorGasGainVzero()   const {
      return (*mSC)[0].innerSectorGasGainVzero;}
   inline double StRTpcSlowControlSim::innerSectorGasGainb()   const {
      return (*mSC)[0].innerSectorGasGainb;}
   inline double StRTpcSlowControlSim::outerSectorGasGain()   const {
      return (*mTSS)[0].gain_out;}
   inline double StRTpcSlowControlSim::outerSectorGasGainVzero()   const {
      return (*mSC)[0].outerSectorGasGainVzero;}
   inline double StRTpcSlowControlSim::outerSectorGasGainb()   const {
      return (*mSC)[0].outerSectorGasGainb;}
   inline double StRTpcSlowControlSim::hallPressure() const {
      return (*mSC)[0].hallPressure;}
   inline double StRTpcSlowControlSim::hallTemperature() const {
      return (*mSC)[0].hallTemperature;}

 

#endif









