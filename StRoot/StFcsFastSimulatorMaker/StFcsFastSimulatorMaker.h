// $Id: StFcsFastSimulatorMaker.h,v 1.1.2.1 2018/08/24 15:15:45 jwebb Exp $
//
// $Log: StFcsFastSimulatorMaker.h,v $
// Revision 1.1.2.1  2018/08/24 15:15:45  jwebb
// Fast simulator for the forward calorimeter system.  Handles both EM and
// hadronic calorimeters.
//
//
// Declaration of StFcsFastSimulatorMaker, the FCS fast simulator

#ifndef ST_FCS_SIMULATOR_MAKER_H
#define ST_FCS_SIMULATOR_MAKER_H

class g2t_emc_hit_st;
class StFcsHit;
class StEvent;

#include "StChain/StMaker.h"

/**
 The FCS fast simulator maker.
 Populates the FCS hit collection in StEvent with StFcsHits, using 
 Geant hits from the g2t table as input.
 (Simulates digitisation of ADC using gains from the database - not yet, hardcoded)
**/

class StFcsFastSimulatorMaker : public StMaker {

public:
    
    explicit StFcsFastSimulatorMaker(const Char_t* name = "fcsSim");
    virtual ~StFcsFastSimulatorMaker() { }
    
    Int_t Make();
    
    void setWCalZS(int v) {mZSch[0]=v;}
    void setHCalZS(int v) {mZSch[1]=v;}
    void setWcalSamplingFraction(int v) {mSF[0]=v;}
    void setHcalSamplingFraction(int v) {mSF[1]=v;}
    void forceWcalFixedGain(float v) {mFixedGain[0]=v;}
    void forceHcalFixedGain(float v) {mFixedGain[1]=v;}
    
private:
    
    Int_t getDetectorId(const g2t_emc_hit_st& hit) const;
    void fillStEvent(StEvent* event);
    void printStEventSummary(const StEvent* event);
    
    Int_t   mZSch[2];        //default set to 1 (zero suppresss adc=0 and 1)
    Float_t mSF[2];          //sampling fraction
    Float_t mFixedGain[2];   //fixed gain (overwrite DB value if >0.0)

    virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag " __DATE__ " " __TIME__ ; return cvs;}  
    ClassDef(StFcsFastSimulatorMaker, 0)
};

#endif  // ST_FCS_SIMULATOR_MAKER_H
