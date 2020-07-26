// $Id: StFcsFastSimulatorMaker.h,v 1.5 2020/05/29 18:51:02 akio Exp $
//
// $Log: StFcsFastSimulatorMaker.h,v $
// Revision 1.5  2020/05/29 18:51:02  akio
// adding EPD g2t reading as PRES
//
// Revision 1.4  2019/10/23 17:15:42  akio
// *** empty log message ***
//
// Revision 1.3  2019/07/22 18:56:41  akio
// Added LeakyHcal option 2 and 3 for 2d light collection efficiency parametrization
//
// Revision 1.2  2019/05/16 16:11:56  akio
// Adding leaky hcal option
//
// Revision 1.1  2018/11/14 16:50:16  akio
// FCS codes in offline/upgrade/akio
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

    void setDebug(int v=1) {mDebug=v;}
    void setLeakyHcal(int v=1){mLeakyHcal=v;} // 0 = straight Birk, 1=leaky hcal, 
                                              // 2 = xy dep light collection eff 1 side(not leaky)
                                              // 3 = xy dep light collection eff 2 side(leaky)
    void setHcalZDepEff(int v=1) {mHcalZdepEff=v;} //  0 = straight Birk
                                                   //  1 = "noarmal"
                                                   //  2 = "bad"
private:
    
    Int_t getDetectorId(const g2t_emc_hit_st& hit) const;
    void fillStEvent(StEvent* event);
    void printStEventSummary(const StEvent* event);

    Int_t mDebug=0;
    Int_t mLeakyHcal=0;
    Int_t mHcalZdepEff=0;

    virtual const Char_t *GetCVS() const {static const Char_t cvs[]="Tag $Name:" __DATE__ " " __TIME__ ; return cvs;}  
    ClassDef(StFcsFastSimulatorMaker, 0)
};

#endif  // ST_FCS_SIMULATOR_MAKER_H
