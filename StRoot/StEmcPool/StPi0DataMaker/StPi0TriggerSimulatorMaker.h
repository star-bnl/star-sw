#ifndef StPi0TriggerSimulatorMaker_StPi0TriggerSimulatorMaker_H
#define StPi0TriggerSimulatorMaker_StPi0TriggerSimulatorMaker_H

#include <StMaker.h>

class StEmcGeom;
class StEmcDecoder;
class StBemcTables;

class TMyTriggerSimulatedData;

class StPi0TriggerSimulatorMaker : public StMaker {
public:
    typedef StMaker inherited;

    StPi0TriggerSimulatorMaker(const Char_t *name = "StPi0TriggerSimulatorMaker");
    virtual ~StPi0TriggerSimulatorMaker();
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    const TMyTriggerSimulatedData *getData() const {return this->mData;}

    TString datasetNameStEvent;
    
    Bool_t isSimulation;
  
    Int_t HT1Threshold;
    Int_t HT2Threshold;
    Int_t TriggerADC;
    ULong_t triggersHT1;
    ULong_t triggersHT2;

    Bool_t towerTriggeredHT1[4800];
    Bool_t towerTriggeredHT2[4800];
  
    Bool_t useFullEmcTriggerSimulator;
    TString triggerFullSimulatorName;

    Bool_t doTowerSwapFix;

    ClassDef(StPi0TriggerSimulatorMaker, 1);

protected:
    TMyTriggerSimulatedData *mData; //!
    StEmcGeom *mEmcGeom; //!
    StEmcDecoder *mEmcDecoder; //!
    StBemcTables *mEmcTables; //!
};
#endif
