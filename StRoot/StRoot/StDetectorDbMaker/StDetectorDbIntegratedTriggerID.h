#ifndef StDetectorDbIntegratedTriggerID_h
#define StDetectorDbIntegratedTriggerID_h
#include "St_triggerInfoC.h"
#include "St_defaultTrgLvlC.h"

class StDetectorDbIntegratedTriggerID{
public:
  static StDetectorDbIntegratedTriggerID*  instance() {if (! fgInstance) fgInstance = new StDetectorDbIntegratedTriggerID(); return fgInstance;}
    virtual ~StDetectorDbIntegratedTriggerID() {fgInstance = 0;}
    UInt_t    getIDNumRows()                           {return St_triggerInfoC::instance()->getNumRows();}
    Int_t     getIDRunNumber()                         {return St_triggerInfoC::instance()->runNumber();}
    Int_t     getIdxTrg(UInt_t entry = 0)              {return St_triggerInfoC::instance()->idxTrg();}
    Int_t     getDaqTrgId(UInt_t entry = 0)            {return St_triggerInfoC::instance()->daqTrgId();}
    Int_t     getOfflineTrgId(UInt_t entry = 0)        {return St_triggerInfoC::instance()->offlineTrgId();}
    Int_t     getTrgNameVersion(UInt_t entry = 0)      {return St_triggerInfoC::instance()->trgNameVersion();}
    Int_t     getTrgVersion(UInt_t entry = 0)          {return St_triggerInfoC::instance()->trgVersion();}
    Int_t     getThreashVersion(UInt_t entry = 0)      {return St_triggerInfoC::instance()->threashVersion();}
    Int_t     getPsVersion(UInt_t entry = 0)           {return St_triggerInfoC::instance()->psVersion();}
    Int_t     getPsL0(UInt_t entry = 0)                {return St_triggerInfoC::instance()->psL0();}
    Char_t*   getName(UInt_t entry = 0)                {return St_triggerInfoC::instance()->name();}
    UInt_t    getDetectorLiveOnBits(UInt_t entry = 0)  {return St_triggerInfoC::instance()->detectorLiveOnBits();}
    UInt_t    getDetectorLiveOffBits(UInt_t entry = 0) {return St_triggerInfoC::instance()->detectorLiveOffBits();}
    UInt_t    getDetectorRequest(UInt_t entry = 0)     {return St_triggerInfoC::instance()->detectorRequest();}
    Int_t     getIdxLevel(UInt_t entry = 0)            {return St_triggerInfoC::instance()->idxLevel();}
    Int_t     getAlgorithmId(UInt_t entry = 0)         {return St_triggerInfoC::instance()->algorithmId();}
    Float_t   getPs(UInt_t entry = 0)                  {return St_triggerInfoC::instance()->ps();}
    UInt_t    getDefaultTriggerLevel()                 {return St_defaultTrgLvlC::instance()->level();}
protected:
    StDetectorDbIntegratedTriggerID(){;}
private:
    static StDetectorDbIntegratedTriggerID* fgInstance;
};

#endif
