//////////////////////////////////////////////////////////////////////////
//                                                             
// StEmcTriggerMaker A. A. P. Suaide (C) 2001  
//                                                             
//////////////////////////////////////////////////////////////////////////

#ifndef STAR_StEmcTriggerMaker
#define STAR_StEmcTriggerMaker

#ifndef StMaker_H
#include "StMaker.h"
#endif
#include <TH1.h>
#include <TH2.h>
#include "emc_def.h"
#include "StBemcTrigger.h"
#include "StEvent/StTriggerDetectorCollection.h"

class StEvent;

class StEmcTriggerMaker : public StMaker 
{
  private:
    StBemcTrigger*    mBemcTrigger;
    bool              mSaveStEvent;
    bool              mPrint;
    
    TH2F*             mHTBefore;
    TH2F*             mPABefore;
    TH2F*             mHTCorrel;
    TH2F*             mPACorrel;
    TH2F*             mHT;
    TH2F*             mPA;
  protected:
  public: 
    
                      StEmcTriggerMaker(const char *name="bemctrigger");
    virtual           ~StEmcTriggerMaker();
    virtual Int_t     Init();
    virtual Int_t     Make();
    virtual Int_t     Finish();    
    void              fillStEvent(StEvent*);    
    void              fillHistograms(StEvent*);
    void              saveHistograms(char*);    
    void              setSaveStEvent(bool a) { mSaveStEvent = a;}
    void              setPrint(bool a)     { mPrint = a;}
    StBemcTrigger*    getTrigger() { return mBemcTrigger;}

  ClassDef(StEmcTriggerMaker,0) 
};
#endif
