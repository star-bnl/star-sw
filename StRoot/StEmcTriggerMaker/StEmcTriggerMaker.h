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

class StEmcTriggerMaker : public StMaker 
{
  private:
    
    TH1F*   EtHist;        //!
    TH1F*   RatioHist;     //!
    TH1F*   HighTowerHist; //!
    TH1F*   PatchHist;     //!
    TH1F*   JetHist;       //!
    TH2F*   HTxPatch;      //!
    TH2F*   HT2D;      //!

  protected:
  public: 

    StBemcTrigger* BemcTrigger;
    TString TitleHist;
    TArrayF EtTh;
    TArrayF RatioTh;
    TArrayF HighTowerTh;
    TArrayF PatchTh;
    TArrayF JetTh;
    
    StEmcTriggerMaker(char*);
    virtual ~StEmcTriggerMaker();
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();
            void  FillHistograms();

  ClassDef(StEmcTriggerMaker, 1) 
};
#endif
