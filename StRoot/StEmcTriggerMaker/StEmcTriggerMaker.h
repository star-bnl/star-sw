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

class StEmcTriggerMaker : public StMaker 
{
  private:
    
    TH1F*   EtHist;        //!
    TH1F*   RatioHist;     //!
    TH1F*   HighTowerHist; //!
    TH1F*   PatchHist;     //!
    TH1F*   JetHist;       //!
    TH2F*   HTxPatch;      //!
//Histos for Trigger/DAQ Comparison
    TH2F*   DAQHTower;     //!High Tower Trigger ADC from data stored in DAQ 
    TH2F*   DAQPatch;      //!Patch Trigger ADC from data stored in DAQ
    TH2F*   TRGHTower;     //!High Tower Trigger ADC from TRG data
    TH2F*   TRGPatch;      //!Patch Trigger ADC from TRG data
    TH2F*   TRGHTxDAQHT;   //!TRG High Tower vs. DAQ High Tower
    TH2F*   TRGPAxDAQPA;   //!TRG ADC Patch Sum vs. DAQ ADC Patch Sum

  protected:
  public: 

    StBemcTrigger* BemcTrigger;
    TString TitleHist;
    TString HistFileName;
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
            void  FillHistograms(StTriggerDetectorCollection* theTriggers);
	    void  SetHistFileName(char* name) { HistFileName = name; }

  ClassDef(StEmcTriggerMaker, 1) 
};
#endif
