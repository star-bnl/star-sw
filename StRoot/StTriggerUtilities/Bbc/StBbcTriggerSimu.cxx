//Author :: Renee Fatemi
//Emulates BBC trigger

#include "StChain.h"
#include "StMaker.h"

//STAR
#include "StMessMgr.h"

//StMuDSTMaker
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

//StEvent
#include "StEvent/StBbcTriggerDetector.h"
#include "StEvent/StTriggerData.h"

//StBBC
#include "StTriggerUtilities/Bbc/StBbcTriggerSimu.h"

ClassImp(StBbcTriggerSimu)
//==================================================
//==================================================

StBbcTriggerSimu::StBbcTriggerSimu(){
    BBCadcNum=48;
}
//==================================================
//==================================================

StBbcTriggerSimu::~StBbcTriggerSimu(){
} 
//==================================================
//==================================================
void  
StBbcTriggerSimu::Init(){

  LOG_INFO <<Form("Bbc::Init() MC_flag=%d, adcThres=%d",mMCflag,AdcTrigThresh)<<endm;
  if(mMCflag) LOG_WARN <<"Bbc:: TDC thresholds not implemented"<<endm;
}

//==================================================
//==================================================
void StBbcTriggerSimu::Clear(){

    bbcTrig=0;
    Wbbc=0;
    Ebbc=0;
    for (int i=0;i<BBCadcNum;i++) {
      BBCadc[i]=0;
    }

}
//==================================================
//==================================================

void StBbcTriggerSimu::Make()
{
  
  muDstMaker = (StMuDstMaker*)StMaker::GetChain()->GetMaker("MuDst");  assert(muDstMaker);
  
  muDst = muDstMaker->muDst();
  assert(muDst);

  muEvent = muDst->event();
  assert(muEvent);
  
  StBbcTriggerDetector *bbc=&(muEvent->bbcTriggerDetector());
  for (UInt_t pmt=0; pmt < bbc->numberOfPMTs(); pmt++){   
    
    BBCadc[pmt]=bbc->adc(pmt);
    
    int bbcadc=bbc->adc(pmt);
    
    if (bbcadc>AdcTrigThresh) {
      if (pmt<16) Ebbc=1;      
      if (23<pmt && pmt<40)  Wbbc=1;
    }

  }
  
  if ((Ebbc==1)&&(Wbbc==1)) bbcTrig=1;

  LOG_DEBUG<<" Wbbc ="<<Wbbc<<" Ebbc="<<Ebbc<<" bbcTrig="<<bbcTrig<<endm;
  for (int i=0;i<BBCadcNum;i++) {
    LOG_DEBUG<<i<<" adc="<<BBCadc[i]<<endm;
  }
  
}


//
// $Log: StBbcTriggerSimu.cxx,v $
// Revision 1.3  2007/07/23 02:59:56  balewski
// cleanup, bbc for M-C still not working
//










