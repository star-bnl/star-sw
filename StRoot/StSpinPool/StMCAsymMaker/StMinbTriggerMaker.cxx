//Author :: Renee Fatemi
//Emulates BBC trigger

#include "StChain.h"
#include "StMinbTriggerMaker.h"

//std
#include <map>
#include <string>
#include <vector>
#include <algorithm>
#include <iostream>
using namespace std;

//StMuDSTMaker
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

//StEvent
#include "StEvent/StBbcTriggerDetector.h"
#include "StEvent/StTriggerData.h"
#include "StEventTypes.h"                                                                                                                             


ClassImp(StMinbTriggerMaker)

StMinbTriggerMaker::StMinbTriggerMaker(const char *name):StMaker(name){
}

StMinbTriggerMaker::~StMinbTriggerMaker(){
}

Int_t StMinbTriggerMaker::Init(){

    muDstMaker = (StMuDstMaker*)GetMaker("MuDst");
    assert(muDstMaker);
    muEvent= new StMuEvent();

    BBCadcNum=48;

    EVTid=0;
    bbcTrig=0;
    Wbbc=0;
    Ebbc=0;
    for (int i=0;i<BBCadcNum;i++) {
      BBCadc[i]=0;
    }
 
    return StMaker::Init();
}

Int_t StMinbTriggerMaker::Make()
{
  
  StMuDst* dst = muDstMaker->muDst();
  assert(dst);
  muEvent = dst->event();
  assert(muEvent); 
  StEventInfo &info=muEvent->eventInfo();
  EVTid=info.id();// in case you want to sync with other code
 

  
  StBbcTriggerDetector *bbc=&(muEvent->bbcTriggerDetector());
  int Npmt=bbc->numberOfPMTs();
  //bbc->dump();
  Wbbc=0;
  Ebbc=0;
  bbcTrig=0;

  for (int pmt=0;pmt<Npmt;pmt++){   
    
    BBCadc[pmt]=bbc->adc(pmt);
    
    int bbcadc=bbc->adc(pmt);
    
    if (bbcadc>5) {
      if (pmt<16) Ebbc=1;      
      if (23<pmt && pmt<40)  Wbbc=1;
    }

  }
  
  if ((Ebbc==1)&&(Wbbc==1)) bbcTrig=1;
  cout<<" Wbbc ="<<Wbbc<<" Ebbc="<<Ebbc<<" bbcTrig="<<bbcTrig<<endl;
  for (int i=0;i<BBCadcNum;i++) cout<<i<<" adc="<<BBCadc[i]<<endl;
  
  
  return kStOK;
}










