#include "StEmcPreDbMaker.h"

#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDebug.h"
#include "StMuDSTMaker/COMMON/StMuEmcUtil.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StEvent/StEventTypes.h"
#include "StEvent/StEvent.h"

StEmcPreDbMaker::StEmcPreDbMaker(const char* self ,const char* muDstMakerName) : StMaker(muDstMakerName) 
{
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
}
StEmcPreDbMaker::~StEmcPreDbMaker() 
{ 
} 
int StEmcPreDbMaker::Make()///< create a StEvent from the muDst and put it into the .data tree 
{   
  if ( mMuDstMaker ) 
  {
    StMuDst* muDst = mMuDstMaker->muDst();
    StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
    StMuEvent* mu = muDst->event();
    if(!hd) { hd = new StEvtHddr();  AddData(hd); }
    hd->SetGMTime(mu->eventInfo().time());
    hd->SetRunNumber(mu->runInfo().runId());    
  }
  return 0;
}

ClassImp(StEmcPreDbMaker)

