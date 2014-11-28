#include "StEmcPreMixerMaker.h"
#include <Stiostream.h>
#include "StEventTypes.h"
#include "StEvent.h"
                                                  
ClassImp(StEmcPreMixerMaker)

//_____________________________________________________________________________
StEmcPreMixerMaker::StEmcPreMixerMaker(const char *name):StMaker(name)
{
}
//_____________________________________________________________________________
StEmcPreMixerMaker::~StEmcPreMixerMaker()
{
}
//_____________________________________________________________________________
Int_t StEmcPreMixerMaker::Init()
{
  return StMaker::Init();
}  
//_____________________________________________________________________________
Int_t StEmcPreMixerMaker::Make()
{
  StEvent* event = (StEvent*)GetInputDS("StEvent");
  if(!event) return kStWarn;
  // time is set based on the first event on the memory
  Int_t GMTTime = event->time();
  StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
  if(!hd) { hd = new StEvtHddr();  AddData(hd); }
  //if(GMTTime>1893463200) GMTTime = 1893463200;
  hd->SetGMTime(GMTTime);
  return kStOk;
}

//_____________________________________________________________________________
Int_t StEmcPreMixerMaker::Finish() 
{
  return StMaker::Finish();
}
