#include "StEEmcPreMixerMaker.h"
#include <Stiostream.h>
#include "StEventTypes.h"
#include "StEvent.h"
                                                  
ClassImp(StEEmcPreMixerMaker)

//_____________________________________________________________________________
StEEmcPreMixerMaker::StEEmcPreMixerMaker(const char *name):StMaker(name)
{
}
//_____________________________________________________________________________
StEEmcPreMixerMaker::~StEEmcPreMixerMaker()
{
}
//_____________________________________________________________________________
Int_t StEEmcPreMixerMaker::Init()
{
  return StMaker::Init();
}  
//_____________________________________________________________________________
Int_t StEEmcPreMixerMaker::Make()
{
  StEvent* event = (StEvent*)GetInputDS("StEvent");
  if(!event) return kStWarn;
  // time is set based on the first event on the memory
  Int_t GMTTime = event->time();
  StEvtHddr *hd = (StEvtHddr*)GetDataSet("EvtHddr");
  if(!hd) { hd = new StEvtHddr();  AddData(hd); }
  cout <<GMTTime<<endl;
  //if(GMTTime>1893463200) GMTTime = 1893463200;
  hd->SetGMTime(GMTTime);
  return kStOk;
}

//_____________________________________________________________________________
Int_t StEEmcPreMixerMaker::Finish() 
{
  return StMaker::Finish();
}

///////////////////////////////////////////////////////////////////////////
//
// $Id: StEEmcPreMixerMaker.cxx,v 1.1.1.1 2005/05/31 18:53:25 wzhang Exp $
// $Log: StEEmcPreMixerMaker.cxx,v $
// Revision 1.1.1.1  2005/05/31 18:53:25  wzhang
// First version
//
//
///////////////////////////////////////////////////////////////////////////
