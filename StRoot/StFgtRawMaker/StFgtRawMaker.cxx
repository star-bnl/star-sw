//
// \class StFgtRawMaker
//  \author Anselm Vossen
//
//   $Id: StFgtRawMaker.cxx,v 1.4 2011/08/24 14:30:44 avossen Exp $
/
//  $Log: StFgtRawMaker.cxx,v $
//  Revision 1.4  2011/08/24 14:30:44  avossen
//  Continued raw maker development
//
//
#include <StFgtUtil/geometry/StFgtGeom.h>


Int_t StFgtRawMaker::Make()
{
  TStopwatch clock;
  clock.Start();
  LOG_DEBUG <<"StEmcRawMaker::Make()******************************************************************"<<endm;
  if(!PrepareEnvironment)
    { LOG_WARN <<"Could not prepare the environment to process the event "<<endm; }
  FillHits();


}

Bool_t FillHits()
{

  StRtsTable* rts_tbl = this->GetNextDaqElement("fgt/adc");
if(rts_tbl)
  mFgtRawData=(fgt_adc_t*)*rts_tbl.begin();
 

//now grab the constants from the header file, loop over the raw data and fill the hits...


}

Bool_t StFgtRawMaker::PrepareEnvironment()
{
  mEvent=0;
  mEvent = (StEvent*)GetInputDS("StEvent");
  StFgtRawCollection *fgt= NULL;
  if(mEvent)
    {
      fgt=mEvent->fgtRawCollection();
    }
  else
    {
      mEvent=new StEvent();
      AddData(mEvent);
      fgt=mEvent->fgtRawCollection();
    }
  if(!fgt)
    {
      fgt=new StFgtRawCollection();
      mEvent->setFgtRawCollection(fgt);
      LOG_DEBUG << "::prepareEnvironment() has added a non existing StEmcCollection()"<<endm;
    }
}



ClassImp(StFgtRawMaker);
