//
// \class StFgtRawMaker
//  \author Anselm Vossen
//
//   $Id: StFgtRawMaker.cxx,v 1.5 2011/09/11 08:06:36 avossen Exp $
/
//  $Log: StFgtRawMaker.cxx,v $
//  Revision 1.5  2011/09/11 08:06:36  avossen
//  added cosmic maker
//
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

  while(this->GetNextDaqElement("fgt/adc"))
    {
      StRtsTable=rts_tbl=DaqDta();
      mFgtRawData=(fgt_adc_t*)*rts_tbl.begin();
      //get pad by Pad() etc (see rtsbasemaker functionality)
      //get geo
      //construct hit
      StFgtRawHit hit(geoId,);
      StFgtDisc disc;
      //getDisc
      disc.getRawHitArray().pushback(hit);

    }
//now grab the constants from the header file, loop over the raw data and fill the hits...


}

Bool_t StFgtRawMaker::PrepareEnvironment()
{
  mEvent=0;
  mEvent = (StEvent*)GetInputDS("StEvent");
  mFgtEvent= NULL;
  if(mEvent)
    {
      mFgtEvent=mEvent->fgtEvent();
    }
  else
    {
      mEvent=new StEvent();
      AddData(mEvent);
      mFgtEvent=mEvent->fgtEvent();
    }
  if(!fgt)
    {
      mFgtEvent=new StFgtEvent();
      mEvent->setFgtEvent(mFgtEvent);
      LOG_DEBUG << "::prepareEnvironment() has added a non existing StFgtEvent()"<<endm;
    }
  //construct the correct number of disc objects
  constructDiscs();

}



ClassImp(StFgtRawMaker);
