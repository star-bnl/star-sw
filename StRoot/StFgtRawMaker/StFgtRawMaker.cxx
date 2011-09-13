//
// \class StFgtRawMaker
//  \author Anselm Vossen
//
//   $Id: StFgtRawMaker.cxx,v 1.7 2011/09/13 18:35:42 avossen Exp $
//
//  $Log: StFgtRawMaker.cxx,v $
//  Revision 1.7  2011/09/13 18:35:42  avossen
//  added RTS header files
//
//  Revision 1.6  2011/09/13 10:06:43  avossen
//  *** empty log message ***
//
//  Revision 1.5  2011/09/11 08:06:36  avossen
//  added cosmic maker
//
//  Revision 1.4  2011/08/24 14:30:44  avossen
//  Continued raw maker development
//
//

#include "StFgtRawMaker.h"


Int_t StFgtRawMaker::Make()
{
  TStopwatch clock;
  clock.Start();
  LOG_DEBUG <<"StEmcRawMaker::Make()******************************************************************"<<endm;
  if(!PrepareEnvironment())
    { LOG_WARN <<"Could not prepare the environment to process the event "<<endm; }
  FillHits();

};

Bool_t StFgtRawMaker::FillHits()
{

  Short_t quadrant=0;      
  Char_t layer=0;
  Double_t ordinate=0;
  Double_t lowerSpan=0;
  Double_t upperSpan=0;
  Int_t rdo=0;
  Int_t arm=0;
  Int_t apv=0;
  Int_t channel=0;
  Short_t adc=0;
  Short_t timebin=0;
  Short_t discIdx=0;

  while(this->GetNextDaqElement("fgt/adc"))
    {
      StRtsTable* rts_tbl=DaqDta();
      //works because '*' operator is giving your the row
      for(StRtsTable::iterator it=rts_tbl->begin();it!=rts_tbl->end();it++)
	{
	  mFgtRawData=(fgt_adc_t*)*it;
	  rdo=rts_tbl->Rdo();
	  //this is different from rts_example
	  channel=mFgtRawData->ch;
	  timebin=mFgtRawData->tb;
	  //look at rts_example for the mapping 
	  adc=mFgtRawData->adc;
	  arm=rts_tbl->Sector();
	  apv=rts_tbl->Pad();
	  Short_t geoId=StFgtGeom::getNaiveGeoIdFromElecCoord(rdo,arm,apv,channel);
	  StFgtGeom::getNaivePhysCoordFromElecCoord(rdo,arm,apv,channel,discIdx,quadrant,layer,ordinate,lowerSpan,upperSpan);
	  StFgtRawHit hit(geoId,adc,timebin);
	  StFgtDisc* pDisc=mFgtEvent->getDiscPtr(discIdx);
	  if(pDisc)
	    pDisc->getRawHitArray().PushBack(hit);
	  else
	    { LOG_WARN <<"Could not access disc "<<endm; }
	}
    }
//now grab the constants from the header file, loop over the raw data and fill the hits...
};

Bool_t StFgtRawMaker::PrepareEnvironment()
{
  mEvent=0;
  Short_t numDiscs=6; //or get the number of discs
  mEvent = (StEvent*)GetInputDS("StEvent");
  mFgtEvent= NULL;
  if(mEvent)
    {
      mFgtEvent=mEvent->fgtEvent();
    }
  else
    {
      mEvent=new StEvent();
      //hmmm.... see stEmcrawmaker
      AddData(mEvent);
      mFgtEvent=mEvent->fgtEvent();
    }
  if(!mFgtEvent)
    {
      mFgtEvent=new StFgtEvent(numDiscs);
      mEvent->setFgtEvent(mFgtEvent);
      LOG_DEBUG << "::prepareEnvironment() has added a non existing StFgtEvent()"<<endm;
    }
};

ClassImp(StFgtRawMaker);
