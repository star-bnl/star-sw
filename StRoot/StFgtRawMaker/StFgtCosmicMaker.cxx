
#include "StFgtCosmicMaker.h"

void StFgtCosmicMaker::PrepareEnvironment()
{


};

void StFgtCosmicMaker::constructDiscs()
{

};

StFgtCosmicMaker::StFgtCosmicMaker()
{
  //default are 6 discs, let's just leave it at that, most of them will be empty
  mFgtEvent=new StFgtEvent();

};
StFgtCosmicMaker::StFgtCosmicMaker(char* daqFileName, int numDiscs)
{
  mFgtEvent=new StFgtEvent();

  mRdr = new daqReader(daqFileName) ;	


};


//read next event from daq file and fill the fgtevent
Int_t StFgtCosmicMaker::Make()
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

  clearHits();

  Short_t discIdx=0;
  //getdiscidx from the point...
  //  Short_t geoId=StFgtGeom::getNaiveGeoIdFromElecCoord(rdo,arm,apv,channel);

  char *ret = mRdr->get(0,EVP_TYPE_ANY);
  if(mRdr->status == EVP_STAT_EOR) {
    LOG_DEBUG <<"End of File reached..."<<endl;
    return EOF;	
  }
  daq_dta *dd ;
  dd = mRdr->det("fgt")->get("adc") ;
      
  while(dd && dd->iterate()) 
    {
      fgt_adc_t *f = (fgt_adc_t *) dd->Void ;
      for(u_int i=0;i<dd->ncontent;i++) 
	{
	  channel=f[i].ch;
	  adc=f[i].adc;
	  arm=dd->sec;
	  apv=dd->pad;
	  rdo=dd->rdo;
	  timebin=f[i].tb;
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
  return 0;
}
void StFgtCosmicMaker::clearHits()
{
  if(mFgtEvent)
    mFgtEvent->Clear();
}
ClassImp(StFgtCosmicMaker);
