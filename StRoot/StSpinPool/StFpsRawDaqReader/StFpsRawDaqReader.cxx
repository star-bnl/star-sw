/*
 *
 * \class StFpsRawMaker
 *
 */

#include "StFpsRawDaqReader.h"
#include "StRoot/StEvent/StEvent.h"

#include "StRoot/St_base/StMessMgr.h"
#include "RTS/src/DAQ_FPS/daq_fps.h"
#include "RTS/src/DAQ_READER/daq_dta.h"
#include "StRoot/StEvent/StTriggerData.h"
#include "StRoot/StEvent/StTriggerData2017.h"
#include "StRoot/StEvent/StFmsCollection.h"
#include "StRoot/StEvent/StFmsHit.h"
#include "RTS/src/DAQ_READER/daqReader.h"
#include "StRoot/StFmsDbMaker/StFmsDbMaker.h"

#include <string.h>
#include <time.h>

StFpsRawDaqReader::StFpsRawDaqReader( const Char_t* name, const Char_t *daqFileName) :
  StMaker(name),mEvent(0),mFmsCollectionPtr(0),mDaqFileName(daqFileName),mRdr(0),mFmsDbMkr(0){
  std::string daqFileNameS( daqFileName );
};

StFpsRawDaqReader::~StFpsRawDaqReader(){
   if(mRdr) delete mRdr;
};

Int_t StFpsRawDaqReader::prepareEnvironment(){
  //mEvent = (StEvent*)GetInputDS("StEvent");  
  if(mEvent) {
    mFmsCollectionPtr=mEvent->fmsCollection();
    LOG_DEBUG <<"::prepareEnvironment() found StEvent"<<endm;
  } else {
    mEvent=new StEvent();
    AddData(mEvent);
    LOG_DEBUG <<"::prepareEnvironment() has added StEvent"<<endm;
  }
  if(!mFmsCollectionPtr) {
    mFmsCollectionPtr=new StFmsCollection();
    mEvent->setFmsCollection(mFmsCollectionPtr);
    LOG_DEBUG <<"::prepareEnvironment() has added StFpsCollection"<<endm;
  } else {
    LOG_DEBUG <<"::prepareEnvironment() found FmsCollection"<<endm;
  };
  return kStOK;
};

Int_t StFpsRawDaqReader::Init(){
   GetEvtHddr()->SetEventNumber(1);
   LOG_INFO << "Opening "<< mDaqFileName.data() <<endm;
   mRdr = new daqReader( const_cast< Char_t* >( mDaqFileName.data() ) ); 	
   if(!mRdr) {
     LOG_FATAL << "Error constructing daqReader" << endm;
     return kStFatal;
   }
   mRdr->get(0,EVP_TYPE_ANY);
   int unixtime=mRdr->evt_time;
   struct tm* local = localtime((const time_t*)&unixtime);
   int date=(local->tm_year+1900)*10000 + (local->tm_mon+1)*100 + local->tm_mday;
   int time=local->tm_hour*10000 + local->tm_min*100 + local->tm_sec;
   printf("Event Unix Time = %d %0d %06d\n",mRdr->evt_time,date,time);   
   mFmsDbMkr = static_cast< StFmsDbMaker*>(GetMaker("fmsDb"));
   if(!mFmsDbMkr){
     LOG_FATAL << "Error finding StFmsDbMaker"<< endm;
     return kStFatal;
   }
   mFmsDbMkr->SetDateTime(date,time);
   LOG_INFO << "Using date and time " << mFmsDbMkr->GetDateTime().GetDate() << ", "
	    << mFmsDbMkr->GetDateTime().GetTime() << endm;
   return kStOK;
};

Int_t StFpsRawDaqReader::Make() {
  enum {kFpsQtCrate=8, kFpostQtCrate=9};
  mTrg=0;

  prepareEnvironment();
  
  mRdr->get(0,EVP_TYPE_ANY);
  if(mRdr->status == EVP_STAT_EOR) {
    LOG_DEBUG <<"End of File reached..."<<endm;
    return kStEOF;	
  }
  
  mTrgMask = mRdr->daqbits64;
  if(Debug()){
    printf("daqbits64="); 
    for(int i=63; i>=0; i--){     
      if(i%10==9) printf(" ");
      printf("%1x",(mTrgMask>>i)&0x1);
    }; 
    printf("\n");
  }

  daq_dta *dd = 0;
  dd = mRdr->det("trg")->get("raw");
  if(!dd){
    printf("trg/raw not found\n");
  }else{
    while(dd->iterate()) {
      u_char *trg_raw = dd->Byte;
      struct simple_desc {
        short len ;
        char evt_desc ;
        char ver ;
      } *desc ;
      desc = (simple_desc *) trg_raw ;
      //printf("Trigger: raw bank has %d bytes: ver 0x%02X, desc %d, len %d\n",dd->ncontent,desc->ver,desc->evt_desc,desc->len);                 
      if(desc->ver==0x44){
	int mDebug=0;
        TriggerDataBlk2017* trgdata2017 = (TriggerDataBlk2017*)dd->Byte;  
	mTrg = (StTriggerData*) new StTriggerData2017(trgdata2017,mRun,1,mDebug);
        if(Debug()) printf("Creating StTriggerData for ver=0x44 (2017) with run=%d\n",mRun);
        //AddData(new TObjectSet("StTriggerData",new StTriggerData2017(trgdata2017,mRun,1,mDebug),kTRUE));
        //printf("Adding dataset StTriggerData for ver=0x44 (2017) with run=%d\n",mRun);
      }else{
        printf("Unknown StTriggerData version = %x\n",desc->ver);
      }
    }
  }

  int ndata=0;
  mRccFps=0; mRccFpost=0;
  for(int fpsfpost=1;fpsfpost<=2;fpsfpost++) {    
    dd = mRdr->det("fps")->get("adc",fpsfpost) ;
    if(dd){
      if(fpsfpost==1){
	mRccFps=((fps_evt_hdr_t *)(dd->meta))->reserved[1];
      }else{
	mRccFpost=((fps_evt_hdr_t *)(dd->meta))->reserved[1];
      }
    }
    while(dd && dd->iterate()) {
      
      // 2015
      //int xing=(char)dd->sec;
      //if(xing>=128) xing-=256;
      //int qt=dd->rdo;
      //int n=dd->ncontent;
      
      // 2017
      //int fpsfpost=dd->sec;
      int xing=(char)dd->pad;
      if(xing>=128) xing-=256;
      int qt=dd->row;
      u_int n=dd->ncontent;
      
      if(Debug()) printf("FPS: fpsfpost %1d xing %2d, QT %d, chs %d\n",fpsfpost,xing,qt,n) ;     
      fps_adc_t *a = (fps_adc_t *) dd->Void ;     
      for(u_int i=0;i<n;i++) {
	ndata++;
	int ch=a[i].ch;
	int adc=a[i].adc;
	int tdc=a[i].tdc;
	if(Debug()) {
	  if(fpsfpost==1) printf("FPS  : xing %2d, QT %4d, ch %2d: ADC %4d, TDC %2d\n",xing,qt,ch,adc,tdc);
	  if(fpsfpost==2) printf("FPOST: xing %2d, QT %4d, ch %2d: ADC %4d, TDC %2d\n",xing,qt,ch,adc,tdc);
	}
	int slatid,q,l,s,flag=0,det,crate;
	if(fpsfpost==1){	
	  slatid = mFmsDbMkr->fpsSlatidFromQT(qt,ch);
	  mFmsDbMkr->fpsQLSfromSlatId(slatid,&q,&l,&s);
	  if(slatid<0)          { /* LOG_WARN << "Invalid SlatId = "<<slatid<<endm;*/      flag=1; }
	  if(q<0 || l<1 || s<1) { /* LOG_WARN << Form("Invalid Q/L/S = %d/%d/%d",q,l,s);*/ flag=1; }
	  det=kFpsDetId;
	  crate=kFpsQtCrate;
	}else if(fpsfpost==2){
	  slatid = mFmsDbMkr->fpostSlatidFromQT(qt,ch); //Get SlatId from QT address and channel
	  mFmsDbMkr->fpostQLSfromSlatId(slatid,&q,&l,&s); //Get Quad/Layer/Slat#s from SlatId
	  if(slatid<0)          { /* LOG_WARN << "Invalid SlatId = "<<slatid<<endm;*/      flag=1; }
	  if(q<0 || l<1 || s<1) { /* LOG_WARN << Form("Invalid Q/L/S = %d/%d/%d",q,l,s);*/ flag=1; }
	  det=kFpostDetId;
	  crate=kFpostQtCrate;
	}	    
	if(flag==0){
	  StFmsHit* hit = new StFmsHit();
	  hit->setDetectorId(det);
	  hit->setChannel(slatid);
	  hit->setQtCrate(crate);
	  hit->setQtSlot(qt);
	  hit->setQtChannel(ch);
	  hit->setAdc(adc);
	  hit->setTdc((char)xing);  //! Hack to keep xing# as tdc for now                                                                                          
	  hit->setEnergy(0.0);
	  if(Debug()) {
	    printf("Nhit=%d  : ",mFmsCollectionPtr->numberOfHits());
	    hit->print();
	  }
	  mFmsCollectionPtr->addHit(hit);
	}
      }
    }
  }   
  LOG_DEBUG <<Form("Found %d data lines",ndata)<<endm;
  return kStOK;
};

void StFpsRawDaqReader::Clear( Option_t *opts ){
  StTriggerData2017* d=(StTriggerData2017*)mTrg;
  if(d) delete d;
  if(mFmsCollectionPtr) mFmsCollectionPtr->hits().clear();  
};

ClassImp(StFpsRawDaqReader);

/*
 * $Id: StFpsRawDaqReader.cxx,v 1.12 2019/03/14 15:29:15 akio Exp $
 * $Log: StFpsRawDaqReader.cxx,v $
 * Revision 1.12  2019/03/14 15:29:15  akio
 * fixing wrong comit
 *
 * Revision 1.10  2017/03/20 21:05:12  akio
 * Moving accessing RCC counters before hit loop
 *
 * Revision 1.9  2017/03/02 23:19:02  akio
 * check before deleting mTrg
 *
 * Revision 1.8  2017/03/02 23:17:06  akio
 * initialize mTrg=0
 *
 * Revision 1.7  2017/02/20 19:17:16  akio
 * Explicit loop over sector to get meta for both FPS and FPOST
 *
 * Revision 1.6  2017/02/18 18:25:40  akio
 * adding RCC counter reading from meta
 *
 * Revision 1.5  2017/02/16 20:54:27  akio
 * fix typo
 *
 * Revision 1.4  2017/02/16 20:19:25  akio
 * modified for run17
 *
 * Revision 1.3  2015/05/21 18:23:51  akio
 * *** empty log message ***
 *
 * Revision 1.2  2015/02/28 02:58:56  akio
 * Some bug fixes
 *
 * Revision 1.1  2015/02/26 20:26:38  akio
 * Adding raw daq file (or EVP) reader for FPS (not for offline BFC, but for online use)
 *
 */
