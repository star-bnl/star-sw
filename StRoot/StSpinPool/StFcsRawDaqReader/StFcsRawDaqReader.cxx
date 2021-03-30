/*
 *
 * \class StFcsRawMaker
 *
 */

/*  
   mRdr->det("fcs")->get("adc")    (or "zs" instead of "adc")
      sec    DAQ PC e.g. 1="fcs01"
      row    DEP bd# (1-2=ecal, 4=hcal, 6=pres)
      pad    DEP ch (0-15=ch#, 16-31=empty, 33=trigger data for DEP16)
      flags  various stamps in time (ignore for now)
      data   ADC 

   mRdr->det("stgc")->get("raw");
      sec    DAQ PC, e.g. 1="stgc01"
      rdo    RDO board# (only 1 for 2019)
      
   mRdr->det("stgc")->get("altro",r)
      r      RDO board# (only 1 for 2019)
      row    FEE ALTRO id (2 per FEEBd)
      pad    Chip channel (16ch)
      tb     Timebin
      adc    ADC
*/


#include "StFcsRawDaqReader.h"
#include "StRoot/StEvent/StEvent.h"

#include "StRoot/St_base/StMessMgr.h"
#include "RTS/src/DAQ_FCS/daq_fcs.h"
#include "RTS/src/DAQ_STGC/daq_stgc.h"
#include "RTS/src/DAQ_READER/daq_dta.h"
#include "StRoot/StEvent/StTriggerData.h"
#include "StRoot/StEvent/StTriggerData2019.h"
#include "StRoot/StEvent/StFcsCollection.h"
#include "StRoot/StEvent/StFcsHit.h"
#include "StRoot/StFcsDbMaker/StFcsDbMaker.h"
#include "StRoot/StFcsDbMaker/StFcsDb.h"
#include "RTS/src/DAQ_READER/daqReader.h"

#include <string.h>
#include <time.h>

StFcsRawDaqReader::StFcsRawDaqReader( const Char_t* name, const Char_t *daqFileName) :
  StMaker(name),mDaqFileName(daqFileName){
  std::string daqFileNameS( daqFileName );
};

StFcsRawDaqReader::~StFcsRawDaqReader(){
   if(mRdr) delete mRdr;
};

Int_t StFcsRawDaqReader::prepareEnvironment(){
  mEvent = (StEvent*)GetInputDS("StEvent");  
  if(mEvent) {
    LOG_DEBUG <<"::prepareEnvironment() found StEvent"<<endm;
  } else {
    mEvent=new StEvent();
    AddData(mEvent);
    LOG_DEBUG <<"::prepareEnvironment() has added StEvent"<<endm;
  }
  mFcsCollectionPtr=mEvent->fcsCollection();
  if(!mFcsCollectionPtr) {
    mFcsCollectionPtr=new StFcsCollection();
    mEvent->setFcsCollection(mFcsCollectionPtr);
    LOG_DEBUG<<"::prepareEnvironment() has added StFcsCollection"<<endm;
  } else {
    mFcsCollectionPtr=mEvent->fcsCollection();
    LOG_DEBUG <<"::prepareEnvironment() found StFcsCollection"<<endm;
  };
  return kStOK;
};

Int_t StFcsRawDaqReader::Init(){
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

   StFcsDbMaker* mFcsDbMkr = static_cast<StFcsDbMaker*>(GetMaker("fcsDbMkr"));
   if(!mFcsDbMkr){
     LOG_FATAL << "Error finding StFcsDbMaker"<< endm;
     return kStFatal;
   }   
   mFcsDbMkr->SetDateTime(date,time);
   LOG_INFO << "Using date and time " << mFcsDbMkr->GetDateTime().GetDate() << ", "
	    << mFcsDbMkr->GetDateTime().GetTime() << endm;

   mFcsDb = static_cast<StFcsDb*>(GetDataSet("fcsDb"));
   if(!mFcsDb){
     LOG_FATAL << "Error finding StFcsDb"<< endm;
     return kStFatal;
   }   
   return kStOK;
};

Int_t StFcsRawDaqReader::Make() {
  static int nskip=0;
  static int nskiptot=0;
  mTrg=0;
  prepareEnvironment();
  
  /*
  mRdr->get(0,EVP_TYPE_ANY);
  if(mRdr->status == EVP_STAT_EOR || (mMaxSector>0 && nInSec>=mMaxEvtPerSector)) {
    LOG_INFO <<"End of File reached..."<<endm;
    if(mMaxSector==0){
      return kStEOF;
    }else{
      mSector++;
      if(mSector>mMaxSector) return kStEOF;
      TString fn(mDaqFileName.data());
      fn.ReplaceAll("_s01_",Form("_s%02d_",mSector));
      LOG_INFO << "Opening "<< fn.Data() <<endm;
      delete mRdr;
      mRdr = new daqReader( const_cast< Char_t* >( fn.Data() ) );
      if(!mRdr) {
	LOG_FATAL << "Error constructing daqReader" << endm;
	return kStFatal;
      }
      mRdr->get(0,EVP_TYPE_ANY);
      nInSec=0;
      nskip=0;
    }    
  }
  int trgcmd = mRdr->trgcmd;
  if(trgcmd != 4 && trgcmd !=10){  // 4=phys/ped 10=LED
    nskip++;
    nskiptot++;
    printf("trgcmd=%d skipping nskip=%d nskiptot=%d\n",trgcmd,nskip,nskiptot);
    return kStOK;
  }
  */

  int GoodOrEOR=0;
  while(GoodOrEOR==0){
    mRdr->get(0,EVP_TYPE_ANY);
    if(mRdr->status != EVP_STAT_EOR){
      int trgcmd = mRdr->trgcmd;
      if(trgcmd != 4 && trgcmd !=10){  // 4=phys/ped 10=LED
	nskip++;
	nskiptot++;
	if(mDebug) printf("trgcmd=%d skipping nskip=%d nskiptot=%d\n",trgcmd,nskip,nskiptot);
	continue;
      }
    }
    if(mRdr->status==EVP_STAT_EOR || (mMaxSector>0 && mEvtInSector>=mMaxEvtPerSector)) {
      if(mMaxSector==0){
	LOG_INFO <<"End of File reached..."<<endm;
        return kStEOF;
      }else{
	LOG_INFO <<"EOF or max event for a sector file..."<<endm;
        mSector++;
        if(mSector>mMaxSector) return kStEOF;
        TString fn(mDaqFileName.data());
        fn.ReplaceAll("_s01_",Form("_s%02d_",mSector));
        LOG_INFO << "Opening "<< fn.Data() <<endm;
        delete mRdr;
        mRdr = new daqReader( const_cast< Char_t* >( fn.Data() ) );
        if(!mRdr) {
          LOG_FATAL << "Error constructing daqReader" << endm;
          return kStFatal;
        }
        mEvtInSector=0;
        nskip=0;
	continue;
      }
    }else{
      GoodOrEOR=1;
    }
  }
  int trgcmd = mRdr->trgcmd;
  if(trgcmd != 4 && trgcmd !=10){  // 4=phys/ped 10=LED
    printf("This should not happen!!!  trgcmd=%d skipping nskip=%d nskiptot=%d\n",trgcmd,nskip,nskiptot);
    return kStOK;
  }
  if(nskip>0){
    printf("Skipped nskip=%d nskiptot=%d\n",nskip,nskiptot);
    nskip=0;
  }
  mEvtInSector++; //only count for valid trgCmd
  
  mTrgMask = mRdr->daqbits64;
  if(mDebug){
    printf("daqbits64="); 
    for(int i=63; i>=0; i--){     
      if(i%8==7) printf(" ");
      printf("%1llx",(mTrgMask>>i)&0x1);
    }; 
    printf("\n");
  }

  daq_dta *dd = 0;
  dd = mRdr->det("trg")->get("raw");
  if(!dd){
    //printf("trg/raw not found\n");
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
      if(desc->ver==0x46){
        TriggerDataBlk2019* trgdata2019 = (TriggerDataBlk2019*)dd->Byte;  
	mTrg = (StTriggerData*) new StTriggerData2019(trgdata2019,mRun,1,mDebug);
        if(mDebug) printf("Creating StTriggerData for ver=0x46 (2019) with run=%d\n",mRun);
        //AddData(new TObjectSet("StTriggerData",new StTriggerData2017(trgdata2017,mRun,1,mDebug),kTRUE));
        //printf("Adding dataset StTriggerData for ver=0x44 (2017) with run=%d\n",mRun);
      }else{
        printf("Unknown StTriggerData version = %x\n",desc->ver);
      }
    }
  }

  int ndata=0, nvaliddata=0;
  //  char* mode[2]={"adc","zs"};
  string mode[2]={"adc","zs"};
  if(mReadMode==1){
    /**/
  }else{
    mReadMode=0;
  }
  dd = mRdr->det("fcs")->get(mode[mReadMode].c_str());  
  if(dd){
    while(dd->iterate()) {
      int sec = ((dd->sec >> 11) & 0x1F) + 1;
      int rdo = ((dd->sec >> 8) & 0x7) + 1;
      int ehp = (dd->sec >> 6) & 0x3;
      int ns  = (dd->sec >> 5) & 1;
      int dep = dd->row ;
      int ch = dd->pad ;
      u_int n=dd->ncontent;      
      int detid,id,crt,sub;
      mFcsDb->getIdfromDep(ehp,ns,dep,ch,detid,id,crt,sub);
      //if(ch>=32) continue;
      u_short *d16 = (u_short *)dd->Void;
      StFcsHit* hit=0;
      //unsigned short tmp[512];
      if(mReadMode==0){
	hit = new StFcsHit(0,detid,id,ns,ehp,dep,ch,n,d16);
      }else{
	/*
	for(u_int i=0; i<n; i++) {
	  u_int tb   = dd->adc[i].tb;
	  u_int data = dd->adc[i].adc;
	  tmp[i*2  ]=data;
	  tmp[i*2+1]=tb;

	  printf("AAA %4d : %4d %4d : %4d %4d\n",i,data&0xfff,d16[i*2]&0xfff,tb,d16[i*2+1]);
	}	
	hit = new StFcsHit(1,detid,id,ns,ehp,dep,ch,2*n,tmp);
	*/
	hit = new StFcsHit(1,detid,id,ns,ehp,dep,ch,2*n,d16);
      }
      mFcsCollectionPtr->addHit(detid,hit);
      ndata++;      
      if(detid<6) nvaliddata++;      
      
      if(mDebug){
	printf("FCS %3s : S%d:%d [det %d, ns %d, dep %d ch %d] det=%d id=%3d : size=%d : adc=",
	       mode[mReadMode].c_str(),sec,rdo,ehp,ns,dep,ch,detid,id,n) ;
	for(unsigned int tb=0; tb<hit->nTimeBin(); tb++) printf("%4d ", hit->adc(tb));
	//for(int tb=0; tb<3; tb++) printf("%4d ", hit->adc(tb));
	printf("\n");
      }
    }
  }   
  LOG_DEBUG <<Form("FCS found %d data lines, and %d valid data lines",
		   ndata,nvaliddata)<<endm;
  if(mDebug>3) mFcsCollectionPtr->print(3);

  ndata=0;
  dd = mRdr->det("stgc")->get("raw");
  if(dd){
    while(dd->iterate()){
      if(mDebug) printf("STGC RAW: stgc%02d(sec) RDO=%1d size=%d(bytes)\n",dd->sec,dd->rdo,dd->ncontent);
    }
  }
  for(int r=1; r<=6; r++) {
    dd = mRdr->det("stgc")->get("altro",r) ;
    while(dd && dd->iterate()) {    //per xing and per RDO    
      if(mDebug) printf("STGC ALTRO: stgc%02d(sec) RDO=%1d ALTRO=%03d(row) Ch=%02d(pad)\n",dd->sec,r,dd->row,dd->pad);
      for(u_int i=0; i<dd->ncontent; i++) {
	if(mDebug) printf(" TB=%3d ADC=%4d",dd->adc[i].tb,dd->adc[i].adc) ;
	ndata++;
      }
      if(mDebug) printf("\n");
    }
  }
  LOG_DEBUG <<Form("STGC found %d data lines",ndata)<<endm;

  return kStOK;
};

void StFcsRawDaqReader::Clear( Option_t *opts ){
  StTriggerData2019* d=(StTriggerData2019*)mTrg;
  if(d) delete d;
  if(mFcsCollectionPtr){
    for(int d=0; d<kFcsNDet+1; d++) { mFcsCollectionPtr->hits(d).clear(); }
  }
};

ClassImp(StFcsRawDaqReader);

/*
 * $Id: StFcsRawDaqReader.cxx,v 1.7 2021/03/30 13:30:11 akio Exp $
 * $Log: StFcsRawDaqReader.cxx,v $
 * Revision 1.7  2021/03/30 13:30:11  akio
 * StFcsDbMAker->StFcsDB
 *
 * Revision 1.6  2021/02/13 21:39:17  akio
 * debug printouts
 *
 * Revision 1.5  2021/01/11 14:39:12  akio
 * Change logic to skip over none standard events at the begining of files.
 * Added function to get event# in a sector=file.
 *
 * Revision 1.4  2019/07/10 07:47:37  akio
 * minor fix for compilation warnings
 *
 * Revision 1.3  2019/07/10 03:09:57  akio
 * fix trigger data version for run19
 *
 * Revision 1.2  2019/06/07 18:22:33  akio
 * *** empty log message ***
 *
 * Revision 1.1  2019/03/14 14:45:35  akio
 * FCS raw daq reader for online monitoring
 *
 * Revision 1.1  2019/03/13 20:31:34  akio
 * FCS daqfile/evp reader for online
 *
 * 
 */
