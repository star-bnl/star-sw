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
    LOG_DEBUG <<"StFcsRawDaqReader::prepareEnvironment() has added StEvent"<<endm;
  }
  mFcsCollectionPtr=mEvent->fcsCollection();
  if(!mFcsCollectionPtr) {
    mFcsCollectionPtr=new StFcsCollection();
    mEvent->setFcsCollection(mFcsCollectionPtr);
    LOG_DEBUG <<"StFcsRawDaqReader::prepareEnvironment() has added StFcsCollection"<<endm;
  } else {
    mFcsCollectionPtr=mEvent->fcsCollection();
    LOG_DEBUG <<"StFcsRawDaqReader::prepareEnvironment() found StFcsCollection"<<endm;
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
  LOG_DEBUG << "Starting StFcsRawDaqReader::Make()"<<endm;

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
  int startrg=0,fcstrg=0;
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
        LOG_DEBUG << "Creating StTriggerData for ver=0x46 (2019) with run="<<mRun<<endm;
	mEvent->setTriggerData((StTriggerData*)new StTriggerData2019(trgdata2019,mRun,1,mDebug));
	LOG_DEBUG << "Added StTriggerData to StEvent"<<endm;
        //AddData(new TObjectSet("StTriggerData",new StTriggerData2019(trgdata2019,mRun,1,mDebug),kTRUE));
        //LOG_DEBUG << "Adding Dataset StTriggerData"<<endm;
	//	mTrg = (StTriggerData*) (GetData("StTriggerData")->GetObject());
	//mTrg = mEvent->triggerData();
        //LOG_DEBUG << "Got back Dataset StTriggerData addr="<<mTrg<<endm;
	
	mFcsTcuBit = mTrg->lastDSM(5);
	//unsigned short lastdsm4 = mTrg->lastDSM(4);
	//unsigned short fcs0   = (lastdsm4 >> 10) & 0x1;
	//unsigned short fcs1   = (lastdsm4 >>  5) & 0x1;
	//unsigned short fcs2   = (lastdsm4 >>  7) & 0x1;
	//unsigned short fcs3   = (lastdsm4 >>  8) & 0x1;
	//unsigned short fcs4   = (lastdsm4 >>  9) & 0x1;
	//unsigned short fcs5   = (lastdsm4 >> 12) & 0x1;
	//unsigned short fcs6   = (lastdsm4 >> 13) & 0x1;
	//unsigned short fcs7   = (lastdsm4 >> 14) & 0x1;
	//unsigned short fcs8   = (lastdsm4 >> 15) & 0x1; 
	//mFcsTcuBit = fcs0 + (fcs1<<1) + (fcs2<<2) + (fcs3<<3) 
	//  + (fcs4<<4) + (fcs5<<5) + (fcs6<<6) + (fcs7<<7) + (fcs8<<8);
	LOG_DEBUG << Form("FCS TCU Bits = 0x%04x",mFcsTcuBit)<<endm;

	unsigned long long l2sum=mTrg->l2sum();
	startrg = (l2sum & 0xFF8000FFFFFFFFFF)?1:0;
	fcstrg  = (l2sum & 0x007FFF0000000000)?1:0;
	LOG_DEBUG << Form("L2SUM = 0x%016llx STAR=%1d FCS=%1d",l2sum,startrg,fcstrg) << endm;

      }else{
        printf("Unknown StTriggerData version = %x\n",desc->ver);
      }
    }
  }

  /*
  //Get DEPIO board info from "adc"
  mFcsDepOut=0;
  dd = mRdr->det("fcs")->get("adc");
  if(dd){
    while(dd->iterate()) {
      //int sec = ((dd->sec >> 11) & 0x1F) + 1;
      //int rdo = ((dd->sec >> 8) & 0x7) + 1;
      int ehp = (dd->sec >> 6) & 0x3;
      int ns  = (dd->sec >> 5) & 1;
      int dep = dd->row ;
      int ch  = dd->pad ;
      //printf("DEPIO EHP=%1d NS=%1d DEP=%02d CH=%02d N=%d\n",
      //     ehp,ns,dep,ch,dd->ncontent);
      if(ehp==3 && ns==0 && dep==0 && (ch==4 || ch==5)){	
	u_int n=dd->ncontent;
	u_short *d16 = (u_short *)dd->Void;
	if(ch==4) mFcsDepOut += (d16[96] & 0xFF);
	if(ch==5) mFcsDepOut += (d16[96] & 0xFF) << 8;
	//for(int i=0; i<n; i++) printf("  tb=%3d  d16=0x%04x\n",i,d16[i]);
      }
    }
  }
  */

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
      //printf("EHP=%1d NS=%1d DEP=%02d CH=%02d DET=%1d id=%4d\n",ehp,ns,dep,ch,detid,id);
      //if(ch>=32) continue;
      u_short *d16 = (u_short *)dd->Void;
      StFcsHit* hit=0;
      unsigned short tmp[1024];
      if(mReadMode==0){
	hit = new StFcsHit(0,detid,id,ns,ehp,dep,ch,n,d16);
      }else{
	/*
	if(startrg==0 && fcstrg==1){
	  for(u_int i=0; i<n; i++) {
	    u_int tb   = dd->adc[i].tb;
	    u_int data = dd->adc[i].adc;
	    tmp[i*2  ]=data;
	    tmp[i*2+1]=tb + 8;	    
	    //printf("AAA %4d : %4d %4d : %4d %4d\n",i,data&0xfff,d16[i*2]&0xfff,tb,d16[i*2+1]);
	  }	
	  hit = new StFcsHit(1,detid,id,ns,ehp,dep,ch,2*n,tmp);	  
	*/
	//}else{
	  hit = new StFcsHit(1,detid,id,ns,ehp,dep,ch,2*n,d16);
	//}
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
  //StTriggerData2019* d=(StTriggerData2019*)mTrg;
  //if(d) delete d;
  //mFcsCollectionPtr->print();
  //StMaker::Clear(opts);
  //mFcsCollectionPtr->print();
  //if(mFcsCollectionPtr){
  //  for(int d=0; d<kFcsNDet+1; d++) { mFcsCollectionPtr->hits(d).clear(); }
  //}
  //mFcsCollectionPtr->print();
};

ClassImp(StFcsRawDaqReader);

/*
 * $Id: StFcsRawDaqReader.cxx,v 1.9 2021/05/30 21:31:30 akio Exp $
 * $Log: StFcsRawDaqReader.cxx,v $
 * Revision 1.9  2021/05/30 21:31:30  akio
 * Adding StTriggerData to StEvent, not dataset
 *
 * Revision 1.8  2021/05/27 13:10:38  akio
 * Many updates for trigger bits and around Clear()
 *
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
