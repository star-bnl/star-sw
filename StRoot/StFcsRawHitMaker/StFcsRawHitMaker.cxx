/*
 *
 * \class StFcsRawHitMaker
 *
 */

#include "StFcsRawHitMaker.h"
#include "StRoot/StEvent/StEvent.h"
#include "StRoot/St_base/StMessMgr.h"
#include "StChain/StRtsTable.h"
#include "StRoot/StEvent/StFcsCollection.h"
#include "StRoot/StEvent/StFcsHit.h"
#include "StRoot/StFcsDbMaker/StFcsDbMaker.h"

StFcsRawHitMaker::StFcsRawHitMaker( const char* name) :
    StRTSBaseMaker("fcs",name){
};

StFcsRawHitMaker::~StFcsRawHitMaker(){};

int StFcsRawHitMaker::prepareEnvironment(){
  mEvent = (StEvent*)GetInputDS("StEvent");  
  if(mEvent) {
    LOG_INFO <<"::prepareEnvironment() found StEvent"<<endm;
  } else {
    mEvent=new StEvent();
    AddData(mEvent);
    LOG_INFO <<"::prepareEnvironment() has added StEvent"<<endm;
  }
  mFcsCollectionPtr=mEvent->fcsCollection();
  if(!mFcsCollectionPtr) {
    mFcsCollectionPtr=new StFcsCollection();
    mEvent->setFcsCollection(mFcsCollectionPtr);
    LOG_INFO <<"::prepareEnvironment() has added StFcsCollection"<<endm;
  } else {
    mFcsCollectionPtr=mEvent->fcsCollection();
    LOG_INFO <<"::prepareEnvironment() found StFcsCollection"<<endm;
  };
  return kStOK;
};

int StFcsRawHitMaker::InitRun(int runNumber){
    mRun=runNumber;
    mFcsDbMkr = static_cast< StFcsDbMaker*>(GetMaker("fcsDb"));
    if(!mFcsDbMkr){
	LOG_FATAL << "Error finding StFcsDbMaker"<< endm;
	return kStFatal;
    }
    return kStOK;
};

int StFcsRawHitMaker::Make() {
    StRtsTable* dd=0;
    prepareEnvironment();    
    int nData=0, nValidData=0;
    const char* mode[2]={"adc","zs"};
    char node[20];
    sprintf(node,"fcs/%s",mode[mReadMode]);
    while((dd = GetNextDaqElement(node))){   
	int s = dd->Sector();
	int sec = ((s >> 11) & 0x1F) + 1;  // sector = fcs DAQ computer (1~10)
	int rdo = ((s >> 8) & 0x7) + 1;    // fiber connecion # (0~7)
	int ehp = (s >> 6) & 0x3;          // 0=Ecal/1=Hcal/2=Pres
	int ns  = (s >> 5) & 1;            // 0=north/1=south  
	int dep = dd->Row() ;              // DEP Board# (0-23)
	int ch = dd->Pad() ;               // Channel (0-31)
	u_int n=dd->GetNRows();   
	int detid,id,crt,sub;
	mFcsDbMkr->getIdfromDep(ehp,ns,dep,ch,detid,id,crt,sub);
	u_short *d16 = (u_short *)dd->GetTable();
	StFcsHit* hit=0;
	if(mReadMode==0){
	    hit = new StFcsHit(0,detid,id,ns,ehp,dep,ch,n,d16);
	}else{
	    hit = new StFcsHit(1,detid,id,ns,ehp,dep,ch,2*n,d16);
	}
	mFcsCollectionPtr->addHit(detid,hit);
	nData++;      
	if(detid<6) nValidData++;      	
	if(GetDebug()){
	    printf("FCS %3s : S%d:%d [det %d, ns %d, dep %d ch %2d] det=%1d id=%4d : size=%2d : adc=",
		   mode[mReadMode],sec,rdo,ehp,ns,dep,ch,detid,id,n) ;
	    int sum=0;
	    for(int tb=0; tb<hit->nTimeBin(); tb++) {
		printf("%4d ", hit->adc(tb)); 
		sum+=hit->adc(tb);
	    }
	    //for(int tb=0; tb<3; tb++) printf("%4d ", hit->adc(tb));
	    printf(" sum=%d\n",sum);
	}
    }
    LOG_INFO <<Form("FCS found %d data lines, and %d valid data lines",
		    nData,nValidData)<<endm;
    if(nData>0 && GetDebug()) mFcsCollectionPtr->print(3);
    return kStOK;
};

void StFcsRawHitMaker::Clear( Option_t *opts ){};

ClassImp(StFcsRawHitMaker);

/*
 * $Id: StFcsRawHitMaker.cxx,v 1.6 2021/02/25 21:55:32 akio Exp $
 * $Log: StFcsRawHitMaker.cxx,v $
 * Revision 1.6  2021/02/25 21:55:32  akio
 * Int_t -> int
 *
 * Revision 1.5  2021/02/25 19:27:10  akio
 * Modified for STAR code review (Hongwei)
 *
 * Revision 1.4  2019/08/01 18:38:00  akio
 * added debug info
 *
 * Revision 1.3  2019/07/15 16:58:51  akio
 * clean up
 *
 * Revision 1.2  2019/07/05 15:00:52  akio
 * small corrections
 *
 * Revision 1.1  2019/07/03 17:02:45  akio
 * Initial version of FCS offline daq file reader
 *
 * 
 */
