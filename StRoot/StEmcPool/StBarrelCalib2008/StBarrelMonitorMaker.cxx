// *-- Author : Jan Balewski
// 
// $Id: StBarrelMonitorMaker.cxx,v 1.5 2012/12/12 22:05:09 fisyak Exp $
#ifdef __APPLE__
#include <sys/types.h>
#endif

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include <StMessMgr.h>
 
#include "StBarrelMonitorMaker.h"
#include "StJanBarrelDbMaker.h"
#include "JanBprsEveA.h"
#include "BprsCapPolygraph.h"
#include "BarrelMipCalib.h"

#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "StEmcRawMaker/defines.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

#include "StDetectorDbMaker/St_tpcGasC.h"
#include "St_db_Maker/St_db_Maker.h"
#include "StEmcUtil/database/StEmcDecoder.h"


#include "StEventTypes.h"
#include "StMcEventTypes.hh"
#include "StMcEvent.hh"


ClassImp(StBarrelMonitorMaker)

//________________________________________________
//________________________________________________
StBarrelMonitorMaker::StBarrelMonitorMaker( const char* self ): StMaker(self){

  cTile[0]='T';  cTile[1]='P';
  cTile4[0]="BTOW";  cTile4[1]="BPRS";

  nInpEve=nAcceptEve=nTrigEve=nCorrEve= mGeantEveInp=0;
 
  setHList(0);
  setTrigIdFilter(0);
  setMC(0);

  mMappB=0;
  hBprs3D=0;// special very large 3D histo for BPRS cap monitoring

  setBprsHisto(1);
  setCalibPass(0);
}
 
//________________________________________________
//________________________________________________
Int_t 
StBarrelMonitorMaker::Init(){

  assert(HList);
  initHistos();

  mBtowGeom = StEmcGeom::instance("bemc");
  mBprsGeom = StEmcGeom::instance("bprs");
  mSmdEGeom = StEmcGeom::instance("bsmde");
  mSmdPGeom = StEmcGeom::instance("bsmdp"); // needed only to map m-s-e --> softID

  mMuDstMaker = (StMuDstMaker*)GetMaker("MuDst"); assert(mMuDstMaker);
  mJanDbMaker = (StJanBarrelDbMaker*)GetMaker("janBarrelDb"); assert(mJanDbMaker);

  bprsPolygraph=new BprsCapPolygraph(HList, mJanDbMaker) ;
  bprsPolygraph->setCut(50, 0.98,1,0.1);
  bprsPolygraph->print(); 

  mipCalib=new BarrelMipCalib(HList, mJanDbMaker,  mMuDstMaker) ;
  mipCalib->setCut(60.,0.35,1.3,0.51, 3.3, 1.0, 180); 
  //               zV, pT, eta, nFit, dedx, zMargin, Rxy
  mipCalib->print(); 

  LOG_INFO<<Form("::Init() filter trigID=%d done", trigID)<<endm;  
  return StMaker::Init();
}

//________________________________________________
//________________________________________________
Int_t 
StBarrelMonitorMaker::InitRun(int runNo){
  LOG_INFO<<Form("::InitRun(%d) start",runNo)<<endm;
  St_db_Maker* mydb = (St_db_Maker*) StMaker::GetChain()->GetMaker("StarDb");
  assert(mydb);
   
  // this is how BTOW mapping is accesible
  mMappB = new StEmcDecoder(mydb->GetDateTime().GetDate(),mydb->GetDateTime().GetTime());
  float airPres=St_tpcGasC::instance()-> barometricPressure();
  LOG_INFO<<Form("::InitRun(%d) AirPressure=%.2f",runNo, airPres)<<endm;
  if(runNo==1000000) {
    LOG_WARN<<Form("::InitRun(%d) ??? , it is OK for M-C ",runNo)<<endm;
  }  

  LOG_INFO<<Form("::InitRun() algo params:  bprsHisto=%d calibPass=%d (bits)",
		 par_bprsHisto,par_calibPass
		 )<<endm;
  return kStOK;
}

//________________________________________________
//________________________________________________
Int_t 
StBarrelMonitorMaker::Finish(){
 
  gMessMgr->Message("","I") <<GetName()<<"::Finish()\n    inputEve="<<nInpEve<<" trigFilterEve="<<nTrigEve<<" nCorrEve="<<nCorrEve<<" nAcceptEve="<<nAcceptEve<<endm;
  return kStOK;
}

//________________________________________________
//________________________________________________
void 
StBarrelMonitorMaker::Clear(const Option_t*){
  eventID=-999;
  janEve.clear();
  for( int icr=0; icr <mxBprsCrate;icr++) janBprsEveA[icr].clear();
}

//________________________________________________
//________________________________________________
//________________________________________________
Int_t 
StBarrelMonitorMaker::Make(){
  nInpEve++;
  StMuEvent* muEve = mMuDstMaker->muDst()->event();

  eventID=muEve->eventId();
  janEve.id=eventID;

  int nPrimV=mMuDstMaker->muDst()->numberOfPrimaryVertices();
 
  LOG_INFO <<GetName()<<"\n\n::Make()================ isMC="<<isMC<<" eveID="<<eventID<<" nInpEve="<<nInpEve<<" nPrimV="<<nPrimV<<endm;
  

  vector<unsigned int> trgL=mMuDstMaker->muDst()->event()->triggerIdCollection().nominal().triggerIds();
  printf("trigL len=%d\n",int(trgL.size()));
  uint ii;
  for( ii=0;ii<trgL.size();ii++) printf("ii=%d trigID=%d\n",ii,trgL[ii]);
  if (mMuDstMaker->muDst()->event()->triggerIdCollection().nominal().isTrigger(19)) return kStOK; // drop bbc-fast events
  

  //  printf("trigOK\n"); 
  // nTrigEve++;


  unpackStTiles(kBTow);
  unpackStTiles(kBPrs);  

  populateBprsEveA();
  
  // ...... check for BPRS capID corruption .....
  for(int bprsCrateID=0; bprsCrateID<mxBprsCrate;  bprsCrateID++) 
  // int bprsCrateID=2;
   {    
    JanBprsEveA &bprsEve=janBprsEveA[bprsCrateID];
    bprsPolygraph->doBaseline(bprsEve,janEve);
    bprsPolygraph->findBestCap(bprsEve,janEve);
    bprsPolygraph->doPedResidua(bprsEve); 
    if(par_calibPass & kPassCapFix)  // do correct  capID corruption
      janEve.bprsCap[bprsCrateID]=bprsEve.getBestCapID();   
  }
  //if(nInpEve<3)  janEve.print(2);  
  calibrateTiles(kBPrs);
  calibrateTiles(kBTow);  
 

 // 

  mipCalib->search(janEve); // full analysis
  // mipCalib->searchEtaBin20(janEve); // just last eta bin location

  return kStOK;
} 



//___________________ _____________________________
//________________________________________________
StBarrelMonitorMaker::~StBarrelMonitorMaker(){
  
}

//___________________ _____________________________
//________________________________________________
void 
StBarrelMonitorMaker::saveHisto(TString fname){
  TString outName=fname+".hist.root";
  TFile f( outName,"recreate");
  assert(f.IsOpen());
  printf("HHH %d histos are written  to '%s' ...\n",HList->GetEntries(),outName.Data());

  HList->Write();
  f.Close();

}

//________________________________________________
//________________________________________________
void
StBarrelMonitorMaker::unpackStTiles(int ibp){
 StEvent *mEvent = (StEvent*)StMaker::GetChain()-> GetInputDS("StEvent");
  assert(mEvent);

  //.........................  BTOW  or  BPRS  ....................
  //use StEvent as default  to get simulation right in BEMC
  StEmcCollection *emc = mEvent->emcCollection();
  assert (emc);

  int jBP=BTOW; 
  StEmcDetector* detector=emc->detector(kBarrelEmcTowerId);
  StEmcGeom  *geomB=mBtowGeom;
  if( ibp==kBPrs) {// change pointers for  preshower
    jBP=BPRS; 
    geomB=mBprsGeom;
    detector=emc->detector(kBarrelEmcPreShowerId);
  }

  if(!detector) { 
    printf("no %s  data, nInpEve=%d\n",cTile4[ibp],nInpEve);
    return;
  }

  // for BPRS unpack caps and store locally
  if(ibp==kBPrs ) {
    for(int icr=0;icr<mxBprsCrate;icr++) {
      StEmcModule* module = detector->module(2+icr*30);assert(module);
      StSPtrVecEmcRawHit& rawHit=module->hits();
      if(rawHit.size()<=0) {
	 printf("ss icr=%d n=%dL, ABORT BpRS for this event\n",icr,int(rawHit.size())); 
	return ;
}	
	assert(rawHit.size()>0);
        janEve.bprsCap[icr]= rawHit[10]->calibrationType();
      // tmp
      if(icr==3)    janEve.bprsCap[icr]= rawHit[32]->calibrationType();

    }
  }

  // unpack raw data and store it locally in an array
  for(Int_t m = 1; m <= 120; ++m) {
    StEmcModule* module = detector->module(m);
    //printf("MMM %s m=%p mod=%d\n",cTile4[ibp],module,m);
    if(!module) continue;
    
    StSPtrVecEmcRawHit& rawHit=module->hits();
    // printf("HHH n=%d\n",rawHit.size());
    for(UInt_t k = 0; k < rawHit.size(); ++k){
      int id;
      
      Int_t m=rawHit[k]->module();
      Int_t e=rawHit[k]->eta();
      Int_t s=abs(rawHit[k]->sub());
      float rawAdc=rawHit[k]->adc()+0.5;// return middle of the bin
      
      //Get software tower id to get DaqID
      geomB->getId(m,e,s,id);

#if 0       // tmp for Matt, to QA new mapping
      if(ibp==kBPrs ){
	int   id1=(int)mJanDbMaker->bprsReMap()->GetBinContent(id);
	printf("BPRSX %d %d %d %d\n",nInpEve-1,id1,rawHit[k]->adc(),rawHit[k]->calibrationType());
      }
      // end tmp
#endif

      assert(id>=1);
      assert(id<=mxBtow);
      janEve.rawAdcTile[ibp][id-1]=rawAdc;
      // if(id%20==7) 
      // if(ibp==kBPrs && (id>650 && id<710)) printf("unpack %s  hit=%d softid=%d, m=%d rawADC=%.1f\n",cTile4[ibp],k,id,m,rawAdc);
    }
  }// end of module loop

  janEve.tileIn[ibp]=1; // tag data block as delivered

  assert(!isMC); // verify peds for MC
}
//________________________________________________
//________________________________________________
void
StBarrelMonitorMaker::calibrateTiles(int ibp){
  // printf("ZZ %d %d\n",ibp, janEve.tileIn[ibp]);
  if(janEve.tileIn[ibp]==0) return; // no data in this event

  for(int id0=0; id0<mxBtow; id0++) {
    int   id=id0+1;
    int   crateID=0;

    int   capID=0;
    int   stat =mJanDbMaker->statTile(ibp,id);

    if(ibp==kBPrs ) {
     crateID= mJanDbMaker->bprsCrate(id);
     capID=janEve.bprsCap[crateID]; //fix capID corruption in 2008 
    }
    
    float ped     =mJanDbMaker->pedTile(ibp,id,capID);
    float rawAdc  =janEve.rawAdcTile[ibp][id0];

    // if(ibp==kBPrs) printf("ss id=%d stat=%d  rawAdc=%.1f\n", id,stat,rawAdc);
    if(ibp==kBPrs  && rawAdc>100 && rawAdc<300){
      //accumulate pedestals vs. capID similat to Tonko's on-line approach
      hTonko0->Fill(id,capID,1.);
      hTonko1->Fill(id,capID,rawAdc);
      hTonko2->Fill(id,capID,rawAdc*rawAdc);
    }

    if(!(par_calibPass & kPassPedSub)) {
      hTile[ibp]->Fill(id,rawAdc);
      if(par_bprsHisto==2) hBprs3D->Fill(id,rawAdc,capID);
    }
    
    if(par_calibPass==0) continue; // no swap info loaded in this mode

    //------ TMP ----- apply BPRS & BTOW swap only for ped-corrected ADC data
    if(ibp==kBTow) {
      id=(int)mJanDbMaker->btowReMap()->GetBinContent(id);
    } else  if(ibp==kBPrs) {
      id=(int)mJanDbMaker->bprsReMap()->GetBinContent(id);
    } else assert(1==2);

    // below  use only 'id' for indexing to pick up this swaping


    if(stat) {
      janEve.statTile[ibp][id-1]=stat; // is bad
      continue;
    } else {
      janEve.statTile[ibp][id-1]=0; // is good
    }

    float adc=rawAdc-ped;    
    janEve.adcTile[ibp][id-1]= adc;
    
    if(par_calibPass & kPassPedSub){
      hTile[ibp]->Fill(id,adc);
      if(ibp==kBPrs){ // later add alos BTOW histos per crate, fix it
	hBprsA[crateID]->Fill(adc);
	if(par_bprsHisto==2) hBprs3D->Fill(id,adc,capID);
      }
    }
    
    //..... energy calibration goes here, later
    //...

  } // end of data loop 


}

//________________________________________________
//________________________________________________
void
StBarrelMonitorMaker::populateBprsEveA(){
  
  if(janEve.tileIn[kBPrs]==0) return; // no data in this event

  for( int icr=0; icr <mxBprsCrate;icr++) {
    int   capID=janEve.bprsCap[icr];
    janBprsEveA[icr].set(capID,icr,eventID);
  }
	 
  int ibp=kBPrs;
  for(int id0=0; id0<mxBtow; id0++) {
    int id=id0+1;
    int   icr=mJanDbMaker->bprsCrate(id);
    float rawAdc  =janEve.rawAdcTile[ibp][id0];
    janBprsEveA[icr].addRawValue(id,rawAdc);
    //printf("id=%d, icr=%d \n",id,icr);
  }
  
}
    

//---------------------------------------------------
// $Log: StBarrelMonitorMaker.cxx,v $
// Revision 1.5  2012/12/12 22:05:09  fisyak
// add sys/types.h include for APPLE
//
// Revision 1.4  2009/08/25 16:17:48  fine
// fix the compilation issues under SL5_64_bits  gcc 4.3.2
//
// Revision 1.3  2009/08/25 16:08:04  fine
// fix the compilation issues under SL5_64_bits  gcc 4.3.2
//
// Revision 1.2  2009/02/04 20:33:32  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.1  2008/11/24 23:06:35  balewski
// start
//
