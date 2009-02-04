// *-- Author : Jan Balewski
// 
// $Id: MuEzSmdCalMaker.cxx,v 1.7 2009/02/04 20:33:22 ogrebeny Exp $

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <StMessMgr.h>
 
#include "MuEzSmdCalMaker.h"

#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"

#include "StMuDSTMaker/EZTREE/EztEventHeader.h"
#include "StMuDSTMaker/EZTREE/EztTrigBlob.h"
#include "StMuDSTMaker/EZTREE/EztEmcRawData.h"
#include "StMuDSTMaker/EZTREE/StTriggerDataMother.h"
//tmp
#include "StTriggerData2005.h" // tmp

#include "StEEmcUtil/database/StEEmcDb.h"
#include "StEEmcUtil/database/EEmcDbItem.h"

ClassImp(MuEzSmdCalMaker)

//________________________________________________
//________________________________________________
MuEzSmdCalMaker::MuEzSmdCalMaker( const char* self ,const char* muDstMakerName) : StMaker(self){
  mMuDstMaker = (StMuDstMaker*)GetMaker(muDstMakerName);
  assert(mMuDstMaker);

  trgAkio=0;
  nAcceptEve=nTrigEve=nCorrEve=0;
  setHList(0);
  setTrigIdFilter(0);
  setMaxCtbSum(0);
  setEZtree();
}

//________________________________________________
//________________________________________________
void MuEzSmdCalMaker::setSector(int sec){
  EEsmdCal::setSector(sec);
  TString name=GetName();
  name+="-";
  name+=sec;
  //  printf("change name to %s sec=%d\n", name.Data(),sec);
  SetName(name);
}

//___________________ _____________________________
//________________________________________________
MuEzSmdCalMaker::~MuEzSmdCalMaker(){
  delete trgAkio;
}

//___________________ _____________________________
//________________________________________________
void 
MuEzSmdCalMaker::saveHisto(TString fname){
  TString outName=fname+".hist.root";
  TFile f( outName,"recreate");
  assert(f.IsOpen());
  printf("%d histos are written  to '%s' ...\n",HList->GetEntries(),outName.Data());

  HList->Write();
  f.Close();

}
 
//________________________________________________
//________________________________________________
Int_t 
MuEzSmdCalMaker::Init(){

  assert(HList);
  eeDb = (StEEmcDb*)this->GetDataSet("StEEmcDb");
  assert(eeDb);  
  EEsmdCal::init();
  
  gMessMgr->Message("","I") <<GetName()<<"::Init() filter trigID="<<trigID<<"  maxCtbSum="<<maxCtbSum<<endm;  
  return StMaker::Init();
}

//________________________________________________
//________________________________________________
Int_t 
MuEzSmdCalMaker::InitRun(int runNo){
  if(runNo==0) {
    gMessMgr->Message("","W")<<GetName()<<"::InitRun("<<runNo<<") ??? changed to 555, it s OK for M-C - perhaps, JB"<<endm;
    runNo=555;
  }

  initRun(runNo);
 
  return kStOK;
}

//________________________________________________
//________________________________________________
Int_t 
MuEzSmdCalMaker::Finish(){
  finish(1);// do not draw
  gMessMgr->Message("","I") <<GetName()<<"::Finish()\n    inputEve="<<nInpEve<<" trigFilterEve="<<nTrigEve<<" nCorrEve="<<nCorrEve<<" nAcceptEve="<<nAcceptEve<<endm;
  return kStOK;
}

//________________________________________________
//________________________________________________
void 
MuEzSmdCalMaker::Clear(const Option_t*){
  eHead=0;
  eETow=0;
  eESmd=0;
  eTrig=0;
  //  delete trgAkio; //JAN: perhaps is a memory leak? But crashes
}

//________________________________________________
//________________________________________________
Int_t 
MuEzSmdCalMaker::Make(){
  clear();
  nInpEve++;
//  gMessMgr->Message("","D") <<GetName()<<"::Make() is called , 
//  useEZtree="<<useEZtree<<endm;
    if(useEZtree)  return MakeEZtree();
  
  return MakeRegular();
}

//________________________________________________
//________________________________________________
Int_t 
MuEzSmdCalMaker::MakeEZtree(){
  //..........  acquire EztHeader
  eHead= mMuDstMaker->muDst()->eztHeader();
  if(eHead==0) {
    gMessMgr->Message("","E") <<GetName()<<"::Make() no  EztEventHeader, skip event  "<<endm;    return kStOK;
  }

  if(nInpEve==1) eHead->print();

  if(trigID ) {// filter by triggerID on demand
    if (! mMuDstMaker->muDst()->event()->triggerIdCollection().nominal().isTrigger(trigID)) return kStOK;
  }
  nTrigEve++;
  //.... get data .....
  eETow=mMuDstMaker->muDst()->eztETow();
  eESmd=mMuDstMaker->muDst()->eztESmd();
  eTrig=mMuDstMaker->muDst()->eztTrig(); 
  //  printf("pp %p %p %p\n",eETow, eESmd, eTrig);
  if(!eETow || !eESmd || !eTrig)  return kStOK;

  //  trgAkio=new StTriggerDataMother(eTrig);
  // trgAkio->dump();
  //eETow->print(0);
  // eESmd->print(0);
  //  eHead->print();

  StMuEvent *muEve = mMuDstMaker -> muDst() -> event();
  assert(muEve);
  StEventInfo &info=muEve->eventInfo();
  int runId=info.runId();

  
  // .... process adata ......
  void *blob=eTrig->trgd->GetArray();

  StTriggerData2005 trgAkio5( (const TrgDataType2005 *)blob,runId);
  if(eETow->doTowerHeadCorruptionTest(trgAkio5.token())||
     eESmd->doMapmtHeadCorruptionTest(trgAkio5.token())
     ) {
    nCorrEve++;
    return kStOK;
  }
  
  int ctbSum=trgAkio5.ctbSum();
  if(maxCtbSum>0 && (ctbSum>maxCtbSum || ctbSum<maxCtbSum/2.))  return kStOK;
  nAcceptEve++;
  
  //  printf("\n ctbSum=%d \n",ctbSum);
  //x  hA[7]->Fill(ctbSum);


  unpackMuEzt(eETow);
  unpackMuEzt(eESmd);
    
  findSectorMip();// do real analysis
 
  return kStOK;
} 

//________________________________________________
//________________________________________________
Int_t 
MuEzSmdCalMaker::MakeRegular(){
  gMessMgr->Message("","D") <<GetName()<<"::MakeRegular() is called , useEZtree="<<useEZtree<<endm;
  
#if 0
  vector<unsigned int> trgL=mMuDstMaker->muDst()->event()->triggerIdCollection().nominal().triggerIds();
  printf("trigL len=%d\n",trgL.size());
  int ii;
  for(ii=0;ii<trgL.size();ii++) printf("ii=%d trigID=%d\n",ii,trgL[ii]);
#endif

  if(trigID ) {// filter by triggerID on demand
    if (! mMuDstMaker->muDst()->event()->triggerIdCollection().nominal().isTrigger(trigID)) return kStOK;
  }
  nTrigEve++;

  // M-C case, when muDst data are zero supressed 
  memset(killT,false,sizeof(killT));// default is working if zero suppressed


  unpackMuTails();
  unpackMuSmd();
  nAcceptEve++;
  
  // printf("UU (bool) killThr T=%d P=%d Q=%d R=%d\n",killT[0][3][14],killT[1][3][14],killT[2][3][14],killT[3][3][14]);
    
  findSectorMip();// do real analysis
 
  return kStOK;
} 


//________________________________________________
//________________________________________________
void
MuEzSmdCalMaker::tileReMap( int &iT,int &sec , char &sub , int &eta){
  assert(1==2); //use only by expert - disabled, JB
  if(sec==1 ) {
    if (iT==99 ) {     ;
    } else if( iT==2 &&  sub=='A' && eta==11 ) { // QA11<==>QB2 
      iT=2;  sub='B'; eta=2;
    } else if( iT==2 &&  sub=='B' && eta==2 ) {  
      iT=2;  sub='A'; eta=11;
    } else if( iT==0 &&  sub=='A' && eta==4 ) {  //TA4 <==> TA5
      iT=0;  sub='A'; eta=5;
    } else if( iT==0 &&  sub=='A' && eta==5 ) { 
      iT=0;  sub='A'; eta=4;
    }
  }
  return;
}

//________________________________________________
//________________________________________________
int
MuEzSmdCalMaker:: stripReMap(const  EEmcDbItem  *x){
  assert(1==2); //use only by expert - disabled, JB
  int str=x->strip;
  if(x->sec==8 && x->plane=='V' ) {
    switch( x->strip) {
      // connector & pair swap
    case 209:  str=216; break;
    case 210:  str=215; break;
    case 211:  str=214; break;
    case 212:  str=213; break;
    case 213:  str=212; break;
    case 214:  str=211; break;
    case 215:  str=210; break;
    case 216:  str=280; break;	  
    case 280:  str=209; break;

      // another connector
    case 265:  str=272; break;	  
    case 266:  str=271; break;	  
    case 267:  str=270; break;	  
    case 268:  str=269; break;	  
    case 269:  str=268; break;	  
    case 270:  str=267; break;	  
    case 271:  str=266; break;	  
    case 272:  str=265; break;	        
    }
  }

  return str;
}

//________________________________________________
//________________________________________________
void
MuEzSmdCalMaker::unpackMuEzt(EztEmcRawData  *eRaw){
  int  n1=0,n2=0,n3=0;

  if(eRaw==0) return ; // no data block
  int icr;
  for(icr=0;icr<eRaw->getNBlocks();icr++) {
    if(eRaw->isCrateVoid(icr)) continue;
    assert(!eRaw ->getCorruption(icr)); // zero-tolerance

    int crateID=eRaw->getCrateID(icr);
    int chan;
    const UShort_t* data=eRaw->data(icr);
    for(chan=0;chan<eRaw->sizeData(icr);chan++) {
      const  EEmcDbItem  *x=eeDb->getByCrate(crateID,chan);
      if(x==0) continue; 
      if(x->sec!=sectID && crateID>6 ) break;// assumes crates do not cross sectors for non-towers, faster code
      if(x->fail  ) continue; // drop broken channels
      if(x->stat & killStat) continue; // drop masked channels 
     // accept this hit
      float  rawAdc=data[chan];  
      float  adc=rawAdc-x->ped; 
      
      if(x->isSMD()) {
	//........................  SMD  U or V .......
	if(rawAdc>x->thr)  n3++;
	int iuv=x->plane-'U';
	int istr=x->strip -1;

	// istr=stripReMap(x)-1; //<<====  S W A P S, not use it

	assert(iuv>=0 && iuv<MaxSmdPlains);
	assert(istr>=0 && istr<MaxSmdStrips);
	smdAdc[iuv][istr]=adc;      
	if(x->gain<=0)continue; // drop channels w/o gains

	smdEne[iuv][istr]=adc/x->gain; 
	// if(rawAdc>x->thr) 	printf("%s %f %f \n",x->name,smdEne[iuv][istr],x->gain);
      } else { 
	//............................... tower/pre/post crates
	int iT=-1;// store T,P,Q,R depending on 'iT'
	if(x->name[2]=='T'){
	  iT=0;
	} else{
	  iT=x->name[2]-'P'+1;
	}
	assert(iT>=0 && iT<mxTile);
	bool aboveThr=rawAdc>x->thr;
	if(iT==1 || iT==2) {
	  if( adc<=thrMipPresAdc ||  adc>(thrMipPresAdc+100) ) aboveThr=false;
	} else if (iT==3) {
	  if( adc<=(thrMipPresAdc/2.) || adc>(thrMipPresAdc/2.+100) ) aboveThr=false;
	}

	//	if(iT==1 || iT==3) continue; // mask alomst all
	int sec=x->sec;
	char sub=x->sub;
	int eta=x->eta;  

	// tileReMap( iT,sec,sub,eta);  //<<====  S W A P S , not use it
	
	int iphi=(sec-1)*MaxSubSec+(sub-'A');
	int ieta=eta-1;
	assert(iphi>=0 && iphi<MaxPhiBins);
	assert(ieta>=0 && ieta<MaxEtaBins);   
	tileAdc[iT][ieta][iphi]=adc;
	tileThr[iT][ieta][iphi]=aboveThr;
	killT[iT][ieta][iphi]=false; // it is alive
	if(aboveThr) { 
	  if(iT==0)  
	    n1++;
	  else
	    n2++;
	}
	
	if(x->gain<=0) continue;// drop channels w/o gains
	tileEne[iT][ieta][iphi]=adc/x->gain;
      }
    } // end of loop over one data block
  }// end of loop over blocks
  // printf("%s-->nTow=%d nPQR=%d nSMD=%d\n",GetName(),n1,n2,n3);
}

//________________________________________________
//________________________________________________
void
MuEzSmdCalMaker::unpackMuTails(){

  // Access to muDst .......................
  StMuEmcCollection* emc = mMuDstMaker->muDst()->muEmcCollection();
  if (!emc) {
    gMessMgr->Warning() <<"No EMC data for this event"<<endm;    return;
  }
  
  int i ;
  //printf("aaa %d %d \n",eeDb->mfirstSecID,eeDb->mlastSecID);
  //.........................  T O W E R S .....................
  for (i=0; i< emc->getNEndcapTowerADC(); i++) {
    int sec,eta,sub,rawAdc; //muDst  ranges:sec:1-12, sub:1-5, eta:1-12
    emc->getEndcapTowerADC(i,rawAdc,sec,sub,eta);
    if(sec!=sectID) continue;
    //Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method
    const EEmcDbItem *x=eeDb->getTile(sec,'A'+sub-1,eta,'T');
    assert(x); // it should never happened for muDst
    if(x->fail || x->stat &  killStat) {// drop not working channels
      killTail(x,0); 
      continue; 
    }
    float adc=rawAdc-x->ped; // ped subtracted ADC

    //W A R N !! HARDCODDED SF correction
    // adc/=0.8; //replace SF of 5% used in M_C generation by 4% believed to be true as of October 2005

    bool aboveThr=rawAdc>x->thr;

    int iT=0; // towers
    recordTail( x,adc,aboveThr,iT);

  }// end of loop over towers

 //.........................  P R E - P O S T .....................  
  int pNh= emc->getNEndcapPrsHits();
  for (i=0; i < pNh; i++) {
    int pre, sec,eta,sub;
    //muDst  ranges: sec:1-12, sub:1-5, eta:1-12 ,pre:1-3==>pre1/pre2/post
    StMuEmcHit *hit=emc->getEndcapPrsHit(i,sec,sub,eta,pre);
    float rawAdc=hit->getAdc();
    //Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method
    const EEmcDbItem *x=eeDb-> getTile(sec,sub-1+'A', eta, pre-1+'P');
    assert(x); // it should never happened for muDst
    if(sec!=sectID) continue;
    int iT=pre;// store P,Q,R depending on 'iT'
    assert(iT>=0 && iT<mxTile);
    if(x->fail || x->stat &  killStat) {// drop not working channels
      killTail(x,iT); 
      continue; 
    }

    float adc=rawAdc-x->ped; // ped subtracted ADC

    bool aboveThr=rawAdc > x->thr;
    //pre1,2
    if( adc<=thrMipPresAdc ||  adc>(thrMipPresAdc+100) ) aboveThr=false;
    if(iT==33){ //post, tmp for M-C ADC is 2x larger
      if( adc<=(thrMipPresAdc/2.) || adc>(thrMipPresAdc/2.+100) ) aboveThr=false;
    }

    recordTail( x,adc,aboveThr,iT);
  }

  return ;
}

//________________________________________________
//________________________________________________
void
MuEzSmdCalMaker::unpackMuSmd(){
  
  // Access to muDst .......................
  StMuEmcCollection* emc = mMuDstMaker->muDst()->muEmcCollection();
  if (!emc) {
    gMessMgr->Warning() <<"No EMC data for this event"<<endm;    return;
  }
  
  //.......................  S M D ................................
  char uv='U';
  for(uv='U'; uv <='V'; uv++) {
    int sec,strip;
    int nh= emc->getNEndcapSmdHits(uv);
    int i;
    for (i=0; i < nh; i++) {
      StMuEmcHit *hit=emc->getEndcapSmdHit(uv,i,sec,strip);
      float rawAdc=hit->getAdc();
      const EEmcDbItem *x=eeDb->getByStrip(sec,uv,strip);
      assert(x); // it should never happened for muDst
      if(sec!=sectID) continue;
      
      if(x->fail ) continue;  // drop broken channels
      if(x->stat &  killStat) continue; // drop not working channels
      float adc=rawAdc-x->ped; // ped subtracted ADC
      // x->print(); printf("adc=%f\n",adc);
      int iuv=x->plane-'U';
      int istr=x->strip -1;
      
      assert(iuv>=0 && iuv<MaxSmdPlains);
      assert(istr>=0 && istr<MaxSmdStrips);
      smdAdc[iuv][istr]=adc;      
      if(x->gain<=0)continue; // drop channels w/o gains
      
      smdEne[iuv][istr]=adc/x->gain; 
     }
  }
}

//________________________________________________
//________________________________________________
void 
MuEzSmdCalMaker::recordTail( const  EEmcDbItem  *x, float  adc, bool aboveThr,	int iT) {
  int sec=x->sec;
  char sub=x->sub;
  int eta=x->eta;  

#if 0
  if(strstr(x->name,"C10")) {
    printf("\nadc=%f iT=%d abTthr=%d \n",adc,iT,aboveThr);  x->print(); 
  }
#endif

  // tileReMap( iT,sec,sub,eta);  //<<====  S W A P S , not use it
  
  int iphi=(sec-1)*MaxSubSec+(sub-'A');
  int ieta=eta-1;
  assert(iphi>=0 && iphi<MaxPhiBins);
  assert(ieta>=0 && ieta<MaxEtaBins);   
  tileAdc[iT][ieta][iphi]=adc;
  tileThr[iT][ieta][iphi]=aboveThr;
  killT[iT][ieta][iphi]=false; // it is alive

  if(x->gain<=0) return;// drop channels w/o gains
  tileEne[iT][ieta][iphi]=adc/x->gain;
}


//________________________________________________
//________________________________________________
void 
MuEzSmdCalMaker::killTail( const  EEmcDbItem  *x, int iT) {
  int sec=x->sec;
  char sub=x->sub;
  int eta=x->eta;  
 
  int iphi=(sec-1)*MaxSubSec+(sub-'A');
  int ieta=eta-1;
  assert(iphi>=0 && iphi<MaxPhiBins);
  assert(ieta>=0 && ieta<MaxEtaBins);   
  killT[iT][ieta][iphi]=true; // is dead
}

//---------------------------------------------------
// $Log: MuEzSmdCalMaker.cxx,v $
// Revision 1.7  2009/02/04 20:33:22  ogrebeny
// Moved the EEMC database functionality from StEEmcDbMaker to StEEmcUtil/database. See ticket http://www.star.bnl.gov/rt2/Ticket/Display.html?id=1388
//
// Revision 1.6  2007/08/21 13:10:04  balewski
// final, used in 2006 offline calibration by soScott
//
//
// VS: ----------------------------------------------------------------------
//
// Revision 1.5  2006/09/15 01:45:34  balewski
// add run# to trg-data unpaker
//
// Revision 1.4  2005/09/29 13:57:57  balewski
// after SMD gains were rescaled
//
// Revision 1.3  2005/08/09 18:46:31  balewski
// after smd calib in 2005
//
// Revision 1.2  2005/05/04 17:00:32  balewski
// tuned for MIP detection in CuCu200
//
// Revision 1.1  2005/03/11 15:44:25  balewski
// works with muEzt, cucu200
//
  
