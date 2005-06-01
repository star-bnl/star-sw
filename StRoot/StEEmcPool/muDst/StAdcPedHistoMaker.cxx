// code 'as is' from Murad, used to generate AuAu200 pedestals from regualr muDst

//std
#include <map>
#include <string>
#include <algorithm>
#include <iostream>

//some libraries
#include "StarClassLibrary/StPhysicalHelixD.hh"
#include "StarClassLibrary/StThreeVectorD.hh"
#include "StarClassLibrary/StThreeVectorF.hh"


//StEvent
#include "StEvent/StEmcDetector.h"
#include "StEvent/StEmcCollection.h"
#include "StEvent/StEmcRawHit.h"
#include "StEvent/StEmcModule.h"

//StEmc
#include "StEmcClusterCollection.h"
#include "StEmcPoint.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include "StEmcUtil/others/emcDetectorName.h"
#include "StEmcADCtoEMaker/StBemcData.h"
#include "StEmcADCtoEMaker/StEmcADCtoEMaker.h"
#include "StEmcRawMaker/defines.h"
#include "StEmcRawMaker/StBemcRaw.h"

//root
#include "TTree.h"
#include "TFriendElement.h"
#include "TFile.h"

//StMuDst
#include "StMuDSTMaker/COMMON/StMuDstMaker.h"
#include "StMuDSTMaker/COMMON/StMuDst.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTriggerIdCollection.h"
#include "StMuDSTMaker/COMMON/StMuTrack.h"

//StTrackMaker
#include "StAdcPedHistoMaker.h"

//Endcap
#include "StEEmcDbMaker/StEEmcDbMaker.h"
#include "StEEmcDbMaker/EEmcDbItem.h"
#include "StEEmcDbMaker/cstructs/eemcConstDB.hh"
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
ClassImp(StAdcPedHistoMaker)

//------------
StAdcPedHistoMaker::StAdcPedHistoMaker(const char* name, StMuDstMaker* uDstMaker)
    : StMaker(name), mDstMaker(uDstMaker)
{
  HList=0; 
  myTable = new StBemcTables();
  cout <<"StAdcPedHistoMaker::StAdcPedHistoMaker()"<<endl;
}


StAdcPedHistoMaker::~StAdcPedHistoMaker()
{
  cout <<"StAdcPedHistoMaker::~StAdcPedHistoMaker()"<<endl;
}

Int_t StAdcPedHistoMaker::Init()
{

  mEeGeom=new EEmcGeomSimple();
  assert(mEeGeom);
  mEeDb = (StEEmcDbMaker*)GetMaker("eemcDb");
  assert(mEeDb); // eemcDB must be in the chain, fix it
  SetTrigIdFilter(0);
  return kStOk;
}

Int_t StAdcPedHistoMaker::InitRun(int runNo){

  printf("************** I'm here *************************\n");

  assert(mEeDb);
  // ...................  histo for individual pixels ...
  int i,k=0;
  for(i=0;i<EEindexMax;i++) {
    const  EEmcDbItem *x=mEeDb->getByIndex(i);
    if(x==0) continue;
    // initialize histos only for pixels acquired from DB
    k++;
    char  tt1[100],tt2[200];
    sprintf(tt1,"a%s",x->name);
    sprintf(tt2,"ADC for %s,  cr/chan=%3.3d/%3.3d,  tube=%s; ADC",x->name,
            x->crate,x->chan,x->tube);
    TH1F* h=new TH1F(tt1,tt2,500,-20.5,489.5);
    hPix[i]=h;
    HList->Add(h);
  }
  return kStOk;
}

Int_t StAdcPedHistoMaker::Make()
{
  HistoAna();

  return kStOk;
}

Int_t StAdcPedHistoMaker::Finish()
{
  return kStOk;
}

//------------------------------------
//------------------------------------
void StAdcPedHistoMaker::HistoAna(){

  StMuEvent *event = mDstMaker -> muDst() -> event();

  StMuTriggerIdCollection tic = event -> triggerIdCollection();
  StTriggerId l1trig = tic.l1();

  //--------------------

  StMuEmcCollection* emc = mDstMaker->muDst()->muEmcCollection();
  assert(emc);

 //.......................... T O W E R S .....................
  const int nEndcapTowers=720;
  for(int i=0;i<nEndcapTowers;i++){
    int sec,eta,sub,val; //muDst  ranges:sec:1-12, sub:1-5, eta:1-12
    emc->getEndcapTowerADC(i,val,sec,sub,eta);
    const EEmcDbItem *x=mEeDb -> getTile(sec,'A'+sub-1,eta,'T');
    if(x==0) continue;
    if( trigID && !l1trig.isTrigger(trigID)) continue;
    hPix[x->key]->Fill(val);
  }
  
  //.........................  P R E - P O S T .....................  
  int pNh= emc->getNEndcapPrsHits();
  for (int i=0; i < pNh; i++) {
    int pre, sec, eta, sub;
    //muDst  ranges: sec:1-12, sub:1-5, eta:1-12 ,pre:1-3==>pre1/pre2/post
    StMuEmcHit *hit=emc->getEndcapPrsHit(i,sec,sub,eta,pre);
    float val=hit->getAdc();
    //Db ranges: sec=1-12,sub=A-E,eta=1-12,type=T,P-R ; slow method
    const EEmcDbItem *x=mEeDb -> getTile(sec,sub-1+'A', eta, pre-1+'P');
    if(x==0) continue;
    if( trigID && !l1trig.isTrigger(trigID)) continue;
    hPix[x->key]->Fill(val);
  }

  //........................... S M D ................................
  char uv='U';
  for(uv='U'; uv <= 'V'; uv++) {
    int sec, strip;
    int nh= emc->getNEndcapSmdHits(uv);
    for (int i=0; i < nh; i++) {
      StMuEmcHit *hit=emc->getEndcapSmdHit(uv,i,sec,strip);
      float val=hit->getAdc();
      const EEmcDbItem *x=mEeDb -> getByStrip(sec,uv,strip);
      assert(x); // it should never happened for muDst
      if( trigID && !l1trig.isTrigger(trigID)) continue;
      hPix[x->key]->Fill(val);
    }
  }

}
//------------------------------------
//------------------------------------
void StAdcPedHistoMaker::Clear( Option_t *opts )
{
  return;
}

