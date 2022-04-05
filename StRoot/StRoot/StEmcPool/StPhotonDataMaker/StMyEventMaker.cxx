#include <TROOT.h>
#include <TTree.h>
#include <TH2F.h>
#include <TVector3.h>
#include <TFile.h>
#include <assert.h>

#include <StEvent.h>
#include <StEventTypes.h>

#include <StMuDSTMaker/COMMON/StMuTrack.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuTriggerIdCollection.h>
#include <StMuDSTMaker/COMMON/StMuPrimaryVertex.h>

#include <StEmcUtil/geometry/StEmcGeom.h>
#include <StEmcUtil/projection/StEmcPosition.h>
#include <StEmcUtil/database/StBemcTables.h>
#include <StEmcUtil/others/emcDetectorName.h>
#include <StEmcRawMaker/defines.h>
#include <StEvent/StEnumerations.h>
#include <St_db_Maker/St_db_Maker.h>
#include <StEmcADCtoEMaker/StEmcADCtoEMaker.h>
#include <StDetectorDbMaker/StDetectorDbTriggerID.h>

//MyEvent
#include <StEmcPool/StPhotonCommon/MyEvent.h>
#include <StEmcPool/StPhotonCommon/MyPoint.h>
#include <StEmcPool/StPhotonCommon/MyMcTrack.h>

#include "StMyEventMaker.h"

ClassImp(StMyEventMaker)
 
  StMyEventMaker::StMyEventMaker(const char *name,const char *outputFile,
			       const char *type,const char *coll,Bool_t debug):StMaker(name)
{
  mFileName=outputFile;

  if(strcmp(type,"real")!=0){
    cout<<"not running on real data??"<<endl;
    assert(0);
  }
 
  mDAU=kFALSE;
  mPP04=kFALSE;
  mPP05=kFALSE;
  mAUAU200=kFALSE;
  if(strcmp(coll,"dAu")==0){
    mDAU=kTRUE;
    mTrig[0]=2001;
    mTrig[1]=2003;
    mTrig[2]=2201;
    mTrig[3]=2202;
  }
  if(strcmp(coll,"pp04")==0){
    mPP04=kTRUE;
    mTrig[0]=45010;
    mTrig[1]=10;
    mTrig[2]=45201;
    mTrig[3]=45202;
  }
  if(strcmp(coll,"pp05")==0){
    mPP05=kTRUE;
    mTrig[0]=96011;
    mTrig[1]=-10000;
    mTrig[2]=96201;
    mTrig[3]=96211;
  }
  if(strcmp(coll,"auau200")==0){
    mAUAU200=kTRUE;
    mTrig[0]=15003;//minbias r<5023099 
    mTrig[1]=15007;//minbias r>5023099 
    mTrig[2]=15203;//bemc-ht-13
    mTrig[3]=-10000;
  }

  mDebug=debug;
  
  mBemcTables=new StBemcTables();
  mEvent=new MyEvent();
}

StMyEventMaker::~StMyEventMaker()
{
  //destructor, leave this!!
}

Int_t StMyEventMaker::Init()
{

  h_EvSum=new TH1F("h_EvSum","see code for details",22,-1.5,20.5);
  
  mEventTree=new TTree("mEventTree","tree with my event structure");
  mEventTree->Branch("branch","MyEvent",&mEvent);

  mN=0;
  mRunId=0;
  mRunPrev=0;
  mEventId=0;
  mTrigger=0;
  mDate=0;
  mTime=0;
  mPs_mb=0;
  mPs_mb2=0;
  mPs_ht1=0;
  mPs_ht2=0; 
  
  return StMaker::Init();
}

Int_t StMyEventMaker::Make()
{
  mN++;
  if(mDebug) cout<<endl<<"#### processing event "<<mN<<endl<<endl;
  h_EvSum->Fill(1);

  StEmcGeom *bemcGeom=StEmcGeom::getEmcGeom("bemc");

  StMuDst* mu=(StMuDst*)GetInputDS("MuDst"); 
  if(!mu){
    if(mDebug) cout << "********* no StMuDst*"<<endl;
    return kStOK;
  }

  if(mDebug) cout<<"number of primverts: "<<mu->numberOfPrimaryVertices()<<endl;

  StMuEvent *muEvent=(StMuEvent*)mu->event();
  if(!muEvent){
    if(mDebug) cout << "********* no StMuEvent*"<<endl;
    return kStOK;
  }
  
  mRunPrev=mRunId;
  mRunId=muEvent->runId();
  mEventId=muEvent->eventId();
  if(mDebug) cout<<"starting run: "<<mRunId<<" and event: "<<mEventId<<endl;
   
  if(mAdcMaker->isCorrupted()){
    if(mDebug) cout<<"event corrupted!"<<endl;
    h_EvSum->Fill(-1);
    return kStOK;
  }

  if(mRunId!=mRunPrev){
    StDetectorDbTriggerID& DetDbTrigId = *(StDetectorDbTriggerID::instance());  
    for(UInt_t i=0;i<DetDbTrigId.getL0NumRows();i++){
      if(mDebug) cout<<"prescales: "<<DetDbTrigId.getPsL0(i)<<endl<<endl;
      if(DetDbTrigId.getL0OfflineTrgId(i)==mTrig[0])
	mPs_mb=DetDbTrigId.getPsL0(i);
      if(DetDbTrigId.getL0OfflineTrgId(i)==mTrig[1])
	mPs_mb2=DetDbTrigId.getPsL0(i);
      if(DetDbTrigId.getL0OfflineTrgId(i)==mTrig[2])
	mPs_ht1=DetDbTrigId.getPsL0(i);
      if(DetDbTrigId.getL0OfflineTrgId(i)==mTrig[3])
	mPs_ht2=DetDbTrigId.getPsL0(i);
    }

    mBemcTables->loadTables((StMaker*)this);
  }
  
  mDate=(Int_t)mDbMaker->GetDateTime().GetDate();
  mTime=(Int_t)mDbMaker->GetDateTime().GetTime();

  //get trigger info
  Float_t mips=-1.;
  Float_t zdcEast=0.;
  Float_t zdcWest=0.;
  Float_t zdcVertex=0.;
  Float_t bbcEast=0.;
  Float_t bbcWest=0.;
  StCtbTriggerDetector &CTB=muEvent->ctbTriggerDetector();
  mips=CTB.mips(0);
  StZdcTriggerDetector &ZDC=muEvent->zdcTriggerDetector();
  zdcEast=ZDC.adcSum(east);
  zdcWest=ZDC.adcSum(west);
  zdcVertex=ZDC.vertexZ();
  StBbcTriggerDetector &BBC=muEvent->bbcTriggerDetector(); 
  bbcEast=BBC.adcSumEastLarge();
  bbcWest=BBC.adcSumWestLarge();

  // get BBC vertex
  int west=BBC.tdcEarliestWest();
  int east=BBC.tdcEarliestEast();
  int diff=west-east;
  float BBCVertexZ = 10.77 + 2.82*diff; // z is now in cm 

  //from Sasha:
  // BBCVertexZ = a + b*diff ,  with a = 10.77 and b = 2.82
  // used to be BBCVertexZ = 2.0*diff ....

  //----- CREATE MyEvent -----
  mEvent=new MyEvent(mRunId,mEventId,mDate,mTime);
  if(!mEvent){
    if(mDebug) cout<<"couldn't create MyEvent*"<<endl;
    return kStErr;
  }

  //get current vertex from mudst?
  StThreeVectorF vPos;
  StMuPrimaryVertex *muVert=mu->primaryVertex();
  if(muVert){
    vPos=muVert->position();
  }
  else if(mAUAU200){
    vPos.setX(muEvent->primaryVertexPosition().x());
    vPos.setY(muEvent->primaryVertexPosition().y());
    vPos.setZ(muEvent->primaryVertexPosition().z());
  }
  else{
    vPos.setX(0);
    vPos.setY(0);
    vPos.setZ(0);
  }
  
  //a priori reject for AuAu:
  if(mAUAU200){
    if(fabs(vPos.z())>30.){
      if(mDebug) cout<<"vertex out of range"<<endl;
      return kStOK;
    }
  }

  //get trig per event
  mTrigger=0;
  if(muEvent->triggerIdCollection().nominal().isTrigger(mTrig[0])||
     muEvent->triggerIdCollection().nominal().isTrigger(mTrig[1])) mTrigger+=1;
  if(muEvent->triggerIdCollection().nominal().isTrigger(mTrig[2])) mTrigger+=2;
  if(muEvent->triggerIdCollection().nominal().isTrigger(mTrig[3])) mTrigger+=4;
  if(muEvent->triggerIdCollection().nominal().isTrigger(2002)) mTrigger+=8;
  
  if(mTrigger==0){
    if(mDebug) cout<<"nothing more useless than an event with no trigger!"<<endl;
    return kStOK;
  }

  StEmcCollection *emcCol=(StEmcCollection*)mu->emcCollection();
  if(!emcCol){
    if(mDebug) cout<<"no StEmcCollection"<<endl;
      h_EvSum->Fill(8);
      return kStOK;
  } 
  StEmcDetector *emcDet=(StEmcDetector*)emcCol->detector(kBarrelEmcTowerId);
  if(!emcDet)
    {
     if(mDebug) cout<<"no emcDet!!"<<endl;
     h_EvSum->Fill(9);
     return kStOK; 
    }
  //get status of trigger tower
  Int_t hiTowId=-1;
  Int_t hiTowAdc=-1;
  Int_t hiTowStatus=-1;
  Float_t hiTowEnergy=0.;
  if(mTrigger>1){
    for(UInt_t m=1;m<=emcDet->numberOfModules();m++){
      StEmcModule *emcMod=emcDet->module(m);
      if(!emcMod){
	if(mDebug) cout<<"no emcMod for module "<<m<<endl;
	continue;
      }
      StSPtrVecEmcRawHit& modHits=emcMod->hits();
      for(UInt_t h=0;h<modHits.size();h++){
	Int_t adc=modHits[h]->adc();
	Float_t nrg=modHits[h]->energy();
	if(adc>hiTowAdc && nrg>0.){
	  hiTowAdc=adc;
	  hiTowEnergy=nrg;
	  bemcGeom->getId(m,modHits[h]->eta(),modHits[h]->sub(),hiTowId);
	}
      }
    }
    //get status
    if(hiTowId>0) mBemcTables->getStatus(BTOW,hiTowId,hiTowStatus);
  }
  
  Int_t nPrimTracks=0;
  Int_t nGlobalTracks=0;  
  Int_t nGoodPrimaries=0;
  Int_t nGoodPrimBarrel=0;
  Int_t nGoodGlobals=0;
  Float_t TpcPt=0.;
  Float_t TpcPtBarrelWest=0.;
  //using current vertex -> best??
  StMuTrack *ptrack;
  if(!mAUAU200){
    nPrimTracks=mu->primaryTracks()->GetEntries(); 
    for(Int_t i=0;i<nPrimTracks;i++){
      ptrack=(StMuTrack*)mu->primaryTracks()->UncheckedAt(i);
      if(ptrack->bad()) continue;
      if(ptrack->dca().mag()<3. && ptrack->nHitsFit()>15){
	nGoodPrimaries++;
	TpcPt+=ptrack->momentum().mag();
	if(fabs(ptrack->eta() - 0.5)<0.5){
	  nGoodPrimBarrel++;
	  TpcPtBarrelWest+=ptrack->momentum().mag();
	}
      }      
    }
    StMuTrack *gtrack;
    nGlobalTracks=mu->globalTracks()->GetEntries();
    for(Int_t i=0;i<nGlobalTracks;i++){
      gtrack=(StMuTrack*)mu->globalTracks()->UncheckedAt(i);
      if(gtrack->bad()) continue;
      if(gtrack->dca().mag()<3. && gtrack->nHitsFit()>15)
	nGoodGlobals++;
    }
  }
  //refmult
  Int_t ftpcRefMultTracks=muEvent->refMultFtpcEast();
  if(mAUAU200) ftpcRefMultTracks=muEvent->refMult();
  if(mDebug) cout<<"event has refMult: "<<ftpcRefMultTracks<<endl;

  
  //start reco tree here:
  Float_t EnergyNeutral=0.;

  StEmcPoint *pt=0;
  StSPtrVecEmcPoint& barrelPoints=emcCol->barrelPoints();
  for(UInt_t i=0;i<barrelPoints.size();i++){
    
    pt=(StEmcPoint*)barrelPoints[i];
    Float_t dist=9999.;
    if(!calcDistanceTrackToPoint(pt,mu,dist)){
      cout<<"wrong bfield or no event pointer (shouldn't see this)"<<endl;
    }
    MyPoint *mypt=new MyPoint();

    if(pt->position().pseudoRapidity()>0.0)//only WEST
      EnergyNeutral+=pt->energy();
    
    StPtrVecEmcCluster& etaClus=pt->cluster(kBarrelSmdEtaStripId);
    StPtrVecEmcCluster& phiClus=pt->cluster(kBarrelSmdPhiStripId);
    Int_t nEtaHits=0;
    Int_t nPhiHits=0;
    Float_t etaEnergy=0.;
    Float_t phiEnergy=0.;
    Float_t etaWidth=0.;
    Float_t phiWidth=0.;
    if(etaClus.size()>0){
      StEmcCluster *clE=(StEmcCluster*)etaClus[0];
      StPtrVecEmcRawHit& hE=clE->hit();
      nEtaHits=hE.size();
      etaEnergy=clE->energy();
      etaWidth=clE->sigmaEta();
    }
    if(phiClus.size()>0){
      StEmcCluster *clP=(StEmcCluster*)phiClus[0];
      StPtrVecEmcRawHit& hP=clP->hit();
      nPhiHits=hP.size();
      phiEnergy=clP->energy();
      phiWidth=clP->sigmaPhi();
    }

    if( mAUAU200 && (nEtaHits<1||nPhiHits<1) ){
      delete mypt;
      continue;
    }
    
    mypt->setEnergy(pt->energy());
    mypt->setQuality((Int_t)pt->quality());
    mypt->setPosition(pt->position().x(),pt->position().y(),pt->position().z());
    mypt->setDistanceToTrack(dist);
    mypt->setHitsEta(nEtaHits);
    mypt->setWidthEta(etaWidth);
    mypt->setEnergyEta(etaEnergy);
    mypt->setHitsPhi(nPhiHits);
    mypt->setWidthPhi(phiWidth);
    mypt->setEnergyPhi(phiEnergy);
    
    mEvent->addPoint(mypt);
    
    delete mypt;
  }
  
  //fill MyEvent
  mEvent->setPrescale(0,mPs_mb);
  mEvent->setPrescale(1,mPs_mb2);
  mEvent->setPrescale(2,mPs_ht1);
  mEvent->setPrescale(3,mPs_ht2);
  mEvent->setHighTowerAdc(hiTowAdc);
  mEvent->setHighTowerId(hiTowId);
  mEvent->setHighTowerStatus(hiTowStatus);
  mEvent->setHighTowerEnergy(hiTowEnergy);
  mEvent->setVertex(vPos.x(),vPos.y(),vPos.z());
  mEvent->setTrigger(mTrigger);
  mEvent->setGoodPrimaries(nGoodPrimaries);
  mEvent->setGoodPrimBarrel(nGoodPrimBarrel);
  mEvent->setGoodGlobals(nGoodGlobals);
  mEvent->setRefMult(ftpcRefMultTracks);
  mEvent->setEnergyInBarrel(EnergyNeutral);
  mEvent->setMomentumInTpc(TpcPt);
  mEvent->setMomentumInTpcWest(TpcPtBarrelWest);
  mEvent->setCtbSum(mips);
  mEvent->setBbcSumEast(bbcEast);
  mEvent->setBbcSumWest(bbcWest);
  mEvent->setZdcSumEast(zdcEast);
  mEvent->setZdcSumWest(zdcWest);
  mEvent->setZdcVertexZ(zdcVertex);
  mEvent->setBbcVertexZ(BBCVertexZ);

  mEventTree->Fill();

  return kStOK;
}

Int_t StMyEventMaker::Finish()
{
  saveHistograms();
  return kStOK;
}

void StMyEventMaker::saveHistograms()
{
  if(mDebug) cout<<"************ saving histograms ****************"<<endl;
  TFile *hfile=(TFile*)gROOT->FindObject(mFileName);
  if(hfile) hfile->Close();
  hfile=new TFile(mFileName,"RECREATE");
 
  h_EvSum->Write();
  mEventTree->Write();

  hfile->Close();
}

Bool_t StMyEventMaker::calcDistanceTrackToPoint(StEmcPoint *point,StMuDst *currMuDst,Float_t &distanceToTrack)
{
  StMuEvent *muEv=(StMuEvent*)currMuDst->event();
  if(!muEv) return kFALSE;
  Double_t bFld=muEv->magneticField()/10.;
  if(mDebug) cout<<"summary()->magneticField()="<<bFld<<"(Tesla)"<<endl;
  
  if(fabs(bFld)<0.01){
    if(mDebug) cout<<"wrong/no BField!"<<endl;
    return kFALSE;
  }

  StEmcPosition* emcPosition = new StEmcPosition();
  StThreeVectorD pos,mom;
  distanceToTrack=999.;
  StMuTrack *track;
  
  Int_t ntracks=currMuDst->primaryTracks()->GetEntries();
  for(Int_t i=0;i<ntracks;i++){
    track=(StMuTrack*)currMuDst->primaryTracks()->UncheckedAt(i);
    if(track->bad()) continue;
    //if(track->momentum().mag()<.5) continue;//E=sqrt(p^2+m^2) -> p=sqrt(E^2-m^2)  protons???
    StPhysicalHelixD helix = track->outerHelix();
    Bool_t projOK=emcPosition->projTrack(&pos,&mom,&helix,bFld,230.705,1);
    if(!projOK) continue;
    Float_t d=(pos - point->position()).mag();//in cm
    
    if(d<distanceToTrack&&d>0.) distanceToTrack=d;
  }
  

  delete emcPosition;
  return kTRUE;
}






