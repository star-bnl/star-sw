#include <TROOT.h>
#include <TTree.h>
#include <TH2F.h>
#include <TVector3.h>
#include <TFile.h>
#include <TMath.h>
#include <assert.h>

#include <StEvent.h>
#include <StEventTypes.h>
#include <StEmcUtil/geometry/StEmcGeom.h>
#include <StEmcUtil/projection/StEmcPosition.h>
#include <StEmcUtil/database/StBemcTables.h>
#include <StEvent/StEnumerations.h>
#include <StEmcRawMaker/defines.h>
#include <StEmcADCtoEMaker/StEmcADCtoEMaker.h>

#include <StMcEvent/StMcEvent.hh>
#include <StMcEvent/StMcVertex.hh>
#include <StMcEvent/StMcTrack.hh>

//pythia record
#include <St_DataSet.h>
#include <St_DataSetIter.h>
#include <tables/St_g2t_event_Table.h>
#include <tables/St_particle_Table.h>
#include <tables/St_g2t_pythia_Table.h>

#include <StDetectorDbMaker/StDetectorDbTriggerID.h>
#include <St_db_Maker/St_db_Maker.h>

#include <StEmcTriggerMaker/StEmcTriggerMaker.h>

//MyEvent
#include <StEmcPool/StPhotonCommon/MyEvent.h>
#include <StEmcPool/StPhotonCommon/MyPoint.h>
#include <StEmcPool/StPhotonCommon/MyMcTrack.h>

#include "StPhotonMaker.h"

ClassImp(StPhotonMaker)
 
  StPhotonMaker::StPhotonMaker(const char *name,const char *outputFile,
			       const char *type,const char *coll,Bool_t debug):StMaker(name)
{
  mFileName=outputFile;

  mMc=kFALSE;
  mEmbed=kFALSE;
  mReal=kFALSE;
  mPythia=kFALSE;
  mHijing=kFALSE;
  if(strcmp(type,"mc")==0) mMc=kTRUE;
  else if(strcmp(type,"embed")==0) mEmbed=kTRUE;
  else if(strcmp(type,"pythia")==0) mPythia=kTRUE;
  else if(strcmp(type,"hijing")==0) mHijing=kTRUE;
  else mReal=kTRUE;
 
  mDAU=kFALSE;
  mPP04=kFALSE;
  mPP05=kFALSE;
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
  mDebug=debug;
  
  mBemcTables=new StBemcTables();
  mEvent=new MyEvent();
}

StPhotonMaker::~StPhotonMaker()
{
  //destructor
}

Int_t StPhotonMaker::Init()
{

  h_EvSum=new TH1F("h_EvSum","see code for details",22,-1.5,20.5);
  h_bsmdeAdc=new TH1F("h_bsmdeAdc","adc",1200,-0.5,1199.5);
  h_bsmdpAdc=new TH1F("h_bsmdpAdc","adc",1200,-0.5,1199.5);
  h_btowAdc=new TH1F("h_btowAdc","adc",1200,-0.5,1199.5);
  h_bsmdeEn=new TH1F("h_bsmdeEn","energy",55,-1.,10.);  
  h_btowEn=new TH1F("h_btowEn","energy",55,-1.,10.);
  h_btowEnVsAdc=new TH2F("h_btowEnVsAdc","h_btowEnVsAdc",1200,-0.5,1199.5,100,0.,30.);

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

Int_t StPhotonMaker::Make()
{
  mN++;
  if(mDebug) cout<<endl<<"#### processing event "<<mN<<endl<<endl;
  h_EvSum->Fill(1);

  StEmcGeom *bemcGeom=StEmcGeom::getEmcGeom("bemc");

  StEvent *event=0;
  event = (StEvent*)GetInputDS("StEvent");
  if(!event){
    h_EvSum->Fill(2);
    if(mDebug) cout << "************ StMyPhotonMaker::Make() no event pointer!! *********" << endl;
    return kStOK;
  }

  //test bbc coincidence:
  //StL0Trigger *l0trig=(StL0Trigger*)event->l0Trigger();
  //unsigned short dsmInput=l0trig->dsmInput();
  //cout<<"testing dsm: "<<dsmInput<<endl;

  
  if(!mHijing && !mMc && !mPythia && mAdcMaker->isCorrupted()){
    if(mDebug) cout<<"event corrupted!"<<endl;
    h_EvSum->Fill(-1);
    return kStOK;
  }

  mRunPrev=mRunId;
  if(!mMc){
    mRunId=event->runId();
    mEventId=event->id();
  }
  else{
    mEventId=mN;
    mRunId=0;
  }
  if(mDebug) cout<<"starting run: "<<mRunId<<" and event: "<<mEventId<<endl;
   
 
  if(mRunId!=mRunPrev&&mReal){
    StDetectorDbTriggerID& DetDbTrigId = *(StDetectorDbTriggerID::instance());  
    for(UInt_t i=0;i<DetDbTrigId.getL0NumRows();i++){
      if(mDebug) cout<<"prescales: "<<DetDbTrigId.getPsL0(i)<<endl<<endl;
      if(DetDbTrigId.getL0OfflineTrgId(i)==mTrig[0]) mPs_mb=DetDbTrigId.getPsL0(i);
      if(DetDbTrigId.getL0OfflineTrgId(i)==mTrig[1]) mPs_mb2=DetDbTrigId.getPsL0(i);
      if(DetDbTrigId.getL0OfflineTrgId(i)==mTrig[2]) mPs_ht1=DetDbTrigId.getPsL0(i);
      if(DetDbTrigId.getL0OfflineTrgId(i)==mTrig[3]) mPs_ht2=DetDbTrigId.getPsL0(i);
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
  StTriggerDetectorCollection *trigDetColl=event->triggerDetectorCollection();
  if(trigDetColl){
    StCtbTriggerDetector &CTB=trigDetColl->ctb();
    mips=CTB.mips(0);
    StZdcTriggerDetector &ZDC=trigDetColl->zdc();
    zdcEast=ZDC.adcSum(east);
    zdcWest=ZDC.adcSum(west);
    zdcVertex=ZDC.vertexZ();
    StBbcTriggerDetector &BBC=trigDetColl->bbc(); 
    bbcEast=BBC.adcSumEast();
    bbcWest=BBC.adcSumWest();
  }
  
  //----- CREATE MyEvent -----
  mEvent=new MyEvent(mRunId,mEventId,mDate,mTime);
  if(!mEvent){
    if(mDebug) cout<<"couldn't create MyEvent* !!"<<endl;
    return kStErr;
  }
  
  //extract MC data:
  StPrimaryVertex *pVert=event->primaryVertex();
  StMcVertex *mc_pVert=0;
  StMcEvent* mc_event=0;
  if(mPythia||mHijing){
    mc_event=(StMcEvent*)GetInputDS("StMcEvent");
    if(!mc_event){
      h_EvSum->Fill(4);
      if(mDebug) cout<<"StPhotonMaker::Make() no McEvent"<<endl;
      return kStOK;
    }
    mc_pVert=mc_event->primaryVertex();
    if(!mc_pVert){
      h_EvSum->Fill(5);
      if(mDebug) cout<<"StPhotonMaker::Make() no mc vertex!"<<endl;
      return kStOK;
    }
  }
  if(mMc||mEmbed){
    mc_event=(StMcEvent*)GetInputDS("StMcEvent");
    if(!mc_event){ 
      h_EvSum->Fill(4);
      if(mDebug) cout<<"StPhotonMaker::Make() no McEvent"<<endl;
      return kStOK;
    }
    mc_pVert=mc_event->primaryVertex();
    if(!mc_pVert){
      h_EvSum->Fill(5);
      if(mDebug) cout<<"StPhotonMaker::Make() no mc vertex!"<<endl;
      return kStOK;
    }
    //get generated particles:
    StPtrVecMcTrack& trcks=mc_pVert->daughters();
    Int_t nDaughters=mc_pVert->numberOfDaughters();
    if(mDebug) cout<<"number of MC part. from vertex: "<<nDaughters<<endl;
    if(nDaughters==1){
      StMcTrackIterator it=trcks.begin();
      Int_t pid=(*it)->particleDefinition()->pdgEncoding();
      if(mDebug) cout<<"daughter pid: "<<pid<<endl;
      // pid=111 -> pi0; pid=221 -> eta; pid=22 -> gamma; pid=(-)2112 -> (anti-)proton; pid=130 -> kzero_L
      // if we're talking mc gammas form the vertex, then we can fill one of the two branches
      if(pid==22){
	MyMcTrack *mytr=new MyMcTrack();
	mytr->setId(pid);
	mytr->setEnergy((*it)->energy());
	if((*it)->stopVertex()){
	  if((*it)->stopVertex()->daughters().size()){
	    mytr->setStopRadius((*it)->stopVertex()->position().perp());
	    if(mDebug) cout<<"radius: "<<mytr->stopRadius()<<endl;
	  }
	  else if(mDebug) cout<<"no size"<<endl;
	}
	else if(mDebug) cout<<"no stop"<<endl;
	mytr->setMomentum((*it)->momentum().x(),(*it)->momentum().y(),(*it)->momentum().z());
	
	StThreeVectorF v;
	v.setX(bemcGeom->Radius()*cos((*it)->momentum().phi()));
	v.setY(bemcGeom->Radius()*sin((*it)->momentum().phi()));
	v.setZ(bemcGeom->Radius()/tan((*it)->momentum().theta()));
	v=v + mc_pVert->position();
	
	mytr->setPosition(v.x(),v.y(),v.z());
	//add generated mc particle to event
	mEvent->setMcTrack(mytr);
	delete mytr;
      }
      else if(pid==111||pid==221||pid==2112||pid==-2112||pid==211||pid==130){

	MyMcTrack *mytr=new MyMcTrack();
	mytr->setId(pid);
	mytr->setEnergy((*it)->energy());
	      
	if((*it)->stopVertex()){
	  if((*it)->stopVertex()->daughters().size()){
	    mytr->setStopRadius((*it)->stopVertex()->position().perp());
	  }
	}
	      
	StThreeVectorF v;
	v.setX(bemcGeom->Radius()*cos((*it)->momentum().phi()));
	v.setY(bemcGeom->Radius()*sin((*it)->momentum().phi()));
	v.setZ(bemcGeom->Radius()/tan((*it)->momentum().theta()));
	v=v + mc_pVert->position();
	
	mytr->setMomentum((*it)->momentum().x(),(*it)->momentum().y(),(*it)->momentum().z());
	mytr->setPosition(v.x(),v.y(),v.z());
	//add generated mc particle to event
	mEvent->setMcTrack(mytr);
	delete mytr;

	//and the daughters -> photons!!
	if((*it)->stopVertex()){
	  StMcVertex *sVert=(*it)->stopVertex();
	  StPtrVecMcTrack& gammaTracks=sVert->daughters();
	  for(StMcTrackIterator itd=gammaTracks.begin();itd!=gammaTracks.end();itd++){
	    Int_t pid2=(*itd)->particleDefinition()->pdgEncoding();
	    MyMcTrack *mytr2=new MyMcTrack();
	    mytr2->setId(pid2);
	    mytr2->setEnergy((*itd)->energy());
	    
	    if((*itd)->stopVertex()){
	      if((*itd)->stopVertex()->daughters().size()){
		mytr2->setStopRadius((*it)->stopVertex()->position().perp());
	      }		      		      
	    }
	    
	    StThreeVectorF vv;
	    vv.setX(bemcGeom->Radius()*cos((*itd)->momentum().phi()));
	    vv.setY(bemcGeom->Radius()*sin((*itd)->momentum().phi()));
	    vv.setZ(bemcGeom->Radius()/tan((*itd)->momentum().theta()));
	    vv=vv + mc_pVert->position();
		      
	    mytr2->setMomentum((*itd)->momentum().x(),(*itd)->momentum().y(),(*itd)->momentum().z());
	    mytr2->setPosition(vv.x(),vv.y(),vv.z());
	    
	    mEvent->addMcPhoton(mytr2);
	    delete mytr2;
	  }
	}
      }
      else
	cout<<"what kind of MC particle is this??? (shouldn't see this)"<<endl;
    }
    else if(mMc && nDaughters>1){
      if(mDebug) cout<<"more than one particle per event"<<endl;

      StMcTrackIterator it=trcks.begin();
      Int_t pid=(*it)->particleDefinition()->pdgEncoding();
      if(mDebug) cout<<"daughter pid: "<<pid<<endl;
      if(pid==-2112 || pid==2112 || pid==221){
	for(UInt_t i=0;i<trcks.size();i++){
	  Float_t neutronenergy=trcks[i]->energy();
	  MyMcTrack *myTR=new MyMcTrack();
	  myTR->setId(pid);
	  myTR->setEnergy(neutronenergy);
	  myTR->setMomentum(trcks[i]->momentum().x(),trcks[i]->momentum().y(),trcks[i]->momentum().z());
	  StThreeVectorF v;
	  v.setX(bemcGeom->Radius()*cos(trcks[i]->momentum().phi()));
	  v.setY(bemcGeom->Radius()*sin(trcks[i]->momentum().phi()));
	  v.setZ(bemcGeom->Radius()/tan(trcks[i]->momentum().theta()));
	  v=v + mc_pVert->position();
	  myTR->setPosition(v.x(),v.y(),v.z());
	  mEvent->addMcPion(myTR);
	  delete myTR;
	}
      }
    }
  }
  
  if(!pVert&&!mc_pVert){
    h_EvSum->Fill(6);
    if(mDebug) cout<<"StPhotonMaker::Make() no primary vertex"<<endl;
    return kStOK;
  }

  if(mPythia){
    //get partonic pT (thanks Frank):
    TDataSet *gEvent=GetDataSet("geant");
    TDataSetIter geantDstI(gEvent);
    //St_particle *particleTabPtr=(St_particle*)geantDstI("particle");
    //particle_st* particleTable=particleTabPtr->GetTable();
    //St_g2t_event *Pg2t_event=(St_g2t_event*)geantDstI("g2t_event");
    //g2t_event_st *g2t_event1=Pg2t_event->GetTable();

    //Get  parontic pT,xb1,xb2
    St_g2t_pythia *Pg2t_pythia=(St_g2t_pythia*)geantDstI("g2t_pythia");
    if(Pg2t_pythia){
       g2t_pythia_st *g2t_pythia1=Pg2t_pythia->GetTable();
      if(g2t_pythia1){
	float hard_p=g2t_pythia1->hard_p;
	mEvent->setPartonPt(hard_p);
      }
    }
  }
  if(mPythia||mHijing){
    StPtrVecMcTrack& tracs=mc_event->tracks();
    for(UInt_t i=0;i<tracs.size();i++){
      //use geant id for pythia!
      if(tracs[i]->geantId()==7){
        Float_t pionenergy=tracs[i]->energy();
        MyMcTrack *myTR=new MyMcTrack();
        myTR->setId(111);
        myTR->setEnergy(pionenergy);
        myTR->setMomentum(tracs[i]->momentum().x(),tracs[i]->momentum().y(),tracs[i]->momentum().z());
        StThreeVectorF v;
        v.setX(bemcGeom->Radius()*cos(tracs[i]->momentum().phi()));
        v.setY(bemcGeom->Radius()*sin(tracs[i]->momentum().phi()));
        v.setZ(bemcGeom->Radius()/tan(tracs[i]->momentum().theta()));
        v=v + mc_pVert->position();
        myTR->setPosition(v.x(),v.y(),v.z());
        mEvent->addMcPion(myTR);
        delete myTR;
      }
      if(tracs[i]->geantId()==1){
        Float_t photonenergy=tracs[i]->energy();
        MyMcTrack *myTR=new MyMcTrack();
        myTR->setId(22);
        
	//get parent id:
	//int parentid=-1;
	//if(tracs[i]->parent()){
	//  parentid=tracs[i]->parent()->geantId();
	//}
	//if(parentid==7) parentid=111;
	//else if(parentid==17) parentid=221;
	//else parentid=0;
	//myTR->setParentId(parentid);
	myTR->setEnergy(photonenergy);
        myTR->setMomentum(tracs[i]->momentum().x(),tracs[i]->momentum().y(),tracs[i]->momentum().z());
        StThreeVectorF v;
        v.setX(bemcGeom->Radius()*cos(tracs[i]->momentum().phi()));
        v.setY(bemcGeom->Radius()*sin(tracs[i]->momentum().phi()));
        v.setZ(bemcGeom->Radius()/tan(tracs[i]->momentum().theta()));
        v=v + mc_pVert->position();
        myTR->setPosition(v.x(),v.y(),v.z());
        mEvent->addMcPhoton(myTR);
        delete myTR;
      }
    }
  }

  if(!pVert&&mEmbed){
    h_EvSum->Fill(7);
    if(mDebug) cout<<"StPhotonMaker::Make() no primary vertex ****"<<endl;
    return kStOK;
  }
  StThreeVectorF vPos,mc_vPos;
  if(mMc||mEmbed||mPythia||mHijing){
    mc_vPos=mc_pVert->position();
    if(mMc||mPythia) vPos=mc_vPos;
  }
  if(mEmbed||mReal||mHijing){
    if(pVert) vPos=pVert->position();
  }

  //get trig per event
  mTrigger=0;
  if(mReal&&event->triggerIdCollection()&&event->triggerIdCollection()->nominal()){
    if(event->triggerIdCollection()->nominal()->isTrigger(mTrig[0])||
       event->triggerIdCollection()->nominal()->isTrigger(mTrig[1])) mTrigger+=1;
    if(event->triggerIdCollection()->nominal()->isTrigger(mTrig[2])) mTrigger+=2;
    if(event->triggerIdCollection()->nominal()->isTrigger(mTrig[3])) mTrigger+=4;
    if(event->triggerIdCollection()->nominal()->isTrigger(2002)) mTrigger+=8;
  }

  StEmcTriggerMaker *triggermaker=dynamic_cast<StEmcTriggerMaker*>(GetMaker("bemctrigger"));
  assert(triggermaker);

  if(mMc||mEmbed){
    mTrigger=1;
    if(mDate<20060001){
      if(triggermaker->is2005HT1()) mTrigger+=2;
      if(triggermaker->is2005HT2()) mTrigger+=4;
    }
    else{
      cout<<"check timestamp -> triggermaker"<<endl;
      assert(0);
    }
  }
  if(mPythia){
    mTrigger=1;
    //get result from triggerMaker, 2005 for now:
    if(mDate<20050101) assert(0);
    if(triggermaker->is2005HT1()) mTrigger+=2;
    if(triggermaker->is2005HT2()) mTrigger+=4;
  }
  if(mHijing){
    mTrigger=1;
  }

  if(mTrigger==0){
    if(mDebug) cout<<"nothing more useless than an event with no trigger!"<<endl;
    return kStOK;
  }

  StEmcCollection *emcCol=(StEmcCollection*)event->emcCollection();
  if(!emcCol){
    if(mDebug) cout<<"no emccollection!!"<<endl;
    h_EvSum->Fill(8);
    return kStOK;
  } 
  StEmcDetector *emcDet=(StEmcDetector*)emcCol->detector(kBarrelEmcTowerId);
  if(!emcDet){
    if(mDebug) cout<<"no emcDet!!"<<endl;
    h_EvSum->Fill(9);
    return kStOK; 
  }
  //get status of trigger tower
  Int_t hiTowId=-1;
  Int_t hiTowAdc=-1;
  Int_t hiTowStatus=-1;
  Float_t hiTowEnergy=0.;
  if(mTrigger>1 || mEmbed || mMc){
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
	//h_btowAdc->Fill(adc);
	//h_btowEn->Fill(nrg);
	//h_btowEnVsAdc->Fill(adc,nrg);
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

  StEmcDetector *bsmdeDet=(StEmcDetector*)emcCol->detector(kBarrelSmdEtaStripId);
  if(!bsmdeDet){
    return kStOK;
  }
  StEmcDetector *bsmdpDet=(StEmcDetector*)emcCol->detector(kBarrelSmdPhiStripId);
  if(!bsmdpDet){
    return kStOK;
  }
  for(UInt_t m2=1;m2<=bsmdeDet->numberOfModules();m2++){
    StEmcModule *bsmdeMod=bsmdeDet->module(m2);
    StSPtrVecEmcRawHit& bsmdeHits=bsmdeMod->hits();
    for(UInt_t h2=0;h2<bsmdeHits.size();h2++){
      h_bsmdeAdc->Fill(bsmdeHits[h2]->adc());
    }
  }
  for(UInt_t m2=1;m2<=bsmdpDet->numberOfModules();m2++){
    StEmcModule *bsmdpMod=bsmdpDet->module(m2);
    StSPtrVecEmcRawHit& bsmdpHits=bsmdpMod->hits();
    for(UInt_t h2=0;h2<bsmdpHits.size();h2++){
      h_bsmdpAdc->Fill(bsmdpHits[h2]->adc());
    }
  }


  StSPtrVecEmcPoint& barrelPoints=emcCol->barrelPoints();
  Int_t nPrimTracks=0;
  Int_t nGoodPrimaries=0;
  Int_t nGoodPrimBarrel=0;
  Int_t nGoodGlobals=0;
  Float_t TpcPt=0.;
  Float_t TpcPtBarrelWest=0.;
  if(event->primaryVertex()){
    nPrimTracks=pVert->numberOfDaughters();
    StSPtrVecTrackNode& trackNode=event->trackNodes();
    if(mDebug) cout<<"number of tracknodes: "<<trackNode.size()<<endl;
    for(UInt_t j=0;j<trackNode.size();j++){
      StPrimaryTrack* track=static_cast<StPrimaryTrack*>(trackNode[j]->track(primary));
      if(track && track->flag()>0 && track->geometry()){
	//get DCA
	Double_t pathlength=track->geometry()->helix().pathLength(vPos,kFALSE);
	StThreeVectorD v=track->geometry()->helix().at(pathlength) - vPos;
	Float_t dca=v.mag();
	//get number of fit points
	const StTrackFitTraits&  fitTraits=track->fitTraits();
	unsigned short numFitPoints=fitTraits.numberOfFitPoints();
	if(numFitPoints>15&&dca<3.){
	  nGoodPrimaries++;
	  Float_t tracEta=track->geometry()->momentum().pseudoRapidity();
	  if(tracEta>0.&&tracEta<1.){
	    nGoodPrimBarrel++;
	    TpcPtBarrelWest+=track->geometry()->momentum().perp();
	  }
	  TpcPt+=track->geometry()->momentum().perp();
	}
      }
      StGlobalTrack* gtrack=static_cast<StGlobalTrack*>(trackNode[j]->track(global));
      if(gtrack && gtrack->flag()>0 && gtrack->geometry()){
	const StTrackFitTraits&  fitTraitsG=gtrack->fitTraits();
	unsigned short numFitPointsG=fitTraitsG.numberOfFitPoints();
	if(numFitPointsG>15) nGoodGlobals++;
      }
    }
  }

  //FTPC tracks for centrality, copied from "StRoot/StEventUtilities/StuFtpcRefMult.hh"
  Int_t ftpcRefMultTracks=0;
  if(event->primaryVertex()){
    const StSPtrVecPrimaryTrack& tracks=pVert->daughters();
    for(StSPtrVecPrimaryTrackConstIterator iter = tracks.begin(); iter != tracks.end(); iter++){
      StTrack* track = (*iter);
      if(!track->geometry()){
	if(mDebug) cout<<"no track geometry! -> NEXT"<<endl;
	continue;
      }
      //check if possible FTPC tracks
      if(track->fitTraits().numberOfFitPoints()<6||(track->fitTraits().numberOfFitPoints()>11))
	continue; 
      //check eta range and FTPC east
      if(track->geometry()->momentum().pseudoRapidity()>-2.8||track->geometry()->momentum().pseudoRapidity()<=-3.8)
	continue;
      //check pt
      if(track->geometry()->momentum().perp()>=3.)
	continue; 
      //finally, check dca, if a track satisfies gets inside the if, count it.
      StTrack *glt=track->node()->track(global);
      if(!glt){
	if(mDebug) cout<<"no global track!"<<endl;
	continue;
      }
      if(!glt->geometry()){
	if(mDebug) cout<<"no geom.!"<<endl;
	continue;
      }
      if(glt->geometry()->helix().distance(vPos)<3.) ftpcRefMultTracks++;
    }
  }
  
  //start reco tree here:
  StEmcPoint *pt=0;
  Float_t EnergyNeutral=0.;
  for(UInt_t i=0;i<barrelPoints.size();i++)
    {
      MyPoint *mypt=new MyPoint();
      pt=(StEmcPoint*)barrelPoints[i];
      
      Float_t dist=9999.;
      if(!calcDistanceTrackToPoint(pt,dist)){
	cout<<"wrong bfield or no StEvent pointer (shouldn't see this)"<<endl;
      }
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
  if(mHijing){
    mEvent->setZdcVertexZ(mc_vPos.z());
  }

  mEventTree->Fill();

  return kStOK;
}





Int_t StPhotonMaker::Finish()
{
  saveHistograms();
  return kStOK;
}


void StPhotonMaker::saveHistograms()
{
  if(mDebug) cout<<"************ saving histograms ****************"<<endl;
  TFile *hfile=(TFile*)gROOT->FindObject(mFileName);
  if(hfile) hfile->Close();
  hfile=new TFile(mFileName,"RECREATE");
  //hfile->SetCompressionLevel(1);
  //----
  h_EvSum->Write();
  h_bsmdeAdc->Write();
  h_bsmdpAdc->Write();
  //h_bsmdeEn->Write();
  //h_btowAdc->Write();
  //h_btowEn->Write();
  //h_btowEnVsAdc->Write();

  mEventTree->Write();

  //----
  hfile->Close();
}

void StPhotonMaker::setDbMaker(St_db_Maker *dbM)
{
  mDbMaker=dbM;
}
void StPhotonMaker::setAdcMaker(StEmcADCtoEMaker *adc)
{
  mAdcMaker=adc;
}
Bool_t StPhotonMaker::calcDistanceTrackToPoint(StEmcPoint *point,Float_t &distanceToTrack)
{
  StEvent *event = (StEvent*)this->GetInputDS("StEvent");
  if(!event){
    if(mDebug) cout<<"can't get Event pointer"<<endl;
    return kFALSE;
  }
  
  Double_t bFld=0.;
  StEventSummary* summary = event->summary();
  if(summary){
    bFld = summary->magneticField()/10.; // bFld in Tesla
    if(mDebug) cout<<"summary()->magneticField()="<<bFld<<"(Tesla)"<<endl;
  }
  if(TMath::Abs(bFld)<0.01&&!mMc){
    if(mDebug) cout<<"wrong mBField!"<<endl;
    return kFALSE;
  }
  StEmcPosition* emcPosition = new StEmcPosition();
  StThreeVectorD pos, mom;
  StSPtrVecTrackNode& trackNodes = event->trackNodes();
  StTrack* track;
  distanceToTrack=999.;
  for (size_t nodeIndex=0; nodeIndex<trackNodes.size();nodeIndex++){
    size_t numberOfTracksInNode=trackNodes[nodeIndex]->entries(global);
    for(size_t trackIndex=0; trackIndex<numberOfTracksInNode;trackIndex++){
      track = trackNodes[nodeIndex]->track(primary,trackIndex);
      if (track && track->flag()>=0){
	if(track->geometry()->momentum().mag()<.5) continue;
	StPhysicalHelixD helix=track->outerGeometry()->helix();
	Bool_t projOK=emcPosition->projTrack(&pos,&mom,track,bFld,230.705,1);
	if(!projOK) continue;
	Float_t d=(pos - point->position()).mag();//in cm
	if(d<distanceToTrack&&d>0.) distanceToTrack=d;
      }
    }
  }
  delete emcPosition;
  return kTRUE;
}
