// $Id: St2009pubMcMaker.cxx,v 1.7 2011/09/14 14:23:21 stevens4 Exp $
//
//*-- Author : Justin Stevens, IUCF
// 

#include "St2009WMaker.h"
#include "St2009ZMaker.h"
#include "St2009pubMcMaker.h"
#include "StEmcUtil/geometry/StEmcGeom.h"
#include <TMath.h>

//need these to get MC record
#include "tables/St_g2t_tpc_hit_Table.h"
#include "StMcEventMaker/StMcEventMaker.h"
#include "StMcEvent/StMcEvent.hh"
#include "StMcEvent/StMcVertex.hh"
#include "StMcEvent/StMcTrack.hh"

//muDst needed for zdcRate
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>
#include <StMuDSTMaker/COMMON/StMuMcTrack.h>

ClassImp(St2009pubMcMaker)

//_______________________________________________________
//_______________________________________________________
St2009pubMcMaker::St2009pubMcMaker(const char *name):StMaker(name){
  wMK=0;HList=0;
    
}


//_______________________________________________________
//_______________________________________________________
St2009pubMcMaker::~St2009pubMcMaker(){
  //
}


//_______________________________________________________
//_______________________________________________________
Int_t St2009pubMcMaker::Init(){
  assert(wMK);
  assert(HList);
  initHistos();
  return StMaker::Init();
}


//_______________________________________________________
//_______________________________________________________
Int_t 
St2009pubMcMaker::Make(){
  //printf("-----------in %s\n", GetName());
  //only get geant particle info for W MC
  if(wMK->isMC==350){
    if(doWMCanalysis()){
      doWefficiency();
      doWanalysis();
    }
  }
  if(wMK->isMC==352){
    if(doZMCanalysis()){
      doZefficiency();
    }
  }
  return kStOK;
}

     
//_______________________________________________________
//_______________________________________________________
void 
St2009pubMcMaker::doWefficiency(){

  //initialize some kinematic variables for later use 
  
  //wrap barrel onto 2 modules to see boundaries
  const float PI=TMath::Pi();
  float elePhi=mElectronP.Phi();
  if(elePhi<0) elePhi+=2*PI;
  elePhi=elePhi*180/PI;
  elePhi+=3.0;
  float modulePair=12.0;
  int resid = (int) (elePhi/modulePair);
  float phiMod= elePhi - resid*modulePair;
  // 'detector' eta
  TVector3 detEle; //where lepton would hit BEMC
  float Rcylinder= wMK->mBtowGeom->Radius();
  detEle.SetPtEtaPhi(Rcylinder,mElectronP.Eta(),mElectronP.Phi());
  detEle.SetZ(detEle.Z()+mVertex.Z());

  float w=1; //determine zVertex weighting for event
  if(wMK->isMC ==350) w=wMK->hReweight->GetBinContent(wMK->hReweight->FindBin(mVertex.Z()));
  float zdcRate=wMK->mMuDstMaker->muDst()->event()->runInfo().zdcCoincidenceRate();
    
  //only count leptons in our eta range
  if(fabs(mElectronP.Eta()) > 1.0) return;
  //only count leptons in our ET range
  if(fabs(mElectronP.Perp()) < 25.) return;

  int bx7=wMK->wEve.bx7;
  if( (bx7>30 && bx7<40) || (bx7>110) ) return; //skip abort gaps

  //ele has |eta| < 1 and ET > 25
  hA[50]->Fill(mElectronP.Perp(),w);
  hA[54]->Fill(mElectronP.Eta(),w);
  hA[58]->Fill(mVertex.Z(),w);
  hA[62]->Fill(mElectronP.Phi(),w);
  hB[83]->Fill(mElectronP.Eta(),mElectronP.Perp(),w);
  hA[89]->Fill(zdcRate,w);

  hA[68]->Fill(mElectronP.Perp(),w); //forJoe
  hA[77]->Fill(phiMod,w); //wrap barrel onto 2 modules
  hB[66]->Fill(detEle.Eta(),mElectronP.Perp(),w);
  hA[79]->Fill(detEle.Eta(),w);
  hA[122]->Fill(zdcRate,wMK->wEve.bx7);

  //check reconstructed vs thrown ET
  for(uint iv=0;iv<wMK->wEve.vertex.size();iv++) {
    WeveVertex &V=wMK->wEve.vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.isMatch2Cl==false) continue;
      assert(T.cluster.nTower>0); // internal logical error
      assert(T.nearTotET>0); // internal logical error
      
      hB[68]->Fill(mElectronP.Perp(),T.cluster.ET,w);
    }
  }

  //trigger efficiency
  if(!wMK->wEve.l2bitET) return;
  //good trig
  hA[51]->Fill(mElectronP.Perp(),w);
  hA[55]->Fill(mElectronP.Eta(),w);
  hA[59]->Fill(mVertex.Z(),w);
  hA[63]->Fill(mElectronP.Phi(),w);
  hB[84]->Fill(mElectronP.Eta(),mElectronP.Perp(),w);
  hA[90]->Fill(zdcRate,w);

  hA[69]->Fill(mElectronP.Perp(),w);
  hA[78]->Fill(phiMod,w);//wrap barrel onto 2 modules
  hB[67]->Fill(detEle.Eta(),mElectronP.Perp(),w);
  hA[80]->Fill(detEle.Eta(),w);

  //vertex efficiency
  //require one vertex w/ rank>0 and |z|<100
  if(wMK->wEve.vertex.size()<=0) return; 
  //require geant vert match reco vert (w/in 5 cm)
  bool matchVert=false;
  for(uint iv=0;iv<wMK->wEve.vertex.size();iv++) {
    WeveVertex &V=wMK->wEve.vertex[iv];
    //cout<<endl<<"vertex diff "<<fabs(V.z - mVertex.Z())<<endl;
    if(fabs(V.z - mVertex.Z()) < 5.0) matchVert=true;
  }
  if(!matchVert) return;
  //cout<<"after matched vert"<<endl;
  hA[52]->Fill(mElectronP.Perp(),w); 
  hA[56]->Fill(mElectronP.Eta(),w);
  hA[60]->Fill(mVertex.Z(),w);
  hA[64]->Fill(mElectronP.Phi(),w);
  hA[91]->Fill(zdcRate,w);

  hA[70]->Fill(mElectronP.Perp(),w);//forJoe

  //get MC track info
  int eleTrId=999; TVector3 eleTrP;
  int size=wMK->mMuDstMaker->muDst()->mcArray(1)->GetEntries();
  cout<<"size="<<size<<endl;
  for(int i=0; i<size; i++){
    StMuMcTrack* muMcTrack=(StMuMcTrack*)wMK->mMuDstMaker->muDst()->mcArray(1)->UncheckedAt(i);
    int geantId=muMcTrack->GePid();
    int id=muMcTrack->Id();
    if(geantId==2 || geantId==3){
      cout<<i<<" id="<<id<<" geantId="<<geantId<<endl;
      eleTrId=id;
      eleTrP=TVector3(muMcTrack->Pxyz().x(),muMcTrack->Pxyz().y(),muMcTrack->Pxyz().z());
      cout<<"phiDiff="<<eleTrP.Phi()-mElectronP.Phi()<<" etaDiff="<<eleTrP.Eta()-mElectronP.Eta()<<endl;
      break;
    }
  }

  int nTrackAssoc=0; float recoPtAssoc=0;
  int flagAssoc=0;
  int nHitAssoc=0; float hitFracAssoc=0.;
  float rInAssoc=0.; float rOutAssoc=0.;
  //check tracking efficiency before any track cuts
  for(uint iv=0;iv<wMK->wEve.vertex.size(); iv++) {
    uint vertID=wMK->wEve.vertex[iv].id;
    wMK->mMuDstMaker->muDst()->setVertexIndex(vertID);
    Int_t nPrimTrAll=wMK->mMuDstMaker->muDst()->GetNPrimaryTrack();
    for(int itr=0;itr<nPrimTrAll;itr++) {
      StMuTrack *prTr=wMK->mMuDstMaker->muDst()->primaryTracks(itr);
      if(prTr->flag()<=0) continue;
      const StMuTrack *glTr=prTr->globalTrack();
      if(glTr==0) continue; // see the reason in _accessMuDst
      if(prTr->flag()!=301) continue;// TPC+prim vertex tracks
      if(prTr->pt()<10.) continue;
      
      StThreeVectorF ro=glTr->lastPoint();
      int secID=WtpcFilter::getTpcSec(ro.phi(),ro.pseudoRapidity());
      if(secID==20) continue;
      
      //only use with private copy of StMuTrack which makes these functions public (currently in dev they're private)
      //int idTruth=prTr->idTruth();
      //int qaTruth=prTr->qaTruth();
      
      int idTruth=0;
      int qaTruth=0;

      float delPhi=prTr->phi()-mElectronP.Phi();
      float delEta=prTr->eta()-mElectronP.Eta();
      if( eleTrId==idTruth ) { //reco track matches to electron
	
	flagAssoc=prTr->flag();
	nHitAssoc=prTr->nHitsFit();
	hitFracAssoc=1.*prTr->nHitsFit()/prTr->nHitsPoss();
	rInAssoc=glTr->firstPoint().perp();
	rOutAssoc=glTr->lastPoint().perp();
	
	cout<<"found electron, delR="<<sqrt(delPhi*delPhi+delEta*delEta)<<" nHits="<<prTr->nHitsFit()<<" fitFrac="<<hitFracAssoc<<endl;
	cout<<"idTruth="<<idTruth<<" qaTruth="<<qaTruth<<endl;		
	recoPtAssoc=prTr->pt();
	nTrackAssoc++;
      }
    }
  }
  if(nTrackAssoc==1) {
    hA[120]->Fill(recoPtAssoc,recoPtAssoc-mElectronP.Perp());
    hA[124]->Fill(recoPtAssoc,w);
    hA[126]->Fill(1./recoPtAssoc,w);
    hA[128]->Fill(recoPtAssoc,nHitAssoc);
    hA[129]->Fill(recoPtAssoc,hitFracAssoc);
    hA[130]->Fill(recoPtAssoc,rInAssoc);
    hA[131]->Fill(recoPtAssoc,rOutAssoc);
    hA[132]->Fill(1./recoPtAssoc,nHitAssoc);
    hA[133]->Fill(1./recoPtAssoc,hitFracAssoc);
    hA[134]->Fill(1./recoPtAssoc,rInAssoc);
    hA[135]->Fill(1./recoPtAssoc,rOutAssoc);
    hA[136]->Fill(flagAssoc);
  }
  cout<<"number of associated tracks = "<<nTrackAssoc<<endl;

  // track efficiency
  bool foundTrack=false; int nTrk=0; float recoPt=0; 
  for(uint iv=0;iv<wMK->wEve.vertex.size();iv++) {
    WeveVertex &V=wMK->wEve.vertex[iv];
    if(V.eleTrack.size()>0){ //track with pt > 10
      hA[85]->Fill(mElectronP.Perp(),w);
      hA[86]->Fill(mElectronP.Eta(),w);
      hA[87]->Fill(mVertex.Z(),w);
      hA[88]->Fill(mElectronP.Phi(),w);
      hA[92]->Fill(zdcRate,w);
      foundTrack=true;
      
      //find track 
      for(uint it=0;it<V.eleTrack.size();it++) {
	WeveEleTrack &T=V.eleTrack[it];
	int idTruth=0; //T.prMuTrack->idTruth(); //again need private StMuTrack
	if( eleTrId==idTruth ) { //reco track matches to electron
	  recoPt=T.primP.Perp();
	  nTrk++;
	}
      }
      break; //only count event once
    }
  }
  if(!foundTrack) return;

  if(nTrackAssoc==1) { //accepted tracks
    hA[125]->Fill(recoPtAssoc,w);
    hA[127]->Fill(1./recoPtAssoc,w);
  }

  if(wMK->wEve.wTag) hA[140]->Fill(mElectronP.Perp(),w);

  //reco efficiency
  for(uint iv=0;iv<wMK->wEve.vertex.size();iv++) {
    WeveVertex &V=wMK->wEve.vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.isMatch2Cl==false) continue;
      assert(T.cluster.nTower>0); // internal logical error
      assert(T.nearTotET>0); // internal logical error
      
      if(wMK->wEve.zTag)
	continue; //event tagged as Z
      if(fabs(T.primP.Eta()) > wMK->par_leptonEta) 
	continue; // reconstructed eta too large

      if(T.cluster.ET/T.nearTotET < wMK->par_nearTotEtFrac) 
	continue; // too large nearET
      if(T.sPtBalance < wMK->par_ptBalance) 
	continue; // too large signed pt balance
      
      if(T.cluster.ET < 25.) 
	continue; // too small reco ET (use same cut as data)

      //pass all W cuts 
      hA[53]->Fill(mElectronP.Perp(),w);
      hA[57]->Fill(mElectronP.Eta(),w);
      hA[61]->Fill(mVertex.Z(),w);
      hA[65]->Fill(mElectronP.Phi(),w);
      hA[93]->Fill(zdcRate,w);

      hA[71]->Fill(mElectronP.Perp(),w);//forJoe
      hA[121]->Fill(T.primP.Phi()-mElectronP.Phi(),T.primP.Eta()-mElectronP.Eta());
      hA[123]->Fill(zdcRate,wMK->wEve.bx7);

      //fill thrown ele ET after all W cuts
      hA[111]->Fill(detEle.Eta(),mElectronP.Perp());
      hA[113]->Fill(detEle.Eta(),mElectronSmearP.Perp());
      hA[115]->Fill(T.cluster.ET/mElectronP.Perp());
      hA[116]->Fill(T.cluster.energy/mElectronP.Mag());

      //template function stuff
      for(int ires=0; ires<10; ires++){
        for(int iscale=0; iscale<101; iscale++){
          float scaleFact = iscale*0.004 + 0.8;
          hB[120+ires]->Fill(iscale,mElectronSmearTempP[ires].Perp()*scaleFact,wRB);
          hB[150+ires]->Fill(scaleFact,mElectronSmearTempP[ires].Perp()*scaleFact,wRB);
          if(fabs(detEle.Eta()) < 0.5) hB[130+ires]->Fill(iscale,mElectronSmearTempP[ires].Perp()*scaleFact,wRB);
          else if(fabs(detEle.Eta()) < 1.0) hB[140+ires]->Fill(iscale,mElectronSmearTempP[ires].Perp()*scaleFact,wRB);
        }
      }
      
      break; //only count event once
    }
  }

}


//_______________________________________________________
//_______________________________________________________
void 
St2009pubMcMaker::doZefficiency(){

  //Acceptance cuts applied
  //only count leptons in our eta range
  if(fabs(mZelectronP.Eta()) > 1.) return;
  if(fabs(mZpositronP.Eta()) > 1.) return;
  //only count leptons in our ET range
  if(mZelectronP.Perp() < 15.) return;
  if(mZpositronP.Perp() < 15.) return;

  TVector3 sumP=mZelectronP+mZpositronP;
  float sumE=mZelectronP.Mag()+mZpositronP.Mag();
  float geantZmass=sqrt(sumE*sumE-sumP.Dot(sumP));
  cout<<"geant mass"<<geantZmass<<endl;
  float zdcRate=wMK->mMuDstMaker->muDst()->event()->runInfo().zdcCoincidenceRate();

  float w=1; //determine zVertex weighting for event
  if(wMK->isMC==352) w=wMK->hReweight->GetBinContent(wMK->hReweight->FindBin(mVertex.Z()));

  //fill all events in acceptance
  hA[100]->Fill(geantZmass,w);
  if(geantZmass>70. && geantZmass<110.) hA[105]->Fill(zdcRate,w);
  
  //trigger efficiency
  if(!wMK->wEve.l2bitET) return;
  //fill good trig events
  hA[101]->Fill(geantZmass,w);
  if(geantZmass>70. && geantZmass<110.) hA[106]->Fill(zdcRate,w);
  
  //vertex efficiency
  if(wMK->wEve.vertex.size()<=0) return;
  //fill vertex rank>0 and |z|<100 events
  bool matchVert=false;
  for(uint iv=0;iv<wMK->wEve.vertex.size();iv++) {
    WeveVertex &V=wMK->wEve.vertex[iv];
    //cout<<endl<<"vertex diff "<<fabs(V.z - mVertex.Z())<<endl;
    if(fabs(V.z - mZvertex.Z()) < 5.0) matchVert=true;
  }
  if(!matchVert) return;
  hA[102]->Fill(geantZmass,w);
  if(geantZmass>70. && geantZmass<110.) hA[107]->Fill(zdcRate,w);
  
  //track efficiency
  bool gt1track=false;
  for(uint iv=0;iv<wMK->wEve.vertex.size();iv++) {
    WeveVertex &V=wMK->wEve.vertex[iv];
    if(V.eleTrack.size()>1){ //fill 2 track with pt > 10 events
      hA[103]->Fill(geantZmass,w);
      if(geantZmass>70. && geantZmass<110.) hA[108]->Fill(zdcRate,w);
      
      gt1track=true;
      break; //only count event once
    }
  }
  if(!gt1track) return;

  //reco efficiency
  for(uint iv=0;iv<wMK->wEve.vertex.size();iv++) {
    WeveVertex &V=wMK->wEve.vertex[iv];
    if(V.eleTrack.size()<2) continue;
    for(uint it=0;it<V.eleTrack.size()-1;it++) {//.....select first track:

      WeveEleTrack &T1=V.eleTrack[it];
      if(T1.isMatch2Cl==false) continue;
      assert(T1.cluster.nTower>0); // internal logical error
      assert(T1.nearTotET>0); // internal logical error

      if(T1.cluster.ET<zMK->par_clusterEtZ) continue;
      float fracET1=T1.cluster.ET /T1.nearTotET;
      if(fracET1<zMK->par_nearTotEtFracZ) continue;
      
      for (uint it2=it+1;it2<V.eleTrack.size();it2++) { //.....select second track:

	WeveEleTrack &T2=V.eleTrack[it2];
        if(T2.isMatch2Cl==false) continue;
        assert(T2.cluster.nTower>0); // internal logical error
        assert(T2.nearTotET>0); // internal logical error

	if(T2.cluster.ET<zMK->par_clusterEtZ) continue;
	float fracET2=T2.cluster.ET /T2.nearTotET;
	if(fracET2<zMK->par_nearTotEtFracZ) continue;
        
	float e1=T1.cluster.energy;
        float e2=T2.cluster.energy;
        TVector3 p1=T1.primP; p1.SetMag(e1);//cluster.position;
        TVector3 p2=T2.primP; p2.SetMag(e2);//cluster.position;

        float del_phi=p1.DeltaPhi(p2);
	if(fabs(del_phi)<zMK->par_delPhi12) continue;
	TVector3 psum=p1+p2;
        float mass2=(e1+e2)*(e1+e2)-(psum.Dot(psum));
        if(mass2<1.) continue; // 9GeV^2) should be param, I'm tired today
	//float mass=sqrt(mass2); 
	
	int Q1Q2=T1.prMuTrack->charge()*T2.prMuTrack->charge();
        if (Q1Q2==1) continue;
        
	//fill reco Z histos
	hA[104]->Fill(geantZmass,w); 
	if(geantZmass>70. && geantZmass<110.) hA[109]->Fill(zdcRate,w);
	
      } //second track
    } //first track
  } //vertex

  return;
}


//________________________________________________
//________________________________________________
bool
St2009pubMcMaker::doWMCanalysis(){
  StMcEvent* mMcEvent = 0;  
  mMcEvent = (StMcEvent*) StMaker::GetChain()->GetDataSet("StMcEvent");
  //assert(mMcEvent);

  if(!mMcEvent) return false;

  //initialize momentum vectors
  StThreeVectorF pW;        float eW;
  StThreeVectorF pNeutrino; //float eNeutrino;
  StThreeVectorF pElectron; //float eElectron;

  StMcVertex *V=mMcEvent->primaryVertex(); 
  mVertex=TVector3(V->position().x(),V->position().y(),V->position().z());
  
  //find geant tracks for W and decay daughters
  uint i=1;
  int found=0;
  while(found<2 && i<mMcEvent->tracks().size()){//loop tracks
    StMcTrack* mcTrack = mMcEvent->tracks()[i];
    int pdgId=mcTrack->pdgId();
    float pt=mcTrack->pt();
    LOG_DEBUG<<"pdgId "<<pdgId<<" pt "<<pt<<" pz "<<mcTrack->momentum().z()<<endm;
    if(pdgId==11 || pdgId==-11){ //select e+ and e-
      if(abs(mcTrack->parent()->pdgId()) == 24 ){ 
	pElectron=mcTrack->momentum();
	//mKeyElectron=mcTrack->key();
	//mEveGenElectron=mcTrack->eventGenLabel();
	//mIdElectron=i;
	LOG_DEBUG<<"pdgId "<<pdgId<<" pt "<<pt<<" pz "<<mcTrack->momentum().z()<<endm;
	pW=mcTrack->parent()->momentum();
	eW=mcTrack->parent()->energy();
	LOG_DEBUG<<"pdgId "<<mcTrack->parent()->pdgId()<<" pt "<<mcTrack->parent()->pt()<<" pz "<<mcTrack->parent()->momentum().z()<<endm;
	found++;
      }
    }
    if(pdgId==12 || pdgId==-12){ //select neutrino
      if(abs(mcTrack->parent()->pdgId()) == 24 ){ 
	pNeutrino=mcTrack->momentum();
	LOG_DEBUG<<"pdgId "<<pdgId<<" pt "<<pt<<" pz "<<mcTrack->momentum().z()<<endm;
	pW=mcTrack->parent()->momentum();
	eW=mcTrack->parent()->energy();
	LOG_DEBUG<<"pdgId "<<mcTrack->parent()->pdgId()<<" pt "<<mcTrack->parent()->pt()<<" pz "<<mcTrack->parent()->momentum().z()<<endm;
	found++;
      }
    }
    i++;
  }

  i=1;
  while(i<mMcEvent->tracks().size()){
    StMcTrack* mcTrack = mMcEvent->tracks()[i];
    int pdgId=mcTrack->pdgId();
    float pt=mcTrack->pt();
    i++;
    if(abs(pdgId)==11 || abs(pdgId)==12 || abs(pdgId)==24 || abs(pdgId)==21 || abs(pdgId) < 10 || abs(pdgId)==92 || pt<10.0) continue;
    cout<<"high pt MC track: pdgId="<<pdgId<<" pt="<<pt<<endl;
    
  }
 
  if(found!=2) return false;
  
  mWP=TVector3(pW.x(),pW.y(),pW.z());
  mNeutrinoP=TVector3(pNeutrino.x(),pNeutrino.y(),pNeutrino.z());
  mElectronP=TVector3(pElectron.x(),pElectron.y(),pElectron.z());
  TVector3 diff=mWP-mNeutrinoP-mElectronP;
  if(diff.Mag() > 0.0001) //should get exactly right
    LOG_INFO<<"\n \n W+e+nu vector sum ="<<diff.Mag()<<endm;
  if(mElectronP.Mag() < 0.0001)
    LOG_INFO<<"\n \n no lepton track ="<<endm;

  //calculate x1 and x2 from W rapidity
  float rapW = 0.5*log((eW+mWP.Z())/(eW-mWP.Z()));
  float mw2sqs=80.4/500.;
  float x1=mw2sqs*exp(rapW);
  float x2=mw2sqs*exp(-rapW);

  hA[72]->Fill(rapW);
  if(fabs(mElectronP.Eta())<1){ //require midrapidity e
    hA[73]->Fill(x1);
    hA[74]->Fill(x2);
    hA[75]->Fill(x1,x2);
    hA[76]->Fill(x1-x2);
  }

  //original electron Pt spectrum
  if(fabs(mElectronP.Eta()) < 1.)
    hA[117]->Fill(mElectronP.Pt());

  //define weighting for rhicbos
  wRB = 0.0; //init weight at 0
  float weight[120] = {0,0,0,0,0,0,0,0,0,0,0,0.0179866,0,0,0,1.02642,0.597259,3.00388,0.88189,1.68209,0.877086,0.787812,0.920501,0.737976,0.735082,0.977448,1.21798,0.745058,0.693323,1.02895,1.69843,1.17129,0.945612,0.917748,0.977399,0.87226,1.0391,0.86036,0.769629,1.03845,0.923467,1.06595,0.843156,1.05421,0.859137,0.896914,0.941134,0.826568,0.857237,0.915694,0.892815,0.901463,1.07729,1.00146,0.935025,1.01036,1.03116,0.873262,1.04329,0.914255,0.976182,0.88009,1.02751,1.12671,1.07694,1.10049,1.04973,1.11188,1.10395,0.914941,1.08072,1.11682,1.0614,1.11588,0.960717,0.992878,1.05434,1.0802,0.967019,1.02817,1.04221,1.01508,1.0033,1.2565,0.987695,1.17665,1.05089,1.00209,0.97153,1.07144,0.824123,1.62538,0.883172,0.860085,0.860167,1.26625,0.58652,0.925282,0.998732,1.06802,0.620375,0.686918,0.845012,0.863964,0.966573,0.52009,0.974503,0.603462,0.305834,0.220393,0.347469,0.849899,0.674756,0.630642,0.613394,0.173667,0.152909,0.325029,0.200083,0.316456};

  float et = mElectronP.Pt();
  for(int j=0; j<120; j++){
    if(et > j*0.5 && et < (j+1)*0.5 && fabs(mElectronP.Eta()) < 1.){
      hA[118]->Fill(et,weight[j]);
      wRB = weight[j];
      break;
    }
  }

  wRB=1.0; // drop weighting for now

  //smear electron energy by BTOW resolution
  float initE=mElectronP.Mag();
  float width=sqrt(0.14*0.14/initE + 0.015*0.015); //resolution factor from NIM
  float smear = gRandom->Gaus(0, width);
  float finalE=mElectronP.Mag()+smear;
  cout<<"initE="<<initE<<" width="<<width<<" smear="<<smear<<" finalE="<<finalE<<endl;
  mElectronSmearP=mElectronP; mElectronSmearP.SetMag(finalE);

  //where lepton would hit BEMC
  TVector3 detEle;
  float Rcylinder= wMK->mBtowGeom->Radius();
  detEle.SetPtEtaPhi(Rcylinder,mElectronP.Eta(),mElectronP.Phi());
  detEle.SetZ(detEle.Z()+mVertex.Z());

  //thrown ET for jacob peak comp
  hA[110]->Fill(detEle.Eta(),mElectronP.Perp());
  //smeared ET
  hA[112]->Fill(detEle.Eta(),mElectronSmearP.Perp());

  //template function stuff
  for(int ires=0; ires<10; ires++){
    float widthTemp = ires/100.;
    float smearTemp = gRandom->Gaus(0, widthTemp);
    float finalETemp = mElectronP.Mag()+smearTemp*mElectronP.Mag();
    //cout<<"initE="<<initE<<" width="<<width<<" smear="<<smear<<" finalE="<<finalE<<endl;
    mElectronSmearTempP[ires]=mElectronP; mElectronSmearTempP[ires].SetMag(finalETemp);
    for(int iscale=0; iscale<101; iscale++){
      float scaleFact = iscale*0.004 +0.9;
      hB[110+ires]->Fill(iscale,mElectronSmearTempP[ires].Perp()*scaleFact);
    }
  }

  return true;

}



//________________________________________________
//________________________________________________
bool
St2009pubMcMaker::doZMCanalysis(){
  StMcEvent* mMcEvent = 0;  
  mMcEvent = (StMcEvent*) StMaker::GetChain()->GetDataSet("StMcEvent");
  //assert(mMcEvent);

  if(!mMcEvent) return false;

  //initialize momentum vectors
  StThreeVectorF pZ;        float eZ;
  StThreeVectorF pPositron; //float eNeutrino;
  StThreeVectorF pElectron; //float eElectron;

  StMcVertex *V=mMcEvent->primaryVertex(); 
  mZvertex=TVector3(V->position().x(),V->position().y(),V->position().z());
  
  //find geant tracks for Z and e+,e-
  uint i=1;
  int found=0;
  while(found<2 && i<mMcEvent->tracks().size()){//loop tracks
    StMcTrack* mcTrack = mMcEvent->tracks()[i];
    int pdgId=mcTrack->pdgId();
    float pt=mcTrack->pt();
    LOG_DEBUG<<"pdgId "<<pdgId<<" pt "<<pt<<" pz "<<mcTrack->momentum().z()<<endm;
    if(pdgId==11){ //select e-
      if(abs(mcTrack->parent()->pdgId()) == 23 ){ 
	pElectron=mcTrack->momentum();
	LOG_DEBUG<<"pdgId "<<pdgId<<" pt "<<pt<<" pz "<<mcTrack->momentum().z()<<endm;
	pZ=mcTrack->parent()->momentum();
	eZ=mcTrack->parent()->energy();
	LOG_DEBUG<<"pdgId "<<mcTrack->parent()->pdgId()<<" pt "<<mcTrack->parent()->pt()<<" pz "<<mcTrack->parent()->momentum().z()<<endm;
	found++;
      }
    }
    if(pdgId==-11){ //select e+
      if(abs(mcTrack->parent()->pdgId()) == 23 ){ 
	pPositron=mcTrack->momentum();
	LOG_DEBUG<<"pdgId "<<pdgId<<" pt "<<pt<<" pz "<<mcTrack->momentum().z()<<endm;
	pZ=mcTrack->parent()->momentum();
	eZ=mcTrack->parent()->energy();
	LOG_DEBUG<<"pdgId "<<mcTrack->parent()->pdgId()<<" pt "<<mcTrack->parent()->pt()<<" pz "<<mcTrack->parent()->momentum().z()<<endm;
	found++;
      }
    }
    i++;
  }
 
  if(found!=2) return false; 
  
  mZP=TVector3(pZ.x(),pZ.y(),pZ.z());
  mZpositronP=TVector3(pPositron.x(),pPositron.y(),pPositron.z());
  mZelectronP=TVector3(pElectron.x(),pElectron.y(),pElectron.z());
  TVector3 diff=mZP-mZpositronP-mZelectronP;
  if(diff.Mag() > 0.0001) //should get exactly right
    LOG_INFO<<"\n \n Z+e+e vector sum ="<<diff.Mag()<<endm;
  if(mZelectronP.Mag() < 0.0001)
    LOG_INFO<<"\n \n no lepton track ="<<endm;

  return true;
}


//_______________________________________________________
//_______________________________________________________
void 
St2009pubMcMaker::doWanalysis(){
  //has access to whole W-algo-maker data via pointer 'wMK'
 
  // run through W cuts to fill other histos............
  for(uint iv=0;iv<wMK->wEve.vertex.size();iv++) {
    WeveVertex &V=wMK->wEve.vertex[iv];
    for(uint it=0;it<V.eleTrack.size();it++) {
      WeveEleTrack &T=V.eleTrack[it];
      if(T.isMatch2Cl==false) continue;
      assert(T.cluster.nTower>0); // internal logical error
      assert(T.nearTotET>0); // internal logical error
      
      if(T.cluster.ET /T.nearTotET< wMK->par_nearTotEtFrac) continue; // too large nearET
      if(T.awayTotET> 30.) continue; // too large awayET , Jan
      //Full W cuts applied at this point

      //W info from pythia record
      hA[1]->Fill(mWP.Perp());
      hA[2]->Fill(mWP.z());
      
      hA[24]->Fill(mWP.Eta());
      hA[25]->Fill(T.hadronicRecoil.Eta());
      hA[26]->Fill(mWP.Perp(),T.hadronicRecoil.Eta());
      if(mWP.Eta()>0) {
	hA[27]->Fill(T.hadronicRecoil.Eta());
      }
      else {
	hA[28]->Fill(T.hadronicRecoil.Eta());
      }
      
      //hadronic recoil and correlations with W from pythia
      TVector3 hadronicPt(T.hadronicRecoil.X(),T.hadronicRecoil.Y(),0); //transverse momentum vector
      hA[3]->Fill(mWP.Perp()-hadronicPt.Perp());
      hA[4]->Fill(mWP.Perp(),hadronicPt.Perp());
      hA[5]->Fill(hadronicPt.Perp()); 

      float delPhi=mWP.DeltaPhi(-hadronicPt);
      hA[6]->Fill(mWP.Perp(),delPhi);
      hA[7]->Fill(mWP.Perp(),mWP.Perp()-hadronicPt.Perp());
      hA[8]->Fill(T.hadronicRecoil.Perp(),delPhi);
      
      //electron reco and from geant record
      hA[9]->Fill(mElectronP.Perp());
      hA[10]->Fill(T.cluster.ET);      
      hA[11]->Fill(mElectronP.Perp(),T.cluster.ET);
      hA[12]->Fill(mElectronP.Perp(),mElectronP.Perp()-T.cluster.ET);

      TVector3 electronPt(T.cluster.position.X(),T.cluster.position.Y(),0); //transvers momentum vector
      electronPt.SetMag(T.cluster.ET);
      
      //neutrino reco and from geant record
      TVector3 neutrinoPt=-1*(hadronicPt+electronPt);
      hA[13]->Fill(neutrinoPt.Perp());  
      hA[14]->Fill(mNeutrinoP.Perp());
      hA[15]->Fill(mNeutrinoP.Perp(),neutrinoPt.Perp());
      hA[16]->Fill(mNeutrinoP.Perp()-neutrinoPt.Perp());

      hA[17]->Fill(mNeutrinoP.Perp(),mElectronP.Perp());

      float recoDeltaPhi=neutrinoPt.DeltaPhi(electronPt);
      float geantDeltaPhi=mNeutrinoP.DeltaPhi(mElectronP);
      hA[18]->Fill(geantDeltaPhi,recoDeltaPhi);
      hA[19]->Fill(cos(geantDeltaPhi)-cos(recoDeltaPhi));

      float Mt=sqrt(2*T.cluster.ET*neutrinoPt.Perp()*(1-cos(recoDeltaPhi)));  //real data
      float gMt=sqrt(2*mElectronP.Perp()*mNeutrinoP.Perp()*(1-cos(geantDeltaPhi)));
      
      hA[20]->Fill(Mt);  
      hA[21]->Fill(gMt);

      hA[22]->Fill(Mt,T.cluster.ET);
      hA[23]->Fill(gMt-Mt);

      //Kinematics 
      //reconstruct W pL from reconstructed quantities
      float trueWpL=mWP.z();
      float eleTheta=T.primP.Theta();
      float ratioE=T.cluster.energy/40.0;  
      float pLRecoPlus=80.0*(ratioE)*((cos(eleTheta))+sqrt(cos(eleTheta)*cos(eleTheta)+sin(eleTheta)*sin(eleTheta)*(1-ratioE*ratioE)))/(ratioE*ratioE*sin(eleTheta)*sin(eleTheta));//+ sqrt solution
      float pLRecoMinus=80.0*(ratioE)*((cos(eleTheta))-sqrt(cos(eleTheta)*cos(eleTheta)+sin(eleTheta)*sin(eleTheta)*(1-ratioE*ratioE)))/(ratioE*ratioE*sin(eleTheta)*sin(eleTheta));//- sqrt solution
      hA[29]->Fill(pLRecoPlus);
      hA[30]->Fill(trueWpL-pLRecoPlus);
      hA[31]->Fill(trueWpL,pLRecoPlus);
      
      hA[32]->Fill(pLRecoMinus);
      hA[33]->Fill(trueWpL-pLRecoMinus);
      hA[34]->Fill(trueWpL,pLRecoMinus);

      const StMuTrack *prTr=T.prMuTrack; assert(prTr);
      float p_chrg=prTr->charge();
      if(p_chrg > 0) continue;

      float eleEta=T.primP.Eta();
      //sort 2 solutions by electron eta
      if(eleEta<-0.8) {
	hA[35]->Fill(trueWpL,pLRecoMinus);
	hA[37]->Fill(trueWpL,pLRecoPlus);
      }
      else if(eleEta>0.8) {
	hA[36]->Fill(trueWpL,pLRecoMinus);
	hA[38]->Fill(trueWpL,pLRecoPlus);
      }
      
      if(T.cluster.ET < 30) continue; //only W's we find in data
      //Correlate W pL with electron E in 3 electron eta ranges
      if(eleEta < -0.8) {
	hA[39]->Fill(mWP.z(),T.cluster.energy);
	hA[42]->Fill(T.cluster.energy);
      }
      if(eleEta > 0.8){
	hA[40]->Fill(mWP.z(),T.cluster.energy);
	hA[43]->Fill(T.cluster.energy);
      }      
      if(eleEta > -0.1 && eleEta < 0.1) {
	hA[41]->Fill(mWP.z(),T.cluster.energy);
	hA[44]->Fill(T.cluster.energy);	
      }

    }
  }
}


// $Log: St2009pubMcMaker.cxx,v $
// Revision 1.7  2011/09/14 14:23:21  stevens4
// update used for cross section PRD paper
//
// Revision 1.6  2010/05/03 19:54:35  stevens4
// only try to calc effic if W->e+nu is found in McEvent
//
// Revision 1.5  2010/03/14 22:50:31  balewski
// *** empty log message ***
//
// Revision 1.4  2010/02/19 21:04:10  stevens4
// cleanup unused variables
//
// Revision 1.3  2010/02/18 19:48:10  stevens4
// add more effic histograms and cleanup
//
// Revision 1.2  2010/01/21 17:54:31  stevens4
// add effic histos and charge seperated background plots
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
// Revision 1.1  2009/11/23 21:11:18  balewski
// start
//
