// $Id: St2011WMaker.cxx,v 1.21 2013/10/14 13:07:31 stevens4 Exp $
//
//*-- Author : Jan Balewski, MIT
//*-- Author for Endcap: Justin Stevens, IUCF
//*-- Author for JetFinder/JetReader interface: Ilya Selyuzhenkov, IUCF

#include <TH1.h>
#include <TH2.h>
#include <TTree.h>
#include <TString.h>
#include <StMessMgr.h>
#include <StThreeVectorF.hh>

//MuDst
#include <StMuDSTMaker/COMMON/StMuDstMaker.h>
#include <StMuDSTMaker/COMMON/StMuDst.h>
#include <StMuDSTMaker/COMMON/StMuEvent.h>

#include "StEmcUtil/database/StBemcTables.h"
#include "StEmcUtil/geometry/StEmcGeom.h"

#include "StEEmcUtil/database/StEEmcDb.h"       
#include "StEEmcUtil/database/EEmcDbItem.h"     
#include "StEEmcUtil/EEmcGeom/EEmcGeomSimple.h"
#include "StEEmcUtil/StEEmcSmd/EEmcSmdGeom.h"

//new jet tree format
#include "StSpinPool/StJetEvent/StJetEvent.h"
#include "StSpinPool/StJetEvent/StJetVertex.h"
#include "StSpinPool/StJetEvent/StJetCandidate.h"

#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"
#include "WeventDisplay.h"
#include "St2011WMaker.h"

ClassImp(St2011WMaker)

//_____________________________________________________________________________
//
St2011WMaker::St2011WMaker(const char *name):StMaker(name){
  char muDstMakerName[]="MuDst"; 
  mMuDstMaker=(StMuDstMaker*)GetMaker(muDstMakerName);  
  coreTitle=name;

  if(!mMuDstMaker) { //load tree if no MuDst
    mTreeChain=new TChain("mWtree","W candidate events");
    index=0;
  }

  //must have either MuDst or W tree
  assert(mMuDstMaker || mTreeChain);

  mJetTreeChain=new TChain("jet","Jet Tree");
  indexJet=0;
  mJetEvent = 0;
  mJetEvent_noEEMC = 0;
    
  if(!mJetTreeChain)
    LOG_WARN<<GetName()<<Form("::constructor() NO JETS , W-algo is not working properly, continue")<<endm;
  
  // preset or clear some params
  par_l2bwTrgID=parE_l2ewTrgID=0;
 
  setHList(0);
  setHListTpc(0);
  setMC(0);
  nInpEve= nTrigEve= nAccEve=0; 

  //... MC trigger simulator
  par_l0emulAdcThresh=30;
  par_l2emulSeedThresh=5.0;
  par_l2emulClusterThresh=12.0;

  //.. vertex
  par_minPileupVert=3; // to reject events w/o TPC, lower it for MC
  par_vertexZ=100; // (cm)

  //... towers
  par_kSigPed=3; // rawADC-ped cut off
  par_AdcThres=8; // ADC threshold to avoid correlated noise
  par_maxADC=200.; // (adc chan) on the highest tower in events 
    
  //... Barrel Algo
  par_clustET=14.; // (GeV/c) 2x2 cluster ET 
  par_clustFrac24=0.95; // ET ratio 2x2/4x4 cluster
  par_nearTotEtFrac=0.88;  // ratio 2x2/near Tot ET 
  par_delR3D=7.; // cm, dist between projected track and center of cluster 
  par_leptonEtaLow=-1.5; // bracket acceptance
  par_leptonEtaHigh=1.5; // bracket acceptance
  par_ptBalance=14.; // (GeV), ele cluster vector + jet sum vector
  //... track
  par_nFitPts=15; // hits on the track
  par_nHitFrac=0.51;
  par_trackRin=90;  par_trackRout=160; // cm
  par_trackPt=10.;//GeV 
  par_highET=25.; // (GeV), cut-off for final Barrel W-cluster 
  par_QET2PTlow = 0.4;  // low cut on |Q*ET/PT|
  par_QET2PThigh = 1.8; // high cut on |W*ET/PT|

  //... Endcap Algo
  parE_trackEtaMin=0.7; // avoid bad extrapolation to ESMD
  parE_clustET=14.; // (GeV/c) 2x2 cluster ET  
  parE_clustFrac24=0.90; // ET ratio 2x2/4x4 cluster
  parE_nearTotEtFrac=0.85;  // ratio 2x2/near Tot ET 
  parE_delR3D=10.; // cm, dist between projected track and center of cluster 
  parE_leptonEtaLow=0.7; // bracket acceptance
  parE_leptonEtaHigh=2.5; // bracket acceptance
  parE_ptBalance=14.; // (GeV), ele cluster vector + jet sum vector
  //... track
  parE_nFitPts=5; // hits on the track
  parE_nHitFrac=0.51;
  parE_trackRin=120;  parE_trackRout=70; // cm
  parE_trackPt=7.;//GeV 
  parE_nSmdStrip=20;
  parE_esmdGL=3; // 2N+1=7 size of the integration gate len
  parE_esmdWL=7; // 2N+1=15 size of the allowed window len

  parE_smdRatio=0.6;
  parE_highET=25.; // (GeV), cut-off for final Endcap W-cluster 
  parE_QET2PTlow = 0.4;  // low cut on |Q*ET/PT|
  parE_QET2PThigh = 1.8; // high cut on |W*ET/PT|

  assert(2*parE_nSmdStrip+1==41);// as hardcoded in Wtree for esmdShower[mxEsmdPlane][], it should be solved by using <vector> or TArray - left for next year to be fixed
  assert(parE_esmdGL<=parE_esmdWL); // if equal then peak adjusting is disabled
  assert(parE_esmdWL<parE_nSmdStrip);


  //... search for W's
  par_nearDeltaR=0.7; //(~rad) near-cone size
  par_awayDeltaPhi=0.7; // (rad) away-'cone' size
    
  setEtowScale(1.0); 
  setBtowScale(1.0);
  
  mRunNo=0;
  nRun=0;
  hbxIdeal=0;

  // irrelevant for W analysis
  par_DsmThres=31; // only for monitoring
  parE_DsmThres=31; // only for monitoring
  par_maxDisplEve=1; // # of displayed selected events

}


//_____________________________________________________________________________
//
Int_t 
St2011WMaker::Init(){
  assert(HList);
  initHistos();
  initEHistos();

  // init TPC histos
  if(mMuDstMaker) {
    for(int isec=0;isec<mxTpcSec;isec++) {
      int sec=isec+1;
      float Rin=par_trackRin,Rout=par_trackRout;
      float RinE=parE_trackRin,RoutE=parE_trackRout;
      
      mTpcFilter[isec].setCuts(par_nFitPts,par_nHitFrac,Rin,Rout);
      mTpcFilterE[isec].setCuts(parE_nFitPts,parE_nHitFrac,RinE,RoutE);
      
      mTpcFilter[isec].init("sec",sec,HListTpc,true);
      mTpcFilterE[isec].init("secEemcTr",sec,HListTpc,false);
    }
  }

  mJetTreeChain->SetBranchAddress(mJetTreeBranch,&mJetEvent);
  mJetTreeChain->SetBranchAddress(mJetTreeBranch_noEEMC,&mJetEvent_noEEMC);

  if(mMuDstMaker) { 
    //only need DB tables for MuDst analysis
    mBarrelTables = new StBemcTables();
    mDbE = (StEEmcDb*)GetDataSet("StEEmcDb"); 
    assert(mDbE);
  }
  else { 
    //setup for reading in tree
    wEve=new Wevent2011();
    mTreeChain-> SetBranchAddress("wEve",&wEve);
  }

  mBtowGeom = StEmcGeom::instance("bemc");
  mBSmdGeom[kBSE] = StEmcGeom::instance("bsmde");
  mBSmdGeom[kBSP] = StEmcGeom::instance("bsmdp");        
  geomE= new EEmcGeomSimple();         
  geoSmd= EEmcSmdGeom::instance();
  initGeom();
 
  wDisaply= new WeventDisplay(this,par_maxDisplEve);

  if(isMC) par_minPileupVert=1;

  //tree only written during MuDst analysis
  if(mMuDstMaker) { 
    mTreeFile=new TFile(mTreeName,"recreate");
    mTreeFile->cd();
    
    wEve=new Wevent2011();
    mWtree=new TTree("mWtree","W candidate Events");
    mWtree->Branch("wEve","Wevent2011",&wEve);
  }

  return StMaker::Init();
}

//________________________________________________
//________________________________________________
Int_t
St2011WMaker::InitRun(int runNo){
  LOG_INFO<<Form("::InitRun(%d) start",runNo)<<endm;
  if(!isMC) assert(mRunNo==0);  // to prevent run merging - it was not tested 
  if(mMuDstMaker) {
    mBarrelTables->loadTables(this );
    mRunNo=runNo;
  }
  else {
    mRunNo=wEve->runNo; 
  }

  //barrel algo params
  LOG_INFO<<Form("::InitRun(%d) %s done\n Barrel W-algo params: trigID L2BW=%d isMC=%d\n TPC: nPileupVert>%d, vertex |Z|<%.1fcm, primEleTrack: nFit>%d, hitFrac>%.2f Rin<%.1fcm, Rout>%.1fcm, PT>%.1fGeV/c\n BTOW ADC: kSigPed=%d AdcThr>%d maxAdc>%.0f clustET>%.1f GeV  ET2x2/ET4x4>%0.2f  ET2x2/nearTotET>%0.2f\n dist(track-clust)<%.1fcm, nearDelR<%.1f\n W selection highET>%.1f awayDelPhi<%.1frad  ptBalance>%.1fGeV  %.1f<leptonEta<%.1f ",
		 mRunNo, coreTitle.Data(), par_l2bwTrgID,isMC,
		 par_minPileupVert,par_vertexZ,
		 par_nFitPts,par_nHitFrac,  par_trackRin,  par_trackRout, par_trackPt,
		 par_kSigPed, par_AdcThres,par_maxADC,par_clustET,par_clustFrac24,par_nearTotEtFrac,
		 par_delR3D,par_nearDeltaR,
		 par_highET,par_awayDeltaPhi,par_ptBalance,par_leptonEtaLow,par_leptonEtaHigh
		 )<<endm;
  //endcap algo params
  cout<<Form("\n Endcap W-algo params: trigID: L2EW=%d isMC=%d\n TPC: nPileupVert>%d, vertex |Z|<%.1fcm, primEleTrack: nFit>%d, hitFrac>%.2f Rin<%.1fcm, Rout>%.1fcm, PT>%.1fGeV/c\n ETOW ADC: kSigPed=%d AdcThr>%d maxAdc>%.0f clustET>%.1f GeV  ET2x1/ET4x4>%0.2f  ET2x1/nearTotET>%0.2f\n dist(track-clust)<%.1fcm, nearDelR<%.1f\n W selection highET>%.1f awayDelPhi<%.1frad  ptBalance>%.1fGeV , \n ESMD: nSmdStrip=%d, gateL=%d windowL=%d smdRatio=%.2f",
		 parE_l2ewTrgID,isMC,
		 par_minPileupVert,par_vertexZ,
		 parE_nFitPts,parE_nHitFrac,parE_trackRin,parE_trackRout,parE_trackPt,
		 par_kSigPed,par_AdcThres,par_maxADC,parE_clustET,parE_clustFrac24,parE_nearTotEtFrac,
		 parE_delR3D,par_nearDeltaR,
	     par_highET,par_awayDeltaPhi,parE_ptBalance, parE_nSmdStrip, parE_esmdGL, parE_esmdWL, parE_smdRatio
		 )<<endl;
  cout<<Form("\n EtowScaleFact=%.2f  BtowScaleFacor=%.2f" ,par_etowScale, par_btowScale)<<endl;

  if(mMuDstMaker) {
    // initialization of TPC cuts is run dependent (only MuDst analysis)
    for(int isec=0;isec<mxTpcSec;isec++) {
      int sec=isec+1;
      float Rin=par_trackRin,Rout=par_trackRout;
      float RinE=parE_trackRin,RoutE=parE_trackRout;
      //.... Rin ..... changes
      
      //Run 9 (final)
      if(sec==4  && mRunNo>=10090089 && mRunNo<=11000000 ) Rin=125.;
      if(sec==11 && mRunNo>=10083013 && mRunNo<=11000000 ) Rin=125.;
      if(sec==15 && mRunNo>=10088096 && mRunNo<=10090112 ) Rin=125.;
      
      //Run 11 (only sector 20 which is already excluded)
      
      //Run 12 (final) These sectors have dead inner padrows for all of pp510 (run#300+nn is for private MC)
      if((sec==5 || sec==6 || sec==7 || sec==21) && (mRunNo>=13000000 || mRunNo/100==3) ) Rin=125.; 

      //.... Rout ..... changes
      
      //Run 9 (final)
      if(sec==5 && mRunNo>=10098029 && mRunNo<=11000000) Rout=140.;
      if(sec==6 && mRunNo<=11000000 ) Rout=140.;
      if(sec==20 && mRunNo>=10095120 && mRunNo<=10099078 ) Rout=140.;
      
      //Run 11 (only sector 20 which is already excluded)
      
      //Run 12 (final) No sectors with outer padrow issues

      // initialize cuts for each run individually
      mTpcFilter[isec].setCuts(par_nFitPts,par_nHitFrac,Rin,Rout);
      mTpcFilterE[isec].setCuts(parE_nFitPts,parE_nHitFrac,RinE,RoutE);
    }
  }

  //.... spinDB monitoring
  if(mMuDstMaker && spinDb)
    {  char txt[1000],txt0[100];
    sprintf(txt0,"bxIdeal%d",nRun);
    sprintf(txt,"intended fill pattern  R%d-%d vs. bXing; %s", runNo,nRun,spinDb->getV124comment());
    nRun++;
    Tfirst=int(2e9); Tlast=-Tfirst;
    hbxIdeal=new TH1F(txt0,txt,128,-0.5,127.5);
    hbxIdeal->SetFillColor(kYellow);
    HList->Add(hbxIdeal);
    
    spinDb->print(0); // 0=short, 1=huge
    for(int bx=0;bx<120;bx++){
      if(spinDb->isBXfilledUsingInternalBX(bx))  hbxIdeal->Fill(bx);   
    }
    
  }

  return kStOK;
}

//________________________________________________
//________________________________________________
Int_t
St2011WMaker::Finish(){
  if(mMuDstMaker){
    LOG_INFO<<endl<<"Output tree file "<<mTreeName<<endl<<endl;
    mTreeFile->Write();
    mTreeFile->Close();

    if(hbxIdeal) {
      char txt[1000];
      sprintf(txt,"events T= %d %d",Tfirst,Tlast);
      printf("Finish run=%d , events time range %s\n",mRunNo,txt);
      hbxIdeal->GetYaxis()->SetTitle(txt);    
    }
  }
  return StMaker::Finish();
}

//________________________________________________
//________________________________________________
Int_t
St2011WMaker::FinishRun(int runNo){
  LOG_INFO<<Form("::FinishRun(%d)",runNo)<<endm;
return kStOK;
}

//________________________________________________
//________________________________________________
void
St2011WMaker::Clear(const Option_t*){
  wEve->clear();
}

//--------------------------------------
//--------------------------------------
Int_t 
St2011WMaker::Make(){
  nInpEve++; 

  if(mMuDstMaker){ //standard MuDst analysis
    wEve->id=mMuDstMaker->muDst()->event()->eventId();
    wEve->runNo=mMuDstMaker->muDst()->event()->runId();
    wEve->time=mMuDstMaker->muDst()->event()->eventInfo().time();
    wEve->zdcRate=mMuDstMaker->muDst()->event()->runInfo().zdcCoincidenceRate();
    int T=wEve->time;
    if(Tlast<T) Tlast=T;
    if(Tfirst>T) Tfirst=T;
    
    hA[13]->Fill(wEve->zdcRate);

    // get next event from jet tree
    mJetTreeChain->GetEntry(indexJet++);

    const char *afile = mMuDstMaker->GetFile();
    //printf("inpEve=%d eveID=%d daqFile=%s\n",nInpEve, wEve->id,afile);
    if(nInpEve%200==1) printf("\n-----in---- %s, muDst nEve: inp=%d trig=%d accpt=%d daqFile=%s\n", GetName(),nInpEve,nTrigEve, nAccEve,afile);

    hA[0]->Fill("inp",1.); 
    hE[0]->Fill("inp",1.); 
    
    int btowStat=accessBTOW(); // get energy in BTOW
    int etowStat=accessETOW(); // get energy in ETOW
   
    int btrig=accessBarrelTrig();
    int etrig=accessEndcapTrig();

    if( btrig && etrig )  {mWtree->Fill(); return kStOK;} //skip event w/o valid trig ID
 
    nTrigEve++; 
    
    if(accessVertex()) {
      mWtree->Fill();
      return kStOK; //skip event w/o ~any reasonable vertex  
    }

    if( accessTracks()) {mWtree->Fill(); return kStOK;} //skip event w/o ~any highPt track

    accessBSMD();// get energy in BSMD
    accessESMD();// get energy in ESMD
    accessEPRS();// get energy in EPRS

    mWtree->Fill(); //write all events w/ pt>10 track to tree
    
    if(wEve->l2bitET && wEve->bemc.tileIn[0]==1) hA[0]->Fill("B-in",1.0);
    if(wEve->l2EbitET && wEve->etow.etowIn==1)    hE[0]->Fill("E-in",1.0);
    if(wEve->l2bitET && !btowStat) hA[0]->Fill("B200",1.0);
    if(wEve->l2EbitET && !etowStat) hE[0]->Fill("E200",1.0);

    if( btowStat && etowStat ) return kStOK; //skip event w/o energy in BTOW && ETOW   
    
    if(mJetTreeChain) {// just QA plots for jets
      getJetEvent(); //get input jet info (new jet format)
      for (int i_jet=0; i_jet<mJetEvent->vertex()->numberOfJets(); ++i_jet){ //just first vertex
	StJetCandidate* jet = mJetEvent->vertex()->jet(i_jet); 
	float jet_pt = jet->pt();
	float jet_eta = jet->eta();
	float jet_phi = jet->phi();
	hA[117]->Fill(jet_eta,jet_phi);
	hA[118]->Fill(jet_pt);
      }
    }    
  }
  else { //analysis of W tree
    if(getEvent(index++,indexJet++)==kStEOF) return kStEOF; //get next event from W and jet tree

    hA[13]->Fill(wEve->zdcRate);

    //allow for manual scale adjustment of BTOW energy (careful!) 
    for(int i=0; i<4800; i++) wEve->bemc.eneTile[0][i]*=par_btowScale;

    if(nInpEve%200==1) printf("\n-----in---- %s, W-Tree  nEve: inp=%d \n", GetName(),nInpEve);//,nTrigEve, nAccEve,afile);  
    
    //fill some bins in muStatEve histos for checks
    hA[0]->Fill("inp",1.); 
    hE[0]->Fill("inp",1.);

    //fill trigger bins for counter histos
    if(wEve->l2bitET) hA[0]->Fill("L2bwET",1.); 
    if(wEve->l2bitRnd) hA[0]->Fill("L2bwRnd",1.);
    if(wEve->l2EbitET) hE[0]->Fill("L2ewET",1.);
    if(wEve->l2EbitRnd) hE[0]->Fill("L2ewRnd",1.);

    if(!wEve->l2bitET && !wEve->l2EbitET) return kStOK; //skip event w/o valid trig ID
    nTrigEve++;

    //fill tpc bins
    int nVerR=0; int nTrOK=0;
    for(uint iv=0;iv<wEve->vertex.size();iv++) {
      if(wEve->vertex[iv].rank > 0) nVerR++;
      if(wEve->vertex[iv].eleTrack.size() > 0) nTrOK++;
    }
    if(wEve->l2bitET && nVerR>0) hA[0]->Fill("vertZ",1.);
    if(wEve->l2EbitET && wEve->vertex.size()>0) hE[0]->Fill("vertZ",1.);
    if(wEve->l2bitET && nTrOK>0) hA[0]->Fill("Pt10",1.);
    if(wEve->l2EbitET && nTrOK>0) hE[0]->Fill("Pt10",1.);

    if(nTrOK<=0) return kStOK;

    //fill some B/ETOW bins
    if(wEve->l2bitET && wEve->bemc.tileIn[0]==1) hA[0]->Fill("B-in",1.0);
    if(wEve->l2EbitET && wEve->etow.etowIn==1)    hE[0]->Fill("E-in",1.0);
    if(wEve->l2bitET && wEve->bemc.maxAdc>par_maxADC) hA[0]->Fill("B200",1.0);
    if(wEve->l2EbitET && wEve->etow.maxAdc>par_maxADC) hE[0]->Fill("E200",1.0);
    
    if(wEve->bemc.maxAdc<par_maxADC && wEve->etow.maxAdc<par_maxADC) return kStOK; //skip event w/o energy in BTOW && ETOW 
    
    if(mJetTreeChain) {// just QA plots for jets
      getJetEvent(); //get input jet info (new jet format)
      for (int i_jet=0; i_jet<mJetEvent->vertex()->numberOfJets(); ++i_jet){
	StJetCandidate* jet = mJetEvent->vertex()->jet(i_jet);
	float jet_pt = jet->pt();
	float jet_eta = jet->eta();
	float jet_phi = jet->phi();
	hA[117]->Fill(jet_eta,jet_phi);
	hA[118]->Fill(jet_pt);
      }
    }
  }
  
  //find barrel candidates
  extendTrack2Barrel(); 
  int bmatch=matchTrack2BtowCluster(); 

  //find endcap candidates
  extendTrack2Endcap();
  int ematch=matchTrack2EtowCluster();

  if(bmatch && ematch) return kStOK; //no matched BTOW or ETOW clusters

  nAccEve++;
 
  /* now it starts to get interesting, process every track 
     on the list  till the end. */
  findNearJet();
  findAwayJet();

  if(mJetTreeChain) {
    findPtBalance();
    if(!bmatch) tag_Z_boson();
  }

  //endcap specific analysis
  if(!ematch) {
    analyzeESMD();
    analyzeEPRS();
  }  

  if(!bmatch) find_W_boson();
  if(!ematch) findEndcap_W_boson();
  if(nAccEve<2 ||nAccEve%1000==1 ) wEve->print(0x0,isMC);
  
  return kStOK;
}


//--------------------------------------
//--------------------------------------
void 
St2011WMaker::initGeom(){

  //...... BTOW ...........
  memset(mapBtowIJ2ID,0,sizeof(mapBtowIJ2ID));
  for(int softID=1; softID<=mxBtow; softID++) {    
    //........... querry BTOW geom
    int m,e,s;
    mBtowGeom->getBin(softID,m,e,s);    
    float etaF,phiF;
    mBtowGeom->getEta(m,e,etaF);
    mBtowGeom->getPhi(m,s,phiF);  // -pi <= phi < pi
    
    int iEta, iPhi;
    assert(L2algoEtaPhi2IJ(etaF,phiF,iEta, iPhi)==0); // tower must be localized at the known position
    int IJ=iEta+ iPhi*mxBTetaBin;
    assert(mapBtowIJ2ID[IJ ]==0); // avoid overlaping mapping
    mapBtowIJ2ID[IJ ]=softID;
    
    Float_t x,y,z;
    assert( mBtowGeom->getXYZ(softID,x,y,z)==0);
    positionBtow[softID-1]=TVector3(x,y,z);
  }// end of loop over towers

  //...... BSMD-E, -P ...........
  for(int iep=0;iep<mxBSmd;iep++) {
    for(int softID=1; softID<=mxBStrips; softID++) {    
      Float_t x,y,z;
      assert( mBSmdGeom[iep]->getXYZ(softID,x,y,z)==0);
      positionBsmd[iep][softID-1]=TVector3(x,y,z);
    }// end of loop over towers
  }

  //...... ETOW .............  
  for(int isec=0;isec<mxEtowSec;isec++){
    for(int isub=0;isub<mxEtowSub;isub++){
      for(int ieta=0;ieta<mxEtowEta;ieta++){
	positionEtow[isec*mxEtowSub+isub][ieta]=geomE->getTowerCenter(isec,isub,ieta);
      }
    }
  }
  
}


//--------------------------------------
//--------------------------------------
int // returns error code
St2011WMaker::L2algoEtaPhi2IJ(float etaF,float phiF,int &iEta, int &iPhi) {
  if( phiF<0) phiF+=2*C_PI; // I want phi in [0,2Pi]
  if(fabs(etaF)>=0.99) return -1;
  int kEta=1+(int)((etaF+1.)/0.05);
  iPhi=24-(int)( phiF/C_PI*60.);
  if(iPhi<0) iPhi+=120;
  // convention:  iPhi=[0,119], kEta=[1,40]  
  iEta=kEta-1;
  //printf("IJ=%d %d\n",iEta,iPhi);
  if(iEta<0 || iEta>=mxBTetaBin) return -2;
  if(iPhi<0 || iPhi>=mxBTphiBin) return -3;
  return 0;
}


//________________________________________________
//________________________________________________
void 
St2011WMaker::getJetEvent(){
  if(mJetTreeChain==0)
    return;
  
  // if jets are out of sink for some reason find matching event
  while(mJetEvent->eventId()!=wEve->id || mJetEvent->runId()!=wEve->runNo) {
    mJetTreeChain->GetEntry(indexJet++);
  }
  
  assert(mJetEvent->eventId()==wEve->id);
  assert(mJetEvent->runId()==wEve->runNo);
  assert(mJetEvent_noEEMC->eventId()==wEve->id);
  assert(mJetEvent_noEEMC->runId()==wEve->runNo);
  return;

}


// ----------------------------------------------------------------------------
Int_t St2011WMaker::getEvent(Int_t i, Int_t ijet)
{
  Int_t stat=mTreeChain->GetEntry(i);
  Int_t statJet=mJetTreeChain->GetEntry(ijet);
  if (!stat && !statJet) return kStEOF;
  return kStOK;
}

// ----------------------------------------------------------------------------
void St2011WMaker::chainFile( const Char_t *file )
{

  TString fname=file;
  cout<<"Chain W tree files"<<endl;
  if ( !fname.Contains("tree.root") ) return;
  cout << "+ " << fname << endl;
  mTreeChain->Add(fname);
}

// ----------------------------------------------------------------------------
void St2011WMaker::chainJetFile( const Char_t *file )
{

  TString fname=file;
  cout<<"Chain jet tree files"<<endl;
  if ( !fname.Contains("jets_") ) return;
  cout << "+ " << fname << endl;
  mJetTreeChain->Add(fname);
}

// $Log: St2011WMaker.cxx,v $
// Revision 1.21  2013/10/14 13:07:31  stevens4
// Move initialization of TPC QA histos from Init() to InitRun()
//
// Revision 1.20  2013/09/13 19:33:13  stevens4
// Updates to code for combined 2011+2012 result presented to spin PWG 9.12.13
//
// Revision 1.19  2012/09/26 14:20:59  stevens4
// use PtBal cos(phi) for WB and WE algos and use Q*ET/PT for barrel charge sign
//
// Revision 1.18  2012/09/21 16:59:09  balewski
// added ESMD peak adjustement - partialy finished
//
// Revision 1.17  2012/09/18 22:30:16  stevens4
// change to new jet tree format with access to all rank>0 vertices
//
// Revision 1.16  2012/09/17 03:29:29  stevens4
// Updates to Endcap algo and Q*ET/PT charge separation
//
// Revision 1.15  2012/08/21 18:29:16  stevens4
// Updates to endcap W selection using ESMD strip ratio
//
// Revision 1.14  2012/08/21 17:40:09  stevens4
// Revert to previous version
//
// Revision 1.12  2012/08/07 21:06:38  stevens4
// update to tree analysis to produce independent histos in a TDirectory for each eta-bin
//
// Revision 1.11  2012/07/13 20:53:16  stevens4
// Add filling of empty events in W tree
// Minor modifications to histograms
//
// Revision 1.10  2012/07/13 16:11:44  balewski
// minor clenup, prevent crash in Finish if zero input events, now it runs on M-C events as well
//
// Revision 1.9  2012/07/12 20:49:21  balewski
// added spin info(star: bx48, bx7, spin4) and maxHtDSM & BTOW to Wtree
// removed dependence of spinSortingMaker from muDst
// Now Wtree can be spin-sorted w/o DB
// rdMu.C & readWtree.C macros modified
// tested so far on real data run 11
// lot of misc. code shuffling
//
// Revision 1.8  2012/07/06 17:47:02  stevens4
// Updates for tree reader
//
// Revision 1.7  2012/06/25 20:53:15  stevens4
// algo and histo cleanup
//
// Revision 1.6  2012/06/22 20:45:58  balewski
// change Rin for M-C matched to 2012 data
//
// Revision 1.5  2012/06/18 18:28:00  stevens4
// Updates for Run 9+11+12 AL analysis
//
// Revision 1.4  2011/02/25 06:03:32  stevens4
// addes some histos and enabled running on MC
//
// Revision 1.3  2011/02/17 04:16:14  stevens4
// move sector dependent track QA cuts before track pt>10 cut and lower par_clustET and par_ptBalance thresholds to 14 GeV
//
// Revision 1.2  2011/02/14 02:35:21  stevens4
// change back to 10 GeV track pt and 15 GeV cluster ET as default for both B + E algos
//
// Revision 1.1  2011/02/10 20:33:22  balewski
// start
//
