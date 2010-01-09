// $Id: St2009WMaker.cxx,v 1.4 2010/01/09 00:07:16 stevens4 Exp $
//
//*-- Author : Jan Balewski, MIT
//*-- Author for Endcap: Justin Stevens, IUCF
//*-- Author for JetFinder/JetReader interface: Ilya Selyuzhenkov, IUCF
// test 1, after DNP2009 tag, Jan

#include <TH1.h>
#include <TH2.h>
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

//jets
#include "StSpinPool/StJets/StJet.h"
#include "StSpinPool/StJets/StJets.h"
#include "StJetMaker/StJetMaker.h"
#include "StSpinPool/StSpinDbMaker/StSpinDbMaker.h"
#include "StJetMaker/StJetReader.h"
#include "StJetMaker/StJetSkimEventMaker.h"


#include "WeventDisplay.h"
#include "St2009WMaker.h"

ClassImp(St2009WMaker)

//_____________________________________________________________________________
//
St2009WMaker::St2009WMaker(const char *name):StMaker(name){
  char muDstMakerName[]="MuDst"; 
  mMuDstMaker= (StMuDstMaker*)GetMaker(muDstMakerName);  assert(mMuDstMaker);
  mJetReaderMaker = (StJetReader*)GetMaker("JetReader");  assert(mJetReaderMaker);

  // preset or clear some params
  par_bht3TrgID= par_l2wTrgID=0;
  setHList(0);
  setMC(0);
  nInpEve= nTrigEve= nAccEve=0; 

  //... MC trigger simulator
  par_l0emulAdcThresh=30;
  par_l2emulSeedThresh=5.0;
  par_l2emulClusterThresh=13.0;

  //.. vertex
  par_minPileupVert=3; // to reject events w/o TPC, lower it for MC
  par_vertexZ=100; // (cm)

  //... track
  par_nFitPts=15; // hits on the track
  par_nHitFrac=0.51;
  par_trackRin=90;  par_trackRout=170; // cm
  par_trackPt=10.;//GeV

  //... ETOW
  par_kSigPed=3; // rawADC-ped cut off
  par_AdcThres=8; // ADC threshold to avoid correlated noise
  par_maxADC=200.; // (adc chan) on the highest tower in events
  par_clustET=15.; // (GeV/c) 2x2 cluster ET
  par_clustFrac24=0.95; // ET ratio 2x2/4x4 cluster
  par_nearDeltaR=0.7; //(~rad) near-cone size
  par_nearTotEtFrac=0.88;  // ratio 2x2/near Tot ET 
  par_smallNearDeltaR=0.1; //(~rad) small near-cone size for tpc tracks

  //... track-cluster
  par_delR3D=7.; // cm, dist between projected track and center of cluster 

  //... near-,away-side track counter thresholds      
  par_countTrPt=1.; // (GeV), track pT thres
  par_countTowEt=1.; // (GeV), tower ET thres
  
  //... search for W's
  par_awayDeltaPhi=0.7; // (rad) away-'cone' size
  par_awayTotET=30.; // (GeV), maximal allowed away-cone  ET
  par_highET=28.; // (GeV), cut-off for final W-cluster ET
  par_ptBalance=15.; // (GeV), ele cluster vector + jet sum vector

  par_useEtow=1; // flag for how to use ETOW in algo
  // flag == 0 -> don't use ETOW
  // flag >= 1 -> use in event display plots
  // flag >= 2 -> use in away sum
  // flag == 3 -> use in near sum
  setEtowScaleMC(1.0); // for old the Endcap geometr you need ~1.3
  setBtowScaleMC(1.0);
  

  mRunNo=0;

  // irrelevant for W analysis
  par_DsmThres=28; // only for monitoring
  par_maxDisplEve=1; // # of displayed selected events

}


//_____________________________________________________________________________
//
Int_t 
St2009WMaker::Init(){
  assert(HList);
  initHistos();
  mBarrelTables = new StBemcTables();
  mBtowGeom = StEmcGeom::instance("bemc");
  mBSmdGeom[kBSE] = StEmcGeom::instance("bsmde");
  mBSmdGeom[kBSP] = StEmcGeom::instance("bsmdp");
  wDisaply= new WeventDisplay(this,par_maxDisplEve);
 
  mDbE = (StEEmcDb*)GetDataSet("StEEmcDb"); 
  assert(mDbE);                                   
  geomE= new EEmcGeomSimple();                    

  initGeom();
  
  if(isMC) par_minPileupVert=1;
  return StMaker::Init();
}

//________________________________________________
//________________________________________________
Int_t
St2009WMaker::InitRun(int runNo){
  LOG_INFO<<Form("::InitRun(%d) start",runNo)<<endm;
  mBarrelTables->loadTables(this );
  
#if 0 
  float airPres=St_tpcGasC::instance()-> barometricPressure();
  LOG_INFO<<Form("::InitRun(%d) AirPressure=%.2f",runNo, airPres)<<endm;
  if(runNo==1000000) {
    LOG_WARN<<Form("::InitRun(%d) ??? , it is OK for M-C ",runNo)<<endm;
  }
#endif

  mRunNo=runNo;
   LOG_INFO<<Form("::InitRun(%d) done, W-algo params: trigID: bht3=%d L2W=%d  isMC=%d\n TPC: nPileupVert>%d, vertex |Z|<%.1fcm, primEleTrack: nFit>%d, hitFrac>%.2f Rin<%.1fcm, Rout>%.1fcm, PT>%.1fGeV/c\n BTOW ADC: kSigPed=%d AdcThr>%d maxAdc>%.0f clustET>%.1f GeV  ET2x2/ET4x4>%0.2f  ET2x2/nearTotET>%0.2f\n dist(track-clust)<%.1fcm, nearDelR<%.1f\n Counters Thresholds: track>%.1f GeV, tower>%.1f GeV  Use ETOW: flag=%d mcScaleFact=%.2f\nmcBtowScaleFacor=%.2f\n W selection highET>%.1f awayDelPhi<%.1frad awayTotET<%.1fGeV ptBalance>%.1fGeV",
		 mRunNo,par_bht3TrgID, par_l2wTrgID,isMC,
		 par_minPileupVert,par_vertexZ,
		 par_nFitPts,par_nHitFrac,  par_trackRin,  par_trackRout, par_trackPt,
		  par_kSigPed, par_AdcThres,par_maxADC,par_clustET,par_clustFrac24,par_nearTotEtFrac,
		  par_delR3D,par_nearDeltaR,
		  par_countTrPt,par_countTowEt,par_useEtow,par_mcEtowScale,
		  par_mcBtowScale,
		  par_highET,par_awayDeltaPhi,par_awayTotET,par_ptBalance
		 )<<endm;

   return kStOK;
}

//________________________________________________
//________________________________________________
Int_t
St2009WMaker::FinishRun(int runNo){
  LOG_INFO<<Form("::FinishRun(%d)",runNo)<<endm;
return kStOK;
}

//________________________________________________
//________________________________________________
void
St2009WMaker::Clear(const Option_t*){
  wEve.clear();
}


//--------------------------------------
//--------------------------------------
Int_t 
St2009WMaker::Make(){
  nInpEve++; 
  wEve.id=mMuDstMaker->muDst()->event()->eventId();
  const char *afile = mMuDstMaker->GetFile();
  //printf("inpEve=%d eveID=%d daqFile=%s\n",nInpEve, wEve.id,afile);
  if(nInpEve%2000==1) printf("\n-----in---- %s, nEve: inp=%d trig=%d accpt=%d daqFile=%s\n", GetName(),nInpEve,nTrigEve, nAccEve,afile);
  hA[0]->Fill("inp",1.); 
  
  if(!isMC) { // skip TRG data for MC
    if( accessTrig()) return kStOK; //skip event w/o valid trig ID
  }
  nTrigEve++; 

  accessBSMD(); 
  if( accessVertex()) return kStOK; //skip event w/o ~any reasonable vertex  

  mJets = getJets(mJetTreeBranch); //get input jet info
  for (int i_jet=0; i_jet< nJets; ++i_jet){
    StJet* jet = getJet(i_jet);
    float jet_pt = jet->Pt();
    float jet_eta = jet->Eta();
    float jet_phi = jet->Phi();
    hA[117]->Fill(jet_eta,jet_phi);
    hA[118]->Fill(jet_pt);
  }

  if( accessTracks()) return kStOK; //skip event w/o ~any highPt track
  if( accessBTOW()) return kStOK; //skip event w/o energy in BTOW
  accessETOW();// get energy in ETOW 

  if( extendTrack2Barrel()) return  kStOK; //skip if not extends to barrel
  if( matchTrack2Cluster()) return  kStOK; //skip if cluster too soft

  nAccEve++;
 
  /* now it starts to get interesting, process every track 
     on the list  till the end. */
  findNearJet();
  findAwayJet();

  findPtBalance();

  hadronicRecoil();

  find_W_boson();
  if(nAccEve<2 ||nAccEve%1000==1 ) wEve.print(0x0,isMC);
  
  return kStOK;
}


//--------------------------------------
//--------------------------------------
void 
St2009WMaker::initGeom(){

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

#if 0
  const Float_t* A=mSmdEGeom->EtaB();
  for(int i=0;i<mxBetaStrMod+1;i++) {
    float etaF,phiF;
    mSmdEGeom->getEta(i+1,etaF);    
    printf("i=%d A=%f str=%f del=%f\n",i,A[i],etaF,A[i]-etaF);
  }
#endif
  
  //...... ETOW .............  
  for(int isec=0;isec<mxEtowSec;isec++){
    for(int isub=0;isub<mxEtowSub;isub++){
      for(int ieta=0;ieta<mxEtowEta;ieta++){
	positionEtow[isec*mxEtowSub+isub][ieta]=geomE->getTowerCenter(isec,isub,ieta);
#if 0 // trash
	if(isec==0 && isub==0) {//find radius for each eta ring
	  float x=positionEtow[isec*mxEtowSub+isub][ieta].x();
	  float y=positionEtow[isec*mxEtowSub+isub][ieta].y();
	  etowR[ieta] = x*x+y*y;
	}
#endif
      }
    }
  }
  
}


//--------------------------------------
//--------------------------------------
int // returns error code
St2009WMaker::L2algoEtaPhi2IJ(float etaF,float phiF,int &iEta, int &iPhi) {
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
TClonesArray*
St2009WMaker::getJets(TString branchName)
{
  StJetReader::JetBranchesMap &jetM= mJetReaderMaker->jetsMap();
  for(StJetReader::JetBranchesMap::iterator it=jetM.begin(); it!=jetM.end(); ++it)
  {
    StJets *stjets = (*it).second;
    nJets = stjets->nJets();
    //cout << "wEve.id " << wEve.id << " stjets->eventId() " << stjets->eventId() << endl;
    assert(stjets->eventId()==wEve.id);
    assert(stjets->runId()==mRunNo);
    if ((*it).first!=branchName) continue;
//     cout << "stjets->nJets():: " <<  nJets << endl;
//     cout << "wEve.id " << wEve.id << " mRunNo " << mRunNo << " branchName: " << (*it).first << endl;
    TClonesArray *jets = stjets->jets();
    return jets;
  }
  
  return 0;
}


// $Log: St2009WMaker.cxx,v $
// Revision 1.4  2010/01/09 00:07:16  stevens4
// add jet finder
//
// Revision 1.3  2010/01/06 19:16:47  stevens4
// track cuts now on primary component, cleanup
//
// Revision 1.2  2009/12/30 19:49:58  balewski
// test
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//


// $Log: St2009WMaker.cxx,v $
// Revision 1.4  2010/01/09 00:07:16  stevens4
// add jet finder
//
// Revision 1.3  2010/01/06 19:16:47  stevens4
// track cuts now on primary component, cleanup
//
// Revision 1.2  2009/12/30 19:49:58  balewski
// test
//
// Revision 1.1  2009/11/23 23:00:18  balewski
// code moved spin-pool
//
// Revision 1.1  2009/11/23 21:11:18  balewski
// start
//
