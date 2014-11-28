
// $Id: St2009WMaker.cxx,v 1.16 2011/09/14 14:23:20 stevens4 Exp $
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
  mJetReaderMaker = (StJetReader*)GetMaker("JetReader");  
  if(mJetReaderMaker ==0) {
    LOG_WARN<<GetName()<<Form("::constructor() NO JETS , W-algo is not working properly, continue")<<endm;
  }

  // preset or clear some params
  par_bht3TrgID= par_l2wTrgID=0;
  setHList(0);
  setMC(0);
  nInpEve= nTrigEve= nAccEve=0; 

  //... MC trigger simulator
  par_l0emulAdcThresh=31;
  par_l2emulSeedThresh=5.0;
  par_l2emulClusterThresh=13.0;

  //.. vertex
  par_minPileupVert=3; // to reject events w/o TPC, lower it for MC
  par_vertexZ=100; // (cm)

  //... track
  par_nFitPts=15; // hits on the track
  par_nHitFrac=0.51;
  par_trackRin=90;  par_trackRout=160; // cm
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
  par_highET=25.; // (GeV), cut-off for final W-cluster ET
  par_ptBalance=15.; // (GeV), ele cluster vector + jet sum vector
  par_leptonEta=1.0; // bracket acceptance

  par_useEtow=1; // flag for how to use ETOW in algo
  // flag == 0 -> don't use ETOW
  // flag >= 1 -> use in event display plots
  // flag >= 2 -> use in away sum
  // flag == 3 -> use in near sum
  setEtowScale(1.0); // for old the Endcap geometr you need ~1.3
  setBtowScale(1.0);
  
  setJetNeutScaleMC(1.0);//vary neutral ET scale for systematic
  setJetChrgScaleMC(1.0);//vary charged ET scale for systematic

  mRunNo=0;

  // irrelevant for W analysis
  par_DsmThres=28; // only for monitoring
  par_maxDisplEve=1; // # of displayed selected events

  use_gains_file = 0;

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
  mRand = new TRandom3(0);

  initGeom();
 
  if (use_gains_file == 1) {
    fstream f1; f1.open(gains_file,ios::in);
    cout << "Opening gains file " << gains_file << endl;
    char str[200];
    while (f1 >> str) {
      int softID = atoi(str);
      f1 >> str; gains_BTOW[softID] = atof(str);
      f1 >> str; f1 >> str;
    }
  }

  if(isMC >= 350){ // load vertex reweighting histo
    TString filename="/star/u/stevens4/wAnalysis/efficXsec/zVertReweight.root";
    TFile* reweightFile = new TFile(filename);
    cout<<"Re-weighting vertex z distribution with '"<<nameReweight<<"' histo from file "<<endl<<filename<<endl;
    hReweight = (TH1F*) reweightFile->Get(nameReweight);
  }    
 
  if(isMC) par_minPileupVert=1;

  // ..... initialization of TPC cuts is run dependent, call it 'hack of the day', should be moved to InitRun() and handle multipl runs per job, after APS, JB
  for(int isec=0;isec<mxTpcSec;isec++) {
    int sec=isec+1;
    float Rin=par_trackRin,Rout=par_trackRout;
    //.... Rin ..... changes
    if(sec==4  && par_inpRunNo>=10090089) Rin=125.;
    if(sec==11 && par_inpRunNo>=10083013) Rin=125.;
    if(sec==15 && par_inpRunNo>=10088096 && par_inpRunNo<=10090112 ) Rin=125.;
    //.... Rout ..... changes
    if(sec==5 && par_inpRunNo>=10098029) Rout=140.;
    if(sec==6 ) Rout=140.;
    if(sec==20 && par_inpRunNo>=10095120 && par_inpRunNo<=10099078 ) Rout=140.;
    
    mTpcFilter[isec].setCuts(par_nFitPts,par_nHitFrac,Rin,Rout);
    mTpcFilter[isec].init("sec",sec,HList);
  }

  return StMaker::Init();
}

//________________________________________________
//________________________________________________
Int_t
St2009WMaker::InitRun(int runNo){
  LOG_INFO<<Form("::InitRun(%d) start",runNo)<<endm;
  mBarrelTables->loadTables(this );
  
  //get run number from MuDst name for embedding (each file goes through InitRun since each has different geneated runNum < 5000)
  if(isMC>=350){ 
    const char *file = mMuDstMaker->GetFile();
    const char *p1=strstr(file,"adc_");
    int run=atoi(p1+4);
    par_inpRunNo=run;
  
    LOG_INFO<<Form("::InitRun(%d) reset TPC cuts",par_inpRunNo)<<endm;
    for(int isec=0;isec<mxTpcSec;isec++) {
      int sec=isec+1;
      float Rin=par_trackRin,Rout=par_trackRout;
      //.... Rin ..... changes
      if(sec==4  && par_inpRunNo>=10090089) Rin=125.;
      if(sec==11 && par_inpRunNo>=10083013) Rin=125.;
      if(sec==15 && par_inpRunNo>=10088096 && par_inpRunNo<=10090112 ) Rin=125.;
      //.... Rout ..... changes
      if(sec==5 && par_inpRunNo>=10098029) Rout=140.;
      if(sec==6 ) Rout=140.;
      if(sec==20 && par_inpRunNo>=10095120 && par_inpRunNo<=10099078 ) Rout=140.;
      
      //reset cuts for TPC filter based on runNum for embedding
      if(Rin != par_trackRin || Rout!=par_trackRout)
        LOG_INFO<<"Sector "<<sec<<" cuts aren't default: Rin="<<Rin<<" and Rout="<<Rout<<endl; 
      mTpcFilter[isec].setCuts(par_nFitPts,par_nHitFrac,Rin,Rout);
    }
  }

  mRunNo=runNo;
  if(runNo>1000000) assert(mRunNo==par_inpRunNo); // what a hack, assurs TPC cuts change w/ time for data, JB. It will crash if multiple runs are analyzed by the same job.

   LOG_INFO<<Form("::InitRun(%d) done, W-algo params: trigID: bht3=%d L2W=%d  isMC=%d\n TPC: nPileupVert>%d, vertex |Z|<%.1fcm, primEleTrack: nFit>%d, hitFrac>%.2f Rin<%.1fcm, Rout>%.1fcm, PT>%.1fGeV/c\n BTOW ADC: kSigPed=%d AdcThr>%d maxAdc>%.0f clustET>%.1f GeV  ET2x2/ET4x4>%0.2f  ET2x2/nearTotET>%0.2f\n dist(track-clust)<%.1fcm, nearDelR<%.1f\n Counters Thresholds: track>%.1f GeV, tower>%.1f GeV  Use ETOW: flag=%d ScaleFact=%.2f\nBtowScaleFacor=%.2f\n W selection highET>%.1f awayDelPhi<%.1frad  ptBalance>%.1fGeV  |leptonEta|<%.1f",
		 par_inpRunNo,par_bht3TrgID, par_l2wTrgID,isMC,
		 par_minPileupVert,par_vertexZ,
		 par_nFitPts,par_nHitFrac,  par_trackRin,  par_trackRout, par_trackPt,
		  par_kSigPed, par_AdcThres,par_maxADC,par_clustET,par_clustFrac24,par_nearTotEtFrac,
		  par_delR3D,par_nearDeltaR,
		  par_countTrPt,par_countTowEt,par_useEtow,par_etowScale,
		  par_btowScale,
		  par_highET,par_awayDeltaPhi,par_ptBalance,par_leptonEta
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
  
  int btowStat=accessBTOW(); //need btow info for MC trg simu
  
  if( accessTrig()) return kStOK; //skip event w/o valid trig ID
  nTrigEve++; 

  accessBSMD(); 
  if( accessVertex()) return kStOK; //skip event w/o ~any reasonable vertex  

  if(mJetReaderMaker) {// just QA plots for jets
    mJets = getJets(mJetTreeBranch); //get input jet info
    for (int i_jet=0; i_jet< nJets; ++i_jet){
      StJet* jet = getJet(i_jet);
      float jet_pt = jet->Pt();
      float jet_eta = jet->Eta();
      float jet_phi = jet->Phi();
      hA[117]->Fill(jet_eta,jet_phi);
      hA[118]->Fill(jet_pt);
    }
  }

  if( accessTracks()) return kStOK; //skip event w/o ~any highPt track
  
  if(wEve.bemc.tileIn[0]==1) 
    hA[0]->Fill("B-in",1.0);
  if( btowStat ) return kStOK; //skip event w/o energy in BTOW
  hA[0]->Fill("B200",1.0);
  
  accessETOW();// get energy in ETOW 

  if( extendTrack2Barrel()) return  kStOK; //skip if not extends to barrel
  if( matchTrack2Cluster()) return  kStOK; //skip if cluster too soft

  nAccEve++;
 
  /* now it starts to get interesting, process every track 
     on the list  till the end. */
  findNearJet();
  findAwayJet();

  if(mJetReaderMaker) findPtBalance();

  hadronicRecoil();

  if(mJetReaderMaker) tag_Z_boson();

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
St2009WMaker::getJets(TString branchName){
  if(mJetReaderMaker ==0) {
    nJets=-1; return 0;
  }
  assert(mJetReaderMaker->getStJets(branchName)->eventId()==wEve.id);
  assert(mJetReaderMaker->getStJets(branchName)->runId()==mRunNo);
  nJets = mJetReaderMaker->getStJets(branchName)->nJets();
  return mJetReaderMaker->getStJets(branchName)->jets();

}


// $Log: St2009WMaker.cxx,v $
// Revision 1.16  2011/09/14 14:23:20  stevens4
// update used for cross section PRD paper
//
// Revision 1.15  2010/05/04 12:14:35  balewski
// runs now w/o jet tree
//
// Revision 1.14  2010/04/27 16:53:44  stevens4
// add code to remove events tagged as Zs from W candidates
//
// Revision 1.13  2010/04/14 22:23:30  balewski
// *** empty log message ***
//
// Revision 1.12  2010/03/23 15:33:55  seelej
// Edit to files to allow the use of a text file for the gains instead of using the DB.
//
// Revision 1.11  2010/03/14 22:50:31  balewski
// *** empty log message ***
//
// Revision 1.10  2010/02/18 22:34:50  stevens4
// add tpc effic study and allow energy scaling for data and MC
//
// Revision 1.9  2010/01/27 22:12:24  balewski
// spin code matched to x-section code
//
// Revision 1.8  2010/01/23 02:35:38  stevens4
// add ability to scale jet et and use real btow peds for rcf mc
//
// Revision 1.7  2010/01/21 00:15:25  balewski
// added sector & run  dependent TPC cuts on Rin, Rout
//
// Revision 1.6  2010/01/18 03:26:15  balewski
// expanded TPC track filtering, not finished
//
// Revision 1.5  2010/01/13 03:34:20  stevens4
// give trig emulator access to barrel hits
//
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
// Revision 1.16  2011/09/14 14:23:20  stevens4
// update used for cross section PRD paper
//
// Revision 1.15  2010/05/04 12:14:35  balewski
// runs now w/o jet tree
//
// Revision 1.14  2010/04/27 16:53:44  stevens4
// add code to remove events tagged as Zs from W candidates
//
// Revision 1.13  2010/04/14 22:23:30  balewski
// *** empty log message ***
//
// Revision 1.12  2010/03/23 15:33:55  seelej
// Edit to files to allow the use of a text file for the gains instead of using the DB.
//
// Revision 1.11  2010/03/14 22:50:31  balewski
// *** empty log message ***
//
// Revision 1.10  2010/02/18 22:34:50  stevens4
// add tpc effic study and allow energy scaling for data and MC
//
// Revision 1.9  2010/01/27 22:12:24  balewski
// spin code matched to x-section code
//
// Revision 1.8  2010/01/23 02:35:38  stevens4
// add ability to scale jet et and use real btow peds for rcf mc
//
// Revision 1.7  2010/01/21 00:15:25  balewski
// added sector & run  dependent TPC cuts on Rin, Rout
//
// Revision 1.6  2010/01/18 03:26:15  balewski
// expanded TPC track filtering, not finished
//
// Revision 1.5  2010/01/13 03:34:20  stevens4
// give trig emulator access to barrel hits
//
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
