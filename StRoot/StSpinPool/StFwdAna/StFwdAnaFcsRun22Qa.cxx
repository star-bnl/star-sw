#include "StEnumerations.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFcsCluster.h"
#include "StEvent/StFcsCollection.h"
#include "StEvent/StFcsHit.h"
#include "StEvent/StEventTypes.h"
#include "StFcsDbMaker/StFcsDbMaker.h"
#include "StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "StSpinPool/StFcsQaMaker/StFcsQaMaker.h"
#include "StSpinPool/StFcsRawDaqReader/StFcsRawDaqReader.h"
#include "StRoot/StEpdUtil/StEpdGeom.h"
#include "StThreeVectorF.hh"
#include "Stypes.h"
#include "StBTofHeader.h"

#include "StFwdAnaFcsRun22Qa.h"


ClassImp(StFwdAnaFcsRun22Qa)


StFwdAnaFcsRun22Qa::StFwdAnaFcsRun22Qa()
{
  memset(mH2F_Hit_adcVtb,0,sizeof(mH2F_Hit_adcVtb));
  memset(mH2F_Hit_enVid,0,sizeof(mH2F_Hit_enVid));
  memset(mH2F_Hit_fitpeakVid,0,sizeof(mH2F_Hit_fitpeakVid));
  memset(mH2F_Hit_chi2Vid,0,sizeof(mH2F_Hit_chi2Vid));
  memset(mH2F_Hit_npeaksVid,0,sizeof(mH2F_Hit_npeaksVid));
  memset(mH1F_Hit_NHits,0,sizeof(mH1F_Hit_NHits));
  memset(mH1F_Hit_NHitsCut,0,sizeof(mH1F_Hit_NHitsCut));
  memset(mH1F_Hit_ESum,0,sizeof(mH1F_Hit_ESum));

  memset(mH2F_HitPres_depVqt,0,sizeof(mH2F_HitPres_depVqt));
  memset(mH2F_HitPres_peakVtac,0,sizeof(mH2F_HitPres_peakVtac));

  memset(mH1F_NClusters,0,sizeof(mH1F_NClusters));
  memset(mH1F_Clu_NTowers,0,sizeof(mH1F_Clu_NTowers));
  memset(mH1F_Clu_NNei,0,sizeof(mH1F_Clu_NNei));
  memset(mH1F_Clu_NPoints,0,sizeof(mH1F_Clu_NPoints));
  memset(mH1F_Clu_En,0,sizeof(mH1F_Clu_En));
  memset(mH2F_Clu_yVx,0,sizeof(mH2F_Clu_yVx));
  memset(mH2F_Clu_sigmaxVsigmin,0,sizeof(mH2F_Clu_sigmaxVsigmin));
  memset(mH1F_Clu_theta,0,sizeof(mH1F_Clu_theta));
  memset(mH2F_Clu_Chi2NdfPhoton_2V1,0,sizeof(mH2F_Clu_Chi2NdfPhoton_2V1));

  memset(mH1F_NPoints,0,sizeof(mH1F_NPoints));
  memset(mH1F_Poi_En,0,sizeof(mH1F_Poi_En));
  memset(mH1F_Poi_NCluPhotons,0,sizeof(mH1F_Poi_NCluPhotons));
  memset(mH2F_Poi_yVx,0,sizeof(mH2F_Poi_yVx));

  memset(mG_Triggers,0,sizeof(mG_Triggers));
  memset(mGE_NHits,0,sizeof(mGE_NHits));
  memset(mGE_NHitsCut,0,sizeof(mGE_NHitsCut));
  memset(mGE_NClusters,0,sizeof(mGE_NClusters));
  memset(mGE_NPoints,0,sizeof(mGE_NPoints));
  memset(mG_Clu_En,0,sizeof(mG_Clu_En));
  memset(mG_Poi_En,0,sizeof(mG_Poi_En));

  //memset(mSpinPattern,0,sizeof(mSpinPattern));
}

StFwdAnaFcsRun22Qa::~StFwdAnaFcsRun22Qa()
{
  for( UShort_t i=0; i<kFcsNDet; ++i ){
    delete mH2F_Hit_adcVtb[i];
    //delete mH2F_Hit_enVid[i];
    //delete mH2F_Hit_fitpeakVid[i];
    //delete mH2F_Hit_chi2Vid[i];
    //delete mH2F_Hit_npeaksVid[i];
    //delete mH1F_Hit_NHits[i];

    //if( i<3 ){ delete mH1F_Hit_ESum[0]; }
    
    if( i<2 ){
      delete mH2F_HitPres_depVqt[i];
      delete mH2F_HitPres_peakVtac[i];
      //delete mH2F_HitEpd_tacVadcmip[i];
    }
  }
}

UInt_t StFwdAnaFcsRun22Qa::LoadHists(TFile* file, HistManager* histman, StFwdAnaData* anadata)
{
  if( histman==0 ){ return 0; }
  UInt_t loaded = 0;
  loaded += histman->AddH1F(file,mH1F_AllFcsTriggers,"H1F_AllFcsTriggers","All FCS Triggers;;",65,0,65);
  //@[June 3, 2024] > This is almost all of them as some ids are not in this range but good enough for now
  //@[September 6, 2024] > Learned that for Run 22 only FCS triggers above 890000 are production triggers and can ignore triggers lower than that. Loooking at STAR's RunLog Browser I don't see any triggers larger than 892000. For this reason trigger histogram is only showing these Ids.
  //@[September 13, 2024] > There are a total number of 64 triggers in Run 22. Add an extra bin for any not found
  //@[September 16, 2024] > Note that if loading this histogram from a file the bin names should already be set so there is no need to set them again
  int trigsize = anadata->sizeOfFcsTriggers();
  //std::cout << "|trigsize:"<<trigsize << std::endl;
  if( trigsize==64 ){ //Check to make sure all triggers are in the map and it matches the bin size
    for( int i=0; i<trigsize; ++i ){
      //std::cout << "|itrig:"<<i << "|trigname:"<< mFcsTrigMap->triggerName(i) << std::endl;
      mH1F_AllFcsTriggers->GetXaxis()->SetBinLabel(i+1,anadata->fcsTriggerName(i)); //Bin numbers are offset by 1
    }
  }
  mH1F_AllFcsTriggers->GetXaxis()->SetBinLabel(65,"NF");  //Last bin is named "NF" for not found which is what nameFromId() returns if trigger is not in the map. This way if no map was loaded  searching for triggers will return "NF"
  
  loaded += histman->AddH2F(file,mH2F_BxId_7V48,"H2F_BxId7V48","Bunch Crossing Id;48 bit;7 bit", 120,-0.5,119.5, 120,-0.5,119.5);
  loaded += histman->AddH2F(file,mH2F_Mult_tofVref,"H2F_Mult_tofVref","TOF multiplicity vs. Reference multiplicity;RefMult;TofMult",100,0,100,100,0,100);
  loaded += histman->AddH2F(file,mH2F_Mult_tofVecal,"H2F_Mult_tofVecal","TOF multiplicity vs. Fcs Ecal multiplicity;EcalMult;TofMult",200,0,1000,100,0,100);

  loaded += histman->AddH1F(file,mH1F_Spin,"H1F_Spin","Spin",3,-1.5,1.5);
  //loaded += histman->AddH1F(file,mH1F_BunchXing, "H1F_BunchXing", "Bunch Crossing number (blue clock)",    120,0,120);
  //loaded += histman->AddH1F(file,mH1F_spin4Vbx7, "H1F_spin4Vbx7", "Spin 4 value vs. Bx7 Id;Bx7 Id;Spin4",  120,-0.5,119.5);
  //loaded += histman->AddH1F(file,mH1F_spin4Vbx48,"H1F_spin4Vbx48","Spin 4 value vs. Bx48 Id;Bx48 Id;Spin4",120,-0.5,119.5);
  
  TString namesuffix[6] = {"EN","ES","HN","HS","PN","PS"};
  for( UShort_t i=0; i<kFcsNDet; ++i ){
    UInt_t nchs = 0;
    UInt_t ncol = 0;
    UInt_t nrow = 0;
    if( i<=kFcsEcalSouthDetId )                              { nchs = kFcsEcalMaxId; ncol = kFcsEcalNCol; nrow = kFcsEcalNRow; }
    else if( kFcsHcalNorthDetId<=i && i<=kFcsHcalSouthDetId ){ nchs = kFcsHcalMaxId; ncol = kFcsHcalNCol; nrow = kFcsHcalNRow; }
    else if( kFcsPresNorthDetId<=i && i<=kFcsPresSouthDetId ){ nchs = kFcsPresMaxId; ncol = kFcsPresNCol; nrow = kFcsPresNRow; }
    else{ LOG_ERROR << "StFcsFun22QaMaker::LoadHists() too many detector ids for some reason" << endm; return kStErr; }
    TString histname = "H2F_Hit_adcVtb_" + namesuffix[i];
    TString histtitle = "Adc vs. Tb for " + namesuffix[i] + ";tb;adc";
    if( mFcsAdcTbOn ){
      if( mH2F_Hit_adcVtb[i]==0 ){ mH2F_Hit_adcVtb[i] = new TObjArray(); } //Create new array for each detector
      loaded += histman->AddH2FArr(file,(mH2F_Hit_adcVtb[i]),nchs,histname.Data(),histtitle.Data(),100,0,100,400,0,4000);
    }
    histname = "H2F_Hit_enVid_" + namesuffix[i];
    histtitle = "Energy vs. Id for " + namesuffix[i] + ";id;energy (GeV)";
    loaded += histman->AddH2F(file,(mH2F_Hit_enVid[i]),histname.Data(),histtitle.Data(),nchs,0,nchs,200,0,200);
    histname = "H2F_Hit_fitpeakVid_" + namesuffix[i];
    histtitle = "Fitted Peak Location vs. Id for " + namesuffix[i] + ";id;tb";
    loaded += histman->AddH2F(file,(mH2F_Hit_fitpeakVid[i]),histname.Data(),histtitle.Data(),nchs,0,nchs,100,0,100);
    histname = "H2F_Hit_chi2Vid_" + namesuffix[i];
    histtitle = "Chi^2/NDF for fitted peaks (npeaks>1) vs. Id for " + namesuffix[i] + ";id;Chi^2/NDF";
    loaded += histman->AddH2F(file,(mH2F_Hit_chi2Vid[i]),histname.Data(),histtitle.Data(),nchs,0,nchs,110,-10,100);
    histname = "H2F_Hit_npeaksVid_" + namesuffix[i];
    histtitle = "Number of fitted peaks vs. Id for " + namesuffix[i] + ";id;NPeaks";
    loaded += histman->AddH2F(file,(mH2F_Hit_npeaksVid[i]),histname.Data(),histtitle.Data(),nchs,0,nchs,15,0,15);
    histname = "H1F_Hit_Nhits_" + namesuffix[i];
    histtitle = "Hit Multiplicity for " + namesuffix[i] + ";NChs";
    loaded += histman->AddH1F(file,(mH1F_Hit_NHits[i]),histname.Data(),histtitle.Data(),nchs,0,nchs);
    histname = "H1F_Hit_NhitsCut_" + namesuffix[i];
    histtitle = "Hit Multiplicity with Cuts for " + namesuffix[i] + ";NChs";
    loaded += histman->AddH1F(file,(mH1F_Hit_NHitsCut[i]),histname.Data(),histtitle.Data(),nchs,0,nchs);

    histname = "H1F_NClusters_" + namesuffix[i];
    histtitle = "Number of Clusters for " + namesuffix[i] + ";NClusters";
    loaded += histman->AddH1F(file,(mH1F_NClusters[i]),histname.Data(),histtitle.Data(),50,0,50);
    histname = "H1F_Clu_NTowers_" + namesuffix[i];
    histtitle = "Number of towers in a cluster for " + namesuffix[i] + ";NTowers/cluster";
    loaded += histman->AddH1F(file,(mH1F_Clu_NTowers[i]),histname.Data(),histtitle.Data(),100,0,100);
    histname = "H1F_Clu_NNei_" + namesuffix[i];
    histtitle = "Number of neighbor clusters for a given cluster for " + namesuffix[i] + ";NNeighbors/cluster";
    loaded += histman->AddH1F(file,(mH1F_Clu_NNei[i]),histname.Data(),histtitle.Data(),10,0,10);
    histname = "H1F_Clu_NPoints_" + namesuffix[i];
    histtitle = "Number of points in a cluster for " + namesuffix[i] + ";NPoints/cluster";
    loaded += histman->AddH1F(file,(mH1F_Clu_NPoints[i]),histname.Data(),histtitle.Data(),4,0,4);
    histname = "H1F_Clu_En_" + namesuffix[i];
    histtitle = "Energy in a cluster for " + namesuffix[i] + ";energy (GeV)";
    loaded += histman->AddH1F(file,(mH1F_Clu_En[i]),histname.Data(),histtitle.Data(),100,0,100);
    histname = "H2F_Clu_yVx_" + namesuffix[i];
    histtitle = "Cluster position in row vs. column space for " + namesuffix[i] + ";col;row";
    loaded += histman->AddH2F(file,(mH2F_Clu_yVx[i]),histname.Data(),histtitle.Data(), ncol+1,1,ncol+1, nrow+1,1,nrow+1);
    histname = "H2F_Clu_sigmaxVsigmin_" + namesuffix[i];
    histtitle = "Cluster Sigma Max vs. Sigma Min for " + namesuffix[i] + ";sigma min;sigma max";
    loaded += histman->AddH2F(file,(mH2F_Clu_sigmaxVsigmin[i]),histname.Data(),histtitle.Data(), 100,0,1, 100,0,1);
    histname = "H1F_Clu_theta_" + namesuffix[i];
    histtitle = "Cluster theta for " + namesuffix[i];
    loaded += histman->AddH1F(file,(mH1F_Clu_theta[i]),histname.Data(),histtitle.Data(), 20,-1.0*TMath::Pi(),TMath::Pi());
    histname = "H2F_Clu_Chi2NdfPhoton_2V1_" + namesuffix[i];
    histtitle = "Cluster Chi^2/NDF for 2 photon fit vs. 1 photon fit for " + namesuffix[i] + ";1 photon Chi^2/NDF;2 photon Chi^2/NDF";
    loaded += histman->AddH2F(file,(mH2F_Clu_Chi2NdfPhoton_2V1[i]),histname.Data(),histtitle.Data(), 100,0,100, 100,0,100);

    histname = "H1F_NPoints_" + namesuffix[i];
    histtitle = "Number of points for " + namesuffix[i] + ";NPoints";
    loaded += histman->AddH1F(file,(mH1F_NPoints[i]),histname.Data(),histtitle.Data(), 50,0,50);
    histname = "H1F_Poi_En_" + namesuffix[i];
    histtitle = "Point Energy for " + namesuffix[i] + ";Energy (GeV)";
    loaded += histman->AddH1F(file,(mH1F_Poi_En[i]),histname.Data(),histtitle.Data(), 100,0,100);
    histname = "H1F_Poi_NCluPhotons_" + namesuffix[i];
    histtitle = "Number of photons from parent cluster for " + namesuffix[i] + ";NPoints in Parent Cluster";
    loaded += histman->AddH1F(file,(mH1F_Poi_NCluPhotons[i]),histname.Data(),histtitle.Data(), 4,0,4);
    histname = "H2F_Poi_yVx_" + namesuffix[i];
    histtitle = "Point positions for " + namesuffix[i] + ";x (cm);y (cm)";
    loaded += histman->AddH2F(file,(mH2F_Poi_yVx[i]),histname.Data(),histtitle.Data(), ncol+1,1,ncol+1, nrow+1,1,nrow+1 );
  }
  loaded += histman->AddH1F(file,mH1F_Hit_ESum[0],"H1F_Hit_ESum_Ecal","Total energy sum in Ecal;Energy (GeV)", 200,0,200);
  loaded += histman->AddH1F(file,mH1F_Hit_ESum[1],"H1F_Hit_ESum_Hcal","Total energy sum in Hcal;Energy (GeV)", 100,0,100);
  loaded += histman->AddH1F(file,mH1F_Hit_ESum[2],"H1F_Hit_ESum_Pres","Total energy sum in Pres;Energy (GeV)", 100,0,100);

  if( mEpdAdcQaOn || mEpdTacQaOn ){
    if( mEpdAdcQaOn ){
      if( mH2F_HitPres_depVqt[0]==0 ){ mH2F_HitPres_depVqt[0] = new TObjArray(); }
      if( mH2F_HitPres_depVqt[1]==0 ){ mH2F_HitPres_depVqt[1] = new TObjArray(); }
      loaded += histman->AddH2FArr(file,mH2F_HitPres_depVqt[0],192,"H2F_HitPres_depVqt_PN","QT sum vs. DEP ADC sum for Fcs Preshower North hits;QtSum;DepSum", 64,0,4096, 64,0,4096);
      loaded += histman->AddH2FArr(file,mH2F_HitPres_depVqt[1],192,"H2F_HitPres_depVqt_PS","QT sum vs. DEP ADC sum for Fcs Preshower South hits;QtSum;DepSum", 64,0,4096, 64,0,4096);
    }
    if( mEpdTacQaOn ){
      if( mH2F_HitPres_peakVtac[0]==0 ){ mH2F_HitPres_peakVtac[0] = new TObjArray(); }
      if( mH2F_HitPres_peakVtac[1]==0 ){ mH2F_HitPres_peakVtac[1] = new TObjArray(); }
      loaded += histman->AddH2FArr(file,mH2F_HitPres_peakVtac[0],192,"H2F_HitPres_peakVtac_PN","Qt TAC vs. Found peak tb for Fcs Preshower North hits;peak (tb);Qt TAC", 100,0,100, 100,0,100);
      loaded += histman->AddH2FArr(file,mH2F_HitPres_peakVtac[1],192,"H2F_HitPres_peakVtac_PS","Qt TAC vs. Found peak tb for Fcs Preshower South hits;peak (tb);Qt TAC", 100,0,100, 100,0,100); 
    }
  }
  
  if( mBestMassOn ){
    loaded += histman->AddH2F(file,mH2F_CluHigh_angleVesum,"H2F_CluHigh_angleVesum", "Highest two energy clusters opening angle vs. total cluster energy;esum (GeV);opening angle",100,0,100, 60,0,TMath::Pi());
    loaded += histman->AddH2F(file,mH2F_CluHighEn_lowVhigh,"H2F_CluHighEn_lowVhigh","Highest two energy clusters energies energy 1 vs. energy 2;E1 (GeV); E2(GeV)", 100,0,100, 100,0,100);
    loaded += histman->AddH2F(file,mH2F_CluHigh_dggVesum,"H2F_CluHigh_dggVesum","Highest two energy clusters Dgg vs. energy sum;esum (GeV);Dgg (cm)", 100,0,100, 100,0,100);
    loaded += histman->AddH2F(file,mH2F_CluHigh_invmassVesum,"H2F_CluHigh_invmassVesum","Highest two energy clusters invariant mass vs. energy sum;esum (GeV);invariant mass (GeV/c^2)", 100,0,100, 100,0,1.0);
    loaded += histman->AddH2F(file,mH2F_CluHigh_invmassVdgg,"H2F_CluHigh_invmassVdgg","Highest two energy clusters invariant mass vs. Dgg;Dgg (cm);invariant mass (GeV/c^2)", 100,0,100, 100,0,1.0);
    loaded += histman->AddH2F(file,mH2F_CluHigh_invmassVzgg,"H2F_CluHigh_invmassVzgg","Highest two energy clusters invariant mass vs. Zgg;Zgg |E1-E2|/(E1+E2);invariant mass (GeV/c^2)", 100,0,1.0, 100,0,1.0);

    loaded += histman->AddH2F(file,mH2F_PoiHigh_angleVesum,"H2F_PoiHigh_angleVesum", "Highest two energy points opening angle vs. total point energy;esum (GeV);opening angle",100,0,100, 60,0,TMath::Pi());
    loaded += histman->AddH2F(file,mH2F_PoiHighEn_lowVhigh,"H2F_PoiHighEn_lowVhigh","Highest two energy points energies energy 1 vs. energy 2;E1 (GeV); E2(GeV)", 100,0,100, 100,0,100);
    loaded += histman->AddH2F(file,mH2F_PoiHigh_dggVesum,"H2F_PoiHigh_dggVesum","Highest two energy points Dgg vs. energy sum;esum (GeV);Dgg (cm)", 100,0,100, 100,0,100);
    loaded += histman->AddH2F(file,mH2F_PoiHigh_invmassVesum,"H2F_PoiHigh_invmassVesum","Highest two energy points invariant mass vs. energy sum;esum (GeV);invariant mass (GeV/c^2)", 100,0,100, 100,0,1.0);
    loaded += histman->AddH2F(file,mH2F_PoiHigh_invmassVdgg,"H2F_PoiHigh_invmassVdgg","Highest two energy points invariant mass vs. Dgg;Dgg (cm);invariant mass (GeV/c^2)", 100,0,100, 100,0,1.0);
    loaded += histman->AddH2F(file,mH2F_PoiHigh_invmassVzgg,"H2F_PoiHigh_invmassVzgg","Highest two energy points invariant mass vs. Zgg;Zgg |E1-E2|/(E1+E2);invariant mass (GeV/c^2)", 100,0,1.0, 100,0,1.0);
  }
  
  return loaded;
}

Int_t StFwdAnaFcsRun22Qa::DoMake(StFwdAnaData* anadata)
{
  // Int_t infostatus = this->FillEventInfo(anadata);
  // switch( infostatus ){
  // case kStEOF: return kStEOF;
  // case kStErr: return kStErr;
  // case kStFatal: return kStFatal;
  // case kStSkip: return kStSkip;
  // case kStStop: return kStStop;
  // }

  Int_t fcsstatus = this->FillFcsInfo(anadata);
  switch( fcsstatus ){
  case kStEOF: return kStEOF;
  case kStErr: return kStErr;
  case kStFatal: return kStFatal;
  case kStSkip: return kStSkip;
  case kStStop: return kStStop;
  }
  //std::cout << "Filled Fcs" << std::endl;
  //Local copy of needed variables to make things easier
  //StMuFcsCollection* MuFcsColl = anadata->fcsColl();
  //const StTriggerData* TrigData = anadata->trigData();

  //unsigned int totalecalhits = MuFcsColl->numberOfHits(kFcsEcalNorthDetId) + MuFcsColl->numberOfHits(kFcsEcalSouthDetId);
  //mH2F_Mult_tofVecal->Fill(totalecalhits,TrigData->tofMultiplicity());

  //std::cout << "Finished Make" << std::endl;
  //if( infostatus==kStWarn || fcsstatus==kStWarn ){ return kStWarn; } //Now check if either returned a warning and if so returning warning
  //else{ return kStOk; } //Both were ok
  return kStOk;
}

Int_t StFwdAnaFcsRun22Qa::FillEventInfo(StFwdAnaData* anadata)
{
  //Local copy of needed variables to make things easier
  StMuEvent* MuEvent = anadata->muEvent();
  const StTriggerData* TrigData = anadata->trigData();
  StSpinDbMaker* SpinDbMkr = anadata->spinDbMkr();

  mH2F_BxId_7V48->Fill(TrigData->bunchId48Bit(),TrigData->bunchId7Bit());
  mH2F_Mult_tofVref->Fill(MuEvent->refMult(),TrigData->tofMultiplicity());

  //Spin information
  if( SpinDbMkr==0 ){
    //Do twice. Once for blue beam and another time for yellow beam
    for( int i=0; i<2; ++i ){
      Double_t rndm = anadata->randomNum();
      if( rndm<0.5 ){ mH1F_Spin->Fill(-1); }
      else{ mH1F_Spin->Fill(1); }
    }
  }
  else{
    //int spin4bit = mSpinDbMkr->spin4usingBX48( TrigData->bunchId48Bit() );
    //Int_t runnum = mMuEvent->runNumber();
    //StL0Trigger& trig = mMuEvent->l0Trigger();
    //int bx7 = (trig.bunchCrossingId7bit(runnum)) % 120;
    int spinx7 = SpinDbMkr->spin4usingBX7(TrigData->bunchId7Bit());
    //std::cout << "|trigbx7:"<<TrigData->bunchId7Bit() << "|l0bx7:"<<bx7 << "|spinx7:"<<spinx7 << "|spinx7arr:"<<spinx7arr <<"|trgspin:"<<TrigData->spinBit() <<"|l0spin:"<<trig.spinBits(runnum) <<"|trigname:"<<TrigData->ClassName()<< std::endl;
    int bluespin = StFwdDataEvent::BlueSpin(spinx7);
    int yellowspin = StFwdDataEvent::YellowSpin(spinx7);
    mH1F_Spin->Fill(bluespin);
    mH1F_Spin->Fill(yellowspin);
  }
  
  //Trigger Information
  StMuTriggerIdCollection* TrigMuColl = &(MuEvent->triggerIdCollection());
  if( !TrigMuColl ){ LOG_ERROR <<"StFwdAnaFcsRun22Qa::FillEventInfo - !TrigMuColl" <<endl; return kStErr; }
  const StTriggerId& trgIDs = TrigMuColl->nominal();
  Int_t ntrig = trgIDs.triggerIds().size();
  for( Int_t i=0; i<ntrig; ++i ){
    unsigned int trig = trgIDs.triggerId(i);
    //std::cout << "|i:"<<i << "|trig:"<<trig << "|runnum:"<<mMuEvent->runNumber() << "|name:"<<mFcsTrigMap->nameFromId(trig,mMuEvent->runNumber()) << std::endl;
    mH1F_AllFcsTriggers->Fill( anadata->fcsTrigNameFromId(trig,MuEvent->runNumber()),1 );
  }
  
  return kStOk;
}

Int_t StFwdAnaFcsRun22Qa::FillFcsInfo(StFwdAnaData* anadata)
{
  //std::cout << "========== StFwdAnaFcsRun22Qa::FillFcsInfo Start ==========" << std::endl;
  //std::cout << "Filled Event" << std::endl;
  TClonesArray* MuEpdHits = 0;
  StEpdCollection* EpdColl = 0;
  anadata->epdColl(MuEpdHits,EpdColl);

  StMuFcsCollection* MuFcsColl = anadata->fcsColl();
  if (!MuFcsColl) { LOG_ERROR << "StFwdAnaFcsRun22Qa::FillFcsInfo did not find MuFcsCollection" << endm; return kStErr; }
  StFcsDb* FcsDb = anadata->fcsDb();
  Double_t EnCut = anadata->enCut();
  
  bool check_fillclu = false;
  bool check_fillpoi = false;

  float bestclu_invmass         = -1;
  float bestclu_totale          = -1;
  float bestclu_dgg             = -1;
  float bestclu_zgg             = -1;
  float bestclu_opening_angle   = -1;
  float bestclu_hightowerenergy = -1;
  float bestclu_lowtowerenergy  = -1;
  
  float bestpoi_invmass         = -1;
  float bestpoi_totale          = -1;
  float bestpoi_dgg             = -1;
  float bestpoi_zgg             = -1;
  float bestpoi_opening_angle   = -1;
  float bestpoi_hightowerenergy = -1;
  float bestpoi_lowtowerenergy  = -1;

  double esum[3] = {0,0,0}; //total energy deposited from all hits in the Ecal, Hcal, and Pres respectively

  TClonesArray* hits = MuFcsColl->getHitArray();
  if( hits==0 ){ LOG_INFO << "StFwdAnaFcsRun22Qa::FillFcsInfo - No FCS hits" << endm; }
  TClonesArray* clusters = MuFcsColl->getClusterArray();
  if( clusters==0 ){ LOG_INFO << "StFwdAnaFcsRun22Qa::FillFcsInfo - No FCS clusters" << endm; }
  TClonesArray* points = MuFcsColl->getPointArray();
  if( points==0 ){ LOG_INFO << "StFwdAnaFcsRun22Qa::FillFcsInfo - No FCS points" << endm; }
  
  //std::cout << "|hits:"<<hits << "|clusters:"<<clusters << "|points:"<<points << std::endl;
    
  for( UInt_t idet=0; idet<kFcsNDet; ++idet ){
    //std::cout << "+ |idet:"<<idet << "|maxdet:"<<kFcsNDet;
    if( hits!=0 ){
      unsigned int nh = MuFcsColl->numberOfHits(idet);
      mH1F_Hit_NHits[idet]->Fill(nh);
      unsigned int ihit = MuFcsColl->indexOfFirstHit(idet);
      nh += ihit; //Need to correct for the fact that number of hits is just that and doesn't correspond to max index to loop to
      //std::cout << "|nh:" << MuFcsColl->numberOfHits(idet) << "|index:"<<MuFcsColl->indexOfFirstHit(idet) << "|actualmax:"<<nh << std::endl;
      int nhitcut = 0;
      for( ; ihit<nh; ++ihit ){
	StMuFcsHit* hit = (StMuFcsHit*)hits->At(ihit);
	//unsigned short hit_det = hit->detectorId();
	unsigned short hit_ehp = hit->ehp();
	unsigned short hit_id = hit->id();
	float hit_energy = hit->energy();
	float hit_adcsum = hit->adcSum();
	float hit_tbpeak = hit->fitPeak();
	int   hit_npeak  = hit->nPeak();
	float hit_chi2   = hit->fitChi2();
	unsigned int ntb = hit->nTimeBin();
	if( idet<=kFcsHcalSouthDetId && hit_energy>1 ){ ++nhitcut; } //Ecal and Hcal Cut for nhit
	//std::cout << "   + |ihit:"<<ihit << "|idet:"<<hit_det << "|hit_id:"<<hit_id << "|hit_ehp:"<<hit_ehp << "|hit_adcsum:"<<hit_adcsum << "|hit_energy:"<<hit_energy << "|hit_ntb:"<<ntb << "|nh:"<<nh << "|th:"<<MuFcsColl->numberOfHits() << std::endl;
	esum[hit_ehp] += hit_energy;
	mH2F_Hit_enVid[idet]     ->Fill(hit_id,hit_energy);
	mH2F_Hit_fitpeakVid[idet]->Fill(hit_id,hit_tbpeak);
	mH2F_Hit_chi2Vid[idet]   ->Fill(hit_id,hit_chi2);
	mH2F_Hit_npeaksVid[idet] ->Fill(hit_id,hit_npeak);
	if( mFcsAdcTbOn ){
	  for( unsigned int i=0; i<ntb; ++i ){ ((TH1*) mH2F_Hit_adcVtb[idet]->UncheckedAt(hit_id))->Fill(hit->timebin(i),hit->adc(i)); }
	}
	if( kFcsPresNorthDetId<=idet && idet<=kFcsPresSouthDetId ){
	  continue;
	  if( hit_adcsum>250 ){ ++nhitcut; } //Pres Cut for nhit
	  if( mEpdAdcQaOn || mEpdTacQaOn ){
	    unsigned int nepdhits = 0;

	    StSPtrVecEpdHit* epdhits = 0;
	    if( MuEpdHits!=0 ){ nepdhits = MuEpdHits->GetEntriesFast(); }
	    else if( EpdColl!=0 ){
	      epdhits = &(EpdColl->epdHits());
	      nepdhits = epdhits->size();
	    }
	    else{ LOG_ERROR << "StFwdAnaFcsRun22Qa::FillFcsInfo() - If you see this error then there is a bug that is setting EPD hits improperly" << endm; return kStErr; }
	    
	    int fcspp; int fcstt;
	    FcsDb->getEPDfromId(idet,hit_id,fcspp,fcstt);
	    //For processing epd hits
	    int adc = 0;
	    int tac = 0;
	    StMuEpdHit* muepdhit = 0;
	    StEpdHit* epdhit = 0;
	    for(unsigned int i=0; i<nepdhits; ++i ){
	      if( MuEpdHits!=0 ){ muepdhit = (StMuEpdHit*)MuEpdHits->UncheckedAt(i); } //To match similar in StMuDstMaker->epdHit(int i)
	      else if( epdhits!=0 ){ epdhit = (StEpdHit*)(*epdhits)[i]; }
	      else{ LOG_ERROR << "IF YOU SEE THIS ERROR THEN THERE IS A VERY SERIOUS BUG IN THE CODE" << endm; return kStErr; }
	      int ew    = muepdhit!=0 ? muepdhit->side()    : epdhit->side();      //east=-1, west=1
	      int epdpp = muepdhit!=0 ? muepdhit->position(): epdhit->position();  //Supersector runs [1,12]
	      int epdtt = muepdhit!=0 ? muepdhit->tile()    : epdhit->tile();      //Tile number [1,31]
	      if( 1==ew ){
		adc = muepdhit!=0 ? muepdhit->adc() : epdhit->adc();
		tac = muepdhit!=0 ? muepdhit->tac() : epdhit->tac();
		if( fcspp==epdpp && fcstt==epdtt ){ break; }
	      }
	    }
	    //Here adc and tac is equal to ADC and TAC from matched epd hit
	    if( mEpdAdcQaOn ){ ((TH1*) mH2F_HitPres_depVqt[idet-kFcsPresNorthDetId]->UncheckedAt(hit_id))->Fill(adc,hit_adcsum); }
	    if( mEpdTacQaOn ){ ((TH1*) mH2F_HitPres_peakVtac[idet-kFcsPresNorthDetId]->UncheckedAt(hit_id))->Fill(tac,hit_tbpeak); }
	  }
	}
      }//fcs hits
      mH1F_Hit_NHitsCut[idet]->Fill(nhitcut);
    }
    else{ LOG_INFO <<"|hits is empty:"<<hits << endm; }

    if( clusters!=0 ){
      unsigned int nc = MuFcsColl->numberOfClusters(idet);
      mH1F_NClusters[idet]->Fill(nc);
      unsigned int iclus=MuFcsColl->indexOfFirstCluster(idet);
      nc += iclus;
      for( ; iclus<nc; ++iclus){
	StMuFcsCluster* clu = (StMuFcsCluster*)clusters->At(iclus);
	float iclu_x = clu->x();
	float iclu_y = clu->y();
	float iclu_energy = clu->energy();
	mH1F_Clu_NTowers[idet]->Fill(clu->nTowers());
	mH1F_Clu_NNei[idet]->Fill(clu->nNeighbor());
	mH1F_Clu_NPoints[idet]->Fill(clu->nPoints());
	mH1F_Clu_En[idet]->Fill(iclu_energy);
	mH2F_Clu_yVx[idet]->Fill(iclu_x,iclu_y);
	mH2F_Clu_sigmaxVsigmin[idet]->Fill(clu->sigmaMin(),clu->sigmaMax());
	mH1F_Clu_theta[idet]->Fill(clu->theta());
	mH2F_Clu_Chi2NdfPhoton_2V1[idet]->Fill(clu->chi2Ndf1Photon(),clu->chi2Ndf2Photon());

	StThreeVectorD iclu_pos = FcsDb->getStarXYZfromColumnRow( idet, iclu_x, iclu_y );
	StLorentzVectorD iclu_p = FcsDb->getLorentzVector( iclu_pos, iclu_energy, 0 );

	//std::cout << " + |idet:"<<idet <<"|iclus:"<<iclus << "|clusid:"<<clu->id() << "|npoints:"<<clu->nPoints() <<"|nTowers:"<<clu->nTowers()<<"|sigmamin:"<<clu->sigmaMin() << "|sigmamax:"<<clu->sigmaMax() << std::endl;
	/* @[June 3, 2026] > I was printing this out at some point, maybe February 2026) because I wanted to check if clusters with low sigma min and max values are coming from single tower clusters.
	if( clu->sigmaMin()<0.00001 || clu->sigmaMax()<0.00001 ){
	  TRefArray* cluhits = clu->hits();
	  for(int itow=0; itow<cluhits->GetEntriesFast(); ++itow ){
	    StMuFcsHit* hit = (StMuFcsHit*)cluhits->At(itow);
	    //std::cout << "    * |hit:"<<itow << "|col:"<< FcsDb->getColumnNumber(hit->detectorId(),hit->id()) << "|row:"<<FcsDb->getRowNumber(hit->detectorId(),hit->id()) << std::endl;
	  }
	  }*/

	if( mBestMassOn ){
	  if( idet<=kFcsEcalSouthDetId ){
	    if( iclus==(nc-1) ){ continue; }
	    for( unsigned int j=iclus+1; j<nc; j++ ){
	      StMuFcsCluster* cluj = (StMuFcsCluster*)clusters->At(j);
	      float jclu_energy = cluj->energy();
	      float jclu_x = cluj->x();
	      float jclu_y = cluj->y();
	      StThreeVectorD jclu_pos = FcsDb->getStarXYZfromColumnRow( idet, jclu_x, jclu_y );
	      double ensum = iclu_energy + jclu_energy;
	      float zgg = (fabs(iclu_energy - jclu_energy)) / (ensum);
	      StLorentzVectorD jclu_p = FcsDb->getLorentzVector( jclu_pos, jclu_energy, 0 );
	  
	      if( jclu_energy<EnCut ){ continue; }
	      if( zgg>0.7 ){ continue; }
	      if( ensum>bestclu_totale ){
		check_fillclu = true;
		bestclu_invmass = ((iclu_p + jclu_p).m());
		bestclu_totale = ensum;
		bestclu_dgg = sqrt( (iclu_pos[0]-jclu_pos[0])*(iclu_pos[0]-jclu_pos[0]) + (iclu_pos[1]-jclu_pos[1])*(iclu_pos[1]-jclu_pos[1]) + (iclu_pos[2]-jclu_pos[2])*(iclu_pos[2]-jclu_pos[2]) );
		bestclu_zgg = zgg;
		double cluidotj = iclu_pos[0]*jclu_pos[0] + iclu_pos[1]*jclu_pos[1] + iclu_pos[2]*jclu_pos[2];          //dot product of vectors for the current cluster position and cluster j position
		double iclumag = sqrt( iclu_pos[0]*iclu_pos[0] + iclu_pos[1]*iclu_pos[1] + iclu_pos[2]*iclu_pos[2] );  //magnitude of position vector for current cluster
		double jclumag = sqrt( jclu_pos[0]*jclu_pos[0] + jclu_pos[1]*jclu_pos[1] + jclu_pos[2]*jclu_pos[2] );//magnitude of position vector for cluster j
		bestclu_opening_angle = acos( cluidotj / (iclumag*jclumag) );
		if( iclu_energy>jclu_energy ){ bestclu_hightowerenergy = iclu_energy; bestclu_lowtowerenergy = jclu_energy; }
		else                         { bestclu_hightowerenergy = iclu_energy; bestclu_lowtowerenergy = iclu_energy; }
	      }
	    }//jclu
	  }
	}
      }//iclu
    }

    if( points!=0 ){
      unsigned int np = MuFcsColl->numberOfPoints(idet);
      mH1F_NPoints[idet]->Fill(np);
      unsigned int ipoint=MuFcsColl->indexOfFirstPoint(idet);
      np += ipoint;
      for( ; ipoint<np; ++ipoint ){
	StMuFcsPoint* point = (StMuFcsPoint*)points->At(ipoint);
	float ipoi_x = point->x();
	float ipoi_y = point->y();
	float ipoi_energy = point->energy();
	mH1F_Poi_En[idet]->Fill(ipoi_energy);
	mH1F_Poi_NCluPhotons[idet]->Fill(point->nParentClusterPhotons());
	mH2F_Poi_yVx[idet]->Fill(ipoi_x,ipoi_y);
	StThreeVectorD ipoi_pos = FcsDb->getStarXYZfromColumnRow( idet, ipoi_x, ipoi_y );
	StLorentzVectorD ipoi_p = FcsDb->getLorentzVector(ipoi_pos, ipoi_energy, 0);

	//std::cout << " - |idet:"<<idet << "|ipoint:"<<ipoint << "|clusid:"<<point->parentClusterId() << "|nparentpoints:"<<point->nParentClusterPhotons()<</*"|parentclusternpoints:"<< point->cluster()->nPoints() <<"|sigmamin:"<< point->cluster()->sigmaMin() << "|sigmamax:"<<point->cluster()->sigmaMax() <<*/ std::endl;

	if( mBestMassOn ){
	  if( idet<=kFcsEcalSouthDetId ){
	    if( ipoint==(np-1) ){ continue; }
	    for( unsigned int j=(ipoint+1); j<np; ++j ){
	      StMuFcsPoint* poij = (StMuFcsPoint*)points->At(j);
	      float jpoi_energy = poij->energy();
	      float poiesum = ipoi_energy+jpoi_energy;
	      float zgg = (fabs(ipoi_energy - jpoi_energy)) / (poiesum);
	      StThreeVectorD jpoi_pos = FcsDb->getStarXYZfromColumnRow(idet, poij->x(), poij->y());
	      StLorentzVectorD jpoi_p = FcsDb->getLorentzVector(jpoi_pos, jpoi_energy, 0);

	      if( idet<=kFcsEcalSouthDetId ){
		if( jpoi_energy<EnCut ){ continue; }
		if( zgg>=0.7 ){ continue; }
		if( poiesum>bestpoi_totale ){
		  check_fillpoi = true;
		  bestpoi_invmass = (ipoi_p + jpoi_p).m();
		  bestpoi_totale = poiesum;
		  bestpoi_dgg = sqrt( (ipoi_pos[0]-jpoi_pos[0])*(ipoi_pos[0]-jpoi_pos[0]) + (ipoi_pos[1]-jpoi_pos[1])*(ipoi_pos[1]-jpoi_pos[1]) + (ipoi_pos[2]-jpoi_pos[2])*(ipoi_pos[2]-jpoi_pos[2]) );
		  bestpoi_zgg = zgg;
		  double poiidotj = ipoi_pos[0]*jpoi_pos[0] + ipoi_pos[1]*jpoi_pos[1] + ipoi_pos[2]*jpoi_pos[2];          //dot product of vectors for the current point position and point j position
		  double ipoimag = sqrt( ipoi_pos[0]*ipoi_pos[0] + ipoi_pos[1]*ipoi_pos[1] + ipoi_pos[2]*ipoi_pos[2] );  //magnitude of position vector for current point
		  double jpoimag = sqrt( jpoi_pos[0]*jpoi_pos[0] + jpoi_pos[1]*jpoi_pos[1] + jpoi_pos[2]*jpoi_pos[2] );//magnitude of position vector for point j
		  bestpoi_opening_angle = acos( poiidotj / (ipoimag*jpoimag) );
		  if( ipoi_energy>jpoi_energy ){ bestpoi_hightowerenergy = ipoi_energy; bestpoi_lowtowerenergy = jpoi_energy; }
		  else                         { bestpoi_hightowerenergy = jpoi_energy; bestpoi_lowtowerenergy = ipoi_energy; }
		}
	      }
	    }//j point
	  }
	}
      }//i point
    }
    
  }//fcs dets

  mH1F_Hit_ESum[0]->Fill(esum[0]);
  mH1F_Hit_ESum[1]->Fill(esum[1]);
  mH1F_Hit_ESum[2]->Fill(esum[2]);

  if( mBestMassOn ){
    if( check_fillclu ){
      mH2F_CluHigh_angleVesum->Fill(bestclu_totale,bestclu_opening_angle);
      mH2F_CluHighEn_lowVhigh->Fill(bestclu_lowtowerenergy,bestclu_hightowerenergy);
      mH2F_CluHigh_dggVesum->Fill(bestclu_totale,bestclu_dgg);
      mH2F_CluHigh_invmassVesum->Fill(bestclu_totale,bestclu_invmass);
      mH2F_CluHigh_invmassVdgg->Fill(bestclu_dgg,bestclu_invmass);
      mH2F_CluHigh_invmassVzgg->Fill(bestclu_zgg,bestclu_invmass);
    }
    if( check_fillpoi ){
      mH2F_PoiHigh_angleVesum->Fill(bestpoi_totale,bestpoi_opening_angle);
      mH2F_PoiHighEn_lowVhigh->Fill(bestpoi_lowtowerenergy,bestpoi_hightowerenergy);
      mH2F_PoiHigh_dggVesum->Fill(bestpoi_totale,bestpoi_dgg);
      mH2F_PoiHigh_invmassVesum->Fill(bestpoi_totale,bestpoi_invmass);
      mH2F_PoiHigh_invmassVdgg->Fill(bestpoi_dgg,bestpoi_invmass);
      mH2F_PoiHigh_invmassVzgg->Fill(bestpoi_zgg,bestpoi_invmass);
    }
  }
  
  return kStOk;
}
/*
void StFwdAnaFcsRun22Qa::DrawEventInfo(TCanvas* canv, const char* savename)
{
  canv->Clear();
  canv->Divide(4,3);
  canv->cd(1);
  //mH1F_Entries->Draw("hist e");
  canv->cd(2)->SetLogy();
  mH1F_AllFcsTriggers->Draw("hist e");
  canv->cd(3);
  mH2F_Mult_tofVref->Draw("colz");
  mH2F_BxId_7V48->Draw("colz");
  canv->cd(10);
  TH1* tofmult = ((TH2*)mH2F_Mult_tofVecal)->ProjectionY("tofmult");
  tofmult->SetTitle("TOF Multiplicty");
  tofmult->Draw("hist e");
  canv->cd(11);
  mH2F_Mult_tofVecal->Draw("colz");
  canv->cd(12);
  mH1F_Spin->Draw("hist e");
  canv->Print(savename);
}
*/
void StFwdAnaFcsRun22Qa::DrawTrigger(TCanvas* canv, const char* savename)
{
  canv->Clear();
  canv->cd();
  TPad* pad = new TPad("PAD_DRAWTRIGGER","",0,0.08,1,1);
  pad->SetFillStyle(4000); //Make this pad fully transparent
  pad->Draw();
  pad->cd()->SetLogy();
  mH1F_AllFcsTriggers->Draw("hist e");
  canv->Print(savename);
}

void StFwdAnaFcsRun22Qa::DrawBxId(TCanvas* canv, const char* savename)
{
  canv->Clear();
  mH2F_BxId_7V48->Draw("colz");
  canv->Print(savename);
}

void StFwdAnaFcsRun22Qa::DrawBx7Bx48Ana(TCanvas* canv, const char* savename)
{
  TH1* out_h1_bx7 = 0;
  TH1* out_h1_bx48 = 0;
  TH1* out_h1_bx7minusbx48 = 0;
  out_h1_bx7 = ((TH2*)mH2F_BxId_7V48)->ProjectionY( "H1F_bx7" );
  out_h1_bx7->SetTitle("bx7;bx7;");
  out_h1_bx48 = ((TH2*)mH2F_BxId_7V48)->ProjectionX( "H1F_bx48" );
  out_h1_bx48->SetTitle("bx48;bx48;");
  out_h1_bx7minusbx48 = new TH1F( "H2F_bx7minusbx48", "bx7 minus bx48;bx7-bx48;", 241, -120.5, 120.5 );
  for( Int_t xbin=1; xbin<=mH2F_BxId_7V48->GetNbinsX(); ++xbin ){
    for( Int_t ybin=1; ybin<=mH2F_BxId_7V48->GetNbinsY(); ++ybin ){
      if( mH2F_BxId_7V48->GetBinContent(xbin,ybin)>0 ){
	Double_t xlow = mH2F_BxId_7V48->GetXaxis()->GetBinLowEdge(xbin);
	Double_t ylow = mH2F_BxId_7V48->GetYaxis()->GetBinLowEdge(ybin);
	Double_t diff = ylow-xlow;
	if( diff<0 ){ diff = 120 + diff; } //Bunch Id maxes at 120 so fix it so that negative differences are still positive. i.e. a negative difference means add by 120
	//out_h1_bx7minusbx48->Fill(ylow-xlow);
	out_h1_bx7minusbx48->Fill(diff);
      }
    }
  }

  canv->Clear();
  canv->Divide(2,2);
  canv->cd(1)->SetGrid();
  mH2F_BxId_7V48->SetStats(0);
  mH2F_BxId_7V48->Draw("colz");
  canv->cd(2);
  out_h1_bx7->Draw("hist e");
  canv->cd(3);
  out_h1_bx48->Draw("hist e");
  canv->cd(4);
  out_h1_bx7minusbx48->Draw("hist e");
  canv->SaveAs( savename );
  mH2F_BxId_7V48->SetStats(1);  
}

void StFwdAnaFcsRun22Qa::DrawSpinInfo(TCanvas* canv, const char* savename)
{
  canv->Clear();
  canv->Divide(2,1);
  canv->cd(1);
  //mH1F_spin4Vbx7->SetStats(0);
  //mH1F_spin4Vbx7->Draw("hist e");
  canv->cd(2);
  //mH1F_spin4Vbx48->SetStats(0);
  //mH1F_spin4Vbx48->Draw("hist e");
  canv->Print(savename);
}

void StFwdAnaFcsRun22Qa::DrawFcsHitSingle(TCanvas* canv, unsigned int det, const char* savename)
{
  canv->Clear();
  canv->Divide(3,2);
  canv->cd(1)->SetLogz(1);
  mH2F_Hit_enVid[det]->Draw("colz");
  canv->cd(2)->SetLogz(1);
  mH2F_Hit_fitpeakVid[det]->Draw("colz");
  canv->cd(3)->SetLogz(1);
  mH2F_Hit_chi2Vid[det]->Draw("colz");
  canv->cd(4)->SetLogz(1);
  mH2F_Hit_npeaksVid[det]->Draw("colz");
  canv->cd(5);
  mH1F_Hit_NHits[det]->Draw("hist e");
  canv->Print(savename);
}

void StFwdAnaFcsRun22Qa::DrawFcsClusterSingle(TCanvas* canv, unsigned int det, const char* savename)
{
  canv->Clear();
  canv->Divide(3,3);
  canv->cd(1);
  mH1F_NClusters[det]->Draw("hist e");
  canv->cd(2);
  mH1F_Clu_NTowers[det]->Draw("hist e");
  canv->cd(3);
  mH1F_Clu_NNei[det]->Draw("hist e");
  canv->cd(4);
  mH1F_Clu_NPoints[det]->Draw("hist e");
  canv->cd(5);
  mH1F_Clu_En[det]->Draw("hist e");
  canv->cd(6);
  mH2F_Clu_yVx[det]->Draw("colz");
  canv->cd(7);
  mH2F_Clu_sigmaxVsigmin[det]->Draw("colz");
  canv->cd(8);
  mH1F_Clu_theta[det]->Draw("hist e");
  canv->cd(9);
  mH2F_Clu_Chi2NdfPhoton_2V1[det]->Draw("colz");
  canv->Print(savename);
}

void StFwdAnaFcsRun22Qa::DrawFcsPointSingle(TCanvas* canv, unsigned int det, const char* savename)
{
  canv->Clear();
  canv->Divide(2,2);
  canv->cd(1);
  mH1F_NPoints[det]->Draw("hist e");
  canv->cd(2);
  mH1F_Poi_En[det]->Draw("hist e");
  canv->cd(3);
  mH1F_Poi_NCluPhotons[det]->Draw("hist e");
  canv->cd(4);
  mH2F_Poi_yVx[det]->Draw("colz");
  canv->Print(savename);
}

void StFwdAnaFcsRun22Qa::DrawFcsTotalE(TCanvas* canv, const char* savename)
{
  canv->Clear();
  canv->Divide(2,2);
  canv->cd(1);
  mH1F_Hit_ESum[0]->Draw("hist e");
  canv->cd(2);
  mH1F_Hit_ESum[1]->Draw("hist e");
  canv->cd(3);
  mH1F_Hit_ESum[2]->Draw("hist e");
  canv->Print(savename);
}

void StFwdAnaFcsRun22Qa::DrawAdcVTb(TCanvas* canv, const char* savename)
{
  if( mFcsAdcTbOn ){
    canv->Clear();
    canv->Divide(5,5);
    for( UInt_t i=0; i<kFcsNDet; ++i ){
      for( Int_t ich=0, ipad=1; ich<mH2F_Hit_adcVtb[i]->GetEntriesFast(); ++ich,++ipad ){
	if( ipad>25 ){ ipad=1; canv->Print(savename); canv->Clear(); canv->Divide(5,5); }
	canv->cd(ipad);
	((TH1*)mH2F_Hit_adcVtb[i]->UncheckedAt(ich))->Draw("colz");
      }
    }
    canv->Print(savename);
  }
}

void StFwdAnaFcsRun22Qa::DrawFcsHitQa(TCanvas* canv, const char* savename)
{
  for( UShort_t i=0; i<kFcsNDet; ++i ){
    canv->Clear();
    canv->Divide(3,2);
    canv->cd(1)->SetLogz(1);
    mH2F_Hit_enVid[i]->Draw("colz");
    canv->cd(2)->SetLogz(1);
    mH2F_Hit_fitpeakVid[i]->Draw("colz");
    canv->cd(3)->SetLogz(1);
    mH2F_Hit_chi2Vid[i]->Draw("colz");
    canv->cd(4)->SetLogz(1);
    mH2F_Hit_npeaksVid[i]->Draw("colz");
    canv->cd(5);
    mH1F_Hit_NHits[i]->Draw("hist e");
    canv->cd(6);
    mH1F_Hit_NHitsCut[i]->Draw("hist e");
    canv->Print(savename);
  }
  canv->Clear();
  canv->Divide(2,2);
  canv->cd(1);
  mH1F_Hit_ESum[0]->Draw("hist e");
  canv->cd(2);
  mH1F_Hit_ESum[1]->Draw("hist e");
  canv->cd(3);
  mH1F_Hit_ESum[2]->Draw("hist e");
  canv->Print(savename);
}


void StFwdAnaFcsRun22Qa::DrawEpdDepAdcQa(TCanvas* canv, const char* savename)
{
  if( mEpdAdcQaOn || (mH2F_HitPres_depVqt[0]!=0 && mH2F_HitPres_depVqt[1]!=0) ){
    canv->Clear();
    canv->Divide(5,5);
    for( UInt_t i=0; i<2; ++i ){
      for( Int_t ich=0, ipad=1; ich<mH2F_HitPres_depVqt[i]->GetEntriesFast(); ++ich,++ipad ){
	if( ipad>25 ){ ipad=1; canv->Print(savename); canv->Clear(); canv->Divide(5,5); }
	canv->cd(ipad)->SetLogz(true);
	((TH1*)mH2F_HitPres_depVqt[i]->UncheckedAt(ich))->Draw("colz");
      }
    }
    canv->Print(savename);
  }  
}

void StFwdAnaFcsRun22Qa::DrawEpdDepTacQa(TCanvas* canv, const char* savename)
{
  if( mEpdTacQaOn || (mH2F_HitPres_peakVtac[0]!=0 && mH2F_HitPres_peakVtac[1]!=0) ){
    canv->Clear();
    canv->Divide(5,5);
    for( UInt_t i=0; i<2; ++i ){
      for( Int_t ich=0, ipad=1; ich<mH2F_HitPres_peakVtac[i]->GetEntriesFast(); ++ich,++ipad ){
	if( ipad>25 ){ ipad=1; canv->Print(savename); canv->Clear(); canv->Divide(5,5); }
	canv->cd(ipad);
	((TH1*)mH2F_HitPres_peakVtac[i]->UncheckedAt(ich))->Draw("colz");
      }
    }
  }
}

void StFwdAnaFcsRun22Qa::DrawFcsClusterQa(TCanvas* canv, const char* savename)
{
  for( UShort_t i=0; i<kFcsNDet; ++i ){
    canv->Clear();
    canv->Divide(3,3);
    canv->cd(1);
    mH1F_NClusters[i]->Draw("hist e");
    canv->cd(2);
    mH1F_Clu_NTowers[i]->Draw("hist e");
    canv->cd(3);
    mH1F_Clu_NNei[i]->Draw("hist e");
    canv->cd(4);
    mH1F_Clu_NPoints[i]->Draw("hist e");
    canv->cd(5);
    mH1F_Clu_En[i]->Draw("hist e");
    canv->cd(6);
    mH2F_Clu_yVx[i]->Draw("colz");
    canv->cd(7);
    mH2F_Clu_sigmaxVsigmin[i]->Draw("colz");
    canv->cd(8);
    mH1F_Clu_theta[i]->Draw("hist e");
    canv->cd(9);
    mH2F_Clu_Chi2NdfPhoton_2V1[i]->Draw("colz");
    canv->Print(savename);
  }
}

void StFwdAnaFcsRun22Qa::DrawFcsClusterPi0(TCanvas* canv, const char* savename)
{
  canv->Clear();
  if( mBestMassOn ){
    canv->Divide(3,2);
    canv->cd(1);
    mH2F_CluHigh_angleVesum->Draw("colz");
    canv->cd(2);
    mH2F_CluHighEn_lowVhigh->Draw("colz");
    canv->cd(3);
    mH2F_CluHigh_dggVesum->Draw("colz");
    canv->cd(4);
    mH2F_CluHigh_invmassVesum->Draw("colz");
    canv->cd(5);
    mH2F_CluHigh_invmassVdgg->Draw("colz");
    canv->cd(6);
    mH2F_CluHigh_invmassVzgg->Draw("colz");
    canv->Print(savename);
  }
}
void StFwdAnaFcsRun22Qa::DrawFcsPointQa(TCanvas* canv, const char* savename)
{
  for( UShort_t i=0; i<kFcsNDet; ++i ){
    canv->Clear();
    canv->Divide(2,2);
    canv->cd(1);
    mH1F_NPoints[i]->Draw("hist e");
    canv->cd(2);
    mH1F_Poi_En[i]->Draw("hist e");
    canv->cd(3);
    mH1F_Poi_NCluPhotons[i]->Draw("hist e");
    canv->cd(4);
    mH2F_Poi_yVx[i]->Draw("colz");
    canv->Print(savename);
  }
}

void StFwdAnaFcsRun22Qa::DrawFcsPointPi0(TCanvas* canv, const char* savename)
{
  canv->Clear();
  if( mBestMassOn ){
    canv->Divide(3,2);
    canv->cd(1);
    mH2F_PoiHigh_angleVesum->Draw("colz");
    canv->cd(2);
    mH2F_PoiHighEn_lowVhigh->Draw("colz");
    canv->cd(3);
    mH2F_PoiHigh_dggVesum->Draw("colz");
    canv->cd(4);
    mH2F_PoiHigh_invmassVesum->Draw("colz");
    canv->cd(5);
    mH2F_PoiHigh_invmassVdgg->Draw("colz");
    canv->cd(6);
    mH2F_PoiHigh_invmassVzgg->Draw("colz");
    canv->Print(savename);
  }
}
/*
Int_t StFwdAnaFcsRun22Qa::LoadGraphsFromFile(TFile* file, TObjArray* graphs, StFwdAnaData* anadata )
{
  Int_t gloaded = 0;
  gloaded += StFwdAnaData::MakeGraph(file,graphs,mG_Entries,"G_Entries","Number of Entries vs. Run Index");
  gloaded += StFwdAnaData::MakeGraph(file,graphs,mGE_VertexVpd,"GE_VertexVpd","VPD vertex mean (Err=RMS) vs. Run index");
  gloaded += StFwdAnaData::MakeGraph(file,graphs,mGE_VertexBbc,"GE_VertexBbc","Bbc vertex mean (Err=RMS) vs. Run index");
  gloaded += StFwdAnaData::MakeGraph(file,graphs,mG_UpSpin,"G_UpSpin","Number of Spin Up states vs. Run Index");
  gloaded += StFwdAnaData::MakeGraph(file,graphs,mG_NoSpin,"G_NoSpin","Number of No Spin states vs. Run Index");
  gloaded += StFwdAnaData::MakeGraph(file,graphs,mG_DownSpin,"G_DownSpin","Number of Spin Down states vs. Run Index");
  
  //Trigger QA
  int trigsize = anadata->sizeOfFcsTriggers();
  //std::cout << "|trigsize:"<<trigsize << std::endl;
  if( trigsize==64 ){ //Check to make sure all triggers are in the map and it matches the bin size
    for( int i=0; i<trigsize; ++i ){
      TString trigname(anadata->fcsTriggerName(i));
      TString gname("G_");
      TString gtitle("Number of Events for trigger ");
      gname += trigname;
      gtitle += trigname + " vs. Run Index";
      gloaded += StFwdAnaData::MakeGraph(file,graphs,mG_Triggers[i],gname.Data(),gtitle.Data());
    }
    gloaded += StFwdAnaData::MakeGraph(file,graphs,mG_Triggers[64],"NF","Number of Events with no trigger vs. Run Index"); //Last bin is named "NF" for not found which is what nameFromId() returns if trigger is not in the map. This way if no map was loaded but an StFcsRun22TriggerMap was found, searching for triggers will return "NF"
  }

  //Nhits QA
  TString namesuffix[6] = {"EN","ES","HN","HS","PN","PS"};
  for( UShort_t i=0; i<6; ++i ){  //kFcsNDet=6, hard coded so I don't need database access
    //UInt_t nchs = 0;
    //UInt_t ncol = 0;
    //UInt_t nrow = 0;
    //if( i<=kFcsEcalSouthDetId )                              { nchs = kFcsEcalMaxId; ncol = kFcsEcalNCol; nrow = kFcsEcalNRow; }
    //else if( kFcsHcalNorthDetId<=i && i<=kFcsHcalSouthDetId ){ nchs = kFcsHcalMaxId; ncol = kFcsHcalNCol; nrow = kFcsHcalNRow; }
    //else if( kFcsPresNorthDetId<=i && i<=kFcsPresSouthDetId ){ nchs = kFcsPresMaxId; ncol = kFcsPresNCol; nrow = kFcsPresNRow; }
    //else{ LOG_ERROR << "StFcsFun22QaMaker::LoadHists() too many detector ids for some reason" << endm; return kStErr; }
    //histname = "H2F_Hit_enVid_" + namesuffix[i];
    //histtitle = "Energy vs. Id for " + namesuffix[i] + ";id;energy (GeV)";
    //loaded += mHists->AddH2F(file,(mH2F_Hit_enVid[i]),histname.Data(),histtitle.Data(),nchs,0,nchs,200,0,200);
    TString gname = "GE_NHits_" + namesuffix[i];
    TString gtitle = "Mean number of hits (Err=RMS) for " + namesuffix[i] + " vs. Run Index";
    gloaded += StFwdAnaData::MakeGraph(file,graphs,mGE_NHits[i],gname.Data(),gtitle.Data());
    gname = "GE_NHitsCut_" + namesuffix[i];
    gtitle = "Mean number of hits with Cuts (Err=RMS) for " + namesuffix[i] + " vs. Run Index";
    gloaded += StFwdAnaData::MakeGraph(file,graphs,mGE_NHitsCut[i],gname.Data(),gtitle.Data());

    gname = "GE_NClusters_" + namesuffix[i];
    gtitle = "Mean number of clusters (Err=RMS) for " + namesuffix[i] + " vs. Run Index";
    gloaded += StFwdAnaData::MakeGraph(file,graphs,mGE_NClusters[i],gname.Data(),gtitle.Data());
    gname = "GE_NPoints_" + namesuffix[i];
    gtitle = "Mean number of points (Err=RMS) for " + namesuffix[i] + " vs. RunIndex";
    gloaded += StFwdAnaData::MakeGraph(file,graphs,mGE_NPoints[i],gname.Data(),gtitle.Data());
    gname = "G_Clu_En_" + namesuffix[i];
    gtitle = "Mean of Cluster Energy for " + namesuffix[i] + " vs. RunIndex;;GeV";
    gloaded += StFwdAnaData::MakeGraph(file,graphs,mG_Clu_En[i],gname.Data(),gtitle.Data());
    gname = "G_Poi_En_" + namesuffix[i];
    gtitle = "Mean of Point Energy for " + namesuffix[i] + " vs. RunIndex;;GeV";
    gloaded += StFwdAnaData::MakeGraph(file,graphs,mG_Poi_En[i],gname.Data(),gtitle.Data());
  }
  
  gloaded += StFwdAnaData::MakeGraph(file,graphs,mGE_ESum_Ecal,"GE_ESum_Ecal","Ecal total energy (Err=RMS) vs. Run Index");
  gloaded += StFwdAnaData::MakeGraph(file,graphs,mGE_ESum_Hcal,"GE_ESum_Hcal","Hcal total energy (Err=RMS) vs. Run Index");
  gloaded += StFwdAnaData::MakeGraph(file,graphs,mGE_ESum_Pres,"GE_ESum_Pres","Pres total energy (Err=RMS) vs. Run Index");

  return gloaded;
}

void StFwdAnaFcsRun22Qa::FillGraphs(Int_t irun)
{
  //Entries QA
  //std::cout << "|mG_Entries:"<<mG_Entries << std::endl;
  //std::cout << "|mH1F_Entries:"<<mH1F_Entries << std::endl;
  //mG_Entries->SetPoint(irun,irun,mH1F_Entries->GetEntries());

  //Vertex QA
  mGE_VertexVpd->SetPoint(irun,irun,mH1F_VertexVpd->GetMean());
  mGE_VertexVpd->SetPointError(irun,0,mH1F_VertexVpd->GetRMS());
  mGE_VertexBbc->SetPoint(irun,irun,mH1F_VertexBbc->GetMean());
  mGE_VertexBbc->SetPointError(irun,0,mH1F_VertexBbc->GetRMS());
  
  //Trigger QA
  if( mH1F_AllFcsTriggers==0 || mH1F_AllFcsTriggers->GetNbinsX()!=65 ){ std::cout << "huge errors" << std::endl; return; }
  for( int i=0; i<65; ++i ){
    mG_Triggers[i]->SetPoint(irun,irun,mH1F_AllFcsTriggers->GetBinContent(i+1));
  }

  //Spin state QA
  mG_UpSpin->SetPoint(irun,irun,mH1F_Spin->GetBinContent(3));
  mG_NoSpin->SetPoint(irun,irun,mH1F_Spin->GetBinContent(2));
  mG_DownSpin->SetPoint(irun,irun,mH1F_Spin->GetBinContent(1));

  //NHit QA
  for( UShort_t i=0; i<6; ++i ){  //kFcsNDet=6, hard coded so I don't need database access
    mGE_NHits[i]->SetPoint(irun,irun,mH1F_Hit_NHits[i]->GetMean());
    mGE_NHits[i]->SetPointError(irun,0,mH1F_Hit_NHits[i]->GetRMS());
    mGE_NHitsCut[i]->SetPoint(irun,irun,mH1F_Hit_NHitsCut[i]->GetMean());
    mGE_NHitsCut[i]->SetPointError(irun,0,mH1F_Hit_NHitsCut[i]->GetRMS());

    mGE_NClusters[i]->SetPoint(irun,irun,mH1F_NClusters[i]->GetMean());
    mGE_NClusters[i]->SetPointError(irun,0,mH1F_NClusters[i]->GetRMS());

    mGE_NPoints[i]->SetPoint(irun,irun,mH1F_NPoints[i]->GetMean());
    mGE_NPoints[i]->SetPointError(irun,0,mH1F_NPoints[i]->GetMean());

    mG_Clu_En[i]->SetPoint(irun,irun,mH1F_Clu_En[i]->GetMean());
    mG_Poi_En[i]->SetPoint(irun,irun,mH1F_Poi_En[i]->GetMean());
  }

  //ESum QA
  mGE_ESum_Ecal->SetPoint(irun,irun,mH1F_Hit_ESum[0]->GetMean());
  mGE_ESum_Ecal->SetPointError(irun,0,mH1F_Hit_ESum[0]->GetRMS());

  mGE_ESum_Hcal->SetPoint(irun,irun,mH1F_Hit_ESum[1]->GetMean());
  mGE_ESum_Hcal->SetPointError(irun,0,mH1F_Hit_ESum[1]->GetRMS());

  mGE_ESum_Pres->SetPoint(irun,irun,mH1F_Hit_ESum[2]->GetMean());
  mGE_ESum_Pres->SetPointError(irun,0,mH1F_Hit_ESum[2]->GetRMS());
}

void StFwdAnaFcsRun22Qa::DrawGraphs(TCanvas* canv, const char* savename)
{
  canv->Clear();
  canv->Divide(2,3);
  canv->cd(1);
  mG_Entries->Draw("AL");

  //Vertex QA
  canv->cd(2);
  mGE_VertexVpd->Draw("AL");
  canv->cd(3);
  mGE_VertexBbc->Draw("AL");

  //Spin state QA
  canv->cd(4);
  mG_UpSpin->Draw("AL");
  canv->cd(5);
  mG_NoSpin->Draw("AL");
  canv->cd(6);
  mG_DownSpin->Draw("AL");

  canv->Print(savename);
}

void StFwdAnaFcsRun22Qa::DrawGraphTrig(TCanvas* canv, const char* savename )
{
  canv->Clear();
  canv->Divide(4,4);
  int iplot=0;
  for( int i=0; i<65; ++i ){
    if( GraphAverage(mG_Triggers[i])<1 ){ continue; }
    canv->cd(iplot%16+1);
    mG_Triggers[i]->Draw("AL");
    if( ((iplot+1)%16)==0 ){ canv->Print(savename); canv->Clear(); canv->Divide(4,4); iplot=0; }
    else{ ++iplot; }
  }
  canv->Print(savename);
}

double StFwdAnaFcsRun22Qa::GraphAverage(TGraph* g)
{
  double sum = 0;
  for( int i=0; i<g->GetN(); ++i ){
    double x,y;
    g->GetPoint(i,x,y);
    sum += y;
  }
  return sum;
}

void StFwdAnaFcsRun22Qa::DrawGraphNhits(TCanvas* canv, const char* savename )
{
  canv->Clear();
  canv->Divide(2,3);

  //NHit QA
  for( UShort_t i=0; i<6; ++i ){  //kFcsNDet=6, hard coded so I don't need database access
    canv->cd(i+1);
    mGE_NHits[i]->Draw("AL");
  }
  canv->Print(savename);
}

void StFwdAnaFcsRun22Qa::DrawGraphNhitsCut(TCanvas* canv, const char* savename )
{
  canv->Clear();
  canv->Divide(2,3);

  //NHit QA
  for( UShort_t i=0; i<6; ++i ){  //kFcsNDet=6, hard coded so I don't need database access
    canv->cd(i+1);
    mGE_NHitsCut[i]->Draw("AL");
  }
  canv->Print(savename);
}

void StFwdAnaFcsRun22Qa::DrawGraphESum(TCanvas* canv, const char* savename )
{
  canv->Clear();
  canv->Divide(2,2);

  //ESum QA
  canv->cd(1);
  mGE_ESum_Ecal->Draw("AL");
  canv->cd(2);
  mGE_ESum_Hcal->Draw("AL");
  canv->cd(3);
  mGE_ESum_Pres->Draw("AL");

  canv->Print(savename);
}

void StFwdAnaFcsRun22Qa::DrawGraphNClusters(TCanvas* canv, const char* savename )
{
  canv->Clear();
  canv->Divide(2,3);
  //NClusters QA
  for( UShort_t i=0; i<6; ++i ){  //kFcsNDet=6, hard coded so I don't need database access
    canv->cd(i+1);
    mGE_NClusters[i]->Draw("AL");
  }
  canv->Print(savename);
}

void StFwdAnaFcsRun22Qa::DrawGraphNPoints(TCanvas* canv, const char* savename )
{
  canv->Clear();
  canv->Divide(2,3);
  //NPoints QA
  for( UShort_t i=0; i<6; ++i ){  //kFcsNDet=6, hard coded so I don't need database access
    canv->cd(i+1);
    mGE_NPoints[i]->Draw("AL");
  }
  canv->Print(savename);
}

void StFwdAnaFcsRun22Qa::DrawGraphCluEn(TCanvas* canv, const char* savename)
{
  canv->Clear();
  canv->Divide(2,3);
  //Cluster Energy QA
  for( UShort_t i=0; i<6; ++i ){  //kFcsNDet=6, hard coded so I don't need database access
    canv->cd(i+1);
    mG_Clu_En[i]->Draw("AL");
  }
  canv->Print(savename);
}

void StFwdAnaFcsRun22Qa::DrawGraphPoiEn(TCanvas* canv, const char* savename)
{
  canv->Clear();
  canv->Divide(2,3);
  //Point Energy QA
  for( UShort_t i=0; i<6; ++i ){  //kFcsNDet=6, hard coded so I don't need database access
    canv->cd(i+1);
    mG_Poi_En[i]->Draw("AL");
  }
  canv->Print(savename);
}
*/

void StFwdAnaFcsRun22Qa::PrintSpinBits()
{
  /*
  std::cout << "==================================================" << std::endl;
  if( mH1F_spin4Vbx7!=0 ){
    std::cout << "Spin from Bx7" << std::endl;
    for( Int_t ibx7=1; ibx7<=mH1F_spin4Vbx7->GetNbinsX(); ++ibx7 ){
      std::cout << "|bx7:"<<ibx7-1 << "|spin4:"<<mH1F_spin4Vbx7->GetBinContent(ibx7) << std::endl;
    }
  }
  std::cout << "==================================================" << std::endl;
  if( mH1F_spin4Vbx48!=0 ){
    std::cout << "Spin from Bx48" << std::endl;
    for( Int_t ibx48=1; ibx48<=mH1F_spin4Vbx48->GetNbinsX(); ++ibx48 ){
      std::cout << "|bx48:"<<ibx48-1 << "|spin4:"<<mH1F_spin4Vbx48->GetBinContent(ibx48) << std::endl;
    }
  }
  std::cout << "==================================================" << std::endl;
  */
}



