 /**************************************************************************
 * Class      : StSsdPointMaker.cxx
 **************************************************************************
 *
 * $Log: StSsdPointMaker.cxx,v $
 * Revision 1.1  2004/03/12 06:12:37  jeromel
 * Peer review closed. Yuri/Frank.
 *
 * Revision 1.3  2002/03/25 20:13:05  hippolyt
 * Merged the two former makers 
 *
 *
 **************************************************************************/
#include "StSsdPointMaker.h"

#include "TFile.h"
#include "TH1.h"
#include "TH2.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "StMessMgr.h"

#include "StSsdBarrel.hh"
#include "tables/St_spa_strip_Table.h"
#include "tables/St_scf_cluster_Table.h"
#include "tables/St_scm_spt_Table.h"

#include "tables/St_sdm_geom_par_Table.h" //needed to call StTable->GetTable()
#include "StEvent.h"
#include "StSsdHitCollection.h"
#include "StSsdDynamicControl.h"
#include "StSsdClusterControl.h"


ClassImp(StSsdPointMaker)
//_____________________________________________________________________________
StSsdPointMaker::StSsdPointMaker(const char *name):
StMaker(name),
m_geom_par(0),
m_noise(0),
m_condition_db(0),
m_geom(0)

{
}
//_____________________________________________________________________________
StSsdPointMaker::~StSsdPointMaker(){
}
//_____________________________________________________________________________
Int_t StSsdPointMaker::Init(){
  if (Debug())  gMessMgr->Debug() << "In StSsdPointMaker::Make() ... "
                               << GetName() << endm;
  // 		Create tables
  St_DataSet *svtparams = GetInputDB("svt");
  St_DataSetIter       local(svtparams);

  // Replace tables for dynamic parameters with default values
  mDynamicControl = new StSsdDynamicControl();
  mDynamicControl->printParameters();

  // Replace tables for control parameters
  mClusterControl = new StSsdClusterControl();
  // Set Control Parameters  
  // Former Scf
  mClusterControl->setHighCut(5.0);
  mClusterControl->setTestTolerance(0.2);
  // Former Scm
  mClusterControl->setClusterTreat(13);
  mClusterControl->setAdcTolerance(0.2);
  mClusterControl->setMatchMean(0.);
  mClusterControl->setMatchSigma(8.);
  mClusterControl->printParameters();
  // End of Setting Cluster Control parameters

  m_geom_par     = (St_sdm_geom_par      *)local("ssd/sdm_geom_par");
  m_condition_db = (St_sdm_condition_db  *)local("ssd/sdm_condition_db");
  m_geom         = (St_svg_geom          *)local("svgpars/geom");
  m_noise        = (St_sdm_calib_db      *)local("ssd/sdm_calib_db");

  if ((!m_geom_par)||(!m_geom)) {
    gMessMgr->Error() << "No  access to geometry parameters" << endm;
  }   
  if (!m_condition_db) {
    gMessMgr->Error() << "No  access to condition database" << endm;
  }   
  if (!m_noise) {
    gMessMgr->Error() << "No  access to noise condition" << endm;
  }
  if ((!mDynamicControl)||(!mClusterControl)) {
    gMessMgr->Error() << "No  access to control parameters" << endm;
  } 

  // 		Create SCF histograms
  noisDisP = new TH1F("Noise_p","Noise Distribution",25,0,25);
  snRatioP = new TH1F("SN_p","Signal/Noise (p)",200,0,200);
  stpClusP = new TH1F("NumberOfStrips_p","Strips per Cluster",8,0,8);
  totChrgP = new TH1F("ChargeElectron_p","Total Cluster Charge",100,0,300000);

  noisDisN = new TH1F("Noise_n","Noise Distribution",25,0,25);
  snRatioN = new TH1F("SN_n","Signal/Noise",200,0,200);
  stpClusN = new TH1F("NumberOfStrips_n","Strips per Cluster",8,0,8);
  totChrgN = new TH1F("ChargeElectron_n","Total Cluster Charge",100,0,300000);

// 		Create SCM histograms
  matchisto = new TH2S("matchingHisto","Matching Adc (1p-1n)",50,0,1000,50,0,1000);
  matchisto->SetXTitle("PSide ADC count");
  matchisto->SetYTitle("NSide ADC count");
  matchisto->SetZTitle("(1p-1n) hits");

  matchisto->SetTitleOffset(2,"X");
  matchisto->SetTitleOffset(2,"Y");
//   matchisto->SetTitleOffset(-1,"Z");

  matchisto->SetLabelSize(0.03,"X");
  matchisto->SetLabelSize(0.03,"Y");
  matchisto->SetLabelSize(0.03,"Z");

  matchisto->SetNdivisions(5,"X");
  matchisto->SetNdivisions(5,"Y");
  matchisto->SetNdivisions(10,"Z");
  orthoproj = new TH1S("ProjectionOrtho","Perfect Matching Deviation",80,-80,80);

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t StSsdPointMaker::Make()
{
  if (Debug())  gMessMgr->Debug() << "In StSsdPointMaker::Make() ... "
                               << GetName() << endm;
  // 		Create output tables
  Int_t res = 0; 
  
  St_spa_strip *spa_strip = (St_spa_strip *)GetDataSet("spa_strip/.data/spa_strip");
  if (!spa_strip){
    gMessMgr->Warning("StSsdPointMaker: no input (fired strip for the SSD)");
    return kStErr;
  }
  
  St_scm_spt *scm_spt = new St_scm_spt("scm_spt",5000);
  m_DataSet->Add(scm_spt);

  mCurrentEvent = (StEvent*) GetInputDS("StEvent");
  if(mCurrentEvent) mSsdHitColl = mCurrentEvent->ssdHitCollection();
  else              mSsdHitColl = 0;
  
  sdm_geom_par_st  *geom_par = m_geom_par->GetTable();

  if ((!geom_par)||(!geom_par)){
    gMessMgr->Error() << "No geometry or control parameters " << endm;
    return kStErr;
  }

  cout<<"#################################################"<<endl;
  cout<<"####     START OF NEW SSD POINT MAKER        ####"<<endl;
  cout<<"####        SSD BARREL INITIALIZATION        ####"<<endl;  
  StSsdBarrel *mySsd = new StSsdBarrel(geom_par);
  mySsd->initLadders(m_geom);
  int stripTableSize = mySsd->readStripFromTable(spa_strip);
  cout<<"####        NUMBER OF SPA STRIPS "<<stripTableSize<<"        ####"<<endl;
  mySsd->sortListStrip();
  int noiseTableSize = mySsd->readNoiseFromTable(m_noise,mDynamicControl);
  cout<<"####       NUMBER OF DB ENTRIES "<<noiseTableSize<<"       ####"<<endl;
  int nClusterPerSide[2];
  nClusterPerSide[0] = 0;
  nClusterPerSide[1] = 0;
  mySsd->doSideClusterisation(nClusterPerSide,mClusterControl);
  cout<<"####      NUMBER OF CLUSTER P SIDE "<<nClusterPerSide[0]<<"      ####"<<endl;
  cout<<"####      NUMBER OF CLUSTER N SIDE "<<nClusterPerSide[1]<<"      ####"<<endl;
  mySsd->sortListCluster();
  int nPackage = mySsd->doClusterMatching(geom_par,mClusterControl);
  cout<<"####   -> "<<nPackage<<" PACKAGES IN THE SSD           ####"<<endl;
  mySsd->convertDigitToAnalog(mDynamicControl);
  mySsd->convertUFrameToOther(geom_par);
  int nSptWritten = mySsd->writePointToContainer(scm_spt,mSsdHitColl);
  cout<<"####   -> "<<nSptWritten<<" HITS WRITTEN INTO TABLE       ####"<<endl;
  if(mSsdHitColl) 
    cout<<"####   -> "<<mSsdHitColl->numberOfHits()<<" HITS WRITTEN INTO CONTAINER   ####"<<endl;
  scm_spt->Purge();
  cout<<"####        END OF SSD NEW POINT MAKER       ####"<<endl;
  cout<<"#################################################"<<endl;
  delete mySsd;
  if (nSptWritten) res = kStOK;
 
  if(res!=kStOK){
    gMessMgr->Warning("StSsdPointMaker: no output");
    return kStWarn;
  }

  makeScmCtrlHistograms();
  
  return kStOK;
}
//_____________________________________________________________________________
void StSsdPointMaker::makeScfCtrlHistograms()
{
  St_DataSetIter scf_iter(m_DataSet);
  St_scf_cluster *scf_cluster = 0;
  scf_cluster = (St_scf_cluster *) scf_iter.Find("scf_cluster"); 

// 		Fill histograms
  if (scf_cluster->GetNRows()){
    Int_t clustSide   = 0;  // pside = 0 et nside = 1 
    scf_cluster_st *dClus = scf_cluster->GetTable();
    Float_t convAdcToE = (mDynamicControl->getADCDynamic()*mDynamicControl->getNElectronInAMip())/(pow(2.0,mDynamicControl->getNBitEncoding()));
    for (Int_t iScf = 0; iScf < scf_cluster->GetNRows(); iScf++, dClus++)
      {
	clustSide = ((dClus->id_cluster/10000)-(dClus->id_cluster/100000)*10);
	if(!clustSide)
	  {
	    noisDisP->Fill(dClus->noise_count/dClus->n_strip);
	    snRatioP->Fill((dClus->adc_count*dClus->n_strip)/dClus->noise_count);
	    stpClusP->Fill(dClus->n_strip);
	    totChrgP->Fill(convAdcToE*dClus->adc_count);
	  }
	else
	  {
	    noisDisN->Fill(dClus->noise_count/dClus->n_strip);
	    snRatioN->Fill((dClus->adc_count*dClus->n_strip)/dClus->noise_count);
	    stpClusN->Fill(dClus->n_strip);
	    totChrgN->Fill(convAdcToE*dClus->adc_count);
	  }
      }
  }
}
//_____________________________________________________________________________
void StSsdPointMaker::writeScfCtrlHistograms()
{  

  ScfCtrlFile = new TFile("event/scfCtrl_histos.root","RECREATE");
  
  noisDisP->Write();
  snRatioP->Write();
  stpClusP->Write();
  totChrgP->Write();
  
  noisDisN->Write();
  snRatioN->Write();
  stpClusN->Write();
  totChrgN->Write();
    
  ScfCtrlFile->Close();
  
}
//_____________________________________________________________________________
void StSsdPointMaker::makeScmCtrlHistograms()
{
  St_DataSetIter scm_iter(m_DataSet);
  St_scm_spt *scm_spt = 0;
  scm_spt = (St_scm_spt *) scm_iter.Find("scm_spt"); 

// 		Fill histograms 
  if (scm_spt->GetNRows()){
    scm_spt_st *dSpt = scm_spt->GetTable();
    Float_t convMeVToAdc = (int)pow(2.0,mDynamicControl->getNBitEncoding())/(mDynamicControl->getPairCreationEnergy()*mDynamicControl->getADCDynamic()*mDynamicControl->getNElectronInAMip());
    for (Int_t iScm = 0; iScm < scm_spt->GetNRows(); iScm++, dSpt++)
      {
	if (dSpt->id_match == 11)// case 11  		    
	  {
	    Float_t a = 0, b = 0;
	    a = convMeVToAdc*(dSpt->de[0]+dSpt->de[1]);
	    b = convMeVToAdc*(dSpt->de[0]-dSpt->de[1]);
	    matchisto->Fill(a,b);
	    orthoproj->Fill((b-a)/TMath::Sqrt(2.));
	  }
      }
//     matchisto->Draw();
  }
}
//_____________________________________________________________________________
void StSsdPointMaker::writeScmCtrlHistograms()
{
  ScmCtrlFile = new TFile("event/scmCtrl_histos.root","RECREATE");

  matchisto->Write();
  orthoproj->Write();

  ScmCtrlFile->Close();
}
//_____________________________________________________________________________
void StSsdPointMaker::PrintInfo()
{
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
Int_t StSsdPointMaker::Finish() {
  if (Debug()) gMessMgr->Debug() << "In StSsdPointMaker::Finish() ... "
                               << GetName() << endm; 
  writeScfCtrlHistograms();
  writeScmCtrlHistograms();
  return kStOK;
}

