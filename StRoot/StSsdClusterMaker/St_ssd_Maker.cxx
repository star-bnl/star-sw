 /**************************************************************************
 * Class      : St_ssd_Maker.cxx
 **************************************************************************
 *
 * $Log: St_ssd_Maker.cxx,v $
 * Revision 1.1  2003/10/08 03:18:09  suire
 * *** empty log message ***
 *
 * Revision 1.3  2002/03/25 20:13:05  hippolyt
 * Merged the two former makers 
 *
 *
 **************************************************************************/
#include <stdlib.h>
#include "St_ssd_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include "StMessMgr.h"

#include "StSsdBarrel.hh"
#include "tables/St_spa_strip_Table.h"
#include "tables/St_scf_cluster_Table.h"
#include "tables/St_scm_spt_Table.h"

#include "tables/St_sdm_geom_par_Table.h" //needed to call StTable->GetTable()
#include "StSsdClusterControl.h"
#include "tables/St_sls_ctrl_Table.h"
#include "tables/St_scf_ctrl_Table.h"
#include "tables/St_scm_ctrl_Table.h"

ClassImp(St_ssd_Maker)
//_____________________________________________________________________________
St_ssd_Maker::St_ssd_Maker(const char *name):
StMaker(name),
m_geom_par(0),
m_noise(0),
m_condition_db(0),
m_geom(0),
m_scf_ctrl(0),
m_sls_ctrl(0),
m_scm_ctrl(0)

{
}
//_____________________________________________________________________________
St_ssd_Maker::~St_ssd_Maker(){
}
//_____________________________________________________________________________
Int_t St_ssd_Maker::Init(){
  if (Debug())  gMessMgr->Debug() << "In St_ssd_Maker::Make() ... "
                               << GetName() << endm;
  // 		Create tables
  St_DataSet *svtparams = GetInputDB("svt");
  St_DataSetIter       local(svtparams);

  // Replace tables for control parameters
  StSsdClusterControl *control = new StSsdClusterControl();
  // Set Control Parameters  
  // Former Scf
  control->setHighCut(5.0);
  control->setTestTolerance(0.2);
  // Former Scm
  control->setClusterTreat(13);
  control->setAdcTolerance(0.2);
  control->setMatchMean(0.);
  control->setMatchSigma(8.);
  control->printParameters();
  // End of Setting control parameters

  m_geom_par     = (St_sdm_geom_par      *)local("ssd/sdm_geom_par");
  m_condition_db = (St_sdm_condition_db  *)local("ssd/sdm_condition_db");
  m_geom         = (St_svg_geom          *)local("svgpars/geom");
  m_noise        = (St_sdm_calib_db      *)local("ssd/sdm_calib_db");
  m_scf_ctrl     = (St_scf_ctrl          *)local("ssd/scf_ctrl");
  m_sls_ctrl     = (St_sls_ctrl          *)local("ssd/sls_ctrl");
  m_scm_ctrl     = (St_scm_ctrl          *)local("ssd/scm_ctrl");

  if ((!m_geom_par)||(!m_geom)) {
    gMessMgr->Error() << "No  access to geometry parameters" << endm;
  }   
  if (!m_condition_db) {
    gMessMgr->Error() << "No  access to condition database" << endm;
  }   
  if (!m_noise) {
    gMessMgr->Error() << "No  access to noise condition" << endm;
  }
  if ((!m_sls_ctrl)||(!m_scf_ctrl)||(!m_scm_ctrl)) {
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
Int_t St_ssd_Maker::Make()
{
  if (Debug())  gMessMgr->Debug() << "In St_ssd_Maker::Make() ... "
                               << GetName() << endm;
  // 		Create output tables
  Int_t res = 0; 
  
  St_spa_strip *spa_strip = (St_spa_strip *)GetDataSet("spa_strip/.data/spa_strip");
  
  St_scf_cluster *scf_cluster = new St_scf_cluster("scf_cluster",10000);
  m_DataSet->Add(scf_cluster);
  
  sdm_geom_par_st  *geom_par = m_geom_par->GetTable();
  sls_ctrl_st      *sls_ctrl = m_sls_ctrl->GetTable();
  scf_ctrl_st      *scf_ctrl = m_scf_ctrl->GetTable();
  
  cout<<"#################################################"<<endl;
  cout<<"####     START OF NEW SSD CLUSTER FINDER     ####"<<endl;
  cout<<"####        SSD BARREL INITIALIZATION        ####"<<endl;  
  StSsdBarrel *barrel = new StSsdBarrel(geom_par);
  int stripTableSize = barrel->readStripFromTable(spa_strip);
  cout<<"####        NUMBER OF SPA STRIPS "<<stripTableSize<<"        ####"<<endl;
  barrel->sortListStrip();
  int noiseTableSize = barrel->readNoiseFromTable(m_noise,sls_ctrl);
  cout<<"####       NUMBER OF DB ENTRIES "<<noiseTableSize<<"       ####"<<endl;
  int nClusterPerSide[2];
  nClusterPerSide[0] = 0;
  nClusterPerSide[1] = 0;
  barrel->doSideClusterisation(nClusterPerSide, sls_ctrl,scf_ctrl);
  cout<<"####      NUMBER OF CLUSTER P SIDE "<<nClusterPerSide[0]<<"      ####"<<endl;
  cout<<"####      NUMBER OF CLUSTER N SIDE "<<nClusterPerSide[1]<<"      ####"<<endl;
  barrel->sortListCluster();
  int nClusterWritten = barrel->writeClusterToTable(scf_cluster);
  cout<<"####      NUMBER OF CLUSTER SAVED  "<<nClusterWritten<<"      ####"<<endl;
  //scf_cluster->Purge(); //remove all unused rows 
  delete barrel;
  cout<<"#################################################"<<endl;
  res = kStOK;
  
  if(res!=kStOK){
    gMessMgr->Warning("St_scf_Maker: no output");
     return kStWarn;
  }
  makeScfCtrlHistograms();  

  // In Former SCM !!
  // Next there is a change of scf_cluster to scm_cluster
  // St_scf_cluster *scf_cluster = (St_scf_cluster *)GetDataSet("scf_cluster/.data/scf_cluster");
  // To:
  St_DataSetIter scm_iter(m_DataSet);
  St_scf_cluster *scm_cluster = 0;
  scm_cluster = (St_scf_cluster *) scm_iter.Find("scf_cluster"); 
  // Then no modification except the name.

  St_scm_spt *scm_spt = new St_scm_spt("scm_spt",5000);
  m_DataSet->Add(scm_spt);

  scm_ctrl_st      *scm_ctrl = m_scm_ctrl->GetTable();
    
  cout<<"#################################################"<<endl;
  cout<<"####     START OF SSD NEW CLUSTER MATCHING   ####"<<endl;
  cout<<"####        SSD BARREL INITIALIZATION        ####"<<endl;
  StSsdBarrel *mySsd = new StSsdBarrel(geom_par);
  cout<<"####        SSD LADDERS INITIALIZATION       ####"<<endl;
  mySsd->initLadders(m_geom);
  //   int deadStripTableSize = mySsd->readDeadStripFromTable(condition_db_h, condition_db);
  //   cout<<"####   -> "<<deadStripTableSize<<" DEAD STRIPS IN THE SSD ####"<<endl;
  int nReadCluster = mySsd->readClusterFromTable(scm_cluster);
  cout<<"####   -> "<<nReadCluster<<" CLUSTERS READ FROM TABLE      ####"<<endl;
  mySsd->sortListCluster();
  int nPackage = mySsd->doClusterMatching(geom_par, scm_ctrl);
  cout<<"####   -> "<<nPackage<<" PACKAGES IN THE SSD           ####"<<endl;
  mySsd->convertDigitToAnalog(sls_ctrl);
  mySsd->convertUFrameToOther(geom_par);
  int nSptWritten = mySsd->writePointToTable(scm_spt);
  cout<<"####   -> "<<nSptWritten<<" HITS WRITTEN INTO TABLE       ####"<<endl;
  scm_spt->Purge();
  cout<<"####      END OF SSD NEW CLUSTER MATCHING    ####"<<endl;
  cout<<"#################################################"<<endl;
  delete mySsd;

  cout<<"Check if tables fullfilled: clusters="<<scm_cluster->GetNRows()<<"\n";
  cout<<"Check if tables fullfilled: spts="<<scm_spt->GetNRows()<<"\n";
  if (nSptWritten) res = kStOK;
 
  if(res!=kStOK){
    gMessMgr->Warning("St_scm_Maker: no output");
    return kStWarn;
  }

  makeScmCtrlHistograms();
  
  return kStOK;
}
//_____________________________________________________________________________
void St_ssd_Maker::makeScfCtrlHistograms()
{
  St_DataSetIter scf_iter(m_DataSet);
  St_scf_cluster *scf_cluster = 0;
  scf_cluster = (St_scf_cluster *) scf_iter.Find("scf_cluster"); 

// 		Fill histograms
  if (scf_cluster->GetNRows()){
    Int_t clustSide   = 0;  // pside = 0 et nside = 1 
    scf_cluster_st *dClus = scf_cluster->GetTable();
    sls_ctrl_st *sls_ctrl_t = m_sls_ctrl->GetTable();
    Float_t convAdcToE = (sls_ctrl_t[0].ADCDynamic*sls_ctrl_t[0].NElectronInAMip)/(pow(2.0,sls_ctrl_t[0].NBitEncoding));
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
void St_ssd_Maker::writeScfCtrlHistograms()
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
void St_ssd_Maker::makeScmCtrlHistograms()
{
  St_DataSetIter scm_iter(m_DataSet);
  St_scm_spt *scm_spt = 0;
  scm_spt = (St_scm_spt *) scm_iter.Find("scm_spt"); 

// 		Fill histograms 
  if (scm_spt->GetNRows()){
    scm_spt_st *dSpt = scm_spt->GetTable();
    sls_ctrl_st *sls_ctrl_t = m_sls_ctrl->GetTable();
    Float_t convMeVToAdc = (int)pow(2.0,sls_ctrl_t[0].NBitEncoding)/(sls_ctrl_t[0].PairCreationEnergy*sls_ctrl_t[0].ADCDynamic*sls_ctrl_t[0].NElectronInAMip);
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
void St_ssd_Maker::writeScmCtrlHistograms()
{
  ScmCtrlFile = new TFile("event/scmCtrl_histos.root","RECREATE");

  matchisto->Write();
  orthoproj->Write();

  ScmCtrlFile->Close();
}
//_____________________________________________________________________________
void St_ssd_Maker::PrintInfo()
{
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
Int_t St_ssd_Maker::Finish() {
  if (Debug()) gMessMgr->Debug() << "In St_ssd_Maker::Finish() ... "
                               << GetName() << endm; 
  writeScfCtrlHistograms();
  writeScmCtrlHistograms();
  return kStOK;
}

