 /**************************************************************************
 * Class      : St_scf_maker.cxx
 **************************************************************************
 *
 * $Log: St_scf_Maker.cxx,v $
 * Revision 1.6  2005/05/13 15:16:54  bouchet
 * reading ssd/geom and no more writeScfCtrlHistograms and writeScmCtrlHistograms methods
 *
 * Revision 1.5  2003/10/08 03:18:09  suire
 * *** empty log message ***
 *
 * Revision 1.3  2002/03/25 20:13:05  suire
 * Small memory leak fixes, doxygen documentation
 *
 *
 **************************************************************************/
#include <stdlib.h>
#include "St_scf_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "TH1.h"
#include "TFile.h"
#include "StMessMgr.h"

#include "StScfBarrel.hh" // introducing StSsdClusterBarrel
#include "tables/St_spa_strip_Table.h"
#include "tables/St_scf_cluster_Table.h"

#include "tables/St_sdm_geom_par_Table.h" //needed to call StTable->GetTable()
#include "StSsdClusterControl.h"
#include "tables/St_sls_ctrl_Table.h"
#include "tables/St_scf_ctrl_Table.h"

ClassImp(St_scf_Maker)
//_____________________________________________________________________________
St_scf_Maker::St_scf_Maker(const char *name):
StMaker(name),
m_geom_par(0),
m_noise(0),
m_scf_ctrl(0),
m_sls_ctrl(0)

{
}
//_____________________________________________________________________________
St_scf_Maker::~St_scf_Maker(){
}
//_____________________________________________________________________________
Int_t St_scf_Maker::Init(){
  if (Debug())  gMessMgr->Debug() << "In St_scf_Maker::Make() ... "
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
  // 

  m_geom_par  = (St_sdm_geom_par      *)local("ssd/sdm_geom_par");
  m_noise     = (St_sdm_calib_db      *)local("ssd/sdm_calib_db");
  m_scf_ctrl  = (St_scf_ctrl          *)local("ssd/scf_ctrl");
  m_sls_ctrl  = (St_sls_ctrl          *)local("ssd/sls_ctrl");
  if (!m_geom_par) {
    gMessMgr->Error() << "No  access to geometry parameters" << endm;
  }   
  if (!m_noise) {
    gMessMgr->Error() << "No  access to noise condition" << endm;
  }
  if ((!m_sls_ctrl)||(!m_scf_ctrl)) {
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

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_scf_Maker::Make()
{
  if (Debug())  gMessMgr->Debug() << "In St_scf_Maker::Make() ... "
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
  cout<<"####       START OF SSD CLUSTER FINDER       ####"<<endl;
  cout<<"####        SSD BARREL INITIALIZATION        ####"<<endl;  
  StScfBarrel *barrel = new StScfBarrel(geom_par);
  barrel->setSsdParameters(geom_par);
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
  res =  kStOK;
  
  if(res!=kStOK){
    gMessMgr->Warning("St_scf_Maker: no output");
     return kStWarn;
  }
  makeScfCtrlHistograms();  
  
  return kStOK;
}
//_____________________________________________________________________________
void St_scf_Maker::makeScfCtrlHistograms()
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
void St_scf_Maker::PrintInfo()
{
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
Int_t St_scf_Maker::Finish() {
  if (Debug()) gMessMgr->Debug() << "In St_scf_Maker::Finish() ... "
                               << GetName() << endm; 
  return kStOK;
}

