//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_scf_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <stdlib.h>
#include "St_scf_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "svt/St_scf_am_Module.h"
#include "TH1.h"
#include "TFile.h"
#include "StMessMgr.h"
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

  noisDisP = new TH1F("Noise (p)","Noise Distribution",25,0,25);
  snRatioP = new TH1F("SN (p)","Signal/Noise (p)",200,0,200);
  stpClusP = new TH1F("Number of Strips (p)","Strips per Cluster",8,0,8);
  totChrgP = new TH1F("Charge(electron) (p)","Total Cluster Charge",100,0,300000);

  noisDisN = new TH1F("Noise (n)","Noise Distribution",25,0,25);
  snRatioN = new TH1F("SN (n)","Signal/Noise",200,0,200);
  stpClusN = new TH1F("Number of Strips (n)","Strips per Cluster",8,0,8);
  totChrgN = new TH1F("Charge(electron) (n)","Total Cluster Charge",100,0,300000);

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
  
  res =  scf_am(m_geom_par, spa_strip, m_sls_ctrl,
		m_noise, m_scf_ctrl, scf_cluster);
  
   if(res!=kSTAFCV_OK){
     gMessMgr->Warning("St_scf_Maker: no output");
     return kStWarn;
   }
  makeScfCtrlHistograms();
  writeScfCtrlHistograms();
  
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
    Float_t convAdcToE = (sls_ctrl_t[0].ADCDynamic*sls_ctrl_t[0].NElectronInAMip)/(pow(2,sls_ctrl_t[0].NBitEncoding));
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
void St_scf_Maker::writeScfCtrlHistograms()
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
void St_scf_Maker::PrintInfo()
{
  printf("**************************************************************\n");
  printf("* $Id: St_scf_Maker.cxx,v 1.2 2000/08/15 19:34:52 hippolyt Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
Int_t St_scf_Maker::Finish() {
  if (Debug()) gMessMgr->Debug() << "In St_spa_Maker::Finish() ... "
                               << GetName() << endm; 
  return kStOK;
}

