//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_scm_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <stdlib.h>
#include "St_scm_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "svt/St_scm_am_Module.h"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include "StMessMgr.h"
ClassImp(St_scm_Maker)
//_____________________________________________________________________________
St_scm_Maker::St_scm_Maker(const char *name):
StMaker(name),
m_geom_par(0),
m_condition_db(0),
m_geom(0),
m_sls_ctrl(0),
m_scm_ctrl(0)

{
}
//_____________________________________________________________________________
St_scm_Maker::~St_scm_Maker(){
}
//_____________________________________________________________________________
Int_t St_scm_Maker::Init(){
  
  // 		Create tables
  St_DataSet *svtparams = GetInputDB("svt");
  St_DataSetIter       local(svtparams);
  
  m_geom_par     = (St_sdm_geom_par      *)local("ssd/sdm_geom_par");
  m_condition_db = (St_sdm_condition_db  *)local("ssd/sdm_condition_db");
  m_geom         = (St_svg_geom          *)local("svgpars/geom");
  m_sls_ctrl     = (St_sls_ctrl          *)local("ssd/sls_ctrl");
  m_scm_ctrl     = (St_scm_ctrl          *)local("ssd/scm_ctrl");

  if ((!m_geom_par)||(!m_geom)) {
    gMessMgr->Error() << "No  access to geometry parameters" << endm;
  }   
  if (!m_condition_db) {
    gMessMgr->Error() << "No  access to condition database" << endm;
  }   
  if ((!m_sls_ctrl)||(!m_sls_ctrl)) {
    gMessMgr->Error() << "No  access to control parameters" << endm;
  } 
// 		Create SCM histograms

  matchisto = new TH2S("matching Histo","Matching Adc (1p-1n)",50,0,1000,50,0,1000);
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
  orthoproj = new TH1S("Projection Ortho","Perfect Matching Deviation",80,-80,80);

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_scm_Maker::Make()
{
  if (Debug())  gMessMgr->Debug() << "In St_scm_Maker::Make() ... "
                               << GetName() << endm;
  // 		Create output tables
  Int_t res = 0; 

  St_scf_cluster *scf_cluster = (St_scf_cluster *)GetDataSet("scf_cluster/.data/scf_cluster");

  St_scm_spt *scm_spt = new St_scm_spt("scm_spt",5000);
  m_DataSet->Add(scm_spt);

  res = scm_am(m_geom_par, m_condition_db, m_geom,
        scf_cluster, m_sls_ctrl, m_scm_ctrl, scm_spt);

   if(res!=kSTAFCV_OK){
     gMessMgr->Warning("St_scm_Maker: no output");
     return kStWarn;
   }

  makeScmCtrlHistograms();
  writeScmCtrlHistograms();

  return kStOK;
}
//_____________________________________________________________________________
void St_scm_Maker::makeScmCtrlHistograms()
{
  St_DataSetIter scm_iter(m_DataSet);
  St_scm_spt *scm_spt = 0;
  scm_spt = (St_scm_spt *) scm_iter.Find("scm_spt"); 

// 		Fill histograms 
  if (scm_spt->GetNRows()){
    scm_spt_st *dSpt = scm_spt->GetTable();
    sls_ctrl_st *sls_ctrl_t = m_sls_ctrl->GetTable();
    Float_t convMeVToAdc = (int)pow(2,sls_ctrl_t[0].NBitEncoding)/(sls_ctrl_t[0].PairCreationEnergy*sls_ctrl_t[0].ADCDynamic*sls_ctrl_t[0].NElectronInAMip);
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
void St_scm_Maker::writeScmCtrlHistograms()
{
  ScmCtrlFile = new TFile("event/scmCtrl_histos.root","RECREATE");

  matchisto->Write();
  orthoproj->Write();

  ScmCtrlFile->Close();
}
//_____________________________________________________________________________
void St_scm_Maker::PrintInfo()
{
  printf("**************************************************************\n");
  printf("* $Id: St_scm_Maker.cxx,v 1.2 2000/08/15 19:34:52 hippolyt Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}

//_____________________________________________________________________________
Int_t St_scm_Maker::Finish() {
  if (Debug()) gMessMgr->Debug() << "In St_scm_Maker::Finish() ... "
                               << GetName() << endm; 
  return kStOK;
}
