 /**************************************************************************
 * Class      : St_scm_maker.cxx
 **************************************************************************
 *
 * $Log: St_scm_Maker.cxx,v $
 * Revision 1.5  2003/10/08 03:18:09  suire
 * *** empty log message ***
 *
 * Revision 1.3  2002/03/25 20:13:05  suire
 * Small memory leak fixes, doxygen documentation
 *
 *
 **************************************************************************/
#include <stdlib.h>
#include "St_scm_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "TH1.h"
#include "TH2.h"
#include "TFile.h"
#include "StMessMgr.h"

#include "StScmBarrel.hh"
#include "tables/St_scf_cluster_Table.h"
#include "tables/St_scm_spt_Table.h"
#include "tables/St_sdm_geom_par_Table.h"
#include "tables/St_sls_ctrl_Table.h"
#include "tables/St_scm_ctrl_Table.h"


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
Int_t St_scm_Maker::Make()
{
  if (Debug())  gMessMgr->Debug() << "In St_scm_Maker::Make() ... "
                               << GetName() << endm;
  // 		Create output tables
  Int_t res = 0; 

  St_scf_cluster *scf_cluster = (St_scf_cluster *)GetDataSet("scf_cluster/.data/scf_cluster");

  St_scm_spt *scm_spt = new St_scm_spt("scm_spt",5000);
  m_DataSet->Add(scm_spt);

  sdm_geom_par_st  *geom_par = m_geom_par->GetTable();
  sls_ctrl_st      *sls_ctrl = m_sls_ctrl->GetTable();
  scm_ctrl_st      *scm_ctrl = m_scm_ctrl->GetTable();
    
  cout<<"#################################################"<<endl;
  cout<<"####      START OF SSD CLUSTER MATCHING      ####"<<endl;
  cout<<"####        SSD BARREL INITIALIZATION        ####"<<endl;
  StScmBarrel *mySsd = new StScmBarrel(geom_par);
  cout<<"####        SSD WAFERS INITIALIZATION        ####"<<endl;
  mySsd->initWafers(m_geom);
  //   int deadStripTableSize = mySsd->readDeadStripFromTable(condition_db_h, condition_db);
  //   cout<<"####   -> "<<deadStripTableSize<<" DEAD STRIPS IN THE SSD ####"<<endl;
  int nReadCluster = mySsd->readClusterFromTable(scf_cluster);
  cout<<"####   -> "<<nReadCluster<<" CLUSTERS READ FROM TABLE      ####"<<endl;
  mySsd->sortListCluster();
  int nPackage = mySsd->doClusterMatching(geom_par, scm_ctrl);
  cout<<"####   -> "<<nPackage<<" PACKAGES IN THE SSD           ####"<<endl;
  mySsd->convertDigitToAnalog(sls_ctrl);
  mySsd->convertUFrameToOther(geom_par);
  int nSptWritten = mySsd->writePointToTable(scm_spt);
  cout<<"####   -> "<<nSptWritten<<" HITS WRITTEN INTO TABLE       ####"<<endl;
  scm_spt->Purge();
  cout<<"####       END OF SSD CLUSTER MATCHING       ####"<<endl;
  cout<<"#################################################"<<endl;
  delete mySsd;
  if (nSptWritten) res = kStOK;
 
   if(res!=kStOK){
     gMessMgr->Warning("St_scm_Maker: no output");
     return kStWarn;
   }

  makeScmCtrlHistograms();

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
  if (Debug()) StMaker::PrintInfo();
}

//_____________________________________________________________________________
Int_t St_scm_Maker::Finish() {
  if (Debug()) gMessMgr->Debug() << "In St_scm_Maker::Finish() ... "
                               << GetName() << endm; 
  writeScmCtrlHistograms();
  return kStOK;
}
