//////////////////////////////////////////////////////////////////////////
//                                                                      //
// St_sce_Maker class for Makers                                        //
//                                                                      //
//////////////////////////////////////////////////////////////////////////
#include <iostream.h>
#include <stdlib.h>
#include "St_sce_Maker.h"
#include "StChain.h"
#include "St_DataSetIter.h"
#include "svt/St_sce_am_Module.h"
#include "TH1.h"
#include "TFile.h"
#include "StMessMgr.h"

ClassImp(St_sce_Maker)
//_____________________________________________________________________________
St_sce_Maker::St_sce_Maker(const char *name):
StMaker(name),
m_geom_par(0),
m_geom(0),
m_ctrl(0)
{
}
//_____________________________________________________________________________
St_sce_Maker::~St_sce_Maker(){
}
//_____________________________________________________________________________
Int_t St_sce_Maker::Init(){

// 		Create tables
  St_DataSet *svtparams = GetInputDB("svt");
  St_DataSetIter       local(svtparams);

// 		Geometry parameters
   m_geom_par    = (St_sdm_geom_par*)local("ssd/sdm_geom_par");
   m_geom        = (St_svg_geom    *)local("svgpars/geom");
   m_ctrl        = (St_sce_ctrl    *)local("ssd/sce_ctrl");

   
  if ((!m_geom_par)||(!m_geom)) {
    gMessMgr->Error() << "No  access to geometry parameters" << endm;
  }   
  if (!m_ctrl) {
    gMessMgr->Error() << "No  access to control parameters" << endm;
  } 
// 		Create SCM histograms
  devXl0 = new TH1F("xl [0] (microns)","Xl [0] deviation:",100,-200,200);
  devXl0 -> SetYTitle("Nbre of Points");
  devXl0 -> SetXTitle("Deviation (microns)");

  devXl1 = new TH1F("xl [1] (microns)","Xl [1] deviation:",50,-6000,6000);
  devXl0 -> SetYTitle("Nbre of Points");
  devXl0 -> SetXTitle("Deviation (microns)");

  devNrg = new TH1F("Energy (MeV)","Energy deviation:",50,-50,50);
  devXl0 -> SetYTitle("Nbre of Points");
  devXl0 -> SetXTitle("Deviation (MeV)");

  devXg0 = new TH1F("xg [0] (microns)","Xg [0] deviation:",100,-200,200);
  devXl0 -> SetYTitle("Nbre of Points");
  devXl0 -> SetXTitle("Deviation (microns)");

  devXg1 = new TH1F("xg [1] (microns)","Xg [1] deviation:",100,-200,200);
  devXl0 -> SetYTitle("Nbre of Points");
  devXl0 -> SetXTitle("Deviation (microns)");

  devXg2 = new TH1F("xg [2] (microns)","Xg [2] deviation:",50,-6000,6000);
  devXl0 -> SetYTitle("Nbre of Points");
  devXl0 -> SetXTitle("Deviation (microns)");

  resetSceStats();

  return StMaker::Init();
}
//_____________________________________________________________________________
Int_t St_sce_Maker::Make()
{
  if (Debug())  gMessMgr->Debug() << "In St_sce_Maker::Make() ... "
                               << GetName() << endm;
  // 		Create output tables
  St_DataSetIter geant(GetInputDS("geant"));
  St_g2t_svt_hit *g2t_svt_hit = (St_g2t_svt_hit *) geant("g2t_svt_hit");
  St_scf_cluster *scf_cluster = (St_scf_cluster *)GetDataSet("scf_cluster/.data/scf_cluster");
  St_scm_spt *scm_spt = (St_scm_spt *)GetDataSet("scm_spt/.data/scm_spt");

  St_sce_dspt *sce_dspt = new St_sce_dspt("sce_dspt",5000);
  m_DataSet->Add(sce_dspt);

  Int_t res = sce_am(g2t_svt_hit, scf_cluster, scm_spt, m_geom, m_geom_par, sce_dspt, m_ctrl);
;
   if(res!=kSTAFCV_OK){
     gMessMgr->Warning("St_sce_Maker: no output");
     return kStWarn;
   }
  
  makeScfStats();
  showScfStats();
  makeScmStats();
  showScmStats();

  makeScmHistograms();
  writeScmHistograms();

  return kStOK;
}
//_____________________________________________________________________________
void St_sce_Maker::resetSceStats()
{
  sce_ctrl_st *sce_ctrl_t = m_ctrl->GetTable();
  sce_ctrl_t[0].TrueClusterP  = 0;
  sce_ctrl_t[0].GhostClusterP = 0;
  sce_ctrl_t[0].LostClusterP  = 0;
  sce_ctrl_t[0].TrueClusterN  = 0;
  sce_ctrl_t[0].GhostClusterN = 0;
  sce_ctrl_t[0].LostClusterN  = 0;
  sce_ctrl_t[0].TrueSpt11     = 0;
  sce_ctrl_t[0].GhostSpt11    = 0;
  sce_ctrl_t[0].LostSpt11     = 0;
  sce_ctrl_t[0].TrueSpt12     = 0;
  sce_ctrl_t[0].GhostSpt12    = 0;
  sce_ctrl_t[0].LostSpt12     = 0;
  sce_ctrl_t[0].TrueSpt22     = 0;
  sce_ctrl_t[0].GhostSpt22    = 0;
  sce_ctrl_t[0].LostSpt22     = 0;
  sce_ctrl_t[0].TrueSpt23     = 0;
  sce_ctrl_t[0].GhostSpt23    = 0;
  sce_ctrl_t[0].LostSpt23     = 0;
  sce_ctrl_t[0].TrueSpt33     = 0;
  sce_ctrl_t[0].GhostSpt33    = 0;
  sce_ctrl_t[0].LostSpt33     = 0;

  Int_t i=0;
  Int_t j=0;

  for(i=0;i<3;i++)
    {
      for (j=0;j<2;j++)
	{
	  statCluster[i][j] = 0;
	}
    }

  for(i=0;i<3;i++)
    {
      for (j=0;j<5;j++)
	{
	  statSpt[i][j] = 0;
	}
    }
}
//_____________________________________________________________________________
void St_sce_Maker::makeScfStats()
{
  sce_ctrl_st *sce_ctrl_t = m_ctrl->GetTable();
  statCluster[0][0] += sce_ctrl_t[0].TrueClusterP;
  statCluster[1][0] += sce_ctrl_t[0].GhostClusterP;
  statCluster[2][0] += sce_ctrl_t[0].LostClusterP;

  statCluster[0][1] += sce_ctrl_t[0].TrueClusterN;
  statCluster[1][1] += sce_ctrl_t[0].GhostClusterN;
  statCluster[2][1] += sce_ctrl_t[0].LostClusterN;

  sce_ctrl_t[0].TrueClusterP  = 0;
  sce_ctrl_t[0].GhostClusterP = 0;
  sce_ctrl_t[0].LostClusterP  = 0;
  sce_ctrl_t[0].TrueClusterN  = 0;
  sce_ctrl_t[0].GhostClusterN = 0;
  sce_ctrl_t[0].LostClusterN  = 0;
}
//_____________________________________________________________________________
void St_sce_Maker::showScfStats()
{
  printf("\n");
  printf("*************************************************\n");
  printf("**************** Stats Clusters *****************\n");
  printf("*************************************************\n");
  printf("***        \t\tp-side \tn-side \t      ***\n");
  printf("***  true  \t\t%d  \t%d \t      ***\n",statCluster[0][0],statCluster[0][1]);
  printf("***  ghost \t\t%d  \t%d \t      ***\n",statCluster[1][0],statCluster[1][1]);
  printf("***  total \t\t%d  \t%d \t      ***\n",statCluster[0][0]+statCluster[2][0],statCluster[0][1]+statCluster[2][1]);
  printf("*************************************************\n");
  Float_t p_effi = 100*float(statCluster[0][0])/(statCluster[0][0]+statCluster[2][0]);
  Float_t p_pure = 100*float(statCluster[0][0])/(statCluster[0][0]+statCluster[1][0]);
  Float_t n_effi = 100*float(statCluster[0][1])/(statCluster[0][1]+statCluster[2][1]);
  Float_t n_pure = 100*float(statCluster[0][1])/(statCluster[0][1]+statCluster[1][1]);
  printf("*************************************************\n");
  printf("***  efficiency: \t%.2f%s  %.2f%s \t      ***\n",p_effi,"%",n_effi,"%");
  printf("***  purity:     \t%.2f%s  %.2f%s \t      ***\n",p_pure,"%",n_pure,"%");
  printf("*************************************************\n");
	
}
//_____________________________________________________________________________
void St_sce_Maker::makeScmStats()
{
  sce_ctrl_st *sce_ctrl_t = m_ctrl->GetTable();
  statSpt[0][0] += sce_ctrl_t[0].TrueSpt11;
  statSpt[1][0] += sce_ctrl_t[0].GhostSpt11;
  statSpt[2][0] += sce_ctrl_t[0].LostSpt11;

  statSpt[0][1] += sce_ctrl_t[0].TrueSpt12;
  statSpt[1][1] += sce_ctrl_t[0].GhostSpt12;
  statSpt[2][1] += sce_ctrl_t[0].LostSpt12;

  statSpt[0][2] += sce_ctrl_t[0].TrueSpt22;
  statSpt[1][2] += sce_ctrl_t[0].GhostSpt22;
  statSpt[2][2] += sce_ctrl_t[0].LostSpt22;

  statSpt[0][3] += sce_ctrl_t[0].TrueSpt23;
  statSpt[1][3] += sce_ctrl_t[0].GhostSpt23;
  statSpt[2][3] += sce_ctrl_t[0].LostSpt23;

  statSpt[0][4] += sce_ctrl_t[0].TrueSpt33;
  statSpt[1][4] += sce_ctrl_t[0].GhostSpt33;
  statSpt[2][4] += sce_ctrl_t[0].LostSpt33;

  sce_ctrl_t[0].TrueSpt11     = 0;
  sce_ctrl_t[0].GhostSpt11    = 0;
  sce_ctrl_t[0].LostSpt11     = 0;
  sce_ctrl_t[0].TrueSpt12     = 0;
  sce_ctrl_t[0].GhostSpt12    = 0;
  sce_ctrl_t[0].LostSpt12     = 0;
  sce_ctrl_t[0].TrueSpt22     = 0;
  sce_ctrl_t[0].GhostSpt22    = 0;
  sce_ctrl_t[0].LostSpt22     = 0;
  sce_ctrl_t[0].TrueSpt23     = 0;
  sce_ctrl_t[0].GhostSpt23    = 0;
  sce_ctrl_t[0].LostSpt23     = 0;
  sce_ctrl_t[0].TrueSpt33     = 0;
  sce_ctrl_t[0].GhostSpt33    = 0;
  sce_ctrl_t[0].LostSpt33     = 0;
}
//_____________________________________________________________________________
void St_sce_Maker::showScmStats()
{
  printf("\n");
  printf("*************************************************\n");
  printf("*************** Stats Space Point ***************\n");
  printf("*************************************************\n");
  printf("***      \tTrue\tGhost\tLost\tEfficiency\tPurity      ***\n");
  printf("*************************************************\n");

  printf("***  11:\t%d \t%d \t%d \t%.2f%s \t%.2f%s ***\n",statSpt[0][0],statSpt[1][0],statSpt[2][0],100*float(statSpt[0][0])/(statSpt[0][0]+statSpt[2][0]),"%",100*float(statSpt[0][0])/(statSpt[0][0]+statSpt[1][0]),"%");
  printf("***  12:\t%d \t%d \t%d \t%.2f%s \t%.2f%s ***\n",statSpt[0][1],statSpt[1][1],statSpt[2][1],100*float(statSpt[0][1])/(statSpt[0][1]+statSpt[2][1]),"%",100*float(statSpt[0][1])/(statSpt[0][1]+statSpt[1][1]),"%");
  printf("***  22:\t%d \t%d \t%d \t%.2f%s \t%.2f%s ***\n",statSpt[0][2],statSpt[1][2],statSpt[2][2],100*float(statSpt[0][2])/(statSpt[0][2]+statSpt[2][2]),"%",100*float(statSpt[0][2])/(statSpt[0][2]+statSpt[1][2]),"%");
  printf("***  23:\t%d \t%d \t%d \t%.2f%s \t%.2f%s ***\n",statSpt[0][3],statSpt[1][3],statSpt[2][3],100*float(statSpt[0][3])/(statSpt[0][3]+statSpt[2][3]),"%",100*float(statSpt[0][3])/(statSpt[0][3]+statSpt[1][3]),"%");
  printf("***  33:\t%d \t%d \t%d \t%.2f%s \t%.2f%s ***\n",statSpt[0][4],statSpt[1][4],statSpt[2][4],100*float(statSpt[0][4])/(statSpt[0][4]+statSpt[2][4]),"%",100*float(statSpt[0][4])/(statSpt[0][4]+statSpt[1][4]),"%");
  printf("*************************************************\n");
  Int_t   totTrueScm = statSpt[0][0]+statSpt[0][1]+statSpt[0][2]+statSpt[0][3]+statSpt[0][4];
  Int_t   totGhostScm = statSpt[1][0]+statSpt[1][1]+statSpt[1][2]+statSpt[1][3]+statSpt[1][4];
  Int_t   totTrueSim = 0;
  if((statCluster[0][0]+statCluster[2][0]) >= (statCluster[0][1]+statCluster[2][1]))
    {totTrueSim =statCluster[0][0]+statCluster[2][0];}
  else
    {totTrueSim =statCluster[0][1]+statCluster[2][1];}
  Float_t effi = 100*float(totTrueScm)/totTrueSim;
  Float_t pure = 100*float(totTrueScm)/(totTrueScm+totGhostScm);
  printf("***  Global:\t%d \t%d \t%d \t%.2f%s \t%.2f%s ***\n",statSpt[0][0]+statSpt[0][1]+statSpt[0][2]+statSpt[0][3]+statSpt[0][4],statSpt[1][0]+statSpt[1][1]+statSpt[1][2]+statSpt[1][3]+statSpt[1][4],statSpt[2][0]+statSpt[2][1]+statSpt[2][2]+statSpt[2][3]+statSpt[2][4],effi,"%",pure,"%");
  printf("*************************************************\n");
}
//_____________________________________________________________________________
void St_sce_Maker::makeScmHistograms()
{
  St_DataSetIter sce_iter(m_DataSet);
  St_sce_dspt *sce_dspt = 0;
  sce_dspt = (St_sce_dspt *) sce_iter.Find("sce_dspt"); 

// 		Fill histograms
  if (sce_dspt->GetNRows()){
      sce_dspt_st *dSpt = sce_dspt->GetTable();
       for (Int_t iSce = 0; iSce < sce_dspt->GetNRows(); iSce++, dSpt++){
	  devXl0->Fill(10000*dSpt->dxl[0]);
	  devXl1->Fill(10000*dSpt->dxl[1]);
	  devNrg->Fill(dSpt->d2e[0]);
	  devXg0->Fill(10000*dSpt->dx[0]);
	  devXg1->Fill(10000*dSpt->dx[1]);
	  devXg2->Fill(10000*dSpt->dx[2]);
	}
  }
}
//_____________________________________________________________________________
void St_sce_Maker::writeScmHistograms()
{
  ScmFile = new TFile("event/scmEval_histos.root","RECREATE");

  devXl0->Write();
  devXl1->Write();
  devNrg->Write();
  devXg0->Write();
  devXg1->Write();
  devXg2->Write();

  ScmFile->Close();
}
//_____________________________________________________________________________
void St_sce_Maker::PrintInfo()
{
  printf("**************************************************************\n");
  printf("* $Id: St_sce_Maker.cxx,v 1.4 2000/09/11 12:36:02 hippolyt Exp $\n");
  printf("**************************************************************\n");
  if (Debug()) StMaker::PrintInfo();
}
//_____________________________________________________________________________
Int_t St_sce_Maker::Finish() {
  if (Debug()) gMessMgr->Debug() << "In St_sce_Maker::Finish() ... "
                               << GetName() << endm; 
  return kStOK;
}
