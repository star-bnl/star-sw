#include <stdio.h>
#include <stdlib.h> 

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include <DAQ_READER/daq_dta.h> //xue
#include <DAQ_L3/daq_l3.h> //xue
#include <TStyle.h> //xue
#include "TVector3.h"//xue
#include "TFile.h"//xue
#include <fstream>//xue
#include <iostream>//xue
#include <iomanip>//xue
#include "TLorentzVector.h"//xue
#include "TRandom.h"//xue
#include "TRandom3.h"//xue
#include "string"
#include "TObjArray.h"
#include "TObjString.h"

#include <TH1I.h>
#include <TH1D.h>
#include <TH2F.h>
#include <TProfile.h>
#include <TCanvas.h>
#include <TH3D.h>
#include <math.h>
#include <TF1.h>
#include <rtsLog.h>
#include "hltphiBuilder.h"

ClassImp(HltphiBuilder);

void HltphiBuilder::initialize(int argc, char *argv[]) {	
  gStyle->SetPalette(1); 
  gStyle->SetOptLogz(1);

  current_day = 0;
  twopi = 6.2831854;
  pi = 3.1415927;
  nbin = 0;
  /*
    double mass_low = 2.;
    double mass_hi= 4.;
    int mass_bin = 100;
    double mean_low = -1.;
    double mean_hi = 1.;
  */
  float mass_low=0.99;
  float mass_hi=1.10;
  int mass_bin=110;
  float mean_low=-1.;
  float mean_hi=1.0;   ////run7  phi test

  flowphi = new TObjArray(20);

  for(int i=0;i<46;i++) {
    run[i] = new JevpPlot();
  }
  for(int i=0;i<10;i++) {
    day[i] = new JevpPlot();
  }
  for(int i=0;i<31;i++) {
    all[i] = new JevpPlot();
  }
  /*	  InvMassv2 = new TH3D("InvMassv2","InvMassv2",60,0.,6.0,100,-1.0,1.0,100,2.0,4.0);
    InvMassv2_Sin = new TH3D("InvMassv2_Sin","InvMassv2_Sin",60,0.,6.0,100,-1.0,1.0,100,2.0,4.0);
    DenInvMass = new TH3D("DenInvMass","DenInvMass",60,0.,6.0,100,-1.0,1.0,100,2.0,4.0);
  */
  InvMassv2 = new TH3D("hltphi_InvMassv2","hltphi_InvMassv2",50,0.,6.0,50,-1.0,1.0,50,0.98,1.2);
  InvMassv2_Sin = new TH3D("hltphi_InvMassv2_Sin","hltphi_InvMassv2_Sin",50,0.,6.0,50,-1.0,1.0,50,0.98,1.2);
  DenInvMass = new TH3D("hltphi_DenInvMass","hltphi_DenInvMass",50,0.,6.0,10,0.,3.1415926,50,0.98,1.2);



  //////////////////// run histograms ////////////////////
	  
  //-------glob tracks-------//
  TH1I *Run_EventStat = new TH1I("hltphi_Run_EventStat","hltphi_Run_EventStat",4,0.,4);
  PlotHisto *ph = new PlotHisto();
  ph->histo = Run_EventStat;
  run[0]->addHisto(ph);
  addPlot(run[0]);

  TH1I *Run_nHits = new TH1I("hltphi_Run_nHits","hltphi_Run_nHits",100,0,100);
  ph = new PlotHisto();
  ph->histo = Run_nHits;
  run[1]->addHisto(ph);
  addPlot(run[1]);

  TH1I *Run_nDedx = new TH1I("hltphi_Run_nDedx","hltphi_Run_nDedx",100,0,100);
  ph = new PlotHisto();
  ph->histo = Run_nDedx;
  run[2]->addHisto(ph);
  addPlot(run[2]);

  TH1D *Run_Glob_Pt = new TH1D("hltphi_Run_Glob_Pt","hltphi_Run_Glob_Pt",150,0.,15.);
  ph = new PlotHisto();
  ph->histo = Run_Glob_Pt;
  run[3]->addHisto(ph);
  addPlot(run[3]);

  TH1D *Run_Glob_Phi = new TH1D("hltphi_Run_Glob_Phi","hltphi_Run_Glob_Phi",360,0.,twopi);
  ph = new PlotHisto();
  ph->histo = Run_Glob_Phi;
  run[4]->addHisto(ph);
  addPlot(run[4]);

  TH1D *Run_Glob_Eta = new TH1D("hltphi_Run_Glob_Eta","hltphi_Run_Glob_Eta",150,-7.5,7.5);
  ph = new PlotHisto();
  ph->histo = Run_Glob_Eta;
  run[5]->addHisto(ph);
  addPlot(run[5]);

  TH1D *Run_Dca = new TH1D("hltphi_Run_Dca","hltphi_Run_Dca",100,0.,10.);
  ph = new PlotHisto();
  ph->histo = Run_Dca;
  run[6]->addHisto(ph);
  addPlot(run[6]);

  run[7]->optstat = 0;
  run[7]->setDrawOpts("colz");
  TH2F *Run_Glob_dEdx = new TH2F("hltphi_Run_Glob_dEdx","hltphi_Run_Glob_dEdx",100,0,5,100,0,1.e-5);
  ph = new PlotHisto();
  ph->histo = Run_Glob_dEdx;
  run[7]->addHisto(ph);
  addPlot(run[7]);

  TH2F *Run_Glob_P_towerEnergy = new TH2F("hltphi_Run_Glob_P_towerEnergy","hltphi_Run_Glob_P_towerEnergy",100,0,15,100,0,20);
  ph = new PlotHisto();
  ph->histo = Run_Glob_P_towerEnergy;
  run[8]->addHisto(ph);
  addPlot(run[8]);

  TH1D *Run_Ln_dEdx = new TH1D("hltphi_Run_Ln_dEdx","hltphi_Run_Ln_dEdx",100,-3.,3.);  //
  ph = new PlotHisto();
  ph->histo = Run_Ln_dEdx;
  run[9]->addHisto(ph);
  addPlot(run[9]);

  TH1D *Run_FlowPhi = new TH1D("hltphi_Run_FlowPhi","hltphi_Run_FlowPhi",360,0.,twopi);
  ph = new PlotHisto();
  ph->histo = Run_FlowPhi;
  run[10]->addHisto(ph);
  addPlot(run[10]);

  //------prim track-----// ||
  TH1D *Run_Prim_Pt = new TH1D("hltphi_Run_Prim_Pt","hltphi_Run_Prim_Pt",150,0.,15.);
  ph = new PlotHisto();
  ph->histo = Run_Prim_Pt;
  run[11]->addHisto(ph);
  addPlot(run[11]);

  TH1D *Run_Prim_Phi = new TH1D("hltphi_Run_Prim_Phi","hltphi_Run_Prim_Phi",360,0.,twopi);
  ph = new PlotHisto();
  ph->histo = Run_Prim_Phi;
  run[12]->addHisto(ph);
  addPlot(run[12]);

  TH1D *Run_Prim_Eta = new TH1D("hltphi_Run_Prim_Eta","hltphi_Run_Prim_Eta",150,-7.5,7.5);
  ph = new PlotHisto();
  ph->histo = Run_Prim_Eta;
  run[13]->addHisto(ph);
  addPlot(run[13]);

  run[14]->optstat = 0;
  run[14]->setDrawOpts("colz");
  TH2F *Run_Prim_dEdx = new TH2F("hltphi_Run_Prim_dEdx","hltphi_Run_Prim_dEdx",100,0,5,100,0,1.e-5);
  ph = new PlotHisto();
  ph->histo = Run_Prim_dEdx;
  run[14]->addHisto(ph);
  addPlot(run[14]);

  TH2F *Run_Prim_P_towerEnergy = new TH2F("hltphi_Run_Prim_P_towerEnergy","hltphi_Run_Prim_P_towerEnergy",100,0,15,100,0,20);
  ph = new PlotHisto();
  ph->histo = Run_Prim_P_towerEnergy;
  run[15]->addHisto(ph);
  addPlot(run[15]);

  //------Event------//
  TH1D *Run_VertexX = new TH1D("hltphi_Run_VertexX","hltphi_Run_VertexX",100,-1.,1.);
  ph = new PlotHisto();
  ph->histo = Run_VertexX;
  run[16]->addHisto(ph);
  addPlot(run[16]);

  TH1D *Run_VertexY = new TH1D("hltphi_Run_VertexY","hltphi_Run_VertexY",100,-1.,1.);
  ph = new PlotHisto();
  ph->histo = Run_VertexY;
  run[17]->addHisto(ph);
  addPlot(run[17]);

  TH1D *Run_VertexZ = new TH1D("hltphi_Run_VertexZ","hltphi_Run_VertexZ",1000,-200.,200.);
  ph = new PlotHisto();
  ph->histo = Run_VertexZ;
  run[18]->addHisto(ph);
  addPlot(run[18]);

  TH1D *Run_Lm_VertexX = new TH1D("hltphi_Run_Lm_VertexX","hltphi_Run_Lm_VertexX",100,-1.,1.);
  ph = new PlotHisto();
  ph->histo = Run_Lm_VertexX;
  run[19]->addHisto(ph);
  addPlot(run[19]);

  TH1D *Run_Lm_VertexY = new TH1D("hltphi_Run_Lm_VertexY","hltphi_Run_Lm_VertexY",100,-1.,1.);
  ph = new PlotHisto();
  ph->histo = Run_Lm_VertexY;
  run[20]->addHisto(ph);
  addPlot(run[20]);

  TH1D *Run_Lm_VertexZ = new TH1D("hltphi_Run_Lm_VertexZ","hltphi_Run_Lm_VertexZ",1000,-200.,200.);
  ph = new PlotHisto();
  ph->histo = Run_Lm_VertexZ;
  run[21]->addHisto(ph);
  addPlot(run[21]);

  TH1I *Run_Multi = new TH1I("hltphi_Run_Multi", "hltphi_Run_Multi",1000,0,1000);
  ph = new PlotHisto();
  ph->histo = Run_Multi;
  run[22]->addHisto(ph);
  addPlot(run[22]);

  TH1D *Run_SubEvent = new TH1D("hltphi_Run_SubEvent","hltphi_Run_SubEvent",180,0.,pi);
  ph = new PlotHisto();
  ph->histo = Run_SubEvent;
  run[23]->addHisto(ph);
  addPlot(run[23]);

  TH1D *Run_Raw_EventPlane = new TH1D("hltphi_Run_Raw_EventPlane","hltphi_Run_Raw_EventPlane",180,0.,pi);
  ph = new PlotHisto();
  ph->histo = Run_Raw_EventPlane;
  run[24]->addHisto(ph);
  addPlot(run[24]);

  TH1D *Run_EventPlane = new TH1D("hltphi_Run_EventPlane","hltphi_Run_EventPlane",180,0.,pi);
  ph = new PlotHisto();
  ph->histo = Run_EventPlane;
  run[25]->addHisto(ph);
  addPlot(run[25]);

  //--------Emc---------//
  TH1D *Run_matchPhi_Diff = new TH1D("hltphi_Run_matchPhi_Diff","hltphi_Run_matchPhi_Diff",50,0.,0.1);
  ph = new PlotHisto();
  ph->histo = Run_matchPhi_Diff;
  run[26]->addHisto(ph);
  addPlot(run[26]);

  TH1D *Run_towerEnergy = new TH1D("hltphi_Run_towerEnergy","hltphi_Run_towerEnergy",200,0.,20.);
  ph = new PlotHisto();
  ph->histo = Run_towerEnergy;
  run[27]->addHisto(ph);
  addPlot(run[27]);

  TH1I *Run_towerDaqId = new TH1I("hltphi_Run_towerDaqId","hltphi_Run_towerDaqId",5000,0.,5000.);
  ph = new PlotHisto();
  ph->histo = Run_towerDaqId;
  run[28]->addHisto(ph);
  addPlot(run[28]);

  TH1I *Run_towerSoftId = new TH1I("hltphi_Run_towerSoftId","hltphi_Run_towerSoftId",5000,0.,5000.);
  ph = new PlotHisto();
  ph->histo = Run_towerSoftId;
  run[29]->addHisto(ph);
  addPlot(run[29]);

  //  run[30]->logy=1;
  TH1D *Run_zEdge = new TH1D("hltphi_Run_zEdge","hltphi_Run_zEdge",100,0.,5.);
  ph = new PlotHisto();
  ph->histo = Run_zEdge;
  run[30]->addHisto(ph);
  addPlot(run[30]);

  TH2F *Run_towerEtaPhi = new TH2F("hltphi_Run_towerEtaPhi","hltphi_Run_towerEtaPhi",100,-2.,2.,100,-3.,3.);
  ph = new PlotHisto();
  ph->histo = Run_towerEtaPhi;
  run[31]->addHisto(ph);
  addPlot(run[31]);

  //-------Pair--1----//
  TH1D *Run_Jpsi = new TH1D("hltphi_Run_Jpsi","hltphi_Run_Jpsi",120,1.,13.);
  ph = new PlotHisto();
  ph->histo = Run_Jpsi;
  run[32]->addHisto(ph);
  addPlot(run[32]);

  TH1D *Run_Signal = new TH1D("hltphi_Run_Signal","hltphi_Run_Signal",130,0.,13.);
  ph = new PlotHisto();
  ph->histo = Run_Signal;
  run[33]->addHisto(ph);
  addPlot(run[33]);

  TH1D *Run_Signal_Cut = new TH1D("hltphi_Run_Signal_Cut","hltphi_Run_Signal_Cut",120,1.,13.);
  ph = new PlotHisto();
  ph->histo = Run_Signal_Cut;
  run[34]->addHisto(ph);
  addPlot(run[34]);

  TH2F *Run_dEdx_P1 = new TH2F("hltphi_Run_dEdx_P1","hltphi_Run_dEdx_P1",50,0.,10.,55,0.,5.5e-06);
  ph = new PlotHisto();
  ph->histo = Run_dEdx_P1;
  run[35]->addHisto(ph);
  addPlot(run[35]);

  //------pair---2-----//
  TH2F *Run_dEdx_P2 = new TH2F("hltphi_Run_dEdx_P2","hltphi_Run_dEdx_P2",50,0.,10.,55,0.,5.5e-06);
  ph = new PlotHisto();
  ph->histo = Run_dEdx_P2;
  run[36]->addHisto(ph);
  addPlot(run[36]);

  //-----jpsi flow-----//
  TH1D *Run_Rapidity = new TH1D("hltphi_Run_Rapidity","hltphi_Run_Rapidity",200,-10.,10.);
  ph = new PlotHisto();
  ph->histo = Run_Rapidity;
  run[37]->addHisto(ph);
  addPlot(run[37]);

  //------Tof histograms-------//
  TH1D *Run_LocalZ = new TH1D("hltphi_Run_LocalZ","hltphi_Run_LocalZ",100,-5.,5.0);
  ph = new PlotHisto();
  ph->histo = Run_LocalZ;
  run[38]->addHisto(ph);
  addPlot(run[38]);

  TH1D *Run_LocalY = new TH1D("hltphi_Run_LocalY","hltphi_Run_LocalY",100,-5.,5.0);
  ph = new PlotHisto();
  ph->histo = Run_LocalY;
  run[39]->addHisto(ph);
  addPlot(run[39]);

  run[40]->optstat = 0;
  run[40]->setDrawOpts("colz");
  TH2F *Run_Beta = new TH2F("hltphi_Run_Beta","hltphi_Run_Beta",80,0,4,60,0.5,3.5);
  ph = new PlotHisto();
  ph->histo = Run_Beta;
  run[40]->addHisto(ph);
  addPlot(run[40]);

  TH2F *Run_matchId_fiberId = new TH2F("hltphi_Run_matchChannelId_fiberChannelId","hltphi_Run_matchChannelId_fiberChannelId",100,0,200,100,0,200); //
  ph = new PlotHisto();
  ph->histo = Run_matchId_fiberId;
  run[41]->addHisto(ph);
  addPlot(run[41]);

  TH2F *Run_TrayID_TrgTime = new TH2F("hltphi_Run_TrayID_TrgTime","hltphi_Run_TrayID_TrgTime",100,0.,122,100,2700,3100);
  ph = new PlotHisto();
  ph->histo = Run_TrayID_TrgTime;
  run[42]->addHisto(ph);
  addPlot(run[42]);

  TH1D *Run_channelID = new TH1D("hltphi_Run_channelID","hltphi_Run_channelID",200,0,200);
  ph = new PlotHisto();
  ph->histo = Run_channelID;
  run[43]->addHisto(ph);
  addPlot(run[43]);

  TH2F *Run_Vzvpd_Vz = new TH2F("hltphi_Run_Vzvpd_Vz","hltphi_Run_Vzvpd_Vz",100,-200,200,100,-200,200);
  ph = new PlotHisto();
  ph->histo = Run_Vzvpd_Vz;
  run[44]->addHisto(ph);
  addPlot(run[44]);

  TH1D *Run_VzDiff = new TH1D("hltphi_Run_VzDiff","hltphi_Run_VzDiff",40,-20,20);
  ph = new PlotHisto();
  ph->histo = Run_channelID;
  run[45]->addHisto(ph);
  addPlot(run[45]);


  ///////////////////////// day histograms ////////////////////////

  TH1I *Day_EventStat = new TH1I("hltphi_Day_EventStat","hltphi_Day_EventStat",4,0.,4);
  ph = new PlotHisto(); //46
  ph->histo = Day_EventStat;
  day[0]->addHisto(ph);
  addPlot(day[0]);

  //------prim track-----// ||
  day[1]->optstat = 0;        //47
  day[1]->setDrawOpts("colz");
  TH2F *Day_Prim_dEdx = new TH2F("hltphi_Day_Prim_dEdx","hltphi_Day_Prim_dEdx",100,0,5,100,0,1.e-5);
  ph = new PlotHisto();
  ph->histo = Day_Prim_dEdx;
  day[1]->addHisto(ph);
  addPlot(day[1]);

  //------Event------//
  TH1D *Day_VertexX = new TH1D("hltphi_Day_VertexX","hltphi_Day_VertexX",100,-1.,1.);
  ph = new PlotHisto();   //48
  ph->histo = Day_VertexX;
  day[2]->addHisto(ph);
  addPlot(day[2]);

  TH1D *Day_VertexY = new TH1D("hltphi_Day_VertexY","hltphi_Day_VertexY",100,-1.,1.);
  ph = new PlotHisto();  //49
  ph->histo = Day_VertexY;
  day[3]->addHisto(ph);
  addPlot(day[3]);

  TH1D *Day_VertexZ = new TH1D("hltphi_Day_VertexZ","hltphi_Day_VertexZ",1000,-200.,200.);
  ph = new PlotHisto();  //50
  ph->histo = Day_VertexZ;
  day[4]->addHisto(ph);
  addPlot(day[4]);

  TH1D *Day_Lm_VertexX = new TH1D("hltphi_Day_Lm_VertexX","hltphi_Day_Lm_VertexX",100,-1.,1.);
  ph = new PlotHisto();  //51
  ph->histo = Day_Lm_VertexX;
  day[5]->addHisto(ph);
  addPlot(day[5]);

  TH1D *Day_Lm_VertexY = new TH1D("hltphi_Day_Lm_VertexY","hltphi_Day_Lm_VertexY",100,-1.,1.);
  ph = new PlotHisto(); //52
  ph->histo = Day_Lm_VertexY;
  day[6]->addHisto(ph);
  addPlot(day[6]);

  TH1D *Day_Lm_VertexZ = new TH1D("hltphi_Day_Lm_VertexZ","hltphi_Day_Lm_VertexZ",1000,-200.,200.);
  ph = new PlotHisto();  //53
  ph->histo = Day_Lm_VertexZ;
  day[7]->addHisto(ph);
  addPlot(day[7]);

  TH1I *Day_Multi = new TH1I("hltphi_Day_Multi", "hltphi_Day_Multi",1000,0,1000);
  ph = new PlotHisto();  //54
  ph->histo = Day_Multi;
  day[8]->addHisto(ph);
  addPlot(day[8]);

  //--------Heavy Fragment-------//
  day[9]->optstat = 0; 
  day[9]->setDrawOpts("colz");
  TH2F *Day_HFM_dEdx = new TH2F("hltphi_Day_HFM_dEdx","hltphi_Day_HFM_dEdx",100,-5,5,100,0,3.e-5);
  ph = new PlotHisto(); //55
  ph->histo = Day_HFM_dEdx;
  day[9]->addHisto(ph);

  TH2F *Day_Ref_dEdx = new TH2F("hltphi_Day_Ref_dEdx","hltphi_Day_Ref_dEdx",100,-5,5,100,0,3.e-5);
  ph = new PlotHisto();
  ph->histo = Day_Ref_dEdx;
  day[9]->addHisto(ph);
  addPlot(day[9]);

  ////////////////////////// cumulate histograms ////////////////////////
  TH1I *Cumu_EventStat = new TH1I("hltphi_Cumu_EventStat","hltphi_Cumu_EventStat",4,0.,4);
  ph = new PlotHisto(); //56
  ph->histo = Cumu_EventStat;
  all[0]->addHisto(ph);
  addPlot(all[0]);
	  
  //------prim track-----//
  all[1]->optstat = 0;
  all[1]->setDrawOpts("colz");
  TH2F *Cumu_Prim_dEdx = new TH2F("hltphi_Cumu_Prim_dEdx","hltphi_Cumu_Prim_dEdx",100,0,5,100,0,1.e-5);
  ph = new PlotHisto(); //57
  ph->histo = Cumu_Prim_dEdx;
  all[1]->addHisto(ph);
  addPlot(all[1]);

  //------Event------//
  TH1D *Cumu_VertexX = new TH1D("hltphi_Cumu_VertexX","hltphi_Cumu_VertexX",100,-1.,1.);
  ph = new PlotHisto();//58
  ph->histo = Cumu_VertexX;
  all[2]->addHisto(ph);
  addPlot(all[2]);

  TH1D *Cumu_VertexY = new TH1D("hltphi_Cumu_VertexY","hltphi_Cumu_VertexY",100,-1.,1.);
  ph = new PlotHisto();//59
  ph->histo = Cumu_VertexY;
  all[3]->addHisto(ph);
  addPlot(all[3]);

  TH1D *Cumu_VertexZ = new TH1D("hltphi_Cumu_VertexZ","hltphi_Cumu_VertexZ",1000,-200.,200.);
  ph = new PlotHisto();//60
  ph->histo = Cumu_VertexZ;
  all[4]->addHisto(ph);
  addPlot(all[4]);

  TH1D *Cumu_Lm_VertexX = new TH1D("hltphi_Cumu_Lm_VertexX","hltphi_Cumu_Lm_VertexX",100,-1.,1.);
  ph = new PlotHisto();//61
  ph->histo = Cumu_Lm_VertexX;
  all[5]->addHisto(ph);
  addPlot(all[5]);

  TH1D *Cumu_Lm_VertexY = new TH1D("hltphi_Cumu_Lm_VertexY","hltphi_Cumu_Lm_VertexY",100,-1.,1.);
  ph = new PlotHisto();//62
  ph->histo = Cumu_Lm_VertexY;
  all[6]->addHisto(ph);
  addPlot(all[6]);

  TH1D *Cumu_Lm_VertexZ = new TH1D("hltphi_Cumu_Lm_VertexZ","hltphi_Cumu_Lm_VertexZ",1000,-200.,200.);
  ph = new PlotHisto();//63
  ph->histo = Cumu_Lm_VertexZ;
  all[7]->addHisto(ph);
  addPlot(all[7]);

  TH1I *Cumu_Multi = new TH1I("hltphi_Cumu_Multi", "hltphi_Cumu_Multi",1000,0,1000);
  ph = new PlotHisto();//64
  ph->histo = Cumu_Multi;
  all[8]->addHisto(ph);
  addPlot(all[8]);
	  
	  
  //-----jpsi flow-----//
  TProfile *Cumu_CosRes = new TProfile("hltphi_Cumu_CosRes","hltphi_Cumu_CosRes",10,0.,10);
  ph = new PlotHisto();//65
  ph->histo = Cumu_CosRes;
  all[9]->addHisto(ph);
  addPlot(all[9]);

  all[10]->optstat = 0;
  all[10]->setDrawOpts("l");
  TProfile *Cumu_Massv2_p1 = new TProfile("hltphi_Cumu_Massv2_p1","hltphi_Cumu_Massv2_p1",mass_bin,mass_low,mass_hi,mean_low,mean_hi);
  ph = new PlotHisto();//66
  ph->histo = Cumu_Massv2_p1;
  all[10]->addHisto(ph);

  TH1D *Cumu_Massv2_fun_p1 = new TH1D("hltphi_Cumu_Massv2_fun_p1","hltphi_Cumu_Massv2_fun_p1",mass_bin,mass_low,mass_hi);
  ph = new PlotHisto();
  ph->histo = Cumu_Massv2_fun_p1;
  all[10]->addHisto(ph);
  addPlot(all[10]);

  all[11]->optstat = 0;
  all[11]->setDrawOpts("l");
  TProfile *Cumu_Massv2_p2 = new TProfile("hltphi_Cumu_Massv2_p2","hltphi_Cumu_Massv2_p2",mass_bin,mass_low,mass_hi,mean_low,mean_hi);
  ph = new PlotHisto();//67
  ph->histo = Cumu_Massv2_p2;
  all[11]->addHisto(ph);

  TH1D *Cumu_Massv2_fun_p2 = new TH1D("hltphi_Cumu_Massv2_fun_p2","hltphi_Cumu_Massv2_fun_p2",mass_bin,mass_low,mass_hi);
  ph = new PlotHisto();
  ph->histo = Cumu_Massv2_fun_p2;
  all[11]->addHisto(ph);
  addPlot(all[11]);

  all[12]->optstat = 0;
  all[12]->setDrawOpts("l");
  TProfile *Cumu_Massv2_p3 = new TProfile("hltphi_Cumu_Massv2_p3","hltphi_Cumu_Massv2_p3",mass_bin,mass_low,mass_hi,mean_low,mean_hi);
  ph = new PlotHisto();//68
  ph->histo = Cumu_Massv2_p3;
  all[12]->addHisto(ph);

  TH1D *Cumu_Massv2_fun_p3 = new TH1D("hltphi_Cumu_Massv2_fun_p3","hltphi_Cumu_Massv2_fun_p3",mass_bin,mass_low,mass_hi);
  ph = new PlotHisto();
  ph->histo = Cumu_Massv2_fun_p3;
  all[12]->addHisto(ph);
  addPlot(all[12]);

  all[13]->optstat = 0;
  all[13]->setDrawOpts("l");
  TProfile *Cumu_Massv2_p4 = new TProfile("hltphi_Cumu_Massv2_p4","hltphi_Cumu_Massv2_p4",mass_bin,mass_low,mass_hi,mean_low,mean_hi);
  ph = new PlotHisto();//69
  ph->histo = Cumu_Massv2_p4;
  all[13]->addHisto(ph);

  TH1D *Cumu_Massv2_fun_p4 = new TH1D("hltphi_Cumu_Massv2_fun_p4","hltphi_Cumu_Massv2_fun_p4",mass_bin,mass_low,mass_hi);
  ph = new PlotHisto();
  ph->histo = Cumu_Massv2_fun_p4;
  all[13]->addHisto(ph);
  addPlot(all[13]);

  all[16]->optstat = 0;
  all[14]->setDrawOpts("l");
  TProfile *Cumu_Massv2_p5 = new TProfile("hltphi_Cumu_Massv2_p5","hltphi_Cumu_Massv2_p5",mass_bin,mass_low,mass_hi,mean_low,mean_hi);
  ph = new PlotHisto();//70
  ph->histo = Cumu_Massv2_p5;
  all[14]->addHisto(ph);

  TH1D *Cumu_Massv2_fun_p5 = new TH1D("hltphi_Cumu_Massv2_fun_p5","hltphi_Cumu_Massv2_fun_p5",mass_bin,mass_low,mass_hi);
  ph = new PlotHisto();
  ph->histo = Cumu_Massv2_fun_p5;
  all[14]->addHisto(ph);
  addPlot(all[14]);

  all[15]->optstat = 0;
  all[15]->setDrawOpts("l");
  TProfile *Cumu_Massv2_p6 = new TProfile("hltphi_Cumu_Massv2_p6","hltphi_Cumu_Massv2_p6",mass_bin,mass_low,mass_hi,mean_low,mean_hi);
  ph = new PlotHisto();//71
  ph->histo = Cumu_Massv2_p6;
  all[15]->addHisto(ph);

  TH1D *Cumu_Massv2_fun_p6 = new TH1D("hltphi_Cumu_Massv2_fun_p6","hltphi_Cumu_Massv2_fun_p6",mass_bin,mass_low,mass_hi);
  ph = new PlotHisto();
  ph->histo = Cumu_Massv2_fun_p6;
  all[15]->addHisto(ph);
  addPlot(all[15]);

  all[16]->optstat = 0;
  all[16]->setDrawOpts("E2");
  TH1D *Cumu_InvMass_p1 = new TH1D("hltphi_Cumu_InvMass_p1","hltphi_Cumu_InvMass_p1",220, 0.98 ,1.2);
  ph = new PlotHisto();//72
  ph->histo = Cumu_InvMass_p1;
  all[16]->addHisto(ph);

  TH1D *Cumu_Background_p1 = new TH1D("hltphi_Cumu_Background_p1","hltphi_Cumu_Background_p1",220, 0.98 ,1.2);
  ph = new PlotHisto();
  ph->histo = Cumu_Background_p1;
  all[16]->addHisto(ph);
  addPlot(all[16]);

  all[17]->optstat = 0;
  all[17]->setDrawOpts("E2");
  TH1D *Cumu_InvMass_p2 = new TH1D("hltphi_Cumu_InvMass_p2","hltphi_Cumu_InvMass_p2",220, 0.98 ,1.2);
  ph = new PlotHisto();//73
  ph->histo = Cumu_InvMass_p2;
  all[17]->addHisto(ph);

  TH1D *Cumu_Background_p2 = new TH1D("hltphi_Cumu_Background_p2","hltphi_Cumu_Background_p2",220, 0.98 ,1.2);
  ph = new PlotHisto();
  ph->histo = Cumu_Background_p2;
  all[17]->addHisto(ph);
  addPlot(all[17]);

  all[18]->optstat = 0;
  all[18]->setDrawOpts("E2");
  TH1D *Cumu_InvMass_p3 = new TH1D("hltphi_Cumu_InvMass_p3","hltphi_Cumu_InvMass_p3",220, 0.98 ,1.2);
  ph = new PlotHisto();//74
  ph->histo = Cumu_InvMass_p3;
  all[18]->addHisto(ph);

  TH1D *Cumu_Background_p3 = new TH1D("hltphi_Cumu_Background_p3","hltphi_Cumu_Background_p3",220, 0.98 ,1.2);
  ph = new PlotHisto();
  ph->histo = Cumu_Background_p3;
  all[18]->addHisto(ph);
  addPlot(all[18]);

  all[19]->optstat = 0;
  all[19]->setDrawOpts("E2");
  TH1D *Cumu_InvMass_p4 = new TH1D("hltphi_Cumu_InvMass_p4","hltphi_Cumu_InvMass_p4",220, 0.98 ,1.2);
  ph = new PlotHisto();//75
  ph->histo = Cumu_InvMass_p4;
  all[19]->addHisto(ph);

  TH1D *Cumu_Background_p4 = new TH1D("hltphi_Cumu_Background_p4","hltphi_Cumu_Background_p4",220, 0.98 ,1.2);
  ph = new PlotHisto();
  ph->histo = Cumu_Background_p4;
  all[19]->addHisto(ph);
  addPlot(all[19]);
 
  all[20]->optstat = 0;
  all[20]->setDrawOpts("E2");
  TH1D *Cumu_InvMass_p5 = new TH1D("hltphi_Cumu_InvMass_p5","hltphi_Cumu_InvMass_p5",220, 0.98 ,1.2);
  ph = new PlotHisto();//76
  ph->histo = Cumu_InvMass_p5;
  all[20]->addHisto(ph);

  TH1D *Cumu_Background_p5 = new TH1D("hltphi_Cumu_Background_p5","hltphi_Cumu_Background_p5",220, 0.98 ,1.2);
  ph = new PlotHisto();
  ph->histo = Cumu_Background_p5;
  all[20]->addHisto(ph);
  addPlot(all[20]);

  all[21]->optstat = 0;
  all[21]->setDrawOpts("E2");
  TH1D *Cumu_InvMass_p6 = new TH1D("hltphi_Cumu_InvMass_p6","hltphi_Cumu_InvMass_p6",220, 0.98 ,1.2);
  ph = new PlotHisto();//77
  ph->histo = Cumu_InvMass_p6;
  all[21]->addHisto(ph);

  TH1D *Cumu_Background_p6 = new TH1D("hltphi_Cumu_Background_p6","hltphi_Cumu_Background_p6",220, 0.98 ,1.2);
  ph = new PlotHisto();
  ph->histo = Cumu_Background_p6;
  all[21]->addHisto(ph);
  addPlot(all[21]);

  all[22]->optstat = 0;
  all[22]->setDrawOpts("E2");
  TH1D *Cumu_Signal_p1 = new TH1D("hltphi_Cumu_Signal_p1","hltphi_Cumu_Signal_p1",220, 0.98 ,1.2);
  ph = new PlotHisto(); //78
  ph->histo = Cumu_Signal_p1;
  all[22]->addHisto(ph);

  TH1D *Cumu_ReBack_p1 = new TH1D("hltphi_Cumu_ReBack_p1","hltphi_Cumu_ReBack_p1",220, 0.98 ,1.2);
  ph = new PlotHisto();
  ph->histo = Cumu_ReBack_p1;
  all[22]->addHisto(ph);
  addPlot(all[22]);

  all[23]->optstat = 0;
  all[23]->setDrawOpts("E2");
  TH1D *Cumu_Signal_p2 = new TH1D("hltphi_Cumu_Signal_p2","hltphi_Cumu_Signal_p2",220, 0.98 ,1.2);
  ph = new PlotHisto(); //79
  ph->histo = Cumu_Signal_p2;
  all[23]->addHisto(ph);

  TH1D *Cumu_ReBack_p2 = new TH1D("hltphi_Cumu_ReBack_p2","hltphi_Cumu_ReBack_p2",220, 0.98 ,1.2);
  ph = new PlotHisto();
  ph->histo = Cumu_ReBack_p2;
  all[23]->addHisto(ph);
  addPlot(all[23]);

  all[24]->optstat = 0;
  all[24]->setDrawOpts("E2");
  TH1D *Cumu_Signal_p3 = new TH1D("hltphi_Cumu_Signal_p3","hltphi_Cumu_Signal_p3",220, 0.98 ,1.2);
  ph = new PlotHisto();//80
  ph->histo = Cumu_Signal_p3;
  all[24]->addHisto(ph);

  TH1D *Cumu_ReBack_p3 = new TH1D("hltphi_Cumu_ReBack_p3","hltphi_Cumu_ReBack_p3",220, 0.98 ,1.2);
  ph = new PlotHisto();
  ph->histo = Cumu_ReBack_p3;
  all[24]->addHisto(ph);
  addPlot(all[24]);

  all[25]->optstat = 0;
  all[25]->setDrawOpts("E2");
  TH1D *Cumu_Signal_p4 = new TH1D("hltphi_Cumu_Signal_p4","hltphi_Cumu_Signal_p4",220, 0.98 ,1.2);
  ph = new PlotHisto();//81
  ph->histo = Cumu_Signal_p4;
  all[25]->addHisto(ph);

  TH1D *Cumu_ReBack_p4 = new TH1D("hltphi_Cumu_ReBack_p4","hltphi_Cumu_ReBack_p4",220, 0.98 ,1.2);
  ph = new PlotHisto();
  ph->histo = Cumu_ReBack_p4;
  all[25]->addHisto(ph);
  addPlot(all[25]);

  all[26]->optstat = 0;
  all[26]->setDrawOpts("E2");
  TH1D *Cumu_Signal_p5 = new TH1D("hltphi_Cumu_Signal_p5","hltphi_Cumu_Signal_p5",220, 0.98 ,1.2);
  ph = new PlotHisto();//82
  ph->histo = Cumu_Signal_p5;
  all[26]->addHisto(ph);

  TH1D *Cumu_ReBack_p5 = new TH1D("hltphi_Cumu_ReBack_p5","hltphi_Cumu_ReBack_p5",220, 0.98 ,1.2);
  ph = new PlotHisto();
  ph->histo = Cumu_ReBack_p5;
  all[26]->addHisto(ph);
  addPlot(all[26]);

  all[27]->optstat = 0;
  all[27]->setDrawOpts("E2");
  TH1D *Cumu_Signal_p6 = new TH1D("hltphi_Cumu_Signal_p6","hltphi_Cumu_Signal_p6",220, 0.98 ,1.2);
  ph = new PlotHisto();//83
  ph->histo = Cumu_Signal_p6;
  all[27]->addHisto(ph);

  TH1D *Cumu_ReBack_p6 = new TH1D("hltphi_Cumu_ReBack_p6","hltphi_Cumu_ReBack_p6",220, 0.98 ,1.2);
  ph = new PlotHisto();
  ph->histo = Cumu_ReBack_p6;
  all[27]->addHisto(ph);
  addPlot(all[27]);

  TH1D *Cumu_v2 = new TH1D("hltphi_Cumu_Jpsiv2","hltphi_Cumu_Jpsiv2",60,0.,6.0);
  ph = new PlotHisto();//84
  ph->histo = Cumu_v2;
  all[28]->addHisto(ph);
  addPlot(all[28]);

  TH1D *LmPionPos = new TH1D("hltphi_LmPionPos","hltphi_LmPionPos",30,0,30);
  ph = new PlotHisto();//85
  ph->histo = LmPionPos;
  all[29]->addHisto(ph);
  addPlot(all[29]);

  //--------Heavy Fragment-------//
  all[30]->optstat = 0;
  all[30]->setDrawOpts("colz");
  TH2F *Cumu_HFM_dEdx = new TH2F("hltphi_Cumu_HFM_dEdx","hltphi_Cumu_HFM_dEdx",100,-5,5,100,0,3.e-5);
  ph = new PlotHisto(); //86
  ph->histo = Cumu_HFM_dEdx;
  all[30]->addHisto(ph);

  TH2F *Cumu_Ref_dEdx = new TH2F("hltphi_Cumu_Ref_dEdx","hltphi_Cumu_Ref_dEdx",100,-5,5,100,0,3.e-5);
  ph = new PlotHisto();
  ph->histo = Cumu_Ref_dEdx;
  all[30]->addHisto(ph);
  addPlot(all[30]);

  //-------------Bin lebal-------------//
  getPlotByIndex(56)->getHisto(0)->histo->GetXaxis()->SetBinLabel(1,"All");
  getPlotByIndex(56)->getHisto(0)->histo->GetXaxis()->SetBinLabel(2,"High Pt");
  getPlotByIndex(56)->getHisto(0)->histo->GetXaxis()->SetBinLabel(3,"J/psi");
  getPlotByIndex(56)->getHisto(0)->histo->GetXaxis()->SetBinLabel(4,"Heavy Fragment");

	  
  //-------------if crash ! recovery !----------------//

  char file[256];

  ifstream *inStream = new ifstream;
  inStream->open("Current.list");

  if(!(inStream)) printf("can not open list file\n");
  inStream->getline(file,256);

  TFile *ftmp = new TFile(file);
	 
  if(!ftmp||!(ftmp->IsOpen())){
	
    printf("First Run ! Initialize! \n");
	  
  }
  else{
    printf("Crash !! \n");
	
    //-------recovery day histogram---------//
    getPlotByIndex(46)->getHisto(0)->histo = (TH1I *)ftmp->Get("Day_EventStat");
		 
    //-----track
    getPlotByIndex(47)->getHisto(0)->histo = (TH2F *)ftmp->Get("Day_Prim_dEdx");
		
    //------event
    getPlotByIndex(48)->getHisto(0)->histo = (TH1D *)ftmp->Get("Day_VertexX");
    getPlotByIndex(49)->getHisto(0)->histo = (TH1D *)ftmp->Get("Day_VertexY");
    getPlotByIndex(50)->getHisto(0)->histo = (TH1D *)ftmp->Get("Day_VertexZ");
    getPlotByIndex(51)->getHisto(0)->histo = (TH1D *)ftmp->Get("Day_Lm_VertexX");
    getPlotByIndex(52)->getHisto(0)->histo = (TH1D *)ftmp->Get("Day_Lm_VertexY");
    getPlotByIndex(53)->getHisto(0)->histo = (TH1D *)ftmp->Get("Day_Lm_VertexZ");
    getPlotByIndex(54)->getHisto(0)->histo = (TH1I *)ftmp->Get("Day_Multi");
		  
    //--Heavy Fragment
    getPlotByIndex(55)->getHisto(0)->histo = (TH2F *)ftmp->Get("Day_HFM_dEdx");
    getPlotByIndex(55)->getHisto(1)->histo = (TH2F *)ftmp->Get("Day_Ref_dEdx");

    ////////--------------Cumu histograms-------------///
    getPlotByIndex(56)->getHisto(0)->histo = (TH1I *)ftmp->Get("Cumu_EventStat");
    getPlotByIndex(57)->getHisto(0)->histo = (TH2F *)ftmp->Get("Cumu_Prim_dEdx");
		
    //--event
    getPlotByIndex(58)->getHisto(0)->histo = (TH1D *)ftmp->Get("Cumu_VertexX");
    getPlotByIndex(59)->getHisto(0)->histo = (TH1D *)ftmp->Get("Cumu_VertexY");
    getPlotByIndex(60)->getHisto(0)->histo = (TH1D *)ftmp->Get("Cumu_VertexZ");
    getPlotByIndex(61)->getHisto(0)->histo = (TH1D *)ftmp->Get("Cumu_Lm_VertexX");
    getPlotByIndex(62)->getHisto(0)->histo = (TH1D *)ftmp->Get("Cumu_Lm_VertexY");
    getPlotByIndex(63)->getHisto(0)->histo = (TH1D *)ftmp->Get("Cumu_Lm_VertexZ");
    getPlotByIndex(64)->getHisto(0)->histo = (TH1I *)ftmp->Get("Cumu_Multi");
		  
    //----pair
    getPlotByIndex(65)->getHisto(0)->histo = (TProfile *)ftmp->Get("Cumu_CosRes");

    getPlotByIndex(85)->getHisto(0)->histo = (TH1D *)ftmp->Get("LmPionPos");
    getPlotByIndex(86)->getHisto(0)->histo = (TH2F *)ftmp->Get("Cumu_HFM_dEdx");
    getPlotByIndex(86)->getHisto(1)->histo = (TH2F *)ftmp->Get("Cumu_Ref_dEdx");

    InvMassv2 = (TH3D *)ftmp->Get("InvMassv2");
    InvMassv2_Sin = (TH3D *)ftmp->Get("InvMassv2_Sin");
    DenInvMass = (TH3D *)ftmp->Get("DenInvMass");

  }

};
  

void HltphiBuilder::startrun(daqReader *rdr){

  getPlotByIndex(0)->getHisto(0)->histo->GetXaxis()->SetBinLabel(1,"All");
  getPlotByIndex(0)->getHisto(0)->histo->GetXaxis()->SetBinLabel(2,"High Pt");
  getPlotByIndex(0)->getHisto(0)->histo->GetXaxis()->SetBinLabel(3,"J/psi");
  getPlotByIndex(0)->getHisto(0)->histo->GetXaxis()->SetBinLabel(4,"Heavy Fragment");
           
  //----------refresh histograms----------//
  char filename[256];
  ifstream *in = new ifstream;
  in->open("Current.list");
  in->getline(filename,256);
	  
  TFile *f = new TFile(filename);
  if(!(f->IsOpen())){
	
    printf("First Run ! startrun! \n");

  }
  else{
		  
    TObjString *obj = (TObjString *)f->Get("normal");
		  
    if(obj){
		
      cout<<"normal \n"<<endl;

    }
    else{
			 
      for(int i=0;i<87;i++){
			
	getPlotByIndex(i)->getHisto(0)->histo->Reset();
	cout<<"refresh ! \n"<<endl;

      }

      getPlotByIndex(72)->getHisto(1)->histo->Reset();
      getPlotByIndex(73)->getHisto(1)->histo->Reset();
      getPlotByIndex(74)->getHisto(1)->histo->Reset();
      getPlotByIndex(75)->getHisto(1)->histo->Reset();
      getPlotByIndex(76)->getHisto(1)->histo->Reset();
      getPlotByIndex(77)->getHisto(1)->histo->Reset();
      getPlotByIndex(78)->getHisto(1)->histo->Write();
      getPlotByIndex(79)->getHisto(1)->histo->Write();
      getPlotByIndex(80)->getHisto(1)->histo->Write();
      getPlotByIndex(81)->getHisto(1)->histo->Write();
      getPlotByIndex(82)->getHisto(1)->histo->Write();
      getPlotByIndex(83)->getHisto(1)->histo->Write();

      getPlotByIndex(55)->getHisto(1)->histo->Reset();
      getPlotByIndex(86)->getHisto(1)->histo->Reset();

    }

  }


  //------------resolution--------------//

  TProfile *TempCosRes = (TProfile *)getPlotByIndex(65)->getHisto(0)->histo ; 

  if(!(TempCosRes->GetEntries())){

    res2 = 0.74843;  //run7 AuAu resolution
    res2error = 1.8594e-06;

  }
  else{
		  
    double deltaRes2Sub = 0.005;
    double res2Sub = sqrt(TempCosRes->GetBinContent(1));
    double res2SubErr = TempCosRes->GetBinError(1)/(2.*res2Sub);
    double chiSub2 = chi(res2Sub);
    double chiSub2Delta = chi(res2Sub + deltaRes2Sub);

    res2 = resEventPlane(sqrt(2.) * chiSub2);
    double mRes2Delta = resEventPlane(sqrt(2.) * chiSub2Delta);
    res2error = res2SubErr * fabs((double)res2 - mRes2Delta)/ deltaRes2Sub;
    cout <<" Resolution = " << res2 << "+/-" << res2error << endl;
  }

  sprintf(Currentrun,"output/run10_hlt_%d_current_hist.root",rdr->run);
  sprintf(label,"%d",rdr->run);

  TempFlowPhi=new TH1D("hltphi_TempFlowPhi","hltphi_TempFlowPhi",360,0.,twopi);


  //------------phi weight---------------//

  flowphi->Add(getPlotByIndex(10)->getHisto(0)->histo);
  if(flowphi->GetEntries() > 20) flowphi->RemoveAt(0);
  flowphi->Sort();

  TempPhiWgtCal();

  for(int i=0;i<46;i++){
    getPlotByIndex(i)->getHisto(0)->histo->Reset();
  }
  for(int i=66;i<85;i++){
    getPlotByIndex(i)->getHisto(0)->histo->Reset();
  }
  for(int i=72;i<84;i++){
    getPlotByIndex(i)->getHisto(1)->histo->Reset();
  }

  //------------for every day-----------//

  /*  int runnumber = rdr->run;
      char num[60];
      sprintf(num,"%i",runnumber);
      string str(num);
      int len = str.length();
      str.replace(len-3,3,"");
      str.replace(0,2,"");
      unsigned int day = atoi(str.c_str());
	  
      if(day != current_day){

      getPlotByIndex(46)->getHisto(0)->histo->GetXaxis()->SetBinLabel(1,"All");
      getPlotByIndex(46)->getHisto(0)->histo->GetXaxis()->SetBinLabel(2,"High Pt");
      getPlotByIndex(46)->getHisto(0)->histo->GetXaxis()->SetBinLabel(3,"J/psi");
      getPlotByIndex(46)->getHisto(0)->histo->GetXaxis()->SetBinLabel(4,"Heavy Fragment");

      for(int j=46;j<56;j++){
      getPlotByIndex(j)->getHisto(0)->histo->Reset();
      }
	
      current_day = day;
      printf("Starting day #%d\n",current_day);

      }*/
  //printf("Starting run #%d\n",rdr->run);

  delete TempFlowPhi;
};

void HltphiBuilder::stoprun(daqReader *rdr) {

  //----------lndedx pion----------//
  float low = -13.1;
  float high = -12.8;
  nbin++;
  if(nbin > 30){
    nbin = nbin -1;
    for(int i=2;i<=30;i++){
      double tem;
      double temerr;
      char *temlab;
      tem = getPlotByIndex(85)->getHisto(0)->histo->GetBinContent(i);
      temerr=getPlotByIndex(85)->getHisto(0)->histo->GetBinError(i);
      getPlotByIndex(85)->getHisto(0)->histo->SetBinContent(i-1,tem);
      getPlotByIndex(85)->getHisto(0)->histo->SetBinError(i-1,temerr);

      sprintf(temlab,"%s",getPlotByIndex(85)->getHisto(0)->histo->GetXaxis()->GetBinLabel(i));
      getPlotByIndex(85)->getHisto(0)->histo->GetXaxis()->SetBinLabel(i-1,temlab);
    }
  } 

  TF1 *fit = new TF1("fit","[0]*exp(-(x-[1])*(x-[1])/2./[2]/[2])",low,high);   //
  fit->SetParName(0,"Apt");
  fit->SetParName(1,"Pos");
  fit->SetParName(2,"Sig");
  fit->SetParameter(0,50000);
  fit->SetParameter(1,0.2);
  fit->SetParameter(2,0.005);


#ifdef CODE_DIES
  getPlotByIndex(9)->getHisto(0)->histo->Fit(fit,"EMRB","",low,high);
#endif

  Double_t par[3];
  par[0]=fit->GetParameter(0);
  par[1]=fit->GetParameter(1);
  par[2]=fit->GetParameter(2);

  double error;
  error = fit->GetParError(1);

  ((TH1D *)getPlotByIndex(85)->getHisto(0)->histo)->SetBinContent(nbin,par[1]);
  ((TH1D *)getPlotByIndex(85)->getHisto(0)->histo)->SetBinError(nbin,error);
  getPlotByIndex(85)->getHisto(0)->histo->GetXaxis()->SetBinLabel(nbin,label);

  //////////////////////////
  TFile f("/star/u/xueliang/lbl/xueliang/Display/phiup.root");
  TH3F *m1 = (TH3F *)f.Get("InvMassv2_Cen_0");
  InvMassv2->Add(m1);
  delete m1;
  TH3F *m2 = (TH3F *)f.Get("InvMassv2_Cen_1");
  InvMassv2->Add(m2);
  delete m2;
  TH3F *m3 = (TH3F *)f.Get("InvMassv2_Cen_2");
  InvMassv2->Add(m3);
  delete m3;
  TH3F *m4 = (TH3F *)f.Get("InvMassv2_Cen_3");
  InvMassv2->Add(m4);
  delete m4;
  TH3F *m5 = (TH3F *)f.Get("InvMassv2_Cen_4");
  InvMassv2->Add(m5);
  delete m5;
  TH3F *m6 = (TH3F *)f.Get("InvMassv2_Cen_5");
  InvMassv2->Add(m6);
  delete m6;
  TH3F *m7 = (TH3F *)f.Get("InvMassv2_Cen_6");
  InvMassv2->Add(m7);
  delete m7;
  TH3F *m8 = (TH3F *)f.Get("InvMassv2_Cen_7");
  InvMassv2->Add(m8);
  delete m8;
  TH3F *m9 = (TH3F *)f.Get("InvMassv2_Cen_8");
  InvMassv2->Add(m9);
  delete m9;

  //---------------------

  TH3F *n1 = (TH3F *)f.Get("flowDenNeutral_Cen_0");
  DenInvMass->Add(n1);
  delete n1;
  TH3F *n2 = (TH3F *)f.Get("flowDenNeutral_Cen_1");
  DenInvMass->Add(n2);
  delete n2;
  TH3F *n3 = (TH3F *)f.Get("flowDenNeutral_Cen_2");
  DenInvMass->Add(n3);
  delete n3;
  TH3F *n4 = (TH3F *)f.Get("flowDenNeutral_Cen_3");
  DenInvMass->Add(n4);
  delete n4;
  TH3F *n5 = (TH3F *)f.Get("flowDenNeutral_Cen_4");
  DenInvMass->Add(n5);
  delete n5;
  TH3F *n6 = (TH3F *)f.Get("flowDenNeutral_Cen_5");
  DenInvMass->Add(n6);
  delete n6;
  TH3F *n7 = (TH3F *)f.Get("flowDenNeutral_Cen_6");
  DenInvMass->Add(n7);
  delete n7;
  TH3F *n8 = (TH3F *)f.Get("flowDenNeutral_Cen_7");
  DenInvMass->Add(n8);
  delete n8;
  TH3F *n9 = (TH3F *)f.Get("flowDenNeutral_Cen_8");
  DenInvMass->Add(n9);
  delete n9;
  ////////////////////////////////////////////

  Jpsiflow();
  WriteHistogram(Currentrun);
  WriteList(Currentrun);
  printf("Stopping run #%d\n",rdr->run);
};
 
  
/////////////////////////////////////////////////////////////////////
//////---raw phi | dedx | Eventplane && subevents | jpsiflow---//////
///////////////////////////////////////////////////////////////////// 
void HltphiBuilder::event(daqReader *rdr) {
  
  float Qx;
  float Qy;
  float Qx_ran1;
  float Qy_ran1;
  float Qx_ran2;
  float Qy_ran2;

  //Primary Track Multiplicity Cuts
  float cos_part=0.;
  float sin_part=0.;
  float sinsum = 0;
  float cossum = 0;

  float cos_part0=0.;
  float sin_part0=0.;
  float sinsum0 = 0;
  float cossum0 = 0;

  // sub events for reaction plane determination
  float sinsumQ1 = 0;
  float cossumQ1 = 0;
  float sinsumQ2 = 0;
  float cossumQ2 = 0;

  int triggerBitHighPt = 0x10000;
  int triggerBitDiElectron = 0x20000;
  int triggerBitHeavyFragment = 0x40000;
  int triggerBitAllEvents = 0x80000;

  ((TH1I *)getPlotByIndex(0)->getHisto(0)->histo)->Fill(0.);  //run  event statistic
  ((TH1I *)getPlotByIndex(46)->getHisto(0)->histo)->Fill(0.); //day
  ((TH1I *)getPlotByIndex(56)->getHisto(0)->histo)->Fill(0.); //all

  int ret = 0;
  daq_dta *dd ;
  TRandom *random = new TRandom();

  dd = rdr->det("hlt")->get("gl3") ;
  if(!dd) return;

  HLT_EVE  *hlt_eve ;
  HLT_TOF  *hlt_tof ;
  HLT_PVPD *hlt_pvpd ;
  HLT_EMC  *hlt_emc ;
  HLT_GT   *hlt_gt ;
  HLT_PT   *hlt_pt ;
  HLT_NODE *hlt_node ;
  HLT_HIPT *hlt_hipt ;
  HLT_DIEP *hlt_diep ;
  HLT_HF *hlt_hf ;


  while(dd && dd->iterate()){

    hlt_gl3_t *hlt = (hlt_gl3_t *) dd->Void ;

    if(strcmp(hlt->name,"HLT_EVE")==0) hlt_eve = (HLT_EVE *)hlt->data;

    else if(strcmp(hlt->name,"HLT_TOF")==0) hlt_tof = (HLT_TOF *)hlt->data;

    else if(strcmp(hlt->name,"HLT_PVPD")==0) hlt_pvpd = (HLT_PVPD *)hlt->data;

    else if(strcmp(hlt->name,"HLT_EMC")==0) hlt_emc = (HLT_EMC *)hlt->data;

    else if(strcmp(hlt->name,"HLT_GT")==0) hlt_gt = (HLT_GT *)hlt->data;

    else if(strcmp(hlt->name,"HLT_PT")==0) hlt_pt = (HLT_PT *)hlt->data;

    else if(strcmp(hlt->name,"HLT_NODE")==0) hlt_node = (HLT_NODE *)hlt->data;

    else if(strcmp(hlt->name,"HLT_HIPT")==0) hlt_hipt = (HLT_HIPT *)hlt->data;

    else if(strcmp(hlt->name,"HLT_DIEP")==0) hlt_diep = (HLT_DIEP *)hlt->data;

    else if(strcmp(hlt->name,"HLT_HF")==0) hlt_hf = (HLT_HF *)hlt->data;

  }


  int decision = hlt_eve->hltDecision;

  if(decision & triggerBitHighPt){

    ((TH1I *)getPlotByIndex(0)->getHisto(0)->histo)->Fill(1.); //run
    ((TH1I *)getPlotByIndex(46)->getHisto(0)->histo)->Fill(1.); //day
    ((TH1I *)getPlotByIndex(56)->getHisto(0)->histo)->Fill(1.); //all

  }
  if(decision & triggerBitDiElectron){

    ((TH1I *)getPlotByIndex(0)->getHisto(0)->histo)->Fill(2.); //run
    ((TH1I *)getPlotByIndex(46)->getHisto(0)->histo)->Fill(2.); //day
    ((TH1I *)getPlotByIndex(56)->getHisto(0)->histo)->Fill(2.); //all

  }
  if(decision & triggerBitHeavyFragment){

    ((TH1I *)getPlotByIndex(0)->getHisto(0)->histo)->Fill(3.); //run
    ((TH1I *)getPlotByIndex(46)->getHisto(0)->histo)->Fill(3.); //day
    ((TH1I *)getPlotByIndex(56)->getHisto(0)->histo)->Fill(3.); //all

  }

  //----------fill events-----------//
  float vertX = hlt_eve->vertexX ;
  float vertY = hlt_eve->vertexY ;
  float vertZ = hlt_eve->vertexZ ;
  float lmvertX = hlt_eve->lmVertexX ;
  float lmvertY = hlt_eve->lmVertexY ;
  float lmvertZ = hlt_eve->lmVertexZ ;

  float VzVpd =  hlt_eve->vpdVertexZ ;

  ((TH1D *)getPlotByIndex(16)->getHisto(0)->histo)->Fill(vertX);  //run
  ((TH1D *)getPlotByIndex(48)->getHisto(0)->histo)->Fill(vertX);  //day
  ((TH1D *)getPlotByIndex(58)->getHisto(0)->histo)->Fill(vertX);  //all
  ((TH1D *)getPlotByIndex(17)->getHisto(0)->histo)->Fill(vertY);  //run
  ((TH1D *)getPlotByIndex(49)->getHisto(0)->histo)->Fill(vertY);  //day
  ((TH1D *)getPlotByIndex(59)->getHisto(0)->histo)->Fill(vertY);  //all
  ((TH1D *)getPlotByIndex(18)->getHisto(0)->histo)->Fill(vertZ);  //run
  ((TH1D *)getPlotByIndex(50)->getHisto(0)->histo)->Fill(vertZ);  //day
  ((TH1D *)getPlotByIndex(60)->getHisto(0)->histo)->Fill(vertZ);  //all

  ((TH1D *)getPlotByIndex(19)->getHisto(0)->histo)->Fill(lmvertX);  //run
  ((TH1D *)getPlotByIndex(51)->getHisto(0)->histo)->Fill(lmvertX);  //day
  ((TH1D *)getPlotByIndex(61)->getHisto(0)->histo)->Fill(lmvertX);  //all
  ((TH1D *)getPlotByIndex(20)->getHisto(0)->histo)->Fill(lmvertY);  //run
  ((TH1D *)getPlotByIndex(52)->getHisto(0)->histo)->Fill(lmvertY);  //day
  ((TH1D *)getPlotByIndex(62)->getHisto(0)->histo)->Fill(lmvertY);  //all
  ((TH1D *)getPlotByIndex(21)->getHisto(0)->histo)->Fill(lmvertZ);  //run
  ((TH1D *)getPlotByIndex(53)->getHisto(0)->histo)->Fill(lmvertZ);  //day
  ((TH1D *)getPlotByIndex(63)->getHisto(0)->histo)->Fill(lmvertZ);  //all

  ((TH2F *)getPlotByIndex(44)->getHisto(0)->histo)->Fill(VzVpd, vertZ);  //run
  ((TH1D *)getPlotByIndex(45)->getHisto(0)->histo)->Fill(VzVpd - vertZ);  //run


  //------------------fill tof hits--------------//

  for(u_int i=0 ; i < hlt_tof->nTofHits ; i++){

    short trayId     = hlt_tof->tofHit[i].trayId;
    short channel    = hlt_tof->tofHit[i].channel;
    float tdc         = hlt_tof->tofHit[i].tdc;
    float tof         = hlt_tof->tofHit[i].tof; 
    float triggertime = hlt_tof->tofHit[i].triggertime;

    ((TH2F *)getPlotByIndex(42)->getHisto(0)->histo)->Fill(trayId, triggertime);
    ((TH1D *)getPlotByIndex(43)->getHisto(0)->histo)->Fill(channel); 

  }

  //-----------------fill pvpd hit------------//

  for(u_int i=0 ; i < hlt_pvpd->nPvpdHits ; i++){

    short trayId     = hlt_pvpd->pvpdHit[i].trayId;
    short channel    = hlt_pvpd->pvpdHit[i].channel;
    float tdc         = hlt_pvpd->pvpdHit[i].tdc;
    float tof         = hlt_pvpd->pvpdHit[i].tof;
    float triggertime = hlt_pvpd->pvpdHit[i].triggertime;

  }

  //--------------fill Emc--------------//

  for(u_int i=0 ; i < hlt_emc->nEmcTowers ; i++){

    int adc = hlt_emc->emcTower[i].adc;
    short energy     = hlt_emc->emcTower[i].energy;

    short phi   = hlt_emc->emcTower[i].phi;
    float  eta   = hlt_emc->emcTower[i].eta;
    float  z     = hlt_emc->emcTower[i].z; 
    int softId  = hlt_emc->emcTower[i].softId;
    int daqId   = hlt_emc->emcTower[i].daqId;

    ((TH1D *)getPlotByIndex(27)->getHisto(0)->histo)->Fill(energy);  //run
    ((TH1D *)getPlotByIndex(28)->getHisto(0)->histo)->Fill(daqId);  //run
    ((TH1D *)getPlotByIndex(29)->getHisto(0)->histo)->Fill(softId);  //run
    ((TH2F *)getPlotByIndex(31)->getHisto(0)->histo)->Fill(eta,phi);  //run
  }


  //---------------global track-------------//
  //	  cout<<"nGlobalTracks ="<<hlt_gt->nGlobalTracks <<endl;
  for(u_int i=0 ; i < hlt_gt->nGlobalTracks ; i++)
    {
      int nHits = hlt_gt->globalTrack[i].nHits;
      int nDedx = hlt_gt->globalTrack[i].ndedx;

      ((TH1I *)getPlotByIndex(1)->getHisto(0)->histo)->Fill(nHits);  //run
      ((TH1I *)getPlotByIndex(2)->getHisto(0)->histo)->Fill(nDedx);  //run

      if(hlt_gt->globalTrack[i].flag < 0.) continue;
      if(hlt_gt->globalTrack[i].q != +1 && hlt_gt->globalTrack[i].q != -1) continue;

      float pt = hlt_gt->globalTrack[i].pt;
      float px = cos(hlt_gt->globalTrack[i].psi)*hlt_gt->globalTrack[i].pt ;
      float py = sin(hlt_gt->globalTrack[i].psi)*hlt_gt->globalTrack[i].pt ;
      float pz = hlt_gt->globalTrack[i].tanl*hlt_gt->globalTrack[i].pt ;


      TVector3 mom(px,py,pz);

      float eta=mom.PseudoRapidity();
      float phi = mom.Phi();
      if(phi<0.0) phi += twopi;
      float p = mom.Mag();
      float dedx = hlt_gt->globalTrack[i].dedx;

      ((TH1D *)getPlotByIndex(3)->getHisto(0)->histo)->Fill(pt);   //run
      ((TH1D *)getPlotByIndex(4)->getHisto(0)->histo)->Fill(phi);   //run
      ((TH1D *)getPlotByIndex(5)->getHisto(0)->histo)->Fill(eta);   //run

      if(nHits >= 20 && nDedx >= 15) {

	((TH2F *)getPlotByIndex(7)->getHisto(0)->histo)->Fill(p,dedx);   //run

      }
      if(nHits >= 20 && nDedx >= 15 && p >= 0.45 && p <= 0.55 ){

	((TH1D *)getPlotByIndex(9)->getHisto(0)->histo)->Fill(log(dedx));  //run

      }

    }//loop of tracks


  //--------------primary tracks---------------//

  int refMult = 0;

  for(u_int i=0 ; i < hlt_pt->nPrimaryTracks ; i++)
    {
      int nHits = hlt_pt->primaryTrack[i].nHits;
      int nDedx = hlt_pt->primaryTrack[i].ndedx;

      if(hlt_pt->primaryTrack[i].flag < 0.) continue;
      if(hlt_pt->primaryTrack[i].q != +1 && hlt_pt->primaryTrack[i].q != -1) continue;

      float pt = hlt_pt->primaryTrack[i].pt;
      float px = cos(hlt_pt->primaryTrack[i].psi)*hlt_pt->primaryTrack[i].pt ;
      float py = sin(hlt_pt->primaryTrack[i].psi)*hlt_pt->primaryTrack[i].pt ;
      float pz = hlt_pt->primaryTrack[i].tanl*hlt_pt->primaryTrack[i].pt ;

      TVector3 mom(px,py,pz);

      float eta=mom.PseudoRapidity();
      float dca = 0;//
      float phi = mom.Phi();
      if(phi<0.0) phi += twopi;
      float p = mom.Mag();
      float dedx = hlt_pt->primaryTrack[i].dedx;

      int phiPointer = (int)(phi/(twopi)*360);
      double weight=0.;
      double ptWeight=0.;
      double phiWeight=0.;

      ((TH1D *)getPlotByIndex(11)->getHisto(0)->histo)->Fill(pt);   //run
      ((TH1D *)getPlotByIndex(12)->getHisto(0)->histo)->Fill(phi);   //run
      ((TH1D *)getPlotByIndex(13)->getHisto(0)->histo)->Fill(eta);   //run
      ((TH1D *)getPlotByIndex(6)->getHisto(0)->histo)->Fill(dca);   //run

      if(nHits >= 20 && nDedx >= 15) {

	((TH2F *)getPlotByIndex(14)->getHisto(0)->histo)->Fill(p,dedx);   //run
	((TH2F *)getPlotByIndex(47)->getHisto(0)->histo)->Fill(p,dedx);   //day
	((TH2F *)getPlotByIndex(57)->getHisto(0)->histo)->Fill(p,dedx);   //all

	((TH2F *)getPlotByIndex(55)->getHisto(1)->histo)->Fill(p,dedx) ; // for HF reference
	((TH2F *)getPlotByIndex(86)->getHisto(1)->histo)->Fill(p,dedx) ; // for HF reference

      }

      if(nHits >= 15  && pt >= 0.1 && pt < 2.0 && fabs(eta) < 1.0 && fabs(dca) < 2.0)
	{

	  if(pt<2.0) ptWeight = pt;
	  else ptWeight = 2.0;

	  ((TH1D *)getPlotByIndex(10)->getHisto(0)->histo)->Fill(phi,ptWeight);  //run

	  phiWeight = TempFlowPhiWgt->GetBinContent(phiPointer+1); 

	  weight =ptWeight*phiWeight;

	  cos_part = (weight)*cos(2*phi);
	  sin_part = (weight)*sin(2*phi);
	  cos_part0 = cos(2*phi);
	  sin_part0 = sin(2*phi);

	  //Event Plane Vector
	  cossum += cos_part;
	  sinsum += sin_part;

	  cossum0 += cos_part0;
	  sinsum0 += sin_part0;

	  int randval = (int) floor(0.5 + random->Rndm());

	  if(randval == 0){//subevent 1

	    cossumQ1 += cos_part;
	    sinsumQ1 += sin_part;

	  }
	  else{//subevent 2

	    cossumQ2 += cos_part;
	    sinsumQ2 += sin_part;

	  }

	}

      if(fabs(eta) < 0.5 && nHits >= 10 && fabs(dca) < 3.) refMult++;

    }

  ((TH1I *)getPlotByIndex(22)->getHisto(0)->histo)->Fill(refMult);  //run
  ((TH1I *)getPlotByIndex(51)->getHisto(0)->histo)->Fill(refMult);  //day
  ((TH1I *)getPlotByIndex(61)->getHisto(0)->histo)->Fill(refMult);  //all

  random->Delete();

  TVector2 *Q_Full = new TVector2(cossum, sinsum);

  if(Q_Full->Mod() == 0){
    delete Q_Full;
  }

  Qx = cossum;
  Qy = sinsum;
  Qx_ran1 = cossumQ1;
  Qy_ran1 = sinsumQ1;
  Qx_ran2 = cossumQ2;
  Qy_ran2 = sinsumQ2;

  TVector2 *Qran1= new TVector2(Qx_ran1,Qy_ran1);
  TVector2 *Qran2= new TVector2(Qx_ran2,Qy_ran2);

  if((Qran1->Mod2()) !=0.0 && (Qran2->Mod2()) !=0.0){

    float psi_diff = 0.5*Qran1->Phi() - 0.5*Qran2->Phi();
    if (psi_diff < 0.0) psi_diff += twopi /2.0;
    ((TH1D *)getPlotByIndex(23)->getHisto(0)->histo)->Fill(psi_diff);

  }

  //Phi defined from 0 - 2PI in TVector2
  TVector2 *Q_Raw = new TVector2(cossum0, sinsum0);
  if((Q_Raw->Mod())!=0.0){

    float Plane;
    Plane = Q_Raw->Phi()/2.;
    if(Plane<0.) Plane += twopi/2.0;
    ((TH1D *)getPlotByIndex(24)->getHisto(0)->histo)->Fill(Plane);

  }

  if((Q_Full->Mod())!=0.0){

    float rxnPlane;
    rxnPlane = Q_Full->Phi()/2.;              //reaction plane angle ranges from 0 to PI
    if(rxnPlane<0.) rxnPlane += twopi/2.0;
    ((TH1D *)getPlotByIndex(25)->getHisto(0)->histo)->Fill(rxnPlane);

  }


  //------------------fille nodes-----------------//

  //	  cout<<"nNodes = "<< hlt_node->nNodes <<endl;

  for(int i=0 ; i < hlt_node->nNodes ; i++){

    int globalTrackSN  =  hlt_node->node[i].globalTrackSN;
    int primaryTrackSN =  hlt_node->node[i].primaryTrackSN;
    int tofHitSN       =  hlt_node->node[i].tofHitSN;
    int emcTowerSN     =  hlt_node->node[i].emcTowerSN;

    //	  cout<<"globalTrackSN ="<<globalTrackSN<<endl;
    //	  cout<<"primaryTrackSN ="<<primaryTrackSN<<endl;

    float gpx = hlt_pt->primaryTrack[primaryTrackSN].pt*cos(hlt_pt->primaryTrack[primaryTrackSN].psi);
    float gpy = hlt_pt->primaryTrack[primaryTrackSN].pt*sin(hlt_pt->primaryTrack[primaryTrackSN].psi);
    float gpz = hlt_pt->primaryTrack[primaryTrackSN].tanl*hlt_pt->primaryTrack[primaryTrackSN].pt;
    float gp = sqrt(gpx*gpx + gpy*gpy + gpz*gpz) ;

    float px = hlt_pt->primaryTrack[primaryTrackSN].pt*cos(hlt_pt->primaryTrack[primaryTrackSN].psi);
    float py = hlt_pt->primaryTrack[primaryTrackSN].pt*sin(hlt_pt->primaryTrack[primaryTrackSN].psi);
    float pz = hlt_pt->primaryTrack[primaryTrackSN].tanl*hlt_pt->primaryTrack[primaryTrackSN].pt;
    float p = sqrt(px*px + py*py +pz*pz) ;

    if(tofHitSN >= 0) {

      float localY = hlt_node->node[i].localY;
      float localZ = hlt_node->node[i].localZ;
      float beta   = hlt_node->node[i].beta;
      float tof    = hlt_node->node[i].tof;
      int  projChannel = hlt_node->node[i].projChannel;

      ((TH1D *)getPlotByIndex(38)->getHisto(0)->histo)->Fill(localZ);
      ((TH1D *)getPlotByIndex(39)->getHisto(0)->histo)->Fill(localY);
      ((TH2F *)getPlotByIndex(40)->getHisto(0)->histo)->Fill(p, 1/beta);
      ((TH2F *)getPlotByIndex(41)->getHisto(0)->histo)->Fill(projChannel, hlt_tof->tofHit[tofHitSN].channel);

    }

    if(emcTowerSN >= 0) {

      double emcMatchPhiDiff = hlt_node->node[i].emcMatchPhiDiff;
      double emcMatchZEdge   = hlt_node->node[i].emcMatchZEdge;

      ((TH2F *)getPlotByIndex(8)->getHisto(0)->histo)->Fill(gp, hlt_emc->emcTower[emcTowerSN].energy);
      ((TH2F *)getPlotByIndex(15)->getHisto(0)->histo)->Fill(p, hlt_emc->emcTower[emcTowerSN].energy);

      ((TH1D *)getPlotByIndex(26)->getHisto(0)->histo)->Fill(emcMatchPhiDiff);
      if(emcMatchZEdge > 0.) ((TH1D *)getPlotByIndex(30)->getHisto(0)->histo)->Fill(emcMatchZEdge);
    }

  }

  //-------------------High Pt---------------------//

  for(u_int i=0 ; i < hlt_hipt->nHighPt ; i++){

  }


  //-------------------Heavy Fragment-----------------//

  for(u_int i=0 ; i < hlt_hf->nHeavyFragments ; i++){


    int heavyFragmentSN = hlt_node->node[hlt_hf->heavyFragmentSN[i]].globalTrackSN ;

    hlt_track HFtrack = hlt_gt->globalTrack[heavyFragmentSN] ;

    int nHits =  HFtrack.nHits ;
    int nDedx =  HFtrack.ndedx ;

    float hfpx    = HFtrack.pt*cos(HFtrack.psi) ;
    float hfpy    = HFtrack.pt*sin(HFtrack.psi) ;
    float hfpz    = HFtrack.pt*HFtrack.tanl ;
    float hfp     = sqrt(hfpx*hfpx + hfpy*hfpy + hfpz*hfpz) ;

    float hfdedx  =  HFtrack.dedx ;

    if(nHits >= 20 && nDedx >= 15) {

      ((TH2F *)getPlotByIndex(55)->getHisto(0)->histo)->Fill(hfp , hfdedx) ; //day
      ((TH2F *)getPlotByIndex(55)->getHisto(0)->histo)->SetMarkerStyle(29) ;
      ((TH2F *)getPlotByIndex(55)->getHisto(0)->histo)->SetMarkerSize(1.2) ;
      ((TH2F *)getPlotByIndex(55)->getHisto(0)->histo)->SetMarkerColor(4) ;

      ((TH2F *)getPlotByIndex(86)->getHisto(0)->histo)->Fill(hfp , hfdedx) ; //all
      ((TH2F *)getPlotByIndex(86)->getHisto(0)->histo)->SetMarkerStyle(29) ;
      ((TH2F *)getPlotByIndex(86)->getHisto(0)->histo)->SetMarkerSize(1.2) ;
      ((TH2F *)getPlotByIndex(86)->getHisto(0)->histo)->SetMarkerColor(4) ;

    }

  }


  //---------------  Di electrons ------------------// 

  for(u_int i=0 ; i < hlt_diep->nEPairs ; i++) {

    TVector2 Qall(Qx,Qy);

    int dau1NodeSN = hlt_diep->ePair[i].dau1NodeSN ;
    int dau2NodeSN = hlt_diep->ePair[i].dau2NodeSN ;

    int dau1TrackSN = hlt_node->node[dau1NodeSN].primaryTrackSN ;
    int dau2TrackSN = hlt_node->node[dau2NodeSN].primaryTrackSN ;


    /////////////// ---daughter 1-- ////////////

    float dau1q     = hlt_pt->primaryTrack[dau1TrackSN].q ;
    float dau1pt    = hlt_pt->primaryTrack[dau1TrackSN].pt ;
    float dau1px    = hlt_pt->primaryTrack[dau1TrackSN].pt*cos(hlt_pt->primaryTrack[dau1TrackSN].psi) ; 
    float dau1py    = hlt_pt->primaryTrack[dau1TrackSN].pt*sin(hlt_pt->primaryTrack[dau1TrackSN].psi) ;
    float dau1pz    = hlt_pt->primaryTrack[dau1TrackSN].pt*hlt_pt->primaryTrack[dau1TrackSN].tanl ;
    float dau1nHits = hlt_pt->primaryTrack[dau1TrackSN].nHits ;
    float dau1dedx  =  hlt_pt->primaryTrack[dau1TrackSN].dedx ;

    float dau1Qx;
    float dau1Qy;


    double dau1weight = 0. ;
    double dau1ptWeight = 0. ;
    double dau1phiWeight = 0. ;

    TVector3 dau1( dau1px, dau1py, dau1pz ) ;

    float dau1p = dau1.Mag() ;

    float dau1eta = dau1.PseudoRapidity() ;
    if( fabs(dau1eta) > 1.) continue ;

    float dau1phi = dau1.Phi() ;
    if( dau1phi < 0.0 ) dau1phi += twopi ;
    int dau1phiPointer = (int) (dau1phi/(twopi)*360) ;

    if( dau1pt < 2.0 ) dau1ptWeight = dau1pt ;
    else dau1ptWeight = 2.0 ;

    dau1phiWeight = TempFlowPhiWgt->GetBinContent(dau1phiPointer+1);

    dau1weight = dau1ptWeight*dau1phiWeight ;

    dau1Qx = (dau1weight)*cos(2*dau1phi) ;
    dau1Qy = (dau1weight)*sin(2*dau1phi) ;

    TVector2 Qdau1(dau1Qx,dau1Qy) ;


    ((TH2F *)getPlotByIndex(35)->getHisto(0)->histo)->Fill(dau1p , dau1dedx ) ;


    //////////////// ---daughter 2--- /////////////////

    float dau2q     =  hlt_pt->primaryTrack[dau2TrackSN].q  ;
    float dau2pt    =  hlt_pt->primaryTrack[dau2TrackSN].pt ;
    float dau2px    =  hlt_pt->primaryTrack[dau2TrackSN].pt*cos(hlt_pt->primaryTrack[dau2TrackSN].psi) ;
    float dau2py    =  hlt_pt->primaryTrack[dau2TrackSN].pt*sin(hlt_pt->primaryTrack[dau2TrackSN].psi) ;
    float dau2pz    =  hlt_pt->primaryTrack[dau2TrackSN].pt*hlt_pt->primaryTrack[dau2TrackSN].tanl ;
    float dau2nHits =  hlt_pt->primaryTrack[dau2TrackSN].nHits ;
    float dau2dedx  =  hlt_pt->primaryTrack[dau2TrackSN].dedx ; 
    float dau2Qx;
    float dau2Qy;

    double dau2weight = 0. ;
    double dau2ptWeight = 0. ;
    double dau2phiWeight = 0. ;

    TVector3 dau2( dau2px, dau2py, dau2pz ) ;

    float dau2p = dau2.Mag() ;

    float dau2eta = dau2.PseudoRapidity();
    if( fabs( dau2eta ) > 1. ) continue ;

    float dau2phi = dau2.Phi() ;
    if( dau2phi < 0.0 ) dau2phi += twopi ;
    int dau2phiPointer = (int)(dau2phi/(twopi)*360) ;

    if( dau2pt < 2.0 ) dau2ptWeight = dau2pt ;
    else dau2ptWeight = 2.0 ;

    dau2phiWeight = TempFlowPhiWgt->GetBinContent( dau2phiPointer + 1 ) ;
    dau2weight = dau2ptWeight*dau2phiWeight ;

    dau2Qx = (dau2weight)*cos(2*dau2phi) ;
    dau2Qy = (dau2weight)*sin(2*dau2phi) ;

    TVector2 Qdau2( dau2Qx, dau2Qy ) ;


    ((TH2F *)getPlotByIndex(36)->getHisto(0)->histo)->Fill(dau2p , dau2dedx ) ;


    //////////////////  ----jpsi ----  ///////////////////

    float pt = hlt_diep->ePair[i].pt ;
    float px = cos(hlt_diep->ePair[i].psi)*hlt_diep->ePair[i].pt ;
    float py = sin(hlt_diep->ePair[i].psi)*hlt_diep->ePair[i].pt ;
    float pz = hlt_diep->ePair[i].tanl*hlt_diep->ePair[i].pt ;
    float m = hlt_diep->ePair[i].invariantMass ;

    ((TH1D *)getPlotByIndex(32)->getHisto(0)->histo)->Fill(m);

    ((TH1D *)getPlotByIndex(33)->getHisto(0)->histo)->Fill(m);

    ((TH1D *)getPlotByIndex(34)->getHisto(0)->histo)->Fill(m);

    TLorentzVector jpsi(0,0,0,0) ;

    jpsi.SetXYZM(px,py,pz,m) ;

    float rapidity = jpsi.Rapidity() ;

    ((TH1D *)getPlotByIndex(37)->getHisto(0)->histo)->Fill(rapidity);

    if(rapidity < -1. || rapidity > 1.) continue ;   /// ask aihong and na !

    if( dau1nHits >= 15  &&  dau1pt >= 0.1  &&  dau1pt < 2.0 &&  fabs( dau1eta ) < 1.0 ) Qall -= Qdau1 ;
    if( dau2nHits >= 15  &&  dau2pt >= 0.1  &&  dau2pt < 2.0 &&  fabs( dau2eta ) < 1.0 ) Qall -= Qdau2 ;

    float psi = Qall.Phi() / 2.0 ;
    if( psi < 0. ) psi += twopi / 2.0 ;

    // float phiAngle = jpsi.Phi() ;
    float phiAngle = hlt_diep->ePair[i].psi ;
    if( phiAngle < 0. ) phiAngle = phiAngle + twopi ;

    phiAngle -= psi ; 

    if( phiAngle < 0.0 ) phiAngle += twopi ;
    if( phiAngle > 0.5*twopi ) phiAngle = twopi - phiAngle ;

    if( dau1q + dau2q == 0.){

      InvMassv2->Fill( pt, cos(2*phiAngle), m ) ;
      InvMassv2_Sin->Fill( pt, sin(2*phiAngle), m ) ;

    }
    else{

      DenInvMass->Fill( pt, cos(2*phiAngle), m ) ;

    }

    float psi_ran1 = Qran1->Phi() / 2.0 ;
    if( psi_ran1 < 0. ) psi_ran1 += twopi / 2.0 ;
    float psi_ran2 = Qran2->Phi() / 2.0 ;
    if( psi_ran2 < 0. ) psi_ran2 += twopi / 2.0 ;

    ((TProfile *)getPlotByIndex(65)->getHisto(0)->histo)->Fill(0., cos(2.0 *(psi_ran1-psi_ran2)) ) ;
  }

} ;


////////////////////////////////////////////////////////////////////////

int HltphiBuilder::selectEvent(daqReader *rdr) {
  return 1;
};


int HltphiBuilder::selectRun(daqReader *rdr) {
  return 1;
}

void HltphiBuilder::main(int argc, char *argv[]) 
{
        
  HltphiBuilder hltdis;
  hltdis.Main(argc, argv);

}

//---------calculate resolution
double HltphiBuilder::chi(double res) {
  // Calculates chi from the event plane resolution
  double chi   = 2.0;
  double delta = 1.0;
  for (int i = 0; i < 15; i++) {
    chi   = (resEventPlane(chi) < res) ? chi + delta : chi - delta;
    delta = delta / 2.;
  }
  return chi;
}

double HltphiBuilder::resEventPlane(double chi) {
  // Calculates the event plane resolution as a function of chi
  double con = 0.626657;                   // sqrt(pi/2)/2
  double arg = chi * chi / 4.;
  double res = con * chi * exp(-arg) * (TMath::BesselI0(arg) +
					TMath::BesselI1(arg));
  return res;
}


//----------create a list
void HltphiBuilder::WriteList(char *outFile)
{
  char filename[256];
  sprintf(filename,"%s",outFile);

  ofstream outStream;
  outStream.open("Current.list");
  outStream<<filename<<endl;
  outStream.close();
}


//-----------write current histograms
void HltphiBuilder::WriteHistogram(char *outFile)
{       
  TString str("normal");
  TObjString *obj = new TObjString(str);
	
  char filehist[256];
  sprintf(filehist,"%s",outFile);
  TFile f(filehist,"RECREATE");
  obj->Write();

  for(int i=0;i<87;i++){
    getPlotByIndex(i)->getHisto(0)->histo->Write();
  }
  getPlotByIndex(72)->getHisto(1)->histo->Write();
  getPlotByIndex(73)->getHisto(1)->histo->Write();
  getPlotByIndex(74)->getHisto(1)->histo->Write();
  getPlotByIndex(75)->getHisto(1)->histo->Write();
  getPlotByIndex(76)->getHisto(1)->histo->Write();
  getPlotByIndex(77)->getHisto(1)->histo->Write();
  getPlotByIndex(78)->getHisto(1)->histo->Write();
  getPlotByIndex(79)->getHisto(1)->histo->Write();
  getPlotByIndex(80)->getHisto(1)->histo->Write();
  getPlotByIndex(81)->getHisto(1)->histo->Write();
  getPlotByIndex(82)->getHisto(1)->histo->Write();
  getPlotByIndex(83)->getHisto(1)->histo->Write();
	
  InvMassv2->Write();
  InvMassv2_Sin->Write();
  DenInvMass->Write();

  f.Close();
}

//------------calculate phiwgt
void HltphiBuilder::TempPhiWgtCal()
{
  TempFlowPhiWgt=new TH1D("hltphi_TempFlowPhiWgt","hltphi_TempFlowPhiWgt",360,0,twopi);
  TH1D *tempDis = new TH1D("hltphi_tempDis","hltphi_tempDis",360,0,twopi); tempDis->Sumw2();
  TH1D *tempWgt = new TH1D("hltphi_tempWgt","hltphi_tempWgt",360,0,twopi); tempWgt->Sumw2();
  double mean;

  TH1D *p = (TH1D *)flowphi->First();
  if(!p->GetEntries()){
    printf("First Run! PhiWeight!\n");
    flowphi->RemoveAt(0);   // remove the first "Null" object
    for(int i=1;i<=360;i++){
      TempFlowPhiWgt->SetBinContent(i,1.);
    }
  }
  else{
    int entries = flowphi->GetEntries();
    for(int m=0;m<entries;m++){
      TempFlowPhi->Add((TH1D *)flowphi->At(entries-1 - m));
      if(TempFlowPhi->GetEntries() > 1.0e+07) break;
    }

    for(int i=1; i<=360; i++){
      tempDis->SetBinContent(i, TempFlowPhi->GetBinContent(i));
    }
    mean = tempDis->Integral() / 360.;
    for(int i=1; i<=360; i++){
      tempWgt->SetBinContent(i, mean);
      tempWgt->SetBinError(i, 0.);
    }
    tempWgt->Divide(tempDis);
    for(int i=1; i<=360; i++){
      TempFlowPhiWgt->SetBinContent(i,tempWgt->GetBinContent(i));
      TempFlowPhiWgt->SetBinError(i,tempWgt->GetBinError(i));
    }
  }
  delete tempDis;
  delete tempWgt;
}

//------------raw v2 fit function
static double ratio[70];

// Double_t HltDisplayHist::
static Double_t fitfun_raw(Double_t *x, Double_t *para)
{
  /*
    Int_t bin=(x[0]-2.0)/0.02;
    Double_t fitval=para[0]*ratio[bin]+(para[1]+para[2]*x[0])*(1-ratio[bin]);
    return fitval;
  */
  Int_t bin=(x[0]-0.99)/0.001;
  Double_t fitval=para[0]*ratio[bin]+(para[1]+para[2]*x[0])*(1-ratio[bin]);
  return fitval;
}


//-------------fit jpsi flow

void HltphiBuilder::Jpsiflow()
{
  //-------------TProfile raw flow------------//

  gStyle->SetOptDate(0);
  gStyle->SetLabelSize(0.05,"x");
  gStyle->SetLabelSize(0.05,"y");
  gStyle->SetTitleW(0.60);
  gStyle->SetTitleH(0.06);
  gStyle->SetOptStat(000000);
  gStyle->SetOptFit(111);
  gStyle->SetOptTitle(0);

  double temp_mass;
  double temp_cos;
  double weight;

  /*	for(int i=1;i<=100;i++){
	
  for(int k=0;k<=100;k++){
  temp_mass=k*0.02+1.99;
  temp_cos=i*0.02+(-1.01);
  */
  for(int i=1;i<=200;i++){
	             
    for(int k=10;k<=120;k++){
      temp_mass=k*0.001+0.9795;
      temp_cos=i*0.01+(-1.005);

      for(int j=1;j<=10;j++){ //0.0GeV---1.0GeV
	
	weight = InvMassv2->GetBinContent(j,i,k);
	((TProfile *)getPlotByIndex(66)->getHisto(0)->histo)->Fill(temp_mass,temp_cos,weight);

      }
      for(int j=11;j<=20;j++){ //1.0GeV---2.0GeV
			
	weight = InvMassv2->GetBinContent(j,i,k);
	((TProfile *)getPlotByIndex(67)->getHisto(0)->histo)->Fill(temp_mass,temp_cos,weight);

      }
      for(int j=21;j<=30;j++){ //2.0GeV---3.0GeV

	weight = InvMassv2->GetBinContent(j,i,k);
	((TProfile *)getPlotByIndex(68)->getHisto(0)->histo)->Fill(temp_mass,temp_cos,weight);

      }
      for(int j=31;j<=40;j++){ //3.0GeV---4.0GeV

	weight = InvMassv2->GetBinContent(j,i,k);
	((TProfile *)getPlotByIndex(69)->getHisto(0)->histo)->Fill(temp_mass,temp_cos,weight);

      }
      for(int j=41;j<=50;j++){ //4.0GeV---5.0GeV

	weight = InvMassv2->GetBinContent(j,i,k);
	((TProfile *)getPlotByIndex(70)->getHisto(0)->histo)->Fill(temp_mass,temp_cos,weight);

      }
      for(int j=51;j<=60;j++){ //5.0GeV---6.0GeV

	weight = InvMassv2->GetBinContent(j,i,k);
	((TProfile *)getPlotByIndex(71)->getHisto(0)->histo)->Fill(temp_mass,temp_cos,weight);

      }
    }
  }

	
  /////////////// --jpsi mass peak-- ---S/(S+B)--  ////////////////
  /*
    double fitlow = 2.;
    double fithi  = 4.;
    double ptlow ;
    double pthi ;
    int philow ; 
    int phihi ;
    double rebin = 1.0 ;

  */

  double  ptlow;
  double pthi;
  double  philow;
  double phihi;
  double fitlow = 0.99;
  double fithi = 1.06;
  double scalenorm=1.00;
  double lownorm = 1.04;
  double hinorm =  1.06;
  double rebin = 1.0;
  double lowplot = .98;
  double hiplot = 1.1;


  double temp_low[6]  = { 1,  11, 21, 31, 41, 51 } ;
  double temp_high[6] = { 10, 20, 30, 40, 50, 60 } ;

  for(int i=0; i<6; i++) {

    ptlow=temp_low[i] ;
    pthi=temp_high[i] ;

    /*	philow = 1 ;
      phihi  = 100 ;
      fitlow  = 2.;
      fithi   = 4.;
      rebin  = 1.0;
    */

    philow=1;
    phihi=200;
    fitlow = 0.99;
    fithi = 1.06;
    scalenorm=1.00;
    lownorm = 1.04;
    hinorm =  1.06;
    rebin = 1.0;
    lowplot = .98;
    hiplot = 1.1;

    char title[256] ;

    TH1D *hq = ((TH1D *)InvMassv2->ProjectionZ("sig",ptlow,pthi,philow,phihi));
    TH1D *hbq= ((TH1D *)DenInvMass->ProjectionZ("bkg",ptlow,pthi,philow,phihi));

    //--------------------------//  run7 phi test
    lownorm = hq->GetXaxis()->FindBin(lownorm);
    hinorm = hq->GetXaxis()->FindBin(hinorm);
    double qintegral = hq->Integral(lownorm, hinorm);
    double bqintegral = hbq->Integral(lownorm,hinorm);
    if(qintegral <= 0 || bqintegral <=0) continue;
    hbq->Scale(qintegral*scalenorm/bqintegral);
    //--------------------------// run7 phi test


    ((TH1D *) getPlotByIndex(72 + i)->getHisto(0)->histo)->Add(hq) ;
    ((TH1D *) getPlotByIndex(72 + i)->getHisto(1)->histo)->Add(hbq) ;

    getPlotByIndex(72 + i)->getHisto(0)->histo->SetXTitle("M_{inv}(ee) (GeV/c^{2})");
    getPlotByIndex(72 + i)->getHisto(0)->histo->SetXTitle("M_{inv}ee GeV/c^{2}");
		
    sprintf(title,"hltphi_cen100_%3.1fGeV<pt<%3.1fGeV",(ptlow-1)*0.1,pthi*0.1);
    getPlotByIndex(72 + i)->getHisto(0)->histo->SetName(title);
    getPlotByIndex(72 + i)->getHisto(0)->histo->SetTitle(title);
    //		getPlotByIndex(72 + i)->getHisto(0)->histo->SetMarkerStyle(20);
    getPlotByIndex(72 + i)->getHisto(0)->histo->SetMarkerColor(4);	

    //		int xbin=100;
    //		double total[100];
                
    int xbin = 70 ;
    double total[70] ;

    hq->Rebin(rebin);
    hbq->Rebin(rebin);

    for(int g=1;g<=xbin;g++)
      {
	//	total[g-1]=hq->GetBinContent(g);
	total[g-1]=hq->GetBinContent(g+10);
      }


    getPlotByIndex(78 + i)->getHisto(0)->histo->Add(hq, hbq, 1., -1.);

    getPlotByIndex(78 + i)->getHisto(0)->histo->SetName(title);
    getPlotByIndex(78 + i)->getHisto(0)->histo->SetTitle(title);
    getPlotByIndex(78 + i)->getHisto(0)->histo->GetXaxis()->SetRange(0,120);

    //		TF1 *fit = new TF1("fit","[0]*exp(-(x-[1])*(x-[1]) / (2.*[2]*[2])) + [3]+[4]*x",fitlow, fithi);
    //		TF1 *fit_1 =new TF1("fit_1","[0]*exp(-(x-[1])*(x-[1]) / (2.*[2]*[2]))",fitlow,fithi);
    //		TF1 *fit_2 =new TF1("fit_2","[0]+[1]*x",fitlow,fithi);
                
    TF1 *fit = new TF1("fit2","2.0*0.001*[0]*[1]/(2*3.1415926)/(pow(x - [2],2) + [1]*[1]/4) + [3]+[4]*x",fitlow, fithi);
    TF1 *fit_1 =new TF1("fit2_1","2.0*0.001*[0]*[1]/(2*3.1415926)/(pow(x - [2],2) + [1]*[1]/4) ",fitlow,fithi);
    TF1 *fit_2 =new TF1("fit2_2","[0]+[1]*x",fitlow,fithi);

    /*		fit->SetParName(0,"Apti");
      fit->SetParName(1,"Mass");
      fit->SetParName(1,"FWHM")
      fit->SetParName(2,"Sigma");
      fit->SetParName(2,"Mass");
      fit->SetParName(3,"pol0");
      fit->SetParName(4,"pol1");

      fit->SetParameter(0, 1000);
      fit->SetParameter(1, 3.097);
      fit->SetParameter(2, 0.3);
      fit->SetParameter(3, 1.);
      fit->SetParameter(4, 1.);

      fit->SetParLimits(0, 1., 1000000000.);
      fit->SetParLimits(1, 3.06, 3.14 );
    */	//fit->SetParLimits(2, 0., 1. );
	        
    fit->SetParName(0,"Apt");
    fit->SetParName(1,"FWHM");
    fit->SetParName(2,"Mass");
    fit->SetParName(3,"pol0");
    fit->SetParName(4,"pol1");
	 
    fit->SetParameter(0, 1000);
    fit->SetParameter(1, 0.007);
    fit->SetParameter(2, 1.019);
    fit->SetParameter(3, 10.);
    fit->SetParameter(4, -4.);
		
    fit->SetParLimits(0, 10., 100000000.);
    fit->SetParLimits(2, 1.017, 1.021);
    //----------run7 phi test

    fit->SetLineStyle(2);
    fit->SetLineWidth(1);
    fit->SetLineColor(4);
    fit_2->SetLineStyle(2);
    fit_2->SetLineWidth(1);
    fit_2->SetLineColor(4);
    fit_1->SetLineColor(6);

    getPlotByIndex(78 + i)->getHisto(0)->histo->SetMarkerStyle(20);
    getPlotByIndex(78 + i)->getHisto(0)->histo->SetXTitle("M_{inv}(ee) (GeV/c^{2})");

    //		getPlotByIndex(78 + i)->getHisto(0)->histo->Fit("fit","ERMB","",fitlow,fithi);
    getPlotByIndex(78 + i)->getHisto(0)->histo->Fit(fit,"ERMB");

    Double_t par[5];
    par[0]=fit->GetParameter(0);
    par[1]=fit->GetParameter(1);
    par[2]=fit->GetParameter(2);
    par[3]=fit->GetParameter(3);
    par[4]=fit->GetParameter(4);

    fit_1->SetParameters(&par[0]);
    fit_2->SetParameters(&par[3]);

    double tem_x ;
    double tem_y ;

    for(int t = 0; t < 70; t++){
      tem_x = 0.9905 + 0.001*t ;
      tem_y = par[3] + par[4]*tem_x ;

      int bin = getPlotByIndex(78 + i)->getHisto(1)->histo->GetXaxis()->FindBin(tem_x);
      getPlotByIndex(78 + i)->getHisto(1)->histo->SetBinContent(bin , tem_y);
    }
		
    double signal;
    double xx;
    for(int t=0;t<xbin;t++)
      {
	/*
	  x = 2.01+0.02*t;
	  signal = getPlotByIndex(78 + i)->getHisto(0)->histo->GetBinContent(t+1) - (par[3]+par[4]*x);

	  if(fabs(total[t])<1.0e-6) ratio[t]=1.0e-6;
	  else ratio[t] = signal/total[t];
	*/
	xx=0.9905+0.001*t;
	signal = getPlotByIndex(78 + i)->getHisto(0)->histo->GetBinContent(t+11)- ( par[3]+ par[4]*xx);
	if(fabs(total[t])<1.0e-6)ratio[t]=1.0e-6;
	else  ratio[t] = signal/total[t];
      }
	
    ////-----fit v2-----////

    //  Double_t *xx ;
    //  Double_t *Para ;

    getPlotByIndex(66+i)->getHisto(0)->histo->Rebin(1);

    TF1 *fitfun = new TF1("fitfun", fitfun_raw, fitlow , fithi , 3);

    //		fitfun->SetNpx(50000);

    getPlotByIndex(66+i)->getHisto(0)->histo->SetMarkerStyle(20);
    getPlotByIndex(66+i)->getHisto(0)->histo->SetMarkerColor(4);

    fitfun->SetParName(0,"v_{2}^{obs}");
    fitfun->SetParName(1,"a0");
    fitfun->SetParName(2,"a1");

    fitfun->SetParameter(0,0.05);
    fitfun->SetParameter(1,200);
    fitfun->SetParameter(2,5);
    //		fitfun->SetLineStyle(1);
    //		fitfun->SetLineColor(4);

    getPlotByIndex(66+i)->getHisto(0)->histo->Fit("fitfun","ERMNB");
		

    TCanvas *can2 = new TCanvas("can2","can2",20,10,700,460);
    char file[256];
    sprintf(file,"/star/u/xueliang/lbl/xueliang/Display/figs/07AuAu_fit_p%i.gif",i);

    getPlotByIndex(66+i)->getHisto(0)->histo->Draw();

    fitfun->Draw("same");
    //fitfun->GetYaxis()->SetRangeUser(0,0.1);

  
#ifdef CODE_DIES
    can2->SaveAs(file);
#endif

    Double_t jpsipar[3];
    Double_t v2obs, v2obs_err;
    jpsipar[0] = fitfun->GetParameter(0);
    jpsipar[1] = fitfun->GetParameter(1);
    jpsipar[2] = fitfun->GetParameter(2);

    double xxx;

    for(int t = 0; t <xbin ;t++){
      xxx = 0.9905 + 0.001*t ;

      int bin = getPlotByIndex(66 + i)->getHisto(1)->histo->GetXaxis()->FindBin(xxx);
      double tempfun = ratio[t]*jpsipar[0] + (1-ratio[t])*(jpsipar[1] + jpsipar[2]*xxx) ;

      getPlotByIndex(66+i)->getHisto(1)->histo->Fill(xxx, tempfun);

      //	cout << "xxx =" <<xxx << " tempfun =" << tempfun <<endl;
    }


    v2obs = jpsipar[0];
    v2obs_err = fitfun->GetParError(0);

    double pt = (ptlow+pthi) / 20.;

    double v2 = v2obs/res2;
    double v2_err = fabs(v2)*sqrt((v2obs_err/v2obs)*(v2obs_err/v2obs) + (res2error/res2)*(res2error/res2));

    int ptbin = getPlotByIndex(84)->getHisto(0)->histo->FindBin(pt);

    ((TH1D *)getPlotByIndex(84)->getHisto(0)->histo)->SetBinContent(ptbin,v2);
    ((TH1D *)getPlotByIndex(84)->getHisto(0)->histo)->SetBinError(ptbin,v2_err);

    getPlotByIndex(84)->getHisto(0)->histo->SetLineStyle(1);
    getPlotByIndex(84)->getHisto(0)->histo->SetLineColor(4);
    getPlotByIndex(84)->getHisto(0)->histo->GetXaxis()->SetTitle("Pt");
    getPlotByIndex(84)->getHisto(0)->histo->GetYaxis()->SetTitle("J/#psi v_{2}");
  }
}
