#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "trgStructures.h"
#include "L2UpsilonResult.h"
#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "trgBuilder.h"
#include <RTS/include/rtsLog.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//

// JML 3/17/14
//     These are offsets in the display of the vertex
//     May need to change each year!
double h146_zdc_vertex_offset = 12.88 - .55; 

//******************************************************************************
// Values for the log scale y axes for various plots
// 0 means linear, 1 means log scale
//******************************************************************************
const int logYunattenuated = 1; // unattenuated plots log scale y axis
//msimko: the unattenuated plots are in log scale for pp collisions
// may need to be changed to 0 for A+A
const int logYSum = 1; // log scale for the ZDC hardware Sum plot
//msimko: changed to log for pp collisions might want to change back for A+A
const int logTimePlots = 1; // ZDC time plots
//msimko: changed to log because of the peak at 50 in ZDC East
//******************************************************************************

ClassImp(trgBuilder);
  

trgBuilder::trgBuilder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"trg";

  h76_zdc_time_east = NULL;
  h77_zdc_time_west = NULL;
  h78_zdc_timediff_east_west = NULL;
  h146_zdc_Vertex_cm = NULL;
  h480_zdc_unatt_eastsum = NULL;
  h481_zdc_unatt_westsum = NULL;

  h474_zdc_unatt_east1 = NULL;
  h475_zdc_unatt_west1 = NULL; 
  h476_zdc_unatt_east2 = NULL; 
  h477_zdc_unatt_west2 = NULL; 
  h478_zdc_unatt_east3 = NULL; 
  h479_zdc_unatt_west3 = NULL; 

  // Trigger / ZDC sums
  h482_zdc_sum_bbc = NULL; 
  h483_zdc_hardwaresum = NULL; 
  
  // Trigger / Bunch Crossing Counter
  h266_bbc_bunchid_y = NULL; 
  h266_bbc_bunchid_b = NULL; 

  h442_bunch_yellow_fill = NULL; 
  h443_bunch_yellow_up = NULL; 
  h444_bunch_yellow_down = NULL; 
  h445_bunch_yellow_unpol = NULL; 
  h446_bunch_blue_fill = NULL; 
  h447_bunch_blue_up = NULL; 
  h448_bunch_blue_down = NULL; 
  h449_bunch_blue_unpol = NULL; 

  h329_zdcsmd_w_v_N = NULL;
  h330_zdcsmd_w_h_N = NULL;
  h331_zdcsmd_e_v_N = NULL;
  h332_zdcsmd_e_h_N = NULL;
  h333_zdcsmd_w_v_A = NULL;
  h334_zdcsmd_w_h_A = NULL;
  h335_zdcsmd_e_v_A = NULL;
  h336_zdcsmd_e_h_A = NULL;

  // L2UpsilonCounts...
  hL2ups_Tag = NULL;
  hL2ups_Time = NULL;
  hL2ups_Event = NULL;
  hL2ups_NumberOfHotTowers = NULL;
  hL2ups_AbortRate = NULL;
  hL2ups_AbortRateCurrent = NULL;
  hL2ups_EnergyL0 = NULL;
  hL2ups_EnergyL2 = NULL;
  hL2ups_Mass = NULL;
  hL2ups_CosTheta = NULL;
  hL2ups_TriggerTowerIdL0 = NULL;
  hL2ups_TriggerTowerIdL2 = NULL;
  hL2ups_NumberOfTowersL0 = NULL;
  hL2ups_NumberOfTowersL2 = NULL;
  hL2ups_EtaPhiL0 = NULL;
  hL2ups_EtaPhiL2 = NULL;

}

trgBuilder::~trgBuilder() {
  if(h76_zdc_time_east) delete h76_zdc_time_east;
  if(h77_zdc_time_west) delete h77_zdc_time_west;
  if(h78_zdc_timediff_east_west) delete h78_zdc_timediff_east_west;
  if(h146_zdc_Vertex_cm) delete h146_zdc_Vertex_cm;
  if(h480_zdc_unatt_eastsum) delete h480_zdc_unatt_eastsum;
  if(h481_zdc_unatt_westsum) delete h481_zdc_unatt_westsum;
  
  if(h474_zdc_unatt_east1) delete h474_zdc_unatt_east1;
  if(h475_zdc_unatt_west1) delete h475_zdc_unatt_west1;
  if(h476_zdc_unatt_east2) delete h476_zdc_unatt_east2;
  if(h477_zdc_unatt_west2) delete h477_zdc_unatt_west2;
  if(h478_zdc_unatt_east3) delete h478_zdc_unatt_east3;
  if(h479_zdc_unatt_west3) delete h479_zdc_unatt_west3;

  // Trigger / ZDC sums
  if(h482_zdc_sum_bbc) delete h479_zdc_unatt_west3;
  if(h483_zdc_hardwaresum) delete h483_zdc_hardwaresum;
  
  // Trigger / Bunch Crossing Counter
  if(h266_bbc_bunchid_y) delete h266_bbc_bunchid_y;
  if(h266_bbc_bunchid_b) delete h266_bbc_bunchid_b;

  if(h442_bunch_yellow_fill) delete h442_bunch_yellow_fill;
  if(h443_bunch_yellow_up) delete h443_bunch_yellow_up;
  if(h444_bunch_yellow_down) delete h444_bunch_yellow_down;
  if(h445_bunch_yellow_unpol) delete h445_bunch_yellow_unpol;
  if(h446_bunch_blue_fill) delete h446_bunch_blue_fill;
  if(h447_bunch_blue_up) delete h447_bunch_blue_up;
  if(h448_bunch_blue_down) delete h448_bunch_blue_down;
  if(h449_bunch_blue_unpol) delete h449_bunch_blue_unpol;

  if(h329_zdcsmd_w_v_N) delete h329_zdcsmd_w_v_N;
  if(h330_zdcsmd_w_h_N) delete h330_zdcsmd_w_h_N;
  if(h331_zdcsmd_e_v_N) delete h331_zdcsmd_e_v_N;
  if(h332_zdcsmd_e_h_N) delete h332_zdcsmd_e_h_N;
  if(h333_zdcsmd_w_v_A) delete h333_zdcsmd_w_v_A;
  if(h334_zdcsmd_w_h_A) delete h334_zdcsmd_w_h_A;
  if(h335_zdcsmd_e_v_A) delete h335_zdcsmd_e_v_A;
  if(h336_zdcsmd_e_h_A) delete h336_zdcsmd_e_h_A;

  // L2UpsilonCounts...
  if(hL2ups_Tag) delete hL2ups_Tag;
  if(hL2ups_Time) delete hL2ups_Time;
  if(hL2ups_Event) delete hL2ups_Event;
  if(hL2ups_NumberOfHotTowers) delete hL2ups_NumberOfHotTowers;
  if(hL2ups_AbortRate) delete hL2ups_AbortRate;
  if(hL2ups_AbortRateCurrent) delete hL2ups_AbortRateCurrent;
  if(hL2ups_EnergyL0) delete hL2ups_EnergyL0;
  if(hL2ups_EnergyL2) delete hL2ups_EnergyL2;
  if(hL2ups_Mass) delete hL2ups_Mass;
  if(hL2ups_CosTheta) delete hL2ups_CosTheta;
  if(hL2ups_TriggerTowerIdL0) delete hL2ups_TriggerTowerIdL0;
  if(hL2ups_TriggerTowerIdL2) delete hL2ups_TriggerTowerIdL2;
  if(hL2ups_NumberOfTowersL0) delete hL2ups_NumberOfTowersL0;
  if(hL2ups_NumberOfTowersL2) delete hL2ups_NumberOfTowersL2;
  if(hL2ups_EtaPhiL0) delete hL2ups_EtaPhiL0;
  if(hL2ups_EtaPhiL2) delete hL2ups_EtaPhiL2;
}

void trgBuilder::initialize(int argc, char *argv[]) {

  // Build Root Histograms...
  // Trigger / ZDC
  h76_zdc_time_east = new TH1D("h76_zdc_time_east", "ZDC Time East",200,0,1500); 
  //msimko: Upper limit was changed from 1000 to 1500 for pp (but 1000 makes more sense for A+A) collisions
  h77_zdc_time_west = new TH1D("h77_zdc_time_west", "ZDC Time West",200,0,1500); 
  //msimko: Upper limit was changed from 1000 to 1500 for pp (but 1000 makes more sense for A+A) collisions
  h78_zdc_timediff_east_west = new TH1D("h78_zdc_timediff_east_west","ZDC Time (West - East)",200,-2000,2000);
  h146_zdc_Vertex_cm = new TH1D("h146_zdc_Vertex_cm","Vertex Position from ZDC (cm)",50, -100, 100);
  //msimko: Upper limit changed to 300 for pp collisions (changed from 1000 for A+A)
  h480_zdc_unatt_eastsum = new TH1D("h480_zdc_unatt_eastsum","ZDC Unattenuated East Sum",200,0,300);
  h481_zdc_unatt_westsum = new TH1D("h481_zdc_unatt_westsum","ZDC Unattenuated West Sum",200,0,300);
  
  // Trigger / ZDC_seg
  //msimko: Upper limit changed to 300 for pp collisions (changed from 1000 for A+A)
  h474_zdc_unatt_east1 = new TH1D("h474_zdc_unatt_east1","ZDC Unattenuated East1",200,0,300);
  h475_zdc_unatt_west1 = new TH1D("h475_zdc_unatt_west1","ZDC Unattenuated West1",200,0,300);
  h476_zdc_unatt_east2 = new TH1D("h476_zdc_unatt_east2","ZDC Unattenuated East2",200,0,300);
  h477_zdc_unatt_west2 = new TH1D("h477_zdc_unatt_west2","ZDC Unattenuated West2",200,0,300);
  h478_zdc_unatt_east3 = new TH1D("h478_zdc_unatt_east3","ZDC Unattenuated East3",200,0,300);
  h479_zdc_unatt_west3 = new TH1D("h479_zdc_unatt_west3","ZDC Unattenuated West3",200,0,300);

  // Trigger / ZDC sums
  //msimko: Upper limit changed to 300 for pp collisions (changed from 3000 for A+A)
  h482_zdc_sum_bbc = new TH2D("h482_zdc_sum_bbc","ZDC Sum vs. BBC Sum",100,0,60000,100,0,300);
  h483_zdc_hardwaresum = new TH1D("h483_zdc_hardwaresum","ZDC Hardware Sum Central",300,0,300);
  
  // Trigger / Bunch Crossing Counter
  h266_bbc_bunchid_y = new TH1D("h266_bbc_bunchid_y","Bunch Crossing Counter (Yellow)",120,-0.5,119.5);
  h266_bbc_bunchid_b = new TH1D("h266_bbc_bunchid_b","Bunch Crossing Counter (Blue)",120,-0.5,119.5);

  h442_bunch_yellow_fill = new TH1D("h442_bunch_yellow_fill","h442_bunch_yellow_fill",120,-0.5,119.5);
  h443_bunch_yellow_up = new TH1D("h443_bunch_yellow_up","h443_bunch_yellow_up",120,-0.5,119.5);
  h444_bunch_yellow_down = new TH1D("h444_bunch_yellow_down","h444_bunch_yellow_down",120,-0.5,119.5);
  h445_bunch_yellow_unpol = new TH1D("h445_bunch_yellow_unpol","h445_bunch_yellow_unpol",120,-0.5,119.5);
  h446_bunch_blue_fill = new TH1D("h446_bunch_blue_fill","h446_bunch_blue_fill",120,-0.5,119.5);
  h447_bunch_blue_up = new TH1D("h447_bunch_blue_up","h447_bunch_blue_up",120,-0.5,119.5);
  h448_bunch_blue_down = new TH1D("h448_bunch_blue_down","h448_bunch_blue_down",120,-0.5,119.5);
  h449_bunch_blue_unpol = new TH1D("h449_bunch_blue_unpol","h449_bunch_blue_unpol",120,-0.5,119.5);

  // zdcsmd
  h329_zdcsmd_w_v_N = new TH1D("h329_zdcsmd_w_v_N","ZDC_SMD_west_ver_N",8,0.5,8.5);
  h330_zdcsmd_w_h_N = new TH1D("h330_zdcsmd_w_h_N","ZDC_SMD_west_hor_N",8,0.5,8.5);
  h331_zdcsmd_e_v_N = new TH1D("h331_zdcsmd_e_v_N","ZDC_SMD_east_ver_N",8,0.5,8.5);
  h332_zdcsmd_e_h_N = new TH1D("h332_zdcsmd_e_h_N","ZDC_SMD_east_hor_N",8,0.5,8.5);
  h333_zdcsmd_w_v_A = new TH1D("h333_zdcsmd_w_v_A","ZDC_SMD_west_ver_A",8,0.5,8.5);
  h334_zdcsmd_w_h_A = new TH1D("h334_zdcsmd_w_h_A","ZDC_SMD_west_hor_A",8,0.5,8.5);
  h335_zdcsmd_e_v_A = new TH1D("h335_zdcsmd_e_v_A","ZDC_SMD_east_ver_A",8,0.5,8.5);
  h336_zdcsmd_e_h_A = new TH1D("h336_zdcsmd_e_h_A","ZDC_SMD_east_hor_A",8,0.5,8.5);

  // L2UpsilonCounts...
  hL2ups_Tag = new TH1D("hL2ups_Tag","Tag",5,-0.5,4.5);
  hL2ups_Time = new TH1D("hL2ups_Time","Time",250,0.5,1000);
  hL2ups_Event = new TH1D("hL2ups_Event","Events seen/accepted",2,-0.5,1.5);
  hL2ups_NumberOfHotTowers = new TH1D("hL2ups_NumberOfHotTowers","number of hot towers",5000,4,5004);
  hL2ups_AbortRate = new TH1D("hL2ups_AbortRate","abord rate overall",5000,4,5004);
  hL2ups_AbortRateCurrent = new TH1D("hL2ups_AbortRateCurrent","abord rate (25 evts)",5000,4,5004);
  hL2ups_EnergyL0 = new TH1D("hL2ups_EnergyL0","energy L0",100,0.,25.);
  hL2ups_EnergyL2 = new TH1D("hL2ups_EnergyL2","energy L2",100,0.,25.);
  hL2ups_Mass = new TH1D("hL2ups_Mass","inv. mass",100,0.,20.);
  hL2ups_CosTheta = new TH1D("hL2ups_CosTheta","cos(theta)",100,-1.,1.);
  hL2ups_TriggerTowerIdL0 = new TH1D("hL2ups_TriggerTowerIdL0","trigger tower id L0",4800,0.,4800.);
  hL2ups_TriggerTowerIdL2 = new TH1D("hL2ups_TriggerTowerIdL2","trigger tower id L2",4800,0.,4800.);
  hL2ups_NumberOfTowersL0 = new TH1D("hL2ups_NumberOfTowersL0","number of towers L0",25,0.,25);
  hL2ups_NumberOfTowersL2 = new TH1D("hL2ups_NumberOfTowersL2","number of towers L2",100,0.,100);
  hL2ups_EtaPhiL0 = new TH2D("hL2ups_EtaPhiL0","phi vs eta L0",40,-1.3,1.3, 120,-3.14145927,+3.14145927);
  hL2ups_EtaPhiL2 = new TH2D("hL2ups_EtaPhiL2","phi vs eta L2",40,-1.3,1.3, 120,-3.14145927,+3.14145927);

  // Add root histograms to Plots
  JevpPlot *plots[100];
  int n=0;
  plots[n] = new JevpPlot(h76_zdc_time_east);
  plots[n]->logy = logTimePlots;
  plots[++n] = new JevpPlot(h77_zdc_time_west);
  plots[n]->logy = logTimePlots;
  plots[++n] = new JevpPlot(h78_zdc_timediff_east_west);
  plots[++n] = new JevpPlot(h146_zdc_Vertex_cm);
  plots[++n] = new JevpPlot(h480_zdc_unatt_eastsum);
  plots[n]->logy = logYunattenuated;
  plots[++n] = new JevpPlot(h481_zdc_unatt_westsum);
  plots[n]->logy = logYunattenuated;
  
  plots[++n] = new JevpPlot(h474_zdc_unatt_east1);
  plots[n]->logy = logYunattenuated;
  plots[++n] = new JevpPlot(h475_zdc_unatt_west1);
  plots[n]->logy = logYunattenuated;
  plots[++n] = new JevpPlot(h476_zdc_unatt_east2);
  plots[n]->logy = logYunattenuated;
  plots[++n] = new JevpPlot(h477_zdc_unatt_west2);
  plots[n]->logy = logYunattenuated;
  plots[++n] = new JevpPlot(h478_zdc_unatt_east3);
  plots[n]->logy = logYunattenuated;
  plots[++n] = new JevpPlot(h479_zdc_unatt_west3);
  plots[n]->logy = logYunattenuated;

  plots[++n] = new JevpPlot(h482_zdc_sum_bbc);
  plots[n]->setDrawOpts("colz");
  plots[++n] = new JevpPlot(h483_zdc_hardwaresum);
  plots[n]->logy = logYSum;

  plots[++n] = new JevpPlot();
  PlotHisto *ph = new PlotHisto(h266_bbc_bunchid_y);
  ph->setLegText("Y Events");
  ph->setLegArgs("F");
  ph->histo->SetFillColor(1); ph->histo->SetLineColor(1);
  plots[n]->addHisto(ph);  
  ph = new PlotHisto(h442_bunch_yellow_fill);
  ph->setLegText("Y Filled");
  ph->setLegArgs("F");
  ph->histo->SetFillColor(2); ph->histo->SetLineColor(2);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(h443_bunch_yellow_up);
  ph->setLegText("Y Up");
  ph->setLegArgs("F");
  ph->histo->SetFillColor(3); ph->histo->SetLineColor(3);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(h444_bunch_yellow_down);
  ph->setLegText("Y Down");
  ph->setLegArgs("F");
  ph->histo->SetFillColor(4); ph->histo->SetLineColor(4);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(h445_bunch_yellow_unpol);
  ph->setLegText("Y Unpol");
  ph->setLegArgs("F");
  ph->histo->SetFillColor(5); ph->histo->SetLineColor(5);
  plots[n]->addHisto(ph);
  plots[n]->setLegend(.7,.6,.95,.95);
  plots[n]->setOptStat(0);

  plots[++n] = new JevpPlot();
  ph = new PlotHisto(h266_bbc_bunchid_b);
  ph->setLegText("B Events");
  ph->setLegArgs("F");
  ph->histo->SetFillColor(1); ph->histo->SetLineColor(1);
  plots[n]->addHisto(ph);  
  ph = new PlotHisto(h446_bunch_blue_fill);
  ph->setLegText("B Filled");
  ph->setLegArgs("F");
  ph->histo->SetFillColor(2); ph->histo->SetLineColor(2);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(h447_bunch_blue_up);
  ph->setLegText("B Up");
  ph->setLegArgs("F");
  ph->histo->SetFillColor(3); ph->histo->SetLineColor(3);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(h448_bunch_blue_down);
  ph->setLegText("B Down");
  ph->setLegArgs("F");
  ph->histo->SetFillColor(4); ph->histo->SetLineColor(4);
  plots[n]->addHisto(ph);
  ph = new PlotHisto(h449_bunch_blue_unpol);
  ph->setLegText("B Unpol");
  ph->setLegArgs("F");
  ph->histo->SetFillColor(5); ph->histo->SetLineColor(5);
  plots[n]->addHisto(ph);
  plots[n]->setLegend(.7,.6,.95,.95);
  plots[n]->setOptStat(0);

  
  plots[++n] = new JevpPlot(h329_zdcsmd_w_v_N);
  plots[++n] = new JevpPlot(h330_zdcsmd_w_h_N);
  plots[++n] = new JevpPlot(h331_zdcsmd_e_v_N);
  plots[++n] = new JevpPlot(h332_zdcsmd_e_h_N);
  plots[++n] = new JevpPlot(h333_zdcsmd_w_v_A);
  plots[++n] = new JevpPlot(h334_zdcsmd_w_h_A);
  plots[++n] = new JevpPlot(h335_zdcsmd_e_v_A);
  plots[++n] = new JevpPlot(h336_zdcsmd_e_h_A);

  // L2UpsilonCounts...
  plots[++n] = new JevpPlot(hL2ups_Tag);
  plots[++n] = new JevpPlot(hL2ups_Time);
  plots[++n] = new JevpPlot(hL2ups_Event);
  plots[++n] = new JevpPlot(hL2ups_NumberOfHotTowers);
  plots[++n] = new JevpPlot(hL2ups_AbortRate);
  plots[++n] = new JevpPlot(hL2ups_AbortRateCurrent);
  plots[++n] = new JevpPlot(hL2ups_EnergyL0);
  plots[++n] = new JevpPlot(hL2ups_EnergyL2);
  plots[++n] = new JevpPlot(hL2ups_Mass);
  plots[++n] = new JevpPlot(hL2ups_CosTheta);
  plots[++n] = new JevpPlot(hL2ups_TriggerTowerIdL0);
  plots[++n] = new JevpPlot(hL2ups_TriggerTowerIdL2);
  plots[++n] = new JevpPlot(hL2ups_NumberOfTowersL0);
  plots[++n] = new JevpPlot(hL2ups_NumberOfTowersL2);
  plots[++n] = new JevpPlot(hL2ups_EtaPhiL0);
  plots[++n] = new JevpPlot(hL2ups_EtaPhiL2);

  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    LOG(NOTE, "Adding plot %d",i);
    addPlot(plots[i]);
  }
}
  
void trgBuilder::startrun(daqReader *rdr) {
  LOG(DBG, "TriggerPlotBuilder starting run #%d",rdr->run);
  resetAllPlots();
  first_event = 0;
}

void trgBuilder::event(daqReader *rdr)
{
#include "zdc_smd.h"

    if(first_event == 0) {
	addServerTags("|trg|");
	first_event = 1;
    }

  StTriggerData *trgd = getStTriggerData(rdr);
  if(!trgd) return;

  // ZDC
  double mZdcTimeDiff = -9999;
  double mZdcVertex   = -9999;
  int te = trgd->zdcPmtTDC(east,1);
  int tw = trgd->zdcPmtTDC(west,1);
  LOG(DBG,"te = %d tw = %d",te,tw);
  if(te>20 && te<4000) h76_zdc_time_east->Fill(float(te));    
  if(tw>20 && tw<4000) h77_zdc_time_west->Fill(float(tw));    
  if(te>20 && te<4000 && tw>20 && tw<4000){
    mZdcTimeDiff = float(tw-te);
    //mZdcVertex   = mZdcTimeDiff/2*40.0/0.03;
    mZdcVertex   = (mZdcTimeDiff/2)*0.02*30;
    h78_zdc_timediff_east_west->Fill(mZdcTimeDiff);          

    
    h146_zdc_Vertex_cm->Fill(mZdcVertex + h146_zdc_vertex_offset);
  } 
  h480_zdc_unatt_eastsum->Fill(float(trgd->zdcUnAttenuated(east)));
  h481_zdc_unatt_westsum->Fill(float(trgd->zdcUnAttenuated(west)));
  
  h474_zdc_unatt_east1->Fill(float(trgd->zdcADC(east,1)));
  h475_zdc_unatt_west1->Fill(float(trgd->zdcADC(west,1)));
  h476_zdc_unatt_east2->Fill(float(trgd->zdcADC(east,2)));
  h477_zdc_unatt_west2->Fill(float(trgd->zdcADC(west,2)));
  h478_zdc_unatt_east3->Fill(float(trgd->zdcADC(east,3)));
  h479_zdc_unatt_west3->Fill(float(trgd->zdcADC(west,3)));


  // Hardware Sum no longer exists. It is the sum of the attenuated signals...
  // h482_zdc_sum_bbc->Fill(float(trgd->bbcADCSum(east))+float(trgd->bbcADCSum(west)), float(trgd->zdcHardwareSum()));
  // h483_zdc_hardwaresum->Fill(float(trgd->zdcHardwareSum()));

  float zdcHardwareSum = float(trgd->zdcAttenuated(east)) + float(trgd->zdcAttenuated(west));
  h482_zdc_sum_bbc->Fill(float(trgd->bbcADCSum(east))+float(trgd->bbcADCSum(west)), zdcHardwareSum);
  h483_zdc_hardwaresum->Fill(float(zdcHardwareSum));

  unsigned int bunch7bit = trgd->bunchId7Bit();
  h266_bbc_bunchid_y->Fill(bunch7bit);
  h266_bbc_bunchid_b->Fill(bunch7bit);
    
  //Spin Bits    
  int ispinb = trgd->spinBit();
  if(ispinb &   1) h442_bunch_yellow_fill->Fill(bunch7bit);  
  if(ispinb &   2) h443_bunch_yellow_up->Fill(bunch7bit);  
  if(ispinb &   4) h444_bunch_yellow_down->Fill(bunch7bit);  
  if(ispinb &   8) h445_bunch_yellow_unpol->Fill(bunch7bit);  
  if(ispinb &  16) h446_bunch_blue_fill->Fill(bunch7bit);  
  if(ispinb &  32) h447_bunch_blue_up->Fill(bunch7bit);  
  if(ispinb &  64) h448_bunch_blue_down->Fill(bunch7bit);  
  if(ispinb & 128) h449_bunch_blue_unpol->Fill(bunch7bit); 

  // zdcsmd...
  TH1D *zdcsmd[8];
  zdcsmd[0] = (TH1D*)h329_zdcsmd_w_v_N;
  zdcsmd[1] = (TH1D*)h330_zdcsmd_w_h_N;
  zdcsmd[2] = (TH1D*)h331_zdcsmd_e_v_N;
  zdcsmd[3] = (TH1D*)h332_zdcsmd_e_h_N;
  zdcsmd[4] = (TH1D*)h333_zdcsmd_w_v_A;
  zdcsmd[5] = (TH1D*)h334_zdcsmd_w_h_A;
  zdcsmd[6] = (TH1D*)h335_zdcsmd_e_v_A;
  zdcsmd[7] = (TH1D*)h336_zdcsmd_e_h_A;

  for(int i=0; i<2; i++){
    for(int j=0; j<2; j++){
      for(int k=1; k<=8; k++){
	int adc = trgd->zdcSMD((StBeamDirection)i,j,k);
	adc -= zdc_smd_ped[i][j][k-1];
	if(adc>0){
	  zdcsmd[2 +j -i*2]->Fill(k);	    
	  zdcsmd[6 +j -i*2]->Fill(k,adc);
	}
      }
    }
  }

  // L2
  L2UpsilonResult _L2;
  L2UpsilonResult *mL2 = &_L2;

  static list<double> seen;
  static list<double> acce;

  const unsigned int *summary = trgd->l2Result();
  memcpy(mL2, summary+L2RESULTS_2008_OFFSET_UPS, sizeof(L2UpsilonResult));
  mL2->swap();

  hL2ups_Tag->Fill(mL2->tag);
  hL2ups_Time->Fill(mL2->time);
  mNumberOfHotTowers =  mL2->reserved;
  hL2ups_NumberOfHotTowers->Fill(mL2->eventsSeen,mNumberOfHotTowers);
  seen.push_back(mL2->eventsSeen);
  acce.push_back(mL2->eventsAccepted);
  hL2ups_AbortRate->SetBinContent(mL2->eventsSeen, (double)(mL2->eventsSeen-mL2->eventsAccepted)/mL2->eventsSeen );
  if ( seen.size() > 25 ) {
    while ( seen.size()>25 ) {
      seen.pop_front();
      acce.pop_front();
    }
    double rate = 1. - ( acce.back()-acce.front() ) / (seen.back()-seen.front() );
    hL2ups_AbortRateCurrent->SetBinContent(mL2->eventsSeen, rate);
  }
  hL2ups_Event->SetBinContent( 1, mL2->eventsSeen );
  hL2ups_Event->SetBinContent( 2, mL2->eventsAccepted );	 

  hL2ups_EnergyL0->Fill(1.e-3 * mL2->energyOfClusterL0);
  hL2ups_EnergyL2->Fill(1.e-3 * mL2->energyOfClusterL2);
  hL2ups_Mass->Fill(1.e-3 * mL2->invMass);
  hL2ups_CosTheta->Fill(1.e-3 * mL2->cosTheta);

  hL2ups_TriggerTowerIdL0->Fill( mL2->triggerTowerL0 );
  hL2ups_TriggerTowerIdL2->Fill( mL2->triggerTowerL2 );
  hL2ups_NumberOfTowersL0->Fill( mL2->numberOfTowersL0 );
  hL2ups_NumberOfTowersL2->Fill( mL2->numberOfTowersL2 );

  // ? Why not working?   Who knows... perhaps just not important...
  //
  //BemcGeometry* geom = BemcGeometry::instance();
  
  //hEtaPhiL0->Fill( geom->eta(mL2->triggerTowerL0), geom->phi(mL2->triggerTowerL0) );
  //hEtaPhiL2->Fill( geom->eta(mL2->triggerTowerL2), geom->phi(mL2->triggerTowerL2) );
    
  if(trgd) delete trgd;
}

void trgBuilder::stoprun(daqReader *rdr) {
//   printf("Stopping run #%d\n",run);
//   status.setEndOfRun(1);
//   send((TObject *)&status);
}

void trgBuilder::main(int argc, char *argv[])
{
  trgBuilder me;
  
  me.Main(argc, argv);
}

