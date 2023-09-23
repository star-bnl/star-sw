#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "RTS/trg/include/trgDataDefs_46.h"
#include "L2UpsilonResult.h"
#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1.h>
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "trgBuilder.h"
#include <RTS/include/rtsLog.h>
#include <RTS/trg/include/trgConfNum.h>

#include <zdc_smd.h>

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
const int logTimePlots = 0; // ZDC time plots
//msimko: changed to log because of the peak at 50 in ZDC East
//******************************************************************************

ClassImp(trgBuilder);


trgBuilder::trgBuilder(JevpServer *parent) : JevpBuilder(parent) {
    plotsetname = (char *)"trg";
    memset(&contents, 0, sizeof(contents));
}

trgBuilder::~trgBuilder() {
    int n = sizeof(contents) / sizeof(TH1 *);
    
    for(int i=0;i<n;i++) {
  	if(contents.array[i]) delete contents.array[i];
    }
}

void trgBuilder::initialize(int argc, char *argv[]) {

    init_zdc_smd();
    // Build Root Histograms...
    // Trigger / ZDC

    contents.h76_zdc_time_east = new TH1D("h76_zdc_time_east", "ZDC Time East",200,0,1800); 
    //lkramarik: Upper limit was changed from 1500 to 1800 for 2019 (peak at maximum that is at ~1600)
    contents.h77_zdc_time_west = new TH1D("h77_zdc_time_west", "ZDC Time West",200,0,1800); 
    //lkramarik: Upper limit was changed from 1500 to 1800 for 2019

    contents.h78_zdc_timediff_east_west = new TH1D("h78_zdc_timediff_east_west","ZDC Time (West - East)",200,-2000,2000);
    contents.h146_zdc_Vertex_cm = new TH1D("h146_zdc_Vertex_cm","Vertex Position from ZDC (cm)",50, -100, 100);
    //lkramarik: Upper limit changed to 1500 for A+A 2019 collisions (changed from 4000)
    contents.h480_zdc_unatt_eastsum = new TH1D("h480_zdc_unatt_eastsum","ZDC Unattenuated East Sum",200,0,1500);
    contents.h481_zdc_unatt_westsum = new TH1D("h481_zdc_unatt_westsum","ZDC Unattenuated West Sum",200,0,1500);
  
    // Trigger / ZDC_seg
    //lkramarik: Upper limit changed to 1500 for A+A 2019 collisions (changed from 4000)
    contents.h474_zdc_unatt_east1 = new TH1D("h474_zdc_unatt_east1","ZDC Unattenuated East1",200,0,1500);
    contents.h475_zdc_unatt_west1 = new TH1D("h475_zdc_unatt_west1","ZDC Unattenuated West1",200,0,1500);
    contents.h476_zdc_unatt_east2 = new TH1D("h476_zdc_unatt_east2","ZDC Unattenuated East2",200,0,1500);
    contents.h477_zdc_unatt_west2 = new TH1D("h477_zdc_unatt_west2","ZDC Unattenuated West2",200,0,1500);
    contents.h478_zdc_unatt_east3 = new TH1D("h478_zdc_unatt_east3","ZDC Unattenuated East3",200,0,1500);
    contents.h479_zdc_unatt_west3 = new TH1D("h479_zdc_unatt_west3","ZDC Unattenuated West3",200,0,1500);

    // Trigger / ZDC sums
    //lkramarik: Upper limit changed to 2000 for A+A 2019 collisions (changed from 3000)
    contents.h482_zdc_sum_bbc = new TH2D("h482_zdc_sum_bbc","ZDC Sum vs. BBC Sum",100,0,60000,100,0,2000);
    contents.h483_zdc_hardwaresum = new TH1D("h483_zdc_hardwaresum","ZDC hardware Sum Central",300,0,2000);
  
    // Trigger / Bunch Crossing Counter
    contents.h266_bbc_bunchid_y = new TH1D("h266_bbc_bunchid_y","Bunch Crossing Counter (Yellow)",128,-0.5,127.5);
    contents.h266_bbc_bunchid_b = new TH1D("h266_bbc_bunchid_b","Bunch Crossing Counter (Blue)",128,-0.5,127.5);

    contents.h442_bunch_yellow_fill = new TH1D("h442_bunch_yellow_fill","h442_bunch_yellow_fill",128,-0.5,127.5);
    //contents.h443_bunch_yellow_up = new TH1D("h443_bunch_yellow_up","h443_bunch_yellow_up",128,-0.5,127.5);
    //contents.h444_bunch_yellow_down = new TH1D("h444_bunch_yellow_down","h444_bunch_yellow_down",128,-0.5,127.5);
    //contents.h445_bunch_yellow_unpol = new TH1D("h445_bunch_yellow_unpol","h445_bunch_yellow_unpol",128,-0.5,127.5);
    contents.h446_bunch_blue_fill = new TH1D("h446_bunch_blue_fill","h446_bunch_blue_fill",128,-0.5,127.5);
    //contents.h447_bunch_blue_up = new TH1D("h447_bunch_blue_up","h447_bunch_blue_up",128,-0.5,127.5);
    //contents.h448_bunch_blue_down = new TH1D("h448_bunch_blue_down","h448_bunch_blue_down",128,-0.5,127.5);
    //contents.h449_bunch_blue_unpol = new TH1D("h449_bunch_blue_unpol","h449_bunch_blue_unpol",128,-0.5,127.5);

    // zdcsmd
    contents.h329_zdcsmd_w_v_N = new TH1D("h329_zdcsmd_w_v_N","ZDC_SMD_west_ver_N(adc > 80)",8,0.5,8.5);
    contents.h330_zdcsmd_w_h_N = new TH1D("h330_zdcsmd_w_h_N","ZDC_SMD_west_hor_N(adc > 80)",8,0.5,8.5);
    contents.h331_zdcsmd_e_v_N = new TH1D("h331_zdcsmd_e_v_N","ZDC_SMD_east_ver_N(adc > 80)",8,0.5,8.5);
    contents.h332_zdcsmd_e_h_N = new TH1D("h332_zdcsmd_e_h_N","ZDC_SMD_east_hor_N(adc > 80)",8,0.5,8.5);
    contents.h333_zdcsmd_w_v_A = new TH1D("h333_zdcsmd_w_v_A","ZDC_SMD_west_ver_A(adc > 3)",8,0.5,8.5);
    contents.h334_zdcsmd_w_h_A = new TH1D("h334_zdcsmd_w_h_A","ZDC_SMD_west_hor_A(adc > 3)",8,0.5,8.5);
    contents.h335_zdcsmd_e_v_A = new TH1D("h335_zdcsmd_e_v_A","ZDC_SMD_east_ver_A(adc > 3)",8,0.5,8.5);
    contents.h336_zdcsmd_e_h_A = new TH1D("h336_zdcsmd_e_h_A","ZDC_SMD_east_hor_A(adc > 3)",8,0.5,8.5);
    contents.h337_zdcsmd_w_v_A_2D = new TH2D("h337_zdcsmd_w_v_A_2D", "ZDC_SMD_west_ver_A_2D(adc > 3)", 8, 0.5, 8.5, 200, 0, 2000);
    contents.h338_zdcsmd_w_h_A_2D = new TH2D("h338_zdcsmd_w_h_A_2D", "ZDC_SMD_west_hor_A_2D(adc > 3)", 8, 0.5, 8.5, 200, 0, 2000);
    contents.h339_zdcsmd_e_v_A_2D = new TH2D("h339_zdcsmd_e_v_A_2D", "ZDC_SMD_east_ver_A_2D(adc > 3)", 8, 0.5, 8.5, 200, 0, 2000);
    contents.h340_zdcsmd_e_h_A_2D = new TH2D("h340_zdcsmd_e_h_A_2D", "ZDC_SMD_east_hor_A_2D(adc > 3)", 8, 0.5, 8.5, 200, 0, 2000);
    contents.h341_zdcsmd_w_v_A_strip1 = new TH1D("h341_zdcsmd_w_v_A_strip1", "ZDC_SMD_west_ver_A_strip1(adc > 3)", 200, 0, 2000);
    contents.h342_zdcsmd_w_v_A_strip2 = new TH1D("h342_zdcsmd_w_v_A_strip2", "ZDC_SMD_west_ver_A_strip2(adc > 3)", 200, 0, 2000);
    contents.h343_zdcsmd_w_v_A_strip3 = new TH1D("h343_zdcsmd_w_v_A_strip3", "ZDC_SMD_west_ver_A_strip3(adc > 3)", 200, 0, 2000);
    contents.h344_zdcsmd_w_v_A_strip4 = new TH1D("h344_zdcsmd_w_v_A_strip4", "ZDC_SMD_west_ver_A_strip4(adc > 3)", 200, 0, 2000);
    contents.h345_zdcsmd_w_v_A_strip5 = new TH1D("h345_zdcsmd_w_v_A_strip5", "ZDC_SMD_west_ver_A_strip5(adc > 3)", 200, 0, 2000);
    contents.h346_zdcsmd_w_v_A_strip6 = new TH1D("h346_zdcsmd_w_v_A_strip6", "ZDC_SMD_west_ver_A_strip6(adc > 3)", 200, 0, 2000);
    contents.h347_zdcsmd_w_v_A_strip7 = new TH1D("h347_zdcsmd_w_v_A_strip7", "ZDC_SMD_west_ver_A_strip7(adc > 3)", 200, 0, 2000);
    contents.h348_zdcsmd_w_v_A_strip8 = new TH1D("h348_zdcsmd_w_v_A_strip8", "ZDC_SMD_west_ver_A_strip8(adc > 3)", 200, 0, 2000);
    contents.h349_zdcsmd_w_h_A_strip1 = new TH1D("h349_zdcsmd_w_h_A_strip1", "ZDC_SMD_west_hor_A_strip1(adc > 3)", 200, 0, 2000);
    contents.h350_zdcsmd_w_h_A_strip2 = new TH1D("h350_zdcsmd_w_h_A_strip2", "ZDC_SMD_west_hor_A_strip2(adc > 3)", 200, 0, 2000);
    contents.h351_zdcsmd_w_h_A_strip3 = new TH1D("h351_zdcsmd_w_h_A_strip3", "ZDC_SMD_west_hor_A_strip3(adc > 3)", 200, 0, 2000);
    contents.h352_zdcsmd_w_h_A_strip4 = new TH1D("h352_zdcsmd_w_h_A_strip4", "ZDC_SMD_west_hor_A_strip4(adc > 3)", 200, 0, 2000);
    contents.h353_zdcsmd_w_h_A_strip5 = new TH1D("h353_zdcsmd_w_h_A_strip5", "ZDC_SMD_west_hor_A_strip5(adc > 3)", 200, 0, 2000);
    contents.h354_zdcsmd_w_h_A_strip6 = new TH1D("h354_zdcsmd_w_h_A_strip6", "ZDC_SMD_west_hor_A_strip6(adc > 3)", 200, 0, 2000);
    contents.h355_zdcsmd_w_h_A_strip7 = new TH1D("h355_zdcsmd_w_h_A_strip7", "ZDC_SMD_west_hor_A_strip7(adc > 3)", 200, 0, 2000);
    contents.h356_zdcsmd_w_h_A_strip8 = new TH1D("h356_zdcsmd_w_h_A_strip8", "ZDC_SMD_west_hor_A_strip8(adc > 3)", 200, 0, 2000);
    contents.h357_zdcsmd_e_v_A_strip1 = new TH1D("h357_zdcsmd_e_v_A_strip1", "ZDC_SMD_east_ver_A_strip1(adc > 3)", 200, 0, 2000);
    contents.h358_zdcsmd_e_v_A_strip2 = new TH1D("h358_zdcsmd_e_v_A_strip2", "ZDC_SMD_east_ver_A_strip2(adc > 3)", 200, 0, 2000);
    contents.h359_zdcsmd_e_v_A_strip3 = new TH1D("h359_zdcsmd_e_v_A_strip3", "ZDC_SMD_east_ver_A_strip3(adc > 3)", 200, 0, 2000);
    contents.h360_zdcsmd_e_v_A_strip4 = new TH1D("h360_zdcsmd_e_v_A_strip4", "ZDC_SMD_east_ver_A_strip4(adc > 3)", 200, 0, 2000);
    contents.h361_zdcsmd_e_v_A_strip5 = new TH1D("h361_zdcsmd_e_v_A_strip5", "ZDC_SMD_east_ver_A_strip5(adc > 3)", 200, 0, 2000);
    contents.h362_zdcsmd_e_v_A_strip6 = new TH1D("h362_zdcsmd_e_v_A_strip6", "ZDC_SMD_east_ver_A_strip6(adc > 3)", 200, 0, 2000);
    contents.h363_zdcsmd_e_v_A_strip7 = new TH1D("h363_zdcsmd_e_v_A_strip7", "ZDC_SMD_east_ver_A_strip7(adc > 3)", 200, 0, 2000);
    contents.h364_zdcsmd_e_v_A_strip8 = new TH1D("h364_zdcsmd_e_v_A_strip8", "ZDC_SMD_east_ver_A_strip8(adc > 3)", 200, 0, 2000);
    contents.h365_zdcsmd_e_h_A_strip1 = new TH1D("h365_zdcsmd_e_h_A_strip1", "ZDC_SMD_east_hor_A_strip1(adc > 3)", 200, 0, 2000);
    contents.h366_zdcsmd_e_h_A_strip2 = new TH1D("h366_zdcsmd_e_h_A_strip2", "ZDC_SMD_east_hor_A_strip2(adc > 3)", 200, 0, 2000);
    contents.h367_zdcsmd_e_h_A_strip3 = new TH1D("h367_zdcsmd_e_h_A_strip3", "ZDC_SMD_east_hor_A_strip3(adc > 3)", 200, 0, 2000);
    contents.h368_zdcsmd_e_h_A_strip4 = new TH1D("h368_zdcsmd_e_h_A_strip4", "ZDC_SMD_east_hor_A_strip4(adc > 3)", 200, 0, 2000);
    contents.h369_zdcsmd_e_h_A_strip5 = new TH1D("h369_zdcsmd_e_h_A_strip5", "ZDC_SMD_east_hor_A_strip5(adc > 3)", 200, 0, 2000);
    contents.h370_zdcsmd_e_h_A_strip6 = new TH1D("h370_zdcsmd_e_h_A_strip6", "ZDC_SMD_east_hor_A_strip6(adc > 3)", 200, 0, 2000);
    contents.h371_zdcsmd_e_h_A_strip7 = new TH1D("h371_zdcsmd_e_h_A_strip7", "ZDC_SMD_east_hor_A_strip7(adc > 3)", 200, 0, 2000);
    contents.h372_zdcsmd_e_h_A_strip8 = new TH1D("h372_zdcsmd_e_h_A_strip8", "ZDC_SMD_east_hor_A_strip8(adc > 3)", 200, 0, 2000);

    // L2UpsilonCounts...
    contents.hL2ups_Tag = new TH1D("hL2ups_Tag","Tag",5,-0.5,4.5);
    contents.hL2ups_Time = new TH1D("hL2ups_Time","Time",250,0.5,1000);
    contents.hL2ups_Event = new TH1D("hL2ups_Event","Events seen/accepted",2,-0.5,1.5);
    contents.hL2ups_NumberOfHotTowers = new TH1D("hL2ups_NumberOfHotTowers","number of hot towers",5000,4,5004);
    contents.hL2ups_AbortRate = new TH1D("hL2ups_AbortRate","abord rate overall",5000,4,5004);
    contents.hL2ups_AbortRateCurrent = new TH1D("hL2ups_AbortRateCurrent","abord rate (25 evts)",5000,4,5004);
    contents.hL2ups_EnergyL0 = new TH1D("hL2ups_EnergyL0","energy L0",100,0.,25.);
    contents.hL2ups_EnergyL2 = new TH1D("hL2ups_EnergyL2","energy L2",100,0.,25.);
    contents.hL2ups_Mass = new TH1D("hL2ups_Mass","inv. mass",100,0.,20.);
    contents.hL2ups_CosTheta = new TH1D("hL2ups_CosTheta","cos(theta)",100,-1.,1.);
    contents.hL2ups_TriggerTowerIdL0 = new TH1D("hL2ups_TriggerTowerIdL0","trigger tower id L0",4800,0.,4800.);
    contents.hL2ups_TriggerTowerIdL2 = new TH1D("hL2ups_TriggerTowerIdL2","trigger tower id L2",4800,0.,4800.);
    contents.hL2ups_NumberOfTowersL0 = new TH1D("hL2ups_NumberOfTowersL0","number of towers L0",25,0.,25);
    contents.hL2ups_NumberOfTowersL2 = new TH1D("hL2ups_NumberOfTowersL2","number of towers L2",100,0.,100);
    contents.hL2ups_EtaPhiL0 = new TH2D("hL2ups_EtaPhiL0","phi vs eta L0",40,-1.3,1.3, 120,-3.14145927,+3.14145927);
    contents.hL2ups_EtaPhiL2 = new TH2D("hL2ups_EtaPhiL2","phi vs eta L2",40,-1.3,1.3, 120,-3.14145927,+3.14145927);

    contents.qt1_sz_h = new TH1D("qt1_sz_h", "QT1 Data Size", 101, 0, 1400);
    contents.qt1_board_occ_h = new TProfile("qt1_board_occ_h", "QT1 Board Average Occupancy", 16, 16, 32);
    contents.qt1_board_occ_h->Sumw2();
    contents.qt1_readout_time_h = new TH1D("qt1_readout_time_h", "QT1 Readout Time", 100, 0, 1000);
  
    contents.qt2_sz_h = new TH1D("qt2_sz_h", "QT2 Data Size", 101, 0, 1400);
    contents.qt2_board_occ_h  = new TProfile("qt2_board_occ_h", "QT2 Board Average Occupancy", 16,16,32);
    contents.qt2_board_occ_h->Sumw2();
    contents.qt2_readout_time_h = new TH1D("qt2_readout_time_h", "QT2 Readout Time", 100, 0, 1000);
  
    contents.qt3_sz_h = new TH1D("qt3_sz_h", "QT3 Data Size", 101, 0, 1400);
    contents.qt3_board_occ_h  = new TProfile("qt3_board_occ_h", "QT3 Board Average Occupancy", 16, 16 ,32);
    contents.qt3_board_occ_h->Sumw2();  
    contents.qt3_readout_time_h = new TH1D("qt3_readout_time_h", "QT3 Readout Time", 100, 0, 1000);
  
    contents.qt4_sz_h = new TH1D("qt4_sz_h", "QT4 Data Size", 101, 0, 1400);
    contents.qt4_board_occ_h  = new TProfile("qt4_board_occ_h", "QT4 Board Average Occupancy", 16, 16, 32);
    contents.qt4_board_occ_h->Sumw2();
    contents.qt4_readout_time_h = new TH1D("qt4_readout_time_h", "QT4 Readout Time", 100, 0, 1000);
  
    contents.mxq_sz_h = new TH1D("mxq_sz_h", "MXQ Data Size", 101, 0, 1400);
    contents.mxq_board_occ_h = new TProfile("mxq_board_occ_h", "MXQ Board Average Occupancy", 16, 15.5, 31.5);
    contents.mxq_board_occ_h->Sumw2();
    contents.mxq_readout_time_h = new TH1D("mxq_readout_time_h", "MXQ Readout Time", 100, 0, 1000);
  
    contents.bbq_sz_h = new TH1D("bbq_sz_h", "BBQ Data Size", 101, 0, 1400);
    contents.bbq_board_occ_h  = new TProfile("bbq_board_occ_h", "BBQ Board Average Occupancy", 16, 16, 32);
    contents.bbq_board_occ_h->Sumw2();
    contents.bbq_readout_time_h = new TH1D("bbq_readout_time_h", "BBQ Readout Time", 100, 0, 1000);
  
    contents.eq1_sz_h = new TH1D("eq1_sz_h", "EQ1 Data Size", 101, 0, 1400);
    contents.eq1_board_occ_h  = new TProfile("eq1_board_occ_h", "EQ1 Board Average Occupancy", 16, 15.5, 31.5);
    contents.eq1_board_occ_h->Sumw2();
    contents.eq1_readout_time_h = new TH1D("eq1_readout_time_h", "EQ1 Readout Time", 100, 0, 1000);

    contents.eq2_sz_h = new TH1D("eq2_sz_h", "EQ2 Data Size", 101, 0, 1400);
    contents.eq2_board_occ_h  = new TProfile("eq2_board_occ_h", "EQ2 Board Average Occupancy", 16, 15.5, 31.5);
    contents.eq2_board_occ_h->Sumw2();
    contents.eq2_readout_time_h = new TH1D("eq2_readout_time_h", "EQ2 Readout Time", 100, 0, 1000);

    contents.eq3_sz_h = new TH1D("eq3_sz_h", "EQ3 Data Size", 101, 0, 1400);
    contents.eq3_board_occ_h  = new TProfile("eq3_board_occ_h", "EQ3 Board Average Occupancy", 16, 15.5, 31.5);
    contents.eq3_board_occ_h->Sumw2();
    contents.eq3_readout_time_h = new TH1D("eq3_readout_time_h", "EQ3 Readout Time", 100, 0, 1000);

    contents.BlueByTrigger_h = new TH1F("BlueBeamByTrigger_h", "Blue Beam By Trigger", 64,0,64);
    contents.YellowByTrigger_h= new TH1F("YellowBeamByTrigger_h","Yellow Beam By Trigger", 64,0,64);
    
    
    // Add root histograms to Plots
    int np = sizeof(contents) / sizeof(TH1 *);

    JevpPlot *plots[np];
    int n=0;
    plots[n] = new JevpPlot(contents.h76_zdc_time_east);
    plots[n]->logy = logTimePlots;
    plots[++n] = new JevpPlot(contents.h77_zdc_time_west);
    plots[n]->logy = logTimePlots;
    plots[++n] = new JevpPlot(contents.h78_zdc_timediff_east_west);
    plots[++n] = new JevpPlot(contents.h146_zdc_Vertex_cm);
    plots[++n] = new JevpPlot(contents.h480_zdc_unatt_eastsum);
    plots[n]->logy = logYunattenuated;
    plots[++n] = new JevpPlot(contents.h481_zdc_unatt_westsum);
    plots[n]->logy = logYunattenuated;
  
    plots[++n] = new JevpPlot(contents.h474_zdc_unatt_east1);
    plots[n]->logy = logYunattenuated;
    plots[++n] = new JevpPlot(contents.h475_zdc_unatt_west1);
    plots[n]->logy = logYunattenuated;
    plots[++n] = new JevpPlot(contents.h476_zdc_unatt_east2);
    plots[n]->logy = logYunattenuated;
    plots[++n] = new JevpPlot(contents.h477_zdc_unatt_west2);
    plots[n]->logy = logYunattenuated;
    plots[++n] = new JevpPlot(contents.h478_zdc_unatt_east3);
    plots[n]->logy = logYunattenuated;
    plots[++n] = new JevpPlot(contents.h479_zdc_unatt_west3);
    plots[n]->logy = logYunattenuated;

    plots[++n] = new JevpPlot(contents.h482_zdc_sum_bbc);
    plots[n]->setDrawOpts("colz");
    plots[++n] = new JevpPlot(contents.h483_zdc_hardwaresum);
    plots[n]->logy = logYSum;

    plots[++n] = new JevpPlot();
    PlotHisto *ph = new PlotHisto(contents.h266_bbc_bunchid_y);
    ph->setLegText("Y Events");
    ph->setLegArgs("F");
    ph->histo->SetFillColor(1); ph->histo->SetLineColor(1);
    plots[n]->addHisto(ph);  
    ph = new PlotHisto(contents.h442_bunch_yellow_fill);
    ph->setLegText("Y Filled");
    ph->setLegArgs("F");
    ph->histo->SetFillColor(2); ph->histo->SetLineColor(2);
    plots[n]->addHisto(ph);
    /*
    ph = new PlotHisto(contents.h443_bunch_yellow_up);
    ph->setLegText("Y Up");
    ph->setLegArgs("F");
    ph->histo->SetFillColor(3); ph->histo->SetLineColor(3);
    plots[n]->addHisto(ph);
    ph = new PlotHisto(contents.h444_bunch_yellow_down);
    ph->setLegText("Y Down");
    ph->setLegArgs("F");
    ph->histo->SetFillColor(4); ph->histo->SetLineColor(4);
    plots[n]->addHisto(ph);
    ph = new PlotHisto(contents.h445_bunch_yellow_unpol);
    ph->setLegText("Y Unpol");
    ph->setLegArgs("F");
    ph->histo->SetFillColor(5); ph->histo->SetLineColor(5);
    plots[n]->addHisto(ph);
    */
    plots[n]->setLegend(.7,.6,.95,.95);
    plots[n]->setOptStat(0);
   

    plots[++n] = new JevpPlot();
    ph = new PlotHisto(contents.h266_bbc_bunchid_b);
    ph->setLegText("B Events");
    ph->setLegArgs("F");
    ph->histo->SetFillColor(1); ph->histo->SetLineColor(1);
    plots[n]->addHisto(ph);  
    ph = new PlotHisto(contents.h446_bunch_blue_fill);
    ph->setLegText("B Filled");
    ph->setLegArgs("F");
    ph->histo->SetFillColor(2); ph->histo->SetLineColor(2);
    plots[n]->addHisto(ph);
    /*
    ph = new PlotHisto(contents.h447_bunch_blue_up);
    ph->setLegText("B Up");
    ph->setLegArgs("F");
    ph->histo->SetFillColor(3); ph->histo->SetLineColor(3);
    plots[n]->addHisto(ph);
    ph = new PlotHisto(contents.h448_bunch_blue_down);
    ph->setLegText("B Down");
    ph->setLegArgs("F");
    ph->histo->SetFillColor(4); ph->histo->SetLineColor(4);
    plots[n]->addHisto(ph);
    ph = new PlotHisto(contents.h449_bunch_blue_unpol);
    ph->setLegText("B Unpol");
    ph->setLegArgs("F");
    ph->histo->SetFillColor(5); ph->histo->SetLineColor(5);
    plots[n]->addHisto(ph);
    */
    plots[n]->setLegend(.7,.6,.95,.95);
    plots[n]->setOptStat(0);

  
    plots[++n] = new JevpPlot(contents.h329_zdcsmd_w_v_N);
    plots[++n] = new JevpPlot(contents.h330_zdcsmd_w_h_N);
    plots[++n] = new JevpPlot(contents.h331_zdcsmd_e_v_N);
    plots[++n] = new JevpPlot(contents.h332_zdcsmd_e_h_N);
    plots[++n] = new JevpPlot(contents.h333_zdcsmd_w_v_A);
    plots[++n] = new JevpPlot(contents.h334_zdcsmd_w_h_A);
    plots[++n] = new JevpPlot(contents.h335_zdcsmd_e_v_A);
    plots[++n] = new JevpPlot(contents.h336_zdcsmd_e_h_A);
    plots[++n] = new JevpPlot(contents.h337_zdcsmd_w_v_A_2D); plots[n]->setDrawOpts("colz");
    plots[++n] = new JevpPlot(contents.h338_zdcsmd_w_h_A_2D); plots[n]->setDrawOpts("colz");
    plots[++n] = new JevpPlot(contents.h339_zdcsmd_e_v_A_2D); plots[n]->setDrawOpts("colz");
    plots[++n] = new JevpPlot(contents.h340_zdcsmd_e_h_A_2D); plots[n]->setDrawOpts("colz");
    plots[++n] = new JevpPlot(contents.h341_zdcsmd_w_v_A_strip1); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h342_zdcsmd_w_v_A_strip2); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h343_zdcsmd_w_v_A_strip3); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h344_zdcsmd_w_v_A_strip4); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h345_zdcsmd_w_v_A_strip5); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h346_zdcsmd_w_v_A_strip6); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h347_zdcsmd_w_v_A_strip7); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h348_zdcsmd_w_v_A_strip8); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h349_zdcsmd_w_h_A_strip1); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h350_zdcsmd_w_h_A_strip2); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h351_zdcsmd_w_h_A_strip3); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h352_zdcsmd_w_h_A_strip4); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h353_zdcsmd_w_h_A_strip5); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h354_zdcsmd_w_h_A_strip6); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h355_zdcsmd_w_h_A_strip7); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h356_zdcsmd_w_h_A_strip8); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h357_zdcsmd_e_v_A_strip1); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h358_zdcsmd_e_v_A_strip2); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h359_zdcsmd_e_v_A_strip3); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h360_zdcsmd_e_v_A_strip4); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h361_zdcsmd_e_v_A_strip5); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h362_zdcsmd_e_v_A_strip6); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h363_zdcsmd_e_v_A_strip7); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h364_zdcsmd_e_v_A_strip8); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h365_zdcsmd_e_h_A_strip1); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h366_zdcsmd_e_h_A_strip2); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h367_zdcsmd_e_h_A_strip3); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h368_zdcsmd_e_h_A_strip4); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h369_zdcsmd_e_h_A_strip5); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h370_zdcsmd_e_h_A_strip6); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h371_zdcsmd_e_h_A_strip7); plots[n]->logy = 1;
    plots[++n] = new JevpPlot(contents.h372_zdcsmd_e_h_A_strip8); plots[n]->logy = 1;

    // L2UpsilonCounts...
    plots[++n] = new JevpPlot(contents.hL2ups_Tag);
    plots[++n] = new JevpPlot(contents.hL2ups_Time);
    plots[++n] = new JevpPlot(contents.hL2ups_Event);
    plots[++n] = new JevpPlot(contents.hL2ups_NumberOfHotTowers);
    plots[++n] = new JevpPlot(contents.hL2ups_AbortRate);
    plots[++n] = new JevpPlot(contents.hL2ups_AbortRateCurrent);
    plots[++n] = new JevpPlot(contents.hL2ups_EnergyL0);
    plots[++n] = new JevpPlot(contents.hL2ups_EnergyL2);
    plots[++n] = new JevpPlot(contents.hL2ups_Mass);
    plots[++n] = new JevpPlot(contents.hL2ups_CosTheta);
    plots[++n] = new JevpPlot(contents.hL2ups_TriggerTowerIdL0);
    plots[++n] = new JevpPlot(contents.hL2ups_TriggerTowerIdL2);
    plots[++n] = new JevpPlot(contents.hL2ups_NumberOfTowersL0);
    plots[++n] = new JevpPlot(contents.hL2ups_NumberOfTowersL2);
    plots[++n] = new JevpPlot(contents.hL2ups_EtaPhiL0);
    plots[++n] = new JevpPlot(contents.hL2ups_EtaPhiL2);

    plots[++n] = new JevpPlot(contents.qt1_sz_h);
    {	
	plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Crate Size (Bytes)");
    }
    plots[++n] = new JevpPlot(contents.qt1_board_occ_h);
    {
	plots[n]->setDrawOpts("hist");
	for(int i=0;i<16;i++) {
	    contents.qt1_board_occ_h->GetXaxis()->SetBinLabel(i+1,Form("0x%x",i+16));
	}
	contents.qt1_board_occ_h->GetXaxis()->SetTitle("Board Address");
	contents.qt1_board_occ_h->GetYaxis()->SetTitle("Occupancy");
    }
    plots[++n] = new JevpPlot(contents.qt1_readout_time_h);
    plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Readout Time (usec)");

    plots[++n] = new JevpPlot(contents.qt2_sz_h);
    {	
	plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Crate Size (Bytes)");
    }
    plots[++n] = new JevpPlot(contents.qt2_board_occ_h);
    {
	plots[n]->setDrawOpts("hist");
	for(int i=0;i<16;i++) {
	    contents.qt2_board_occ_h->GetXaxis()->SetBinLabel(i+1,Form("0x%x",i+16));
	}
	contents.qt2_board_occ_h->GetXaxis()->SetTitle("Board Address");
	contents.qt2_board_occ_h->GetYaxis()->SetTitle("Occupancy");
    }
    plots[++n] = new JevpPlot(contents.qt2_readout_time_h);
    plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Readout Time (usec)");
      
    plots[++n] = new JevpPlot(contents.qt3_sz_h);
    {	
	plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Crate Size (Bytes)");
    }
    plots[++n] = new JevpPlot(contents.qt3_board_occ_h);
    {
	plots[n]->setDrawOpts("hist");
	for(int i=0;i<16;i++) {
	    contents.qt3_board_occ_h->GetXaxis()->SetBinLabel(i+1,Form("0x%x",i+16));
	}
	contents.qt3_board_occ_h->GetXaxis()->SetTitle("Board Address");
	contents.qt3_board_occ_h->GetYaxis()->SetTitle("Occupancy");
    }
    plots[++n] = new JevpPlot(contents.qt3_readout_time_h);
    plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Readout Time (usec)");
	
    plots[++n] = new JevpPlot(contents.qt4_sz_h);
    {	
	plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Crate Size (Bytes)");
    }
    plots[++n] = new JevpPlot(contents.qt4_board_occ_h);
    {
	plots[n]->setDrawOpts("hist");
	for(int i=0;i<16;i++) {
	    contents.qt4_board_occ_h->GetXaxis()->SetBinLabel(i+1,Form("0x%x",i+16));
	}
	contents.qt4_board_occ_h->GetXaxis()->SetTitle("Board Address");
	contents.qt4_board_occ_h->GetYaxis()->SetTitle("Occupancy");
    }
    plots[++n] = new JevpPlot(contents.qt4_readout_time_h);
    plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Readout Time (usec)");
  
    plots[++n] = new JevpPlot(contents.mxq_sz_h);
    {	
	plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Crate Size (Bytes)");
    }
    plots[++n] = new JevpPlot(contents.mxq_board_occ_h);
    {
	plots[n]->setDrawOpts("hist");
	for(int i=0;i<16;i++) {
	    contents.mxq_board_occ_h->GetXaxis()->SetBinLabel(i+1,Form("0x%x",i+16));
	}
	contents.mxq_board_occ_h->GetXaxis()->SetTitle("Board Address");
	contents.mxq_board_occ_h->GetYaxis()->SetTitle("Occupancy");
    }
    plots[++n] = new JevpPlot(contents.mxq_readout_time_h);
    plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Readout Time (usec)");
  
    plots[++n] = new JevpPlot(contents.bbq_sz_h);
    {	
	plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Crate Size (Bytes)");
    }
    plots[++n] = new JevpPlot(contents.bbq_board_occ_h);    
    {
	plots[n]->setDrawOpts("hist");
	for(int i=0;i<16;i++) {
	    contents.bbq_board_occ_h->GetXaxis()->SetBinLabel(i+1,Form("0x%x",i+16));
	}
	contents.bbq_board_occ_h->GetXaxis()->SetTitle("Board Address");
	contents.bbq_board_occ_h->GetYaxis()->SetTitle("Occupancy");
    }
    plots[++n] = new JevpPlot(contents.bbq_readout_time_h);
    plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Readout Time (usec)");
  
    plots[++n] = new JevpPlot(contents.eq1_sz_h);
    {	
	plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Crate Size (Bytes)");
    }
    plots[++n] = new JevpPlot(contents.eq1_board_occ_h);
    {
	plots[n]->setDrawOpts("hist");
	for(int i=0;i<16;i++) {
	    contents.eq1_board_occ_h->GetXaxis()->SetBinLabel(i+1,Form("0x%x",i+16));
	}
	contents.eq1_board_occ_h->GetXaxis()->SetTitle("Board Address");
	contents.eq1_board_occ_h->GetYaxis()->SetTitle("Occupancy");
    }
    plots[++n] = new JevpPlot(contents.eq1_readout_time_h);
    plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Readout Time (usec)");

    plots[++n] = new JevpPlot(contents.eq2_sz_h);
    {	
	plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Crate Size (Bytes)");
    }
    plots[++n] = new JevpPlot(contents.eq2_board_occ_h);
    {
	plots[n]->setDrawOpts("hist");
	for(int i=0;i<16;i++) {
	    contents.eq2_board_occ_h->GetXaxis()->SetBinLabel(i+1,Form("0x%x",i+16));
	}
	contents.eq2_board_occ_h->GetXaxis()->SetTitle("Board Address");
	contents.eq2_board_occ_h->GetYaxis()->SetTitle("Occupancy");
    }
    plots[++n] = new JevpPlot(contents.eq2_readout_time_h);
    plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Readout Time (usec)");


    plots[++n] = new JevpPlot(contents.eq3_sz_h);
    {	
	plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Crate Size (Bytes)");
    }
    plots[++n] = new JevpPlot(contents.eq3_board_occ_h);
    {
	plots[n]->setDrawOpts("hist");
	for(int i=0;i<16;i++) {
	    contents.eq3_board_occ_h->GetXaxis()->SetBinLabel(i+1,Form("0x%x",i+16));
	}
	contents.eq3_board_occ_h->GetXaxis()->SetTitle("Board Address");
	contents.eq3_board_occ_h->GetYaxis()->SetTitle("Occupancy");
    }
    plots[++n] = new JevpPlot(contents.eq3_readout_time_h);
    plots[n]->getHisto(0)->histo->GetXaxis()->SetTitle("Readout Time (usec)");

    plots[++n] = new JevpPlot(contents.BlueByTrigger_h);
    plots[n]->setDrawOpts("hist");
    for(int i=0;i<64;i+=10) {
	contents.BlueByTrigger_h->GetXaxis()->SetBinLabel(i+1, Form("%d", i));
    }    
    contents.BlueByTrigger_h->GetXaxis()->SetTitle("Trigger Index");
    contents.BlueByTrigger_h->GetYaxis()->SetTitle("Average Blue Sync Set");
    contents.BlueByTrigger_h->SetFillColor(4);

    plots[++n] = new JevpPlot(contents.YellowByTrigger_h);
    plots[n]->setDrawOpts("hist");
    for(int i=0;i<64;i+=10) {
	contents.YellowByTrigger_h->GetXaxis()->SetBinLabel(i+1, Form("%d", i));
    }    
    contents.YellowByTrigger_h->GetXaxis()->SetTitle("Trigger Index");
    contents.YellowByTrigger_h->GetYaxis()->SetTitle("Average Yellow Sync Set");
    contents.YellowByTrigger_h->SetFillColor(5);
 

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

  for(int i=0;i<64;i++) {
      nBlueByTrigger[i] = 0;
      nYellowByTrigger[i] = 0;
      nTrigger[i] = 0;
  }
}

void trgBuilder::fillQtHisto(int conf_num, TriggerDataBlk *trg, TH1D *sz, TH1D *usec, TProfile *board_occ) {
    QtEventInfo info;
    memset(&info, 0, sizeof(info));

    if(trg->MainX[conf_num].offset == 0) return;

    char *base = (char *)trg;
    QTBlock *qtb = (QTBlock *)(base + swap32(trg->MainX[conf_num].offset));
    int len = swap32(trg->MainX[conf_num].length);
    
    LOG(DBG, "len=%d",len);

    if((len - swap32(qtb->length)) != 12) {
	LOG(ERR, "Conf num %d not a QT board!");
	return;
    }
    
    LOG(DBG, "qtb->name = %c%c%c", qtb->name[0], qtb->name[1], qtb->name[2]);
    info.sz = len;
    
    // loop over boards
    
    if(swap32(qtb->length) > 0 ) {
	unsigned int *dword = (unsigned int *)qtb->data;
	for(;;) {
	    unsigned int x = swap32(*dword);
	    LOG(DBG, "x=0x%x", x);
	    if(x == 0xac10) break;
	    
	    int addr = (x>>16) & 0xff;
	    int nlines = x & 0xff;
	    int usec = x & 0xff00;
	    usec >>= 8;
	    //usec *= 4;     // Johns error...
	    
	    LOG(DBG, "addr=%d nlines=%d usec=%d", addr, nlines, usec);
	    
	    info.usec += usec;
	    
	    info.board_occ[addr] = (nlines+1.0) * 4.0;   // maximum of 32 dwords + 1 header word
	    info.board_occ[addr] /= 33.0 * 4.0;
	    
	    while(nlines--) {
		dword++;
	    }
	    dword++;
	}
    }
    
    // Fill histos....
   
    LOG(DBG, "Fill sz = %lf", info.sz);
    sz->Fill(info.sz);
    LOG(DBG, "Fill usec = %lf", info.usec);
    usec->Fill(info.usec);
    for(int i=16;i<32;i++) {
	LOG(DBG, "Fill board: %d %lf", i, info.board_occ[i]);
	board_occ->Fill(i,info.board_occ[i]);
	LOG(DBG, "Filled...");
    }
}


void trgBuilder::handleQTOccupancyPlots(daqReader *rdr) {
    LOG(DBG, "handleQTOcc");
    daq_dta *dd;
    dd = rdr->det("trg")->get("raw");
    if(dd) {
	if(dd->iterate()) {
	    TriggerDataBlk *trg = (TriggerDataBlk *)dd->Byte;

	    LOG(DBG, "QT1");
	    fillQtHisto(QT1_CONF_NUM, trg, (TH1D *)contents.qt1_sz_h, (TH1D *)contents.qt1_readout_time_h, (TProfile *)contents.qt1_board_occ_h);
	    LOG(DBG, "QT2");
	    fillQtHisto(QT2_CONF_NUM, trg, (TH1D *)contents.qt2_sz_h, (TH1D *)contents.qt2_readout_time_h, (TProfile *)contents.qt2_board_occ_h);
	    LOG(DBG, "QT3");
	    fillQtHisto(QT3_CONF_NUM, trg, (TH1D *)contents.qt3_sz_h, (TH1D *)contents.qt3_readout_time_h, (TProfile *)contents.qt3_board_occ_h);
	    LOG(DBG, "QT4");
	    fillQtHisto(QT4_CONF_NUM, trg, (TH1D *)contents.qt4_sz_h, (TH1D *)contents.qt4_readout_time_h, (TProfile *)contents.qt4_board_occ_h);
	    LOG(DBG, "MXQ");
	    fillQtHisto(MXQ_CONF_NUM, trg, (TH1D *)contents.mxq_sz_h, (TH1D *)contents.mxq_readout_time_h, (TProfile *)contents.mxq_board_occ_h);
	    LOG(DBG, "BBQ");
	    fillQtHisto(BBQ_CONF_NUM, trg, (TH1D *)contents.bbq_sz_h, (TH1D *)contents.bbq_readout_time_h, (TProfile *)contents.bbq_board_occ_h);
	    LOG(DBG, "EQ1");
	    fillQtHisto(EQ1_CONF_NUM, trg, (TH1D *)contents.eq1_sz_h, (TH1D *)contents.eq1_readout_time_h, (TProfile *)contents.eq1_board_occ_h);
	    LOG(DBG, "EQ2");
	    fillQtHisto(EQ2_CONF_NUM, trg, (TH1D *)contents.eq2_sz_h, (TH1D *)contents.eq2_readout_time_h, (TProfile *)contents.eq2_board_occ_h);
	    LOG(DBG, "EQ3");
	    fillQtHisto(EQ3_CONF_NUM, trg, (TH1D *)contents.eq3_sz_h, (TH1D *)contents.eq3_readout_time_h, (TProfile *)contents.eq3_board_occ_h);
	    LOG(DBG, "QTs done");
	}
    }	    
    LOG(DBG, "And done with handleQTOcc");
}


void trgBuilder::event(daqReader *rdr)
{
    LOG(DBG, "a %d",rdr->seq);
    if(first_event == 0) {
	addServerTags((char *)"|trg|");
	first_event = 1;
    }

    LOG(DBG, "a %d",rdr->seq);
    handleQTOccupancyPlots(rdr);

    LOG(DBG, "a %d",rdr->seq);
    StTriggerData *trgd = getStTriggerData(rdr);
    if(!trgd) return;

    LOG(DBG, "a %d",rdr->seq);

    // ZDC
    double mZdcTimeDiff = -9999;
    double mZdcVertex   = -9999;
    int te = trgd->zdcPmtTDC(east,1);
    int tw = trgd->zdcPmtTDC(west,1);
    LOG(DBG,"te = %d tw = %d",te,tw);
    if(te>20 && te<4000)contents.h76_zdc_time_east->Fill(float(te));    
    if(tw>20 && tw<4000)contents.h77_zdc_time_west->Fill(float(tw));    
    if(te>20 && te<4000 && tw>20 && tw<4000){
	mZdcTimeDiff = float(tw-te);
	//mZdcVertex   = mZdcTimeDiff/2*40.0/0.03;
	mZdcVertex   = (mZdcTimeDiff/2)*0.02*30;
	contents.h78_zdc_timediff_east_west->Fill(mZdcTimeDiff);          

    
	contents.h146_zdc_Vertex_cm->Fill(mZdcVertex + h146_zdc_vertex_offset);
    } 
    contents.h480_zdc_unatt_eastsum->Fill(float(trgd->zdcUnAttenuated(east)));
    contents.h481_zdc_unatt_westsum->Fill(float(trgd->zdcUnAttenuated(west)));
  
    contents.h474_zdc_unatt_east1->Fill(float(trgd->zdcADC(east,1)));
    contents.h475_zdc_unatt_west1->Fill(float(trgd->zdcADC(west,1)));
    contents.h476_zdc_unatt_east2->Fill(float(trgd->zdcADC(east,2)));
    contents.h477_zdc_unatt_west2->Fill(float(trgd->zdcADC(west,2)));
    contents.h478_zdc_unatt_east3->Fill(float(trgd->zdcADC(east,3)));
    contents.h479_zdc_unatt_west3->Fill(float(trgd->zdcADC(west,3)));

    LOG(DBG, "a %d",rdr->seq);
    

    // Hardware Sum no longer exists. It is the sum of the attenuated signals...
    // h482_zdc_sum_bbc->Fill(float(trgd->bbcADCSum(east))+float(trgd->bbcADCSum(west)), float(trgd->zdcHardwareSum()));
    // h483_zdc_hardwaresum->Fill(float(trgd->zdcHardwareSum()));

    float zdcHardwareSum = float(trgd->zdcAttenuated(east)) + float(trgd->zdcAttenuated(west));
    contents.h482_zdc_sum_bbc->Fill(float(trgd->bbcADCSum(east))+float(trgd->bbcADCSum(west)), zdcHardwareSum);
    contents.h483_zdc_hardwaresum->Fill(float(zdcHardwareSum));

    unsigned int bunch7bit = trgd->bunchId7Bit();
    contents.h266_bbc_bunchid_y->Fill(bunch7bit);
    contents.h266_bbc_bunchid_b->Fill(bunch7bit);
    
    //Spin Bits    
    // int ispinb = trgd->spinBit();
    bool blueFilled = false;
    bool yellowFilled = false;
    if(trgd->spinBitYellowFilled()) {
	contents.h442_bunch_yellow_fill->Fill(bunch7bit);
	yellowFilled = true;
    }

    /*  
    if(trgd->spinBitYellowUp()) contents.h443_bunch_yellow_up->Fill(bunch7bit);  
    if(trgd->spinBitYellowDown()) contents.h444_bunch_yellow_down->Fill(bunch7bit);  
    if(trgd->spinBitYellowUnpol()) contents.h445_bunch_yellow_unpol->Fill(bunch7bit); 
    */ 
    if(trgd->spinBitBlueFilled()) {
	contents.h446_bunch_blue_fill->Fill(bunch7bit);  
	blueFilled = true;
    }
    /*
    if(trgd->spinBitBlueUp()) contents.h447_bunch_blue_up->Fill(bunch7bit);  
    if(trgd->spinBitBlueDown()) contents.h448_bunch_blue_down->Fill(bunch7bit);  
    if(trgd->spinBitBlueUnpol()) contents.h449_bunch_blue_unpol->Fill(bunch7bit); 
    */

    for(int i=0;i<64;i++) {
       

	    
	if(rdr->daqbits64 & (1ll << i)) {
	    if(blueFilled) nBlueByTrigger[i]++;
	    if(yellowFilled) nYellowByTrigger[i]++;
	    nTrigger[i]++;
	}
	float bval = 0;
	if(nTrigger[i] > 0) {
	    bval = nBlueByTrigger[i];
	    bval /= nTrigger[i];
	}    
	contents.BlueByTrigger_h->SetBinContent(i+1, bval);
	//if(bval > 0) printf("[%d] bval=%f\n",i, bval);	
	float yval = 0;
	if(nTrigger[i] > 0) {
	    yval = nYellowByTrigger[i];
	    yval /= nTrigger[i];
	}    
	contents.YellowByTrigger_h->SetBinContent(i+1, yval);
	//if(bval > 0) printf("[%d] bval=%f\n",i, yval);
    }
    
    // zdcsmd...
    TH1 *zdcsmd[12];
    zdcsmd[0] = contents.h329_zdcsmd_w_v_N;
    zdcsmd[1] = contents.h330_zdcsmd_w_h_N;
    zdcsmd[2] = contents.h331_zdcsmd_e_v_N;
    zdcsmd[3] = contents.h332_zdcsmd_e_h_N;
    zdcsmd[4] = contents.h333_zdcsmd_w_v_A;
    zdcsmd[5] = contents.h334_zdcsmd_w_h_A;
    zdcsmd[6] = contents.h335_zdcsmd_e_v_A;
    zdcsmd[7] = contents.h336_zdcsmd_e_h_A;
    zdcsmd[8] = contents.h337_zdcsmd_w_v_A_2D;
    zdcsmd[9] = contents.h338_zdcsmd_w_h_A_2D;
    zdcsmd[10] = contents.h339_zdcsmd_e_v_A_2D;
    zdcsmd[11] = contents.h340_zdcsmd_e_h_A_2D;

    TH1 *zdcsmd_strip[4][8] = {
        {contents.h341_zdcsmd_w_v_A_strip1, contents.h342_zdcsmd_w_v_A_strip2,
         contents.h343_zdcsmd_w_v_A_strip3, contents.h344_zdcsmd_w_v_A_strip4,
         contents.h345_zdcsmd_w_v_A_strip5, contents.h346_zdcsmd_w_v_A_strip6,
         contents.h347_zdcsmd_w_v_A_strip7, contents.h348_zdcsmd_w_v_A_strip8},
        {contents.h349_zdcsmd_w_h_A_strip1, contents.h350_zdcsmd_w_h_A_strip2,
         contents.h351_zdcsmd_w_h_A_strip3, contents.h352_zdcsmd_w_h_A_strip4,
         contents.h353_zdcsmd_w_h_A_strip5, contents.h354_zdcsmd_w_h_A_strip6,
         contents.h355_zdcsmd_w_h_A_strip7, contents.h356_zdcsmd_w_h_A_strip8},
        {contents.h357_zdcsmd_e_v_A_strip1, contents.h358_zdcsmd_e_v_A_strip2,
         contents.h359_zdcsmd_e_v_A_strip3, contents.h360_zdcsmd_e_v_A_strip4,
         contents.h361_zdcsmd_e_v_A_strip5, contents.h362_zdcsmd_e_v_A_strip6,
         contents.h363_zdcsmd_e_v_A_strip7, contents.h364_zdcsmd_e_v_A_strip8},
        {contents.h365_zdcsmd_e_h_A_strip1, contents.h366_zdcsmd_e_h_A_strip2,
         contents.h367_zdcsmd_e_h_A_strip3, contents.h368_zdcsmd_e_h_A_strip4,
         contents.h369_zdcsmd_e_h_A_strip5, contents.h370_zdcsmd_e_h_A_strip6,
         contents.h371_zdcsmd_e_h_A_strip7, contents.h372_zdcsmd_e_h_A_strip8},
    };

    const int ZDCSMDTHR = 80;
    for(int i=0; i<2; i++){
	for(int j=0; j<2; j++){
	    for(int k=1; k<=8; k++){
		int adc = trgd->zdcSMD((StBeamDirection)i,j,k);
		if(adc > ZDCSMDTHR){
		    zdcsmd[2 +j -i*2]->Fill(k);	    
		}
		if (adc > 3) {
		    zdcsmd[6 +j -i*2]->Fill(k,adc);
		    zdcsmd[10 +j -i*2]->Fill(k,adc);
		    zdcsmd_strip[2 +j -i*2][k-1]->Fill(adc);
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

    // L2RESULTS_2008_OFFSET_UPS was 8
    //memcpy(mL2, summary+L2RESULTS_2008_OFFSET_UPS, sizeof(L2UpsilonResult));
    memcpy(mL2, summary+8, sizeof(L2UpsilonResult));
    mL2->swap();


    

    contents.hL2ups_Tag->Fill(mL2->tag);
    contents.hL2ups_Time->Fill(mL2->time);
    mNumberOfHotTowers =  mL2->reserved;
    contents.hL2ups_NumberOfHotTowers->Fill(mL2->eventsSeen,mNumberOfHotTowers);
    seen.push_back(mL2->eventsSeen);
    acce.push_back(mL2->eventsAccepted);
    contents.hL2ups_AbortRate->SetBinContent(mL2->eventsSeen, (double)(mL2->eventsSeen-mL2->eventsAccepted)/mL2->eventsSeen );
    if ( seen.size() > 25 ) {
	while ( seen.size()>25 ) {
	    seen.pop_front();
	    acce.pop_front();
	}
	double rate = 1. - ( acce.back()-acce.front() ) / (seen.back()-seen.front() );
	contents.hL2ups_AbortRateCurrent->SetBinContent(mL2->eventsSeen, rate);
    }
    contents.hL2ups_Event->SetBinContent( 1, mL2->eventsSeen );
    contents.hL2ups_Event->SetBinContent( 2, mL2->eventsAccepted );	 

    contents.hL2ups_EnergyL0->Fill(1.e-3 * mL2->energyOfClusterL0);
    contents.hL2ups_EnergyL2->Fill(1.e-3 * mL2->energyOfClusterL2);
    contents.hL2ups_Mass->Fill(1.e-3 * mL2->invMass);
    contents.hL2ups_CosTheta->Fill(1.e-3 * mL2->cosTheta);

    contents.hL2ups_TriggerTowerIdL0->Fill( mL2->triggerTowerL0 );
    contents.hL2ups_TriggerTowerIdL2->Fill( mL2->triggerTowerL2 );
    contents.hL2ups_NumberOfTowersL0->Fill( mL2->numberOfTowersL0 );
    contents.hL2ups_NumberOfTowersL2->Fill( mL2->numberOfTowersL2 );

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

