#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "tpxBuilder.h"
#include <RTS/include/rtsLog.h>
#include "LaserReader.h"

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(tpxBuilder);
  

static const int NTPCpads[45] = {
    88,96,104,112,118,126,134,142,150,158,166,174,182, // Inner
    98,100,102,104,106,106,108,110,112,112,114,116,118,120,122,122, //Outer 
    124,126,128,128,130,132,134,136,138,138,140,142,144,144,144,144};

tpxBuilder::tpxBuilder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"tpx";

  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
  memset(&extras, 0, sizeof(extras));

  setPhiAngleMap();
  laserReader = new LaserReader();
}

tpxBuilder::~tpxBuilder() {

  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(contents.array[i]) delete contents.array[i];
  }

  n = sizeof(extras) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(extras.array[i]) delete extras.array[i];
  }

  delete laserReader;
}

void tpxBuilder::initialize(int argc, char *argv[]) {

  //contents.tpc_occ_physics = new TH1D("tpc_physics","TPC Channel Occupancy (in %) Physics",100,0,100);
  //contents.h44_tpc_occ_laser = new TH1D("h44_tpc_occ_laser","TPC Channel Occupancy (in %) Lasers",100,0,100);
  //contents.h43_tpc_occ_pulser = new TH1D("h43_tpc_occ_pulser","TPC Channel Occupancy (in %) Pulsers",100,0,100);
  contents.tpc_pix_occ_physics = new TH1D("tpc_pix_occ_physics","TPC Pixel Occupancy (in %) Physics",100,0,2.5);
  contents.tpc_pix_occ_laser = new TH1D("tpc_pix_occ_laser","TPC Pixel Occupancy (in %) Lasers",100,0,1);
  contents.tpc_pix_occ_pulser = new TH1D("tpc_pix_occ_pulser","TPC Pixel Occupancy (in %) Pulsers",100,0,10);
  contents.h15_tpc_sec1 = new TH2D("h15_tpc_sec1","Sec. 1 charge per pad",182,0,182,45,0,45);
  contents.h16_tpc_sec2 = new TH2D("h16_tpc_sec2","Sec. 2 charge per pad",182,0,182,45,0,45);
  contents.h17_tpc_sec3 = new TH2D("h17_tpc_sec3","Sec. 3 charge per pad",182,0,182,45,0,45);
  contents.h18_tpc_sec4 = new TH2D("h18_tpc_sec4","Sec. 4 charge per pad",182,0,182,45,0,45);
  contents.h19_tpc_sec5 = new TH2D("h19_tpc_sec5","Sec. 5 charge per pad",182,0,182,45,0,45);
  contents.h20_tpc_sec6 = new TH2D("h20_tpc_sec6","Sec. 6 charge per pad",182,0,182,45,0,45);
  contents.h21_tpc_sec7 = new TH2D("h21_tpc_sec7","Sec. 7 charge per pad",182,0,182,45,0,45);
  contents.h22_tpc_sec8 = new TH2D("h22_tpc_sec8","Sec. 8 charge per pad",182,0,182,45,0,45);
  contents.h23_tpc_sec9 = new TH2D("h23_tpc_sec9","Sec. 9 charge per pad",182,0,182,45,0,45);
  contents.h24_tpc_sec10 = new TH2D("h24_tpc_sec10","Sec. 10 charge per pad",182,0,182,45,0,45);
  contents.h25_tpc_sec11 = new TH2D("h25_tpc_sec11","Sec. 11 charge per pad",182,0,182,45,0,45);
  contents.h26_tpc_sec12 = new TH2D("h26_tpc_sec12","Sec. 12 charge per pad",182,0,182,45,0,45);
  contents.h27_tpc_sec13 = new TH2D("h27_tpc_sec13","Sec. 13 charge per pad",182,0,182,45,0,45);
  contents.h28_tpc_sec14 = new TH2D("h28_tpc_sec14","Sec. 14 charge per pad",182,0,182,45,0,45);
  contents.h29_tpc_sec15 = new TH2D("h29_tpc_sec15","Sec. 15 charge per pad",182,0,182,45,0,45);
  contents.h30_tpc_sec16 = new TH2D("h30_tpc_sec16","Sec. 16 charge per pad",182,0,182,45,0,45);
  contents.h31_tpc_sec17 = new TH2D("h31_tpc_sec17","Sec. 17 charge per pad",182,0,182,45,0,45);
  contents.h32_tpc_sec18 = new TH2D("h32_tpc_sec18","Sec. 18 charge per pad",182,0,182,45,0,45);
  contents.h33_tpc_sec19 = new TH2D("h33_tpc_sec19","Sec. 19 charge per pad",182,0,182,45,0,45);
  contents.h34_tpc_sec20 = new TH2D("h34_tpc_sec20","Sec. 20 charge per pad",182,0,182,45,0,45);
  contents.h35_tpc_sec21 = new TH2D("h35_tpc_sec21","Sec. 21 charge per pad",182,0,182,45,0,45);
  contents.h36_tpc_sec22 = new TH2D("h36_tpc_sec22","Sec. 22 charge per pad",182,0,182,45,0,45);
  contents.h37_tpc_sec23 = new TH2D("h37_tpc_sec23","Sec. 23 charge per pad",182,0,182,45,0,45);
  contents.h38_tpc_sec24 = new TH2D("h38_tpc_sec24","Sec. 24 charge per pad",182,0,182,45,0,45);  
  contents.h120_chargeStep_s1 = new TH1D("h120_chargeStep_s1","TPC adc vs time sector#1",512,1,512);
  contents.h121_chargeStep_s2 = new TH1D("h121_chargeStep_s2","TPC adc vs time sector#2",512,1,512);
  contents.h122_chargeStep_s3 = new TH1D("h122_chargeStep_s3","TPC adc vs time sector#3",512,1,512);
  contents.h123_chargeStep_s4 = new TH1D("h123_chargeStep_s4","TPC adc vs time sector#4",512,1,512);
  contents.h124_chargeStep_s5 = new TH1D("h124_chargeStep_s5","TPC adc vs time sector#5",512,1,512);
  contents.h125_chargeStep_s6 = new TH1D("h125_chargeStep_s6","TPC adc vs time sector#6",512,1,512);
  contents.h126_chargeStep_s7 = new TH1D("h126_chargeStep_s7","TPC adc vs time sector#7",512,1,512);
  contents.h127_chargeStep_s8 = new TH1D("h127_chargeStep_s8","TPC adc vs time sector#8",512,1,512);
  contents.h128_chargeStep_s9 = new TH1D("h128_chargeStep_s9","TPC adc vs time sector#9",512,1,512);
  contents.h129_chargeStep_s10 = new TH1D("h129_chargeStep_s10","TPC adc vs time sector#10",512,1,512);
  contents.h130_chargeStep_s11 = new TH1D("h130_chargeStep_s11","TPC adc vs time sector#11",512,1,512);
  contents.h131_chargeStep_s12 = new TH1D("h131_chargeStep_s12","TPC adc vs time sector#12",512,1,512);
  contents.h132_chargeStep_s13 = new TH1D("h132_chargeStep_s13","TPC adc vs time sector#13",512,1,512);
  contents.h133_chargeStep_s14 = new TH1D("h133_chargeStep_s14","TPC adc vs time sector#14",512,1,512);
  contents.h134_chargeStep_s15 = new TH1D("h134_chargeStep_s15","TPC adc vs time sector#15",512,1,512);
  contents.h135_chargeStep_s16 = new TH1D("h135_chargeStep_s16","TPC adc vs time sector#16",512,1,512);
  contents.h136_chargeStep_s17 = new TH1D("h136_chargeStep_s17","TPC adc vs time sector#17",512,1,512);
  contents.h137_chargeStep_s18 = new TH1D("h137_chargeStep_s18","TPC adc vs time sector#18",512,1,512);
  contents.h138_chargeStep_s19 = new TH1D("h138_chargeStep_s19","TPC adc vs time sector#19",512,1,512);
  contents.h139_chargeStep_s20 = new TH1D("h139_chargeStep_s20","TPC adc vs time sector#20",512,1,512);
  contents.h140_chargeStep_s21 = new TH1D("h140_chargeStep_s21","TPC adc vs time sector#21",512,1,512);
  contents.h141_chargeStep_s22 = new TH1D("h141_chargeStep_s22","TPC adc vs time sector#22",512,1,512);
  contents.h142_chargeStep_s23 = new TH1D("h142_chargeStep_s23","TPC adc vs time sector#23",512,1,512);
  contents.h143_chargeStep_s24 = new TH1D("h143_chargeStep_s24","TPC adc vs time sector#24",512,1,512);
 
  contents.h102_tpc_drift_vel = new TH1D("h102_tpc_drift_vel", "TPC Drift Velocity (cm/us)",400,5.4,5.8);
  // contents.h113_tpc_drift_vel_dist = new TH1D("113_tpc_drift_vel_dist", "TPC Drift Velocity Distribution(cm/us)",200,4,8);
  contents.h66_tpc_phi_charge = new TH1D("h66_tpc_phi_charge","Azimuthal Distribution of TPC Charge",360,-180,180);
  contents.h67_tpc_sector_charge = new TH1D("h67_tpc_sector_charge","TPC Charge per Sector",24,0.5,24.5);
  
  // cluster based vesions...
  extras.tpc_clpix_occ_physics = new TH1D("tpc_clpix_occ_physics","TPC Pixel Occupancy (in %) Physics",100,0,2.5);
  extras.tpc_clpix_occ_laser = new TH1D("tpc_clpix_occ_laser","TPC Pixel Occupancy (in %) Lasers",100,0,1);
  extras.tpc_clpix_occ_pulser = new TH1D("tpc_clpix_occ_pulser","TPC Pixel Occupancy (in %) Pulsers",100,0,10);
  extras.cl120_chargeStep_s1 = new TH1D("cl120_chargeStep_s1","TPC adc vs time sector#1",512,1,512);
  extras.cl121_chargeStep_s2 = new TH1D("cl121_chargeStep_s2","TPC adc vs time sector#2",512,1,512);
  extras.cl122_chargeStep_s3 = new TH1D("cl122_chargeStep_s3","TPC adc vs time sector#3",512,1,512);
  extras.cl123_chargeStep_s4 = new TH1D("cl123_chargeStep_s4","TPC adc vs time sector#4",512,1,512);
  extras.cl124_chargeStep_s5 = new TH1D("cl124_chargeStep_s5","TPC adc vs time sector#5",512,1,512);
  extras.cl125_chargeStep_s6 = new TH1D("cl125_chargeStep_s6","TPC adc vs time sector#6",512,1,512);
  extras.cl126_chargeStep_s7 = new TH1D("cl126_chargeStep_s7","TPC adc vs time sector#7",512,1,512);
  extras.cl127_chargeStep_s8 = new TH1D("cl127_chargeStep_s8","TPC adc vs time sector#8",512,1,512);
  extras.cl128_chargeStep_s9 = new TH1D("cl128_chargeStep_s9","TPC adc vs time sector#9",512,1,512);
  extras.cl129_chargeStep_s10 = new TH1D("cl129_chargeStep_s10","TPC adc vs time sector#10",512,1,512);
  extras.cl130_chargeStep_s11 = new TH1D("cl130_chargeStep_s11","TPC adc vs time sector#11",512,1,512);
  extras.cl131_chargeStep_s12 = new TH1D("cl131_chargeStep_s12","TPC adc vs time sector#12",512,1,512);
  extras.cl132_chargeStep_s13 = new TH1D("cl132_chargeStep_s13","TPC adc vs time sector#13",512,1,512);
  extras.cl133_chargeStep_s14 = new TH1D("cl133_chargeStep_s14","TPC adc vs time sector#14",512,1,512);
  extras.cl134_chargeStep_s15 = new TH1D("cl134_chargeStep_s15","TPC adc vs time sector#15",512,1,512);
  extras.cl135_chargeStep_s16 = new TH1D("cl135_chargeStep_s16","TPC adc vs time sector#16",512,1,512);
  extras.cl136_chargeStep_s17 = new TH1D("cl136_chargeStep_s17","TPC adc vs time sector#17",512,1,512);
  extras.cl137_chargeStep_s18 = new TH1D("cl137_chargeStep_s18","TPC adc vs time sector#18",512,1,512);
  extras.cl138_chargeStep_s19 = new TH1D("cl138_chargeStep_s19","TPC adc vs time sector#19",512,1,512);
  extras.cl139_chargeStep_s20 = new TH1D("cl139_chargeStep_s20","TPC adc vs time sector#20",512,1,512);
  extras.cl140_chargeStep_s21 = new TH1D("cl140_chargeStep_s21","TPC adc vs time sector#21",512,1,512);
  extras.cl141_chargeStep_s22 = new TH1D("cl141_chargeStep_s22","TPC adc vs time sector#22",512,1,512);
  extras.cl142_chargeStep_s23 = new TH1D("cl142_chargeStep_s23","TPC adc vs time sector#23",512,1,512);
  extras.cl143_chargeStep_s24 = new TH1D("cl143_chargeStep_s24","TPC adc vs time sector#24",512,1,512);
 
  extras.cl66_tpc_phi_charge = new TH1D("cl66_tpc_phi_charge","Azimuthal Distribution of TPC Charge",360,-180,180);
  extras.cl67_tpc_sector_charge = new TH1D("cl67_tpc_sector_charge","TPC Charge per Sector",24,0.5,24.5);
 
  // Add root histograms to Plots
  int np = sizeof(contents) / sizeof(TH1 *);
  JevpPlot *plots[np];

  int n=0;

  //plots[n] = new JevpPlot(contents.tpc_occ_physics);
  // plots[++n] = new JevpPlot(contents.h44_tpc_occ_laser);
  //plots[++n] = new JevpPlot(contents.h43_tpc_occ_pulser);

  plots[n] = new JevpPlot(extras.tpc_clpix_occ_physics);
  plots[n]->addHisto(contents.tpc_pix_occ_physics);
  extras.tpc_clpix_occ_physics->SetLineColor(kRed);
  contents.tpc_pix_occ_physics->SetLineColor(kGreen);
  plots[n]->setLegend(.78,.6,.98,.7);
  plots[n]->getHisto(1)->setLegText("adc's");
  plots[n]->getHisto(0)->setLegText("clusters");
  plots[n]->getHisto(1)->setLegArgs("l");
  plots[n]->getHisto(0)->setLegArgs("l");

  plots[++n] = new JevpPlot(extras.tpc_clpix_occ_laser);
  plots[n]->addHisto(contents.tpc_pix_occ_laser);
  extras.tpc_clpix_occ_laser->SetLineColor(kRed);
  contents.tpc_pix_occ_laser->SetLineColor(kGreen);
  plots[n]->setLegend(.78,.6,.98,.7);
  plots[n]->getHisto(1)->setLegText("adc's");
  plots[n]->getHisto(0)->setLegText("clusters");
  plots[n]->getHisto(1)->setLegArgs("l");
  plots[n]->getHisto(0)->setLegArgs("l");

  plots[++n] = new JevpPlot(extras.tpc_clpix_occ_pulser);
  plots[n]->addHisto(contents.tpc_pix_occ_pulser);
  extras.tpc_clpix_occ_pulser->SetLineColor(kRed);
  contents.tpc_pix_occ_pulser->SetLineColor(kGreen);
  plots[n]->setLegend(.78,.6,.98,.7);
  plots[n]->getHisto(1)->setLegText("adc's");
  plots[n]->getHisto(0)->setLegText("clusters");
  plots[n]->getHisto(1)->setLegArgs("l");
  plots[n]->getHisto(0)->setLegArgs("l");
  
  plots[++n] = new JevpPlot(contents.h15_tpc_sec1);
  
  plots[++n] = new JevpPlot(contents.h16_tpc_sec2);
  plots[++n] = new JevpPlot(contents.h17_tpc_sec3);
  plots[++n] = new JevpPlot(contents.h18_tpc_sec4);
  plots[++n] = new JevpPlot(contents.h19_tpc_sec5);
  plots[++n] = new JevpPlot(contents.h20_tpc_sec6);
  plots[++n] = new JevpPlot(contents.h21_tpc_sec7);
  plots[++n] = new JevpPlot(contents.h22_tpc_sec8);
  plots[++n] = new JevpPlot(contents.h23_tpc_sec9);
  plots[++n] = new JevpPlot(contents.h24_tpc_sec10);
  plots[++n] = new JevpPlot(contents.h25_tpc_sec11);
  plots[++n] = new JevpPlot(contents.h26_tpc_sec12);
  plots[++n] = new JevpPlot(contents.h27_tpc_sec13);
  plots[++n] = new JevpPlot(contents.h28_tpc_sec14);
  plots[++n] = new JevpPlot(contents.h29_tpc_sec15);
  plots[++n] = new JevpPlot(contents.h30_tpc_sec16);
  plots[++n] = new JevpPlot(contents.h31_tpc_sec17);
  plots[++n] = new JevpPlot(contents.h32_tpc_sec18);
  plots[++n] = new JevpPlot(contents.h33_tpc_sec19);
  plots[++n] = new JevpPlot(contents.h34_tpc_sec20);
  plots[++n] = new JevpPlot(contents.h35_tpc_sec21);
  plots[++n] = new JevpPlot(contents.h36_tpc_sec22);
  plots[++n] = new JevpPlot(contents.h37_tpc_sec23);
  plots[++n] = new JevpPlot(contents.h38_tpc_sec24);
  plots[++n] = new JevpPlot(contents.h120_chargeStep_s1);
 
  plots[n]->addHisto(extras.cl120_chargeStep_s1);
  plots[++n] = new JevpPlot(contents.h121_chargeStep_s2);
  plots[n]->addHisto(extras.cl121_chargeStep_s2);
  plots[++n] = new JevpPlot(contents.h122_chargeStep_s3);
  plots[n]->addHisto(extras.cl122_chargeStep_s3);
  plots[++n] = new JevpPlot(contents.h123_chargeStep_s4);
  plots[n]->addHisto(extras.cl123_chargeStep_s4);
  plots[++n] = new JevpPlot(contents.h124_chargeStep_s5);
  plots[n]->addHisto(extras.cl124_chargeStep_s5);
  plots[++n] = new JevpPlot(contents.h125_chargeStep_s6);
  plots[n]->addHisto(extras.cl125_chargeStep_s6);
  plots[++n] = new JevpPlot(contents.h126_chargeStep_s7);
  plots[n]->addHisto(extras.cl126_chargeStep_s7);
  plots[++n] = new JevpPlot(contents.h127_chargeStep_s8);
  plots[n]->addHisto(extras.cl127_chargeStep_s8);
  plots[++n] = new JevpPlot(contents.h128_chargeStep_s9);
  plots[n]->addHisto(extras.cl128_chargeStep_s9);
  plots[++n] = new JevpPlot(contents.h129_chargeStep_s10);
  plots[n]->addHisto(extras.cl129_chargeStep_s10);
  plots[++n] = new JevpPlot(contents.h130_chargeStep_s11);
  plots[n]->addHisto(extras.cl130_chargeStep_s11);
  plots[++n] = new JevpPlot(contents.h131_chargeStep_s12);
  plots[n]->addHisto(extras.cl131_chargeStep_s12);
  plots[++n] = new JevpPlot(contents.h132_chargeStep_s13);
  plots[n]->addHisto(extras.cl132_chargeStep_s13);
  plots[++n] = new JevpPlot(contents.h133_chargeStep_s14);
  plots[n]->addHisto(extras.cl133_chargeStep_s14);
  plots[++n] = new JevpPlot(contents.h134_chargeStep_s15);
  plots[n]->addHisto(extras.cl134_chargeStep_s15);
  plots[++n] = new JevpPlot(contents.h135_chargeStep_s16);
  plots[n]->addHisto(extras.cl135_chargeStep_s16);
  plots[++n] = new JevpPlot(contents.h136_chargeStep_s17);
  plots[n]->addHisto(extras.cl136_chargeStep_s17);
  plots[++n] = new JevpPlot(contents.h137_chargeStep_s18);
  plots[n]->addHisto(extras.cl137_chargeStep_s18);
  plots[++n] = new JevpPlot(contents.h138_chargeStep_s19);
  plots[n]->addHisto(extras.cl138_chargeStep_s19);
  plots[++n] = new JevpPlot(contents.h139_chargeStep_s20);
  plots[n]->addHisto(extras.cl139_chargeStep_s20);
  plots[++n] = new JevpPlot(contents.h140_chargeStep_s21);
  plots[n]->addHisto(extras.cl140_chargeStep_s21);
  plots[++n] = new JevpPlot(contents.h141_chargeStep_s22);
  plots[n]->addHisto(extras.cl141_chargeStep_s22);
  plots[++n] = new JevpPlot(contents.h142_chargeStep_s23);
  plots[n]->addHisto(extras.cl142_chargeStep_s23);
  plots[++n] = new JevpPlot(contents.h143_chargeStep_s24);
  plots[n]->addHisto(extras.cl143_chargeStep_s24);
 
  plots[++n] = new JevpPlot(contents.h102_tpc_drift_vel);
  //plots[++n] = new JevpPlot(contents.h113_tpc_drift_vel_dist);

  plots[++n] = new JevpPlot(contents.h66_tpc_phi_charge);
  plots[n]->addHisto(extras.cl66_tpc_phi_charge);
  plots[n]->optstat = 0;
  extras.cl66_tpc_phi_charge->SetLineColor(kRed);
  contents.h66_tpc_phi_charge->SetLineColor(kGreen);
  plots[n]->setLegend(.78,.6,.98,.7);
  plots[n]->getHisto(0)->setLegText("adc's");
  plots[n]->getHisto(1)->setLegText("clusters");
  plots[n]->getHisto(0)->setLegArgs("l");
  plots[n]->getHisto(1)->setLegArgs("l");

  plots[++n] = new JevpPlot(contents.h67_tpc_sector_charge);
  plots[n]->addHisto(extras.cl67_tpc_sector_charge);
  plots[n]->optstat = 0;
  extras.cl67_tpc_sector_charge->SetLineColor(kRed);
  contents.h67_tpc_sector_charge->SetLineColor(kGreen);
  plots[n]->setLegend(.78,.6,.98,.7);
  plots[n]->getHisto(0)->setLegText("adc's");
  plots[n]->getHisto(1)->setLegText("clusters");
  plots[n]->getHisto(0)->setLegArgs("l");
  plots[n]->getHisto(1)->setLegArgs("l");

  long q_idx = ((long)&contents.h15_tpc_sec1 - (long)contents.array) / (sizeof(TH1 *));
  long qs_idx = ((long)&contents.h120_chargeStep_s1 - (long)contents.array) / (sizeof(TH1 *));
  long cl_qs_idx = ((long)&extras.cl120_chargeStep_s1 - (long)extras.array) / (sizeof(TH1 *));
 
  for(int i=0;i<24;i++) {
    plots[i+q_idx]->setDrawOpts((char *)"colz");
    plots[i+q_idx]->optstat = 0;

    plots[i+qs_idx]->optstat = 0;
    plots[i+qs_idx]->setLegend(.7,.8,.95,.95);
    plots[i+qs_idx]->getHisto(0)->setLegText("adc's");
    plots[i+qs_idx]->getHisto(1)->setLegText("clusters");
    plots[i+qs_idx]->getHisto(0)->setLegArgs("l");
    plots[i+qs_idx]->getHisto(1)->setLegArgs("l");

    contents.array[i+qs_idx]->SetLineColor(kGreen);

    extras.array[i+cl_qs_idx]->SetLineColor(kRed);
  }

  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    addPlot(plots[i]);
  } 
}
  
void tpxBuilder::startrun(daqReader *rdr) {
  LOG("JEFF", "tpxBuilder starting run #%d",rdr->run);
  resetAllPlots();
  laserReader->resetAll();
  n_cld = 0;
  n_adc = 0;
  nlasers = 0;
  drift_vel = 0;
  run = rdr->run;
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void tpxBuilder::event(daqReader *rdr)
{
  int has_adc=0;
  int has_cld=0;

  // printf("aa\n");
  long q_idx = ((long)&contents.h15_tpc_sec1 - (long)contents.array) / (sizeof(TH1 *));
  long qs_idx = ((long)&contents.h120_chargeStep_s1 - (long)contents.array) / (sizeof(TH1 *));
  long cl_qs_idx = ((long)&extras.cl120_chargeStep_s1 - (long)extras.array) / (sizeof(TH1 *));

  // printf("q qs cl:  %d %d %d 0x%x 0x%x\n",q_idx,qs_idx,cl_qs_idx,&extras.cl120_chargeStep_s1,extras.array);
  // printf("bb\n"); 

  double pixel_count = 0;
  double channel_count = 0;
  double cluster_count = 0;
  double charge_count = 0;
  double charge_count_sector;
  double tpc_max_channels = 0.0;
  int tpc_max_channels_sector = 5692;

  double cl_max_channels = 0;

  double pix_count_cl = 0;

  // printf("cc\n");
  for(int s=1;s<=24;s++) {

    charge_count_sector = 0;
    int channel_counts[183][46];
    double charge_counts[183][46];
    double tb_charge_counts[512];

    memset(channel_counts, 0, sizeof(channel_counts));
    memset(charge_counts, 0, sizeof(charge_counts));
    memset(tb_charge_counts, 0, sizeof(tb_charge_counts));

    daq_dta *dd = rdr->det("tpx")->get("adc",s) ;
    if(dd) {   // regular data...
      has_adc = 1;
      tpc_max_channels += tpc_max_channels_sector;

      while(dd->iterate()) {
  	channel_counts[dd->pad][dd->row] = 1;

	pixel_count += dd->ncontent ;

	if(dd->ncontent > 0) {
	  channel_counts[dd->pad][dd->row] = 1;
	}

	for(u_int i=0;i<dd->ncontent;i++) {
	  int tb = dd->adc[i].tb;
	  int adc = dd->adc[i].adc;

	  if((dd->pad >= 183) ||
	     (dd->row >= 46) ||
	     (tb >= 512)) {
	    LOG(ERR, "pad=%d row=%d tb=%d out of range.  Ignore.", dd->pad, dd->row, tb);
	  }
	  else {
	    charge_counts[dd->pad][dd->row] += adc;
	    tb_charge_counts[tb] += adc;
	  }
	}
      }

      for(int i=0;i<512;i++) {
	contents.array[s + qs_idx - 1]->Fill(i,tb_charge_counts[i]);
      }
      
      for(int i=1;i<183;i++) {
	for(int j=1;j<46;j++) {
	  // printf("c\n");
	  channel_count += channel_counts[i][j];
	  charge_count += charge_counts[i][j];
	  charge_count_sector += charge_counts[i][j];
	  
	  if(charge_counts[i][j]) {
	    contents.h66_tpc_phi_charge->Fill(mPhiAngleMap[s-1][j-1][i-1],charge_counts[i][j]);
	    ((TH2D *)contents.array[s + q_idx - 1])->Fill(i, j, charge_counts[i][j]);
	  }
	}
      }

      contents.h67_tpc_sector_charge->Fill(s,charge_count_sector);      
    }

    double charge_cl=0;
    double charge_counts_cl[183][46];
    double tb_charge_counts_cl[512];
    memset(charge_counts_cl, 0, sizeof(charge_counts_cl));
    memset(tb_charge_counts_cl, 0, sizeof(tb_charge_counts_cl));

    dd = rdr->det("tpx")->get("cld",s) ;
    if(dd) {
      has_cld = 1;
      cl_max_channels += tpc_max_channels_sector;

      while(dd->iterate()) {

	for(u_int i=0;i<dd->ncontent;i++) {
	  pix_count_cl += (dd->cld[i].t2 - dd->cld[i].t1)*(dd->cld[i].p2 - dd->cld[i].p1);
	  charge_counts_cl[(int)dd->cld[i].pad][dd->row] += dd->cld[i].charge;
	  tb_charge_counts_cl[(int)dd->cld[i].tb] += dd->cld[i].charge;
	  charge_cl += dd->cld[i].charge;
	}
      }
    
      //printf("cl_qs_idx = %d\n",cl_qs_idx);

      if(has_cld) {
	for(int i=0;i<512;i++) {
	  extras.array[s + cl_qs_idx - 1]->Fill(i,tb_charge_counts_cl[i]);
	}
       
	for(int i=1;i<183;i++) {
	  for(int j=1;j<46;j++) {
	    if(charge_counts_cl[i][j]) {
	      extras.cl66_tpc_phi_charge->Fill(mPhiAngleMap[s-1][j-1][i-1],charge_counts_cl[i][j]);
	    }
	  }
	}
	  
	extras.cl67_tpc_sector_charge->Fill(s,charge_cl);
      }
    }

    /***** no pedestal handling currently!

    // will only exist in token 0 of a pedestal run!
    dd = rdr->det("tpx")->get("pedrms",s) ;
    while(dd && dd->iterate()) {
    //       found = 1 ;
    //       if(do_print) {
    // 	printf("TPX: sec %02d, row %2d, pad %3d (%d pix)\n",dd->sec,dd->row,dd->pad,dd->ncontent) ;
    // 	daq_det_pedrms *ped = (daq_det_pedrms *)dd->Void ;
    // 	for(u_int tb=0;tb<dd->ncontent;tb++) {
    // 	  printf("  tb %3d: ped %3d, rms %.2f\n",tb,ped[tb].ped,ped[tb].rms) ;
    // 	}
    //       }
    }

    */
  }

  //printf("summaries\n");

  // Summaries are completed...
  //printf("%d channel counts:   %lf (%lf)\n",rdr->seq, channel_count, pixel_count);  
  
    
  if(has_adc) {

    n_adc++;
    double adc_scale = (double)(n_adc-1) / (double)n_adc;
    if(n_adc == 1) adc_scale = 1;

    for(int i=1;i<=24;i++) {
      contents.array[i + qs_idx - 1]->Scale(adc_scale);
    }
    contents.h66_tpc_phi_charge->Scale(adc_scale);
    contents.h67_tpc_sector_charge->Scale(adc_scale);
  }

  if(has_cld) {
    n_cld++;   
    double cld_scale = (double)(n_cld-1) / (double)n_cld;
    if(n_cld == 1) cld_scale = 1;

    for(int i=1;i<=24;i++) {
      extras.array[i + cl_qs_idx - 1]->Scale(cld_scale);
    }
    extras.cl66_tpc_phi_charge->Scale(cld_scale);
    extras.cl67_tpc_sector_charge->Scale(cld_scale);
  }

  switch(rdr->trgcmd) {
  case 4:
    //contents.tpc_occ_physics->Fill(100.0 * (double)channel_count / tpc_max_channels);
    contents.tpc_pix_occ_physics->Fill(100.0 * (double)pixel_count / (tpc_max_channels * 400.0));

    extras.tpc_clpix_occ_physics->Fill(100.0 * (double)pix_count_cl / (cl_max_channels * 400.0));

    //printf("chan=%lf pix=%lf max=%lf:  %lf %lf\n",channel_count,pixel_count,tpc_max_channels,channel_count/tpc_max_channels,pixel_count/(tpc_max_channels*400));
    break;
  case 8:  // Lasers...
  case 9:
    {
      
      LOG("JEFF", "Got a laser...");

      //contents.h44_tpc_occ_laser->Fill(100.0 * (double)channel_count / tpc_max_channels);
      contents.tpc_pix_occ_laser->Fill(100.0 * (double)pixel_count / (tpc_max_channels * 400.0));
      extras.tpc_clpix_occ_laser->Fill(100.0 * (double)pix_count_cl / (cl_max_channels * 400.0));

      double vDrift = laserReader->Make(rdr);
     
      LOG("JEFF","%d vDrift = %lf",rdr->event_number, vDrift);

      if((vDrift > 5.4) && (vDrift < 5.8)) {
	nlasers++;
	contents.h102_tpc_drift_vel->Fill(vDrift);
      }

      drift_vel = contents.h102_tpc_drift_vel->GetMean();

      LOG("JEFF", "run=%d nlasers: %d curr_drift=%lf", run, nlasers, drift_vel);
      //if(nlasers == 50) {
      if(1) {    // inneficient!  write all of them :-)
	FILE *f = fopen("/RTS/conf/handler/.l4_drift_velocity","w");
	if(f) {
	  fprintf(f, "%lf", drift_vel);
	  fclose(f);
	}
	f = fopen("/RTS/conf/handler/.l4_drift_velocity_run","w");
	if(f) {
	  fprintf(f, "%d", run);
	  fclose(f);
	}	
      }
      
    }
    break;

  case 10:   // Pulsers..
    //contents.h43_tpc_occ_pulser->Fill(100.0 * (double)channel_count / tpc_max_channels);
    contents.tpc_pix_occ_pulser->Fill(100.0 * (double)pixel_count / (tpc_max_channels * 400.0));
    extras.tpc_clpix_occ_pulser->Fill(100.0 * (double)pix_count_cl / (cl_max_channels * 400.0));

    break;
  default:
    LOG(WARN, "Trigger command other than 4,8,9,10: evt %d, token=%d, cmd=%d",rdr->seq, rdr->token, rdr->trgcmd);
  }

  // Normalize the charge histos
  //
  // adc vs sector
  // phy disto
    
}

  void tpxBuilder::setPhiAngleMap()
  {
    const Int_t NP = 45; // # padrows
    const Float_t DEG = 57.296;//360/2pi

    Double_t Xpads[NP] = { 
      60.0, 64.8, 69.6, 74.4, 79.2, 84.0, 88.8, 
      93.6, 98.8,104.0,109.2,114.4,119.6, // inner Centres
      127.195, 129.195, 131.195, 133.195, 135.195, //Outer
      137.195, 139.195, 141.195, 143.195, 145.195,
      147.195, 149.195, 151.195, 153.195, 155.195,
      157.195, 159.195, 161.195, 163.195, 165.195,
      167.195, 169.195, 171.195, 173.195, 175.195,
      177.195, 179.195, 181.195, 183.195, 185.195,
      187.195, 189.195};   


    Double_t pitches[2] = {0.335, 0.67};


    //Note from GENE
    //So within any supersector, I have a local X and Y.  X you can get from
    //Xpads above, below I have YMIN (the lower Y coordinate of each pad;
    //you would need to add half the "pitch" to get the Y center of each pad)

    float YMIN;
    float pitch;
    float phi0=60;
    float LPhi;//local phi
    float SPhi;//sector phi
    float GPhi;//global phi

    //loop over sectors and find SPhi= phi in middle of sector
    for (int sec = 0; sec < 24; sec++) {
      if (sec<12) {
	SPhi = phi0 - (sec*30);
	if (SPhi<-180) SPhi+=360;
      }
      if (sec>=12) {
	SPhi = phi0 + ((sec-10)*30);
	if (SPhi>180) SPhi-=360;
      } 
   
      //loop over each padrow in a sector
      for (int j=0; j<45; j++) {
	if (j >= 13) pitch = pitches[1];
	else pitch = pitches[0];
	for (int k=0; k<NTPCpads[j]; k++) {//loop over # pads in each padrow
	  YMIN = pitch * (k - 0.5*NTPCpads[j]);//find Y at bottom of padrow
	  LPhi=atan(YMIN/Xpads[j]);//find local Phi (LPhi) within sector
	  LPhi*=DEG;
	  GPhi=LPhi+SPhi;//find global Phi (GPhi) 

	  //oth->fill( 	 h1,LPhi);
	  //oth->fill(          h2,Xpads[j],YMIN);
	  //oth->fill( 	 h3,Xpads[j],LPhi);
	  //oth->fill( 	 h4,YMIN,LPhi);
	  //oth->fill( 	 h5,GPhi);
	  //oth->fill( 	 h6,sec,SPhi);

	  //Fill Look up table for pad phi angle

	  mPhiAngleMap[sec][j][k]=GPhi;


	}//pad
      }//padrow
    }// sector
  }

  void tpxBuilder::main(int argc, char *argv[])
  {
    tpxBuilder me;
  
    me.Main(argc, argv);
  }

