// $Id: itpcBuilder.cxx,v 1.4 2018/02/24 15:50:40 videbaks Exp $
//
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
#include "itpcBuilder.h"
#include <RTS/include/rtsLog.h>
#include "LaserReader.h"


// ITPC Jevp builder
//
//  Author: F.Videbaek
//  Help from Jeff to have it setup.
// Initial version just copied from the TPX with modifications from
// #rows, pad size etc.
//
// There is a change that only the inner sector are dealt with. It will read from the
// itpc subdetector only and thus have max 40 rows
//
// Not sure what to do about laser runs. Sector 20 has lasers.
// Flag to skip the rdr->get("cld") produce error for each event
// change to 1 when clusters are ready 
// skip the laser cal for now - revisit for run19
//
#define checkcld 0
#define checklaser 0
//#define fv 1

ClassImp(itpcBuilder);
  
//
// 
static const int NTPCpads[40] = {
    52, 54, 56, 58, 60, 62, 62, 64, 66, 68,
    70, 71, 74, 74, 76, 78, 80, 82, 84, 86,
    86, 88, 90, 92, 94, 96, 98, 98,100,102,
   104,106,108,110,110,112,114,116,118,120
};

static const int Nrows = 40;
  static const int Npads = 120;
  static const int Nrows1 = Nrows+1;
  static const int Npads1 = Npads+1;

itpcBuilder::itpcBuilder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"itpc";

  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
  memset(&extras, 0, sizeof(extras));

  setPhiAngleMap();
  // should this be included?
  laserReader = new LaserReader();
}

itpcBuilder::~itpcBuilder() {

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

void itpcBuilder::initialize(int argc, char *argv[]) {

  //contents.itpc_occ_physics = new TH1D("itpc_physics","iTPC Channel Occupancy (in %) Physics",100,0,100);
  //contents.h44_itpc_occ_laser = new TH1D("h44_itpc_occ_laser","iTPC Channel Occupancy (in %) Lasers",100,0,100);
  //contents.h43_itpc_occ_pulser = new TH1D("h43_itpc_occ_pulser","iTPC Channel Occupancy (in %) Pulsers",100,0,100);
  contents.itpc_pix_occ_physics = new TH1D("itpc_pix_occ_physics","iTPC Pixel Occupancy (in %) Physics",100,0,2.5);
  contents.itpc_pix_occ_laser = new TH1D("itpc_pix_occ_laser","iTPC Pixel Occupancy (in %) Lasers",100,0,1);
  contents.itpc_pix_occ_pulser = new TH1D("itpc_pix_occ_pulser","iTPC Pixel Occupancy (in %) Pulsers",100,0,10);
  contents.h15_itpc_sec1 = new TH2D("h15_itpc_sec1","iTPC Sec. 1 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h16_itpc_sec2 = new TH2D("h16_itpc_sec2","iTPC Sec. 2 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h17_itpc_sec3 = new TH2D("h17_itpc_sec3","iTPC Sec. 3 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h18_itpc_sec4 = new TH2D("h18_itpc_sec4","iTPC Sec. 4 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h19_itpc_sec5 = new TH2D("h19_itpc_sec5","iTPC Sec. 5 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h20_itpc_sec6 = new TH2D("h20_itpc_sec6","iTPC Sec. 6 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h21_itpc_sec7 = new TH2D("h21_itpc_sec7","iTPC Sec. 7 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h22_itpc_sec8 = new TH2D("h22_itpc_sec8","iTPC Sec. 8 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h23_itpc_sec9 = new TH2D("h23_itpc_sec9","iTPC Sec. 9 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h24_itpc_sec10 = new TH2D("h24_itpc_sec10","iTPC Sec. 10 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h25_itpc_sec11 = new TH2D("h25_itpc_sec11","iTPC Sec. 11 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h26_itpc_sec12 = new TH2D("h26_itpc_sec12","iTPC Sec. 12 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h27_itpc_sec13 = new TH2D("h27_itpc_sec13","iTPC Sec. 13 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h28_itpc_sec14 = new TH2D("h28_itpc_sec14","iTPC Sec. 14 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h29_itpc_sec15 = new TH2D("h29_itpc_sec15","iTPC Sec. 15 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h30_itpc_sec16 = new TH2D("h30_itpc_sec16","iTPC Sec. 16 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h31_itpc_sec17 = new TH2D("h31_itpc_sec17","iTPC Sec. 17 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h32_itpc_sec18 = new TH2D("h32_itpc_sec18","iTPC Sec. 18 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h33_itpc_sec19 = new TH2D("h33_itpc_sec19","iTPC Sec. 19 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h34_itpc_sec20 = new TH2D("h34_itpc_sec20","iTPC Sec. 20 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h35_itpc_sec21 = new TH2D("h35_itpc_sec21","iTPC Sec. 21 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h36_itpc_sec22 = new TH2D("h36_itpc_sec22","iTPC Sec. 22 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h37_itpc_sec23 = new TH2D("h37_itpc_sec23","iTPC Sec. 23 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h38_itpc_sec24 = new TH2D("h38_itpc_sec24","iTPC Sec. 24 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);  
  contents.h120_itpc_chargeStep_s1 = new TH1D("h120_itpc_chargeStep_s1","iTPC adc vs time sector#1",512,0,512);
  contents.h121_itpc_chargeStep_s2 = new TH1D("h121_itpc_chargeStep_s2","iTPC adc vs time sector#2",512,0,512);
  contents.h122_itpc_chargeStep_s3 = new TH1D("h122_itpc_chargeStep_s3","iTPC adc vs time sector#3",512,0,512);
  contents.h123_itpc_chargeStep_s4 = new TH1D("h123_itpc_chargeStep_s4","iTPC adc vs time sector#4",512,0,512);
  contents.h124_itpc_chargeStep_s5 = new TH1D("h124_itpc_chargeStep_s5","iTPC adc vs time sector#5",512,0,512);
  contents.h125_itpc_chargeStep_s6 = new TH1D("h125_itpc_chargeStep_s6","iTPC adc vs time sector#6",512,0,512);
  contents.h126_itpc_chargeStep_s7 = new TH1D("h126_itpc_chargeStep_s7","iTPC adc vs time sector#7",512,0,512);
  contents.h127_itpc_chargeStep_s8 = new TH1D("h127_itpc_chargeStep_s8","iTPC adc vs time sector#8",512,0,512);
  contents.h128_itpc_chargeStep_s9 = new TH1D("h128_itpc_chargeStep_s9","iTPC adc vs time sector#9",512,0,512);
  contents.h129_itpc_chargeStep_s10 = new TH1D("h129_itpc_chargeStep_s10","iTPC adc vs time sector#10",512,0,512);
  contents.h130_itpc_chargeStep_s11 = new TH1D("h130_itpc_chargeStep_s11","iTPC adc vs time sector#11",512,0,512);
  contents.h131_itpc_chargeStep_s12 = new TH1D("h131_itpc_chargeStep_s12","iTPC adc vs time sector#12",512,0,512);
  contents.h132_itpc_chargeStep_s13 = new TH1D("h132_itpc_chargeStep_s13","iTPC adc vs time sector#13",512,0,512);
  contents.h133_itpc_chargeStep_s14 = new TH1D("h133_itpc_chargeStep_s14","iTPC adc vs time sector#14",512,0,512);
  contents.h134_itpc_chargeStep_s15 = new TH1D("h134_itpc_chargeStep_s15","iTPC adc vs time sector#15",512,0,512);
  contents.h135_itpc_chargeStep_s16 = new TH1D("h135_itpc_chargeStep_s16","iTPC adc vs time sector#16",512,0,512);
  contents.h136_itpc_chargeStep_s17 = new TH1D("h136_itpc_chargeStep_s17","iTPC adc vs time sector#17",512,0,512);
  contents.h137_itpc_chargeStep_s18 = new TH1D("h137_itpc_chargeStep_s18","iTPC adc vs time sector#18",512,0,512);
  contents.h138_itpc_chargeStep_s19 = new TH1D("h138_itpc_chargeStep_s19","iTPC adc vs time sector#19",512,0,512);
  contents.h139_itpc_chargeStep_s20 = new TH1D("h139_itpc_chargeStep_s20","iTPC adc vs time sector#20",512,0,512);
  contents.h140_itpc_chargeStep_s21 = new TH1D("h140_itpc_chargeStep_s21","iTPC adc vs time sector#21",512,0,512);
  contents.h141_itpc_chargeStep_s22 = new TH1D("h141_itpc_chargeStep_s22","iTPC adc vs time sector#22",512,0,512);
  contents.h142_itpc_chargeStep_s23 = new TH1D("h142_itpc_chargeStep_s23","iTPC adc vs time sector#23",512,0,512);
  contents.h143_itpc_chargeStep_s24 = new TH1D("h143_itpc_chargeStep_s24","iTPC adc vs time sector#24",512,0,512);
 
  contents.h102_itpc_drift_vel = new TH1D("h102_itpc_drift_vel", "iTPC Drift Velocity (cm/us)",400,5.4,5.8);
  // contents.h113_itpc_drift_vel_dist = new TH1D("113_itpc_drift_vel_dist", "iTPC Drift Velocity Distribution(cm/us)",200,4,8);
  contents.h66_itpc_phi_charge = new TH1D("h66_itpc_phi_charge","Azimuthal Distribution of TPC Charge",360,-180,180);
  contents.h67_itpc_sector_charge = new TH1D("h67_itpc_sector_charge","iTPC Charge per Sector",24,0.5,24.5);
  
  // cluster based vesions...
  extras.itpc_clpix_occ_physics = new TH1D("itpc_clpix_occ_physics","iTPC Pixel Occupancy (in %) Physics",100,0,2.5);
  extras.itpc_clpix_occ_laser = new TH1D("itpc_clpix_occ_laser","iTPC Pixel Occupancy (in %) Lasers",100,0,1);
  extras.itpc_clpix_occ_pulser = new TH1D("itpc_clpix_occ_pulser","iTPC Pixel Occupancy (in %) Pulsers",100,0,10);
  extras.cl120_itpc_chargeStep_s1 = new TH1D("cl120_itpc_chargeStep_s1","iTPC adc vs time sector#1",512,0,512);
  extras.cl121_itpc_chargeStep_s2 = new TH1D("cl121_itpc_chargeStep_s2","iTPC adc vs time sector#2",512,0,512);
  extras.cl122_itpc_chargeStep_s3 = new TH1D("cl122_itpc_chargeStep_s3","iTPC adc vs time sector#3",512,0,512);
  extras.cl123_itpc_chargeStep_s4 = new TH1D("cl123_itpc_chargeStep_s4","iTPC adc vs time sector#4",512,0,512);
  extras.cl124_itpc_chargeStep_s5 = new TH1D("cl124_itpc_chargeStep_s5","iTPC adc vs time sector#5",512,0,512);
  extras.cl125_itpc_chargeStep_s6 = new TH1D("cl125_itpc_chargeStep_s6","iTPC adc vs time sector#6",512,0,512);
  extras.cl126_itpc_chargeStep_s7 = new TH1D("cl126_itpc_chargeStep_s7","iTPC adc vs time sector#7",512,0,512);
  extras.cl127_itpc_chargeStep_s8 = new TH1D("cl127_itpc_chargeStep_s8","iTPC adc vs time sector#8",512,0,512);
  extras.cl128_itpc_chargeStep_s9 = new TH1D("cl128_itpc_chargeStep_s9","iTPC adc vs time sector#9",512,0,512);
  extras.cl129_itpc_chargeStep_s10 = new TH1D("cl129_itpc_chargeStep_s10","iTPC adc vs time sector#10",512,0,512);
  extras.cl130_itpc_chargeStep_s11 = new TH1D("cl130_itpc_chargeStep_s11","iTPC adc vs time sector#11",512,0,512);
  extras.cl131_itpc_chargeStep_s12 = new TH1D("cl131_itpc_chargeStep_s12","iTPC adc vs time sector#12",512,0,512);
  extras.cl132_itpc_chargeStep_s13 = new TH1D("cl132_itpc_chargeStep_s13","iTPC adc vs time sector#13",512,0,512);
  extras.cl133_itpc_chargeStep_s14 = new TH1D("cl133_itpc_chargeStep_s14","iTPC adc vs time sector#14",512,0,512);
  extras.cl134_itpc_chargeStep_s15 = new TH1D("cl134_itpc_chargeStep_s15","iTPC adc vs time sector#15",512,0,512);
  extras.cl135_itpc_chargeStep_s16 = new TH1D("cl135_itpc_chargeStep_s16","iTPC adc vs time sector#16",512,0,512);
  extras.cl136_itpc_chargeStep_s17 = new TH1D("cl136_itpc_chargeStep_s17","iTPC adc vs time sector#17",512,0,512);
  extras.cl137_itpc_chargeStep_s18 = new TH1D("cl137_itpc_chargeStep_s18","iTPC adc vs time sector#18",512,0,512);
  extras.cl138_itpc_chargeStep_s19 = new TH1D("cl138_itpc_chargeStep_s19","iTPC adc vs time sector#19",512,0,512);
  extras.cl139_itpc_chargeStep_s20 = new TH1D("cl139_itpc_chargeStep_s20","iTPC adc vs time sector#20",512,0,512);
  extras.cl140_itpc_chargeStep_s21 = new TH1D("cl140_itpc_chargeStep_s21","iTPC adc vs time sector#21",512,0,512);
  extras.cl141_itpc_chargeStep_s22 = new TH1D("cl141_itpc_chargeStep_s22","iTPC adc vs time sector#22",512,0,512);
  extras.cl142_itpc_chargeStep_s23 = new TH1D("cl142_itpc_chargeStep_s23","iTPC adc vs time sector#23",512,0,512);
  extras.cl143_itpc_chargeStep_s24 = new TH1D("cl143_itpc_chargeStep_s24","iTPC adc vs time sector#24",512,0,512);
 
  extras.cl66_itpc_phi_charge = new TH1D("cl66_itpc_phi_charge","Azimuthal Distribution of iTPC Charge",360,-180,180);
  extras.cl67_itpc_sector_charge = new TH1D("cl67_itpc_sector_charge","iTPC Charge per Sector",24,0.5,24.5);
 
  // Add root histograms to Plots
  int np = sizeof(contents) / sizeof(TH1 *);
  JevpPlot *plots[np];

  int n=0;

  //plots[n] = new JevpPlot(contents.itpc_occ_physics);
  // plots[++n] = new JevpPlot(contents.h44_itpc_occ_laser);
  //plots[++n] = new JevpPlot(contents.h43_itpc_occ_pulser);

  plots[n] = new JevpPlot(extras.itpc_clpix_occ_physics);
  plots[n]->addHisto(contents.itpc_pix_occ_physics);
  extras.itpc_clpix_occ_physics->SetLineColor(kRed);
  contents.itpc_pix_occ_physics->SetLineColor(kGreen);
  plots[n]->setLegend(.78,.6,.98,.7);
  plots[n]->getHisto(1)->setLegText("adc's");
  plots[n]->getHisto(0)->setLegText("clusters");
  plots[n]->getHisto(1)->setLegArgs("l");
  plots[n]->getHisto(0)->setLegArgs("l");

  plots[++n] = new JevpPlot(extras.itpc_clpix_occ_laser);
  plots[n]->addHisto(contents.itpc_pix_occ_laser);
  extras.itpc_clpix_occ_laser->SetLineColor(kRed);
  contents.itpc_pix_occ_laser->SetLineColor(kGreen);
  plots[n]->setLegend(.78,.6,.98,.7);
  plots[n]->getHisto(1)->setLegText("adc's");
  plots[n]->getHisto(0)->setLegText("clusters");
  plots[n]->getHisto(1)->setLegArgs("l");
  plots[n]->getHisto(0)->setLegArgs("l");

  plots[++n] = new JevpPlot(extras.itpc_clpix_occ_pulser);
  plots[n]->addHisto(contents.itpc_pix_occ_pulser);
  extras.itpc_clpix_occ_pulser->SetLineColor(kRed);
  contents.itpc_pix_occ_pulser->SetLineColor(kGreen);
  plots[n]->setLegend(.78,.6,.98,.7);
  plots[n]->getHisto(1)->setLegText("adc's");
  plots[n]->getHisto(0)->setLegText("clusters");
  plots[n]->getHisto(1)->setLegArgs("l");
  plots[n]->getHisto(0)->setLegArgs("l");
  
  plots[++n] = new JevpPlot(contents.h15_itpc_sec1);
  plots[++n] = new JevpPlot(contents.h16_itpc_sec2);
  plots[++n] = new JevpPlot(contents.h17_itpc_sec3);
  plots[++n] = new JevpPlot(contents.h18_itpc_sec4);
  plots[++n] = new JevpPlot(contents.h19_itpc_sec5);
  plots[++n] = new JevpPlot(contents.h20_itpc_sec6);
  plots[++n] = new JevpPlot(contents.h21_itpc_sec7);
  plots[++n] = new JevpPlot(contents.h22_itpc_sec8);
  plots[++n] = new JevpPlot(contents.h23_itpc_sec9);
  plots[++n] = new JevpPlot(contents.h24_itpc_sec10);
  plots[++n] = new JevpPlot(contents.h25_itpc_sec11);
  plots[++n] = new JevpPlot(contents.h26_itpc_sec12);
  plots[++n] = new JevpPlot(contents.h27_itpc_sec13);
  plots[++n] = new JevpPlot(contents.h28_itpc_sec14);
  plots[++n] = new JevpPlot(contents.h29_itpc_sec15);
  plots[++n] = new JevpPlot(contents.h30_itpc_sec16);
  plots[++n] = new JevpPlot(contents.h31_itpc_sec17);
  plots[++n] = new JevpPlot(contents.h32_itpc_sec18);
  plots[++n] = new JevpPlot(contents.h33_itpc_sec19);
  plots[++n] = new JevpPlot(contents.h34_itpc_sec20);
  plots[++n] = new JevpPlot(contents.h35_itpc_sec21);
  plots[++n] = new JevpPlot(contents.h36_itpc_sec22);
  plots[++n] = new JevpPlot(contents.h37_itpc_sec23);
  plots[++n] = new JevpPlot(contents.h38_itpc_sec24);
  plots[++n] = new JevpPlot(contents.h120_itpc_chargeStep_s1);
 
  plots[n]->addHisto(extras.cl120_itpc_chargeStep_s1);
  plots[++n] = new JevpPlot(contents.h121_itpc_chargeStep_s2);
  plots[n]->addHisto(extras.cl121_itpc_chargeStep_s2);
  plots[++n] = new JevpPlot(contents.h122_itpc_chargeStep_s3);
  plots[n]->addHisto(extras.cl122_itpc_chargeStep_s3);
  plots[++n] = new JevpPlot(contents.h123_itpc_chargeStep_s4);
  plots[n]->addHisto(extras.cl123_itpc_chargeStep_s4);
  plots[++n] = new JevpPlot(contents.h124_itpc_chargeStep_s5);
  plots[n]->addHisto(extras.cl124_itpc_chargeStep_s5);
  plots[++n] = new JevpPlot(contents.h125_itpc_chargeStep_s6);
  plots[n]->addHisto(extras.cl125_itpc_chargeStep_s6);
  plots[++n] = new JevpPlot(contents.h126_itpc_chargeStep_s7);
  plots[n]->addHisto(extras.cl126_itpc_chargeStep_s7);
  plots[++n] = new JevpPlot(contents.h127_itpc_chargeStep_s8);
  plots[n]->addHisto(extras.cl127_itpc_chargeStep_s8);
  plots[++n] = new JevpPlot(contents.h128_itpc_chargeStep_s9);
  plots[n]->addHisto(extras.cl128_itpc_chargeStep_s9);
  plots[++n] = new JevpPlot(contents.h129_itpc_chargeStep_s10);
  plots[n]->addHisto(extras.cl129_itpc_chargeStep_s10);
  plots[++n] = new JevpPlot(contents.h130_itpc_chargeStep_s11);
  plots[n]->addHisto(extras.cl130_itpc_chargeStep_s11);
  plots[++n] = new JevpPlot(contents.h131_itpc_chargeStep_s12);
  plots[n]->addHisto(extras.cl131_itpc_chargeStep_s12);
  plots[++n] = new JevpPlot(contents.h132_itpc_chargeStep_s13);
  plots[n]->addHisto(extras.cl132_itpc_chargeStep_s13);
  plots[++n] = new JevpPlot(contents.h133_itpc_chargeStep_s14);
  plots[n]->addHisto(extras.cl133_itpc_chargeStep_s14);
  plots[++n] = new JevpPlot(contents.h134_itpc_chargeStep_s15);
  plots[n]->addHisto(extras.cl134_itpc_chargeStep_s15);
  plots[++n] = new JevpPlot(contents.h135_itpc_chargeStep_s16);
  plots[n]->addHisto(extras.cl135_itpc_chargeStep_s16);
  plots[++n] = new JevpPlot(contents.h136_itpc_chargeStep_s17);
  plots[n]->addHisto(extras.cl136_itpc_chargeStep_s17);
  plots[++n] = new JevpPlot(contents.h137_itpc_chargeStep_s18);
  plots[n]->addHisto(extras.cl137_itpc_chargeStep_s18);
  plots[++n] = new JevpPlot(contents.h138_itpc_chargeStep_s19);
  plots[n]->addHisto(extras.cl138_itpc_chargeStep_s19);
  plots[++n] = new JevpPlot(contents.h139_itpc_chargeStep_s20);
  plots[n]->addHisto(extras.cl139_itpc_chargeStep_s20);
  plots[n]->logy = 1;
  plots[++n] = new JevpPlot(contents.h140_itpc_chargeStep_s21);
  plots[n]->addHisto(extras.cl140_itpc_chargeStep_s21);
  plots[++n] = new JevpPlot(contents.h141_itpc_chargeStep_s22);
  plots[n]->addHisto(extras.cl141_itpc_chargeStep_s22);
  plots[++n] = new JevpPlot(contents.h142_itpc_chargeStep_s23);
  plots[n]->addHisto(extras.cl142_itpc_chargeStep_s23);
  plots[++n] = new JevpPlot(contents.h143_itpc_chargeStep_s24);
  plots[n]->addHisto(extras.cl143_itpc_chargeStep_s24);
 
  plots[++n] = new JevpPlot(contents.h102_itpc_drift_vel);
  //plots[++n] = new JevpPlot(contents.h113_itpc_drift_vel_dist);

  plots[++n] = new JevpPlot(contents.h66_itpc_phi_charge);
  plots[n]->addHisto(extras.cl66_itpc_phi_charge);
  plots[n]->optstat = 0;
  extras.cl66_itpc_phi_charge->SetLineColor(kRed);
  contents.h66_itpc_phi_charge->SetLineColor(kGreen);
  plots[n]->setLegend(.78,.6,.98,.7);
  plots[n]->getHisto(0)->setLegText("adc's");
  plots[n]->getHisto(1)->setLegText("clusters");
  plots[n]->getHisto(0)->setLegArgs("l");
  plots[n]->getHisto(1)->setLegArgs("l");

  plots[++n] = new JevpPlot(contents.h67_itpc_sector_charge);
  plots[n]->addHisto(extras.cl67_itpc_sector_charge);
  plots[n]->optstat = 0;
  extras.cl67_itpc_sector_charge->SetLineColor(kRed);
  contents.h67_itpc_sector_charge->SetLineColor(kGreen);
  plots[n]->setLegend(.78,.6,.98,.7);
  plots[n]->getHisto(0)->setLegText("adc's");
  plots[n]->getHisto(1)->setLegText("clusters");
  plots[n]->getHisto(0)->setLegArgs("l");
  plots[n]->getHisto(1)->setLegArgs("l");

  long q_idx = ((long)&contents.h15_itpc_sec1 - (long)contents.array) / (sizeof(TH1 *));
  long qs_idx = ((long)&contents.h120_itpc_chargeStep_s1 - (long)contents.array) / (sizeof(TH1 *));
  long cl_qs_idx = ((long)&extras.cl120_itpc_chargeStep_s1 - (long)extras.array) / (sizeof(TH1 *));
 
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
  
void itpcBuilder::startrun(daqReader *rdr) {
  LOG("JEFF", "itpcBuilder starting run #%d",rdr->run);
  resetAllPlots();
  laserReader->resetAll();
  n_cld = 0;
  n_adc = 0;
  nlasers = 0;
  drift_vel = 0;
  event_no=0;
  run = rdr->run;
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void itpcBuilder::event(daqReader *rdr)
{
  int has_adc=0;
  int has_cld=0;

  // printf("aa\n");
  long q_idx = ((long)&contents.h15_itpc_sec1 - (long)contents.array) / (sizeof(TH1 *));
  long qs_idx = ((long)&contents.h120_itpc_chargeStep_s1 - (long)contents.array) / (sizeof(TH1 *));
  long cl_qs_idx = ((long)&extras.cl120_itpc_chargeStep_s1 - (long)extras.array) / (sizeof(TH1 *));

  // printf("q qs cl:  %d %d %d 0x%x 0x%x\n",q_idx,qs_idx,cl_qs_idx,&extras.cl120_itpc_chargeStep _s1,extras.array);
  // printf("bb\n"); 

  double pixel_count = 0;
  double channel_count = 0;
  double charge_count = 0;
  double charge_count_sector;
  double tpc_max_channels = 0.0;
  int tpc_max_channels_sector = 3440;

  double cl_max_channels = 0;

  double pix_count_cl = 0;

  event_no++;

  // printf("cc\n");
  for(int s=1;s<=24;s++) {

    charge_count_sector = 0;
    int channel_counts[Npads1][Nrows1];
    double charge_counts[Npads1][Nrows1];
    double tb_charge_counts[512];

    memset(channel_counts, 0, sizeof(channel_counts));
    memset(charge_counts, 0, sizeof(charge_counts));
    memset(tb_charge_counts, 0, sizeof(tb_charge_counts));

    daq_dta *dd = rdr->det("itpc")->get("adc",s) ;
    if(dd) {   // regular data...
      has_adc = 1;
#ifdef fv
      cout << "Next Event" << endl;
#endif

      tpc_max_channels += tpc_max_channels_sector;

      while(dd->iterate()) {
#ifdef fv
	cout << dd->row << " , " << dd->pad << " , " << dd->ncontent << endl;
#endif
	//
	// skip padrow 0 -- don't know what it is?
	//
	if((dd->pad < 1 ) ||
	   (dd->row < 1))
	  {
	    continue;
	  }

	channel_counts[dd->pad][dd->row] = 1;

	pixel_count += dd->ncontent ;

	if(dd->ncontent > 0) {
	  channel_counts[dd->pad][dd->row] = 1;
	}

	for(u_int i=0;i<dd->ncontent;i++) {
	  int tb = dd->adc[i].tb;
	  int adc = dd->adc[i].adc;
#ifdef fv
	  cout << " ( " << tb << " , " << adc <<" )" ;
#endif
	  if((dd->pad >= Npads1) ||
	     (dd->row >= Nrows1) ||
	     (tb >= 512)) {
	    LOG(ERR, "event=%d pad=%d row=%d tb=%d out of range.  Ignore.", event_no, dd->pad, dd->row, tb);
	  }
	  else {
	    if(tb>30 && tb<430) {
	      charge_counts[dd->pad][dd->row] += adc;
	    }
	    tb_charge_counts[tb] += adc;
	  }
	}
#ifdef fv
	cout << endl;
#endif
      }

      for(int i=0;i<512;i++) {
	contents.array[s + qs_idx - 1]->Fill(i,tb_charge_counts[i]);
      }
      
      for(int i=1;i<Npads1;i++) {
	for(int j=1;j<Nrows1;j++) {
	  // printf("c\n");
	  channel_count += channel_counts[i][j];
	  charge_count += charge_counts[i][j];
	  charge_count_sector += charge_counts[i][j];
	  
	  if(charge_counts[i][j]) {
	    contents.h66_itpc_phi_charge->Fill(mPhiAngleMap[s-1][j-1][i-1],charge_counts[i][j]);
	    ((TH2D *)contents.array[s + q_idx - 1])->Fill(i, j, charge_counts[i][j]);
	  }
	}
      }

      contents.h67_itpc_sector_charge->Fill(s,charge_count_sector);      
    }

    double charge_cl=0;
    double charge_counts_cl[Npads1][Nrows1];
    double tb_charge_counts_cl[512];
    memset(charge_counts_cl, 0, sizeof(charge_counts_cl));
    memset(tb_charge_counts_cl, 0, sizeof(tb_charge_counts_cl));

#if checkcld
    dd = rdr->det("itpc")->get("cld",s) ;
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
	
	for(int i=1;i<Npads1;i++) {
	  for(int j=1;j<Nrows1;j++) {
	    if(charge_counts_cl[i][j]) {
	      extras.cl66_itpc_phi_charge->Fill(mPhiAngleMap[s-1][j-1][i-1],charge_counts_cl[i][j]);
	    }
	  }
	}
	
	extras.cl67_itpc_sector_charge->Fill(s,charge_cl);
      }
    }
#endif
    
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
    contents.h66_itpc_phi_charge->Scale(adc_scale);
    contents.h67_itpc_sector_charge->Scale(adc_scale);
  }

  if(has_cld) {
    n_cld++;   
    double cld_scale = (double)(n_cld-1) / (double)n_cld;
    if(n_cld == 1) cld_scale = 1;

    for(int i=1;i<=24;i++) {
      extras.array[i + cl_qs_idx - 1]->Scale(cld_scale);
    }
    extras.cl66_itpc_phi_charge->Scale(cld_scale);
    extras.cl67_itpc_sector_charge->Scale(cld_scale);
  }
#ifdef fv
  cout << "Trig " << rdr->trgcmd << endl;
#endif

  switch(rdr->trgcmd) {
  case 4:
    //contents.itpc_occ_physics->Fill(100.0 * (double)channel_count / tpc_max_channels);
    contents.itpc_pix_occ_physics->Fill(100.0 * (double)pixel_count / (tpc_max_channels * 400.0));

    extras.itpc_clpix_occ_physics->Fill(100.0 * (double)pix_count_cl / (cl_max_channels * 400.0));

    //printf("chan=%lf pix=%lf max=%lf:  %lf %lf\n",channel_count,pixel_count,tpc_max_channels,channel_count/tpc_max_channels,pixel_count/(tpc_max_channels*400));
    break;
  case 8:  // Lasers...
  case 9:
#if checklaser
    {
      
      LOG(DBG, "Got a laser...");

      //contents.h44_itpc_occ_laser->Fill(100.0 * (double)channel_count / tpc_max_channels);
      contents.itpc_pix_occ_laser->Fill(100.0 * (double)pixel_count / (tpc_max_channels * 400.0));
      extras.itpc_clpix_occ_laser->Fill(100.0 * (double)pix_count_cl / (cl_max_channels * 400.0));

      double vDrift = laserReader->Make(rdr);
     
      //LOG("JEFF","%d vDrift = %lf",rdr->event_number, vDrift);

      if((vDrift > 5.4) && (vDrift < 5.8)) {
	nlasers++;
	contents.h102_itpc_drift_vel->Fill(vDrift);
      }

      drift_vel = contents.h102_itpc_drift_vel->GetMean();

      LOG("JEFF", "run=%d nlasers: %d curr_drift=%lf", run, nlasers, drift_vel);
      //if(nlasers == 50) {
      if(1) {    // inneficient!  write all of them :-)
	FILE *f = fopen("/RTS/conf/handler/.l4_drift_velocity","w");
	if(f) {
	    fprintf(f, "%lf", drift_vel);
	    fclose(f);
	}
	else {
	    LOG(OPER, "Can't access drift velocity file!");
	}

	f = fopen("/RTS/conf/handler/.l4_drift_velocity_run","w");
	if(f) {
	  fprintf(f, "%d", run);
	  fclose(f);
	}
	else {
	    LOG(OPER, "Can't access drift velocity run number file!");
	}
      }
      
    }
#endif
    break;

  case 10:   // Pulsers..
    //contents.h43_itpc_occ_pulser->Fill(100.0 * (double)channel_count / tpc_max_channels);
    contents.itpc_pix_occ_pulser->Fill(100.0 * (double)pixel_count / (tpc_max_channels * 400.0));
    extras.itpc_clpix_occ_pulser->Fill(100.0 * (double)pix_count_cl / (cl_max_channels * 400.0));

    break;
  default:
    LOG(WARN, "Trigger command other than 4,8,9,10: evt %d, token=%d, cmd=%d",rdr->seq, rdr->token, rdr->trgcmd);
  }

  // Normalize the charge histos
  //
  // adc vs sector
  // phy disto
    
}

  void itpcBuilder::setPhiAngleMap()
  {
    const Int_t NP = 40; // # padrows
    const Float_t DEG = 57.296;//360/2pi

    Double_t Xpads[NP] = { 
      55.8, 57.4, 59, 60.6, 62.2, 63.8, 65.4, 67, 68.6, 70.2, 
      71.8, 73.4, 75, 76.6, 78.2, 79.8, 81.4, 83, 84.6, 86.2, 
      87.8, 89.4, 91, 92.6, 94.2, 95.8, 97.4, 99, 100.6, 102.2, 
      103.8, 105.4, 107, 108.6, 110.2, 111.8, 113.4, 115, 116.6, 118.2, 
    }; 

 
    Double_t pitches[2] = {0.5, 0.67};


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
      for (int j=0; j<NP; j++) {
	pitch = pitches[0];
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

  void itpcBuilder::main(int argc, char *argv[])
  {
    itpcBuilder me;
  
    me.Main(argc, argv);
  }

