// $Id: tpcBuilder.cxx,v 1.6 2019/04/18 15:21:14 videbaks Exp $
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
#include "tpcBuilder.h"
#include <RTS/include/rtsLog.h>
#include "LaserReader.h"


// TPC Jevp builder
//
//  Author: F.Videbaek
// This builder combines the tpx and itpc readout into a unified
// presentation of TPC data
// 
// The raw row number from the TPX always has 27 added befire using in any plots
// that involves
// Compared to old plots:
//      added cluster widtsh for both TPX and iTPC
//      have seperate plots for itpc and tpx
//      Some summary plots do have thesums for itpc and tpx
//      Legends updated
//
//

#define checkcld 1
#define checklaser 1
#define fv 0
#define fvd 0

ClassImp(tpcBuilder);
  
//
// 
static const int NTPCpads[72] = {
    52, 54, 56, 58, 60, 62, 62, 64, 66, 68,
    70, 72, 74, 74, 76, 78, 80, 82, 84, 86,
    86, 88, 90, 92, 94, 96, 98, 98,100,102,
    104,106,108,110,110,112,114,116,118,120,
    98,100,102,104,106,106,108,110,112,112,114,116,118,120,122,122, //Outer 
    124,126,128,128,130,132,134,136,138,138,140,142,144,144,144,144
};

static const int Nrows = 72;
static const int Npads = 144;
static const int Nrows1 = Nrows+1;
static const int Npads1 = Npads+1;

tpcBuilder::tpcBuilder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"tpc";

  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
  memset(&extras, 0, sizeof(extras));

  setPhiAngleMap();
  laserReader = new LaserReader();


}

tpcBuilder::~tpcBuilder() {

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

void tpcBuilder::initialize(int argc, char *argv[]) {

  contents.itpc_pix_occ_physics = new TH1D("itpc_pix_occ_physics","iTPC Pixel Occupancy (in %) Physics",100,0,2.5);
  contents.itpc_pix_occ_laser   = new TH1D("itpc_pix_occ_laser","iTPC Pixel Occupancy (in %) Lasers",100,0,1);
  contents.itpc_pix_occ_pulser  = new TH1D("itpc_pix_occ_pulser","iTPC Pixel Occupancy (in %) Pulsers",100,0,10);

  contents.tpc_pix_occ_physics = new TH1D("tpc_pic_occ_physics","TPC Pixel Occupancy (in %) Physics",100,0,2.5);
  contents.tpc_pix_occ_laser   = new TH1D("tpc_pic_occ_laser","TPC Pixel Occupancy (in %) Lasers",100,0,1);
  contents.tpc_pix_occ_pulser  = new TH1D("tpc_pic_occ_pulser","TPC Pixel Occupancy (in %) Pulsers",100,0,10);

  // Common for itpc and tpx
  //
  contents.h_tpc_sec1 = new TH2D("h_tpc_sec1","TPC Sec. 1 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec2 = new TH2D("h_tpc_sec2","TPC Sec. 2 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec3 = new TH2D("h_tpc_sec3","TPC Sec. 3 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec4 = new TH2D("h_tpc_sec4","TPC Sec. 4 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec5 = new TH2D("h_tpc_sec5","TPC Sec. 5 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec6 = new TH2D("h_tpc_sec6","TPC Sec. 6 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec7 = new TH2D("h_tpc_sec7","TPC Sec. 7 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec8 = new TH2D("h_tpc_sec8","TPC Sec. 8 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec9 = new TH2D("h_tpc_sec9","TPC Sec. 9 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec10 = new TH2D("h_tpc_sec10","TPC Sec. 10 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec11 = new TH2D("h_tpc_sec11","TPC Sec. 11 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec12 = new TH2D("h_tpc_sec12","TPC Sec. 12 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec13 = new TH2D("h_tpc_sec13","TPC Sec. 13 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec14 = new TH2D("h_tpc_sec14","TPC Sec. 14 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec15 = new TH2D("h_tpc_sec15","TPC Sec. 15 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec16 = new TH2D("h_tpc_sec16","TPC Sec. 16 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec17 = new TH2D("h_tpc_sec17","TPC Sec. 17 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec18 = new TH2D("h_tpc_sec18","TPC Sec. 18 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec19 = new TH2D("h_tpc_sec19","TPC Sec. 19 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec20 = new TH2D("h_tpc_sec20","TPC Sec. 20 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec21 = new TH2D("h_tpc_sec21","TPC Sec. 21 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec22 = new TH2D("h_tpc_sec22","TPC Sec. 22 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec23 = new TH2D("h_tpc_sec23","TPC Sec. 23 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.h_tpc_sec24 = new TH2D("h_tpc_sec24","TPC Sec. 24 charge per pad",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);

  contents.e_tpc_sec1 = new TH2D("e_tpc_sec1","TPC Sec. 1 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec2 = new TH2D("e_tpc_sec2","TPC Sec. 2 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec3 = new TH2D("e_tpc_sec3","TPC Sec. 3 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec4 = new TH2D("e_tpc_sec4","TPC Sec. 4 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec5 = new TH2D("e_tpc_sec5","TPC Sec. 5 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec6 = new TH2D("e_tpc_sec6","TPC Sec. 6 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec7 = new TH2D("e_tpc_sec7","TPC Sec. 7 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec8 = new TH2D("e_tpc_sec8","TPC Sec. 8 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec9 = new TH2D("e_tpc_sec9","TPC Sec. 9 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec10 = new TH2D("e_tpc_sec10","TPC Sec. 10 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec11 = new TH2D("e_tpc_sec11","TPC Sec. 11 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec12 = new TH2D("e_tpc_sec12","TPC Sec. 12 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec13 = new TH2D("e_tpc_sec13","TPC Sec. 13 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec14 = new TH2D("e_tpc_sec14","TPC Sec. 14 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec15 = new TH2D("e_tpc_sec15","TPC Sec. 15 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec16 = new TH2D("e_tpc_sec16","TPC Sec. 16 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec17 = new TH2D("e_tpc_sec17","TPC Sec. 17 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec18 = new TH2D("e_tpc_sec18","TPC Sec. 18 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec19 = new TH2D("e_tpc_sec19","TPC Sec. 19 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec20 = new TH2D("e_tpc_sec20","TPC Sec. 20 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec21 = new TH2D("e_tpc_sec21","TPC Sec. 21 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec22 = new TH2D("e_tpc_sec22","TPC Sec. 22 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec23 = new TH2D("e_tpc_sec23","TPC Sec. 23 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);
  contents.e_tpc_sec24 = new TH2D("e_tpc_sec24","TPC Sec. 24 charge per clust (Event)",Npads,0.5,Npads+0.5,Nrows,0.5,Nrows+0.5);

  // itpc only
  //
  contents.h_itpc_chargeStep_s1 = new TH1D("h_itpc_chargeStep_s1","iTPC adc vs time sector#1",512,0,512);
  contents.h_itpc_chargeStep_s2 = new TH1D("h_itpc_chargeStep_s2","iTPC adc vs time sector#2",512,0,512);
  contents.h_itpc_chargeStep_s3 = new TH1D("h_itpc_chargeStep_s3","iTPC adc vs time sector#3",512,0,512);
  contents.h_itpc_chargeStep_s4 = new TH1D("h_itpc_chargeStep_s4","iTPC adc vs time sector#4",512,0,512);
  contents.h_itpc_chargeStep_s5 = new TH1D("h_itpc_chargeStep_s5","iTPC adc vs time sector#5",512,0,512);
  contents.h_itpc_chargeStep_s6 = new TH1D("h_itpc_chargeStep_s6","iTPC adc vs time sector#6",512,0,512);
  contents.h_itpc_chargeStep_s7 = new TH1D("h_itpc_chargeStep_s7","iTPC adc vs time sector#7",512,0,512);
  contents.h_itpc_chargeStep_s8 = new TH1D("h_itpc_chargeStep_s8","iTPC adc vs time sector#8",512,0,512);
  contents.h_itpc_chargeStep_s9 = new TH1D("h_itpc_chargeStep_s9","iTPC adc vs time sector#9",512,0,512);
  contents.h_itpc_chargeStep_s10 = new TH1D("h_itpc_chargeStep_s10","iTPC adc vs time sector#10",512,0,512);
  contents.h_itpc_chargeStep_s11 = new TH1D("h_itpc_chargeStep_s11","iTPC adc vs time sector#11",512,0,512);
  contents.h_itpc_chargeStep_s12 = new TH1D("h_itpc_chargeStep_s12","iTPC adc vs time sector#12",512,0,512);
  contents.h_itpc_chargeStep_s13 = new TH1D("h_itpc_chargeStep_s13","iTPC adc vs time sector#13",512,0,512);
  contents.h_itpc_chargeStep_s14 = new TH1D("h_itpc_chargeStep_s14","iTPC adc vs time sector#14",512,0,512);
  contents.h_itpc_chargeStep_s15 = new TH1D("h_itpc_chargeStep_s15","iTPC adc vs time sector#15",512,0,512);
  contents.h_itpc_chargeStep_s16 = new TH1D("h_itpc_chargeStep_s16","iTPC adc vs time sector#16",512,0,512);
  contents.h_itpc_chargeStep_s17 = new TH1D("h_itpc_chargeStep_s17","iTPC adc vs time sector#17",512,0,512);
  contents.h_itpc_chargeStep_s18 = new TH1D("h_itpc_chargeStep_s18","iTPC adc vs time sector#18",512,0,512);
  contents.h_itpc_chargeStep_s19 = new TH1D("h_itpc_chargeStep_s19","iTPC adc vs time sector#19",512,0,512);
  contents.h_itpc_chargeStep_s20 = new TH1D("h_itpc_chargeStep_s20","iTPC adc vs time sector#20",512,0,512);
  contents.h_itpc_chargeStep_s21 = new TH1D("h_itpc_chargeStep_s21","iTPC adc vs time sector#21",512,0,512);
  contents.h_itpc_chargeStep_s22 = new TH1D("h_itpc_chargeStep_s22","iTPC adc vs time sector#22",512,0,512);
  contents.h_itpc_chargeStep_s23 = new TH1D("h_itpc_chargeStep_s23","iTPC adc vs time sector#23",512,0,512);
  contents.h_itpc_chargeStep_s24 = new TH1D("h_itpc_chargeStep_s24","iTPC adc vs time sector#24",512,0,512);
 
  // tpx only
  //
  contents.h_tpx_chargeStep_s1 = new TH1D("h_tpx_chargeStep_s1","TPC adc vs time sector#1",512,0,512);
  contents.h_tpx_chargeStep_s2 = new TH1D("h_tpx_chargeStep_s2","TPC adc vs time sector#2",512,0,512);
  contents.h_tpx_chargeStep_s3 = new TH1D("h_tpx_chargeStep_s3","TPC adc vs time sector#3",512,0,512);
  contents.h_tpx_chargeStep_s4 = new TH1D("h_tpx_chargeStep_s4","TPC adc vs time sector#4",512,0,512);
  contents.h_tpx_chargeStep_s5 = new TH1D("h_tpx_chargeStep_s5","TPC adc vs time sector#5",512,0,512);
  contents.h_tpx_chargeStep_s6 = new TH1D("h_tpx_chargeStep_s6","TPC adc vs time sector#6",512,0,512);
  contents.h_tpx_chargeStep_s7 = new TH1D("h_tpx_chargeStep_s7","TPC adc vs time sector#7",512,0,512);
  contents.h_tpx_chargeStep_s8 = new TH1D("h_tpx_chargeStep_s8","TPC adc vs time sector#8",512,0,512);
  contents.h_tpx_chargeStep_s9 = new TH1D("h_tpx_chargeStep_s9","TPC adc vs time sector#9",512,0,512);
  contents.h_tpx_chargeStep_s10 = new TH1D("h_tpx_chargeStep_s10","TPC adc vs time sector#10",512,0,512);
  contents.h_tpx_chargeStep_s11 = new TH1D("h_tpx_chargeStep_s11","TPC adc vs time sector#11",512,0,512);
  contents.h_tpx_chargeStep_s12 = new TH1D("h_tpx_chargeStep_s12","TPC adc vs time sector#12",512,0,512);
  contents.h_tpx_chargeStep_s13 = new TH1D("h_tpx_chargeStep_s13","TPC adc vs time sector#13",512,0,512);
  contents.h_tpx_chargeStep_s14 = new TH1D("h_tpx_chargeStep_s14","TPC adc vs time sector#14",512,0,512);
  contents.h_tpx_chargeStep_s15 = new TH1D("h_tpx_chargeStep_s15","TPC adc vs time sector#15",512,0,512);
  contents.h_tpx_chargeStep_s16 = new TH1D("h_tpx_chargeStep_s16","TPC adc vs time sector#16",512,0,512);
  contents.h_tpx_chargeStep_s17 = new TH1D("h_tpx_chargeStep_s17","TPC adc vs time sector#17",512,0,512);
  contents.h_tpx_chargeStep_s18 = new TH1D("h_tpx_chargeStep_s18","TPC adc vs time sector#18",512,0,512);
  contents.h_tpx_chargeStep_s19 = new TH1D("h_tpx_chargeStep_s19","TPC adc vs time sector#19",512,0,512);
  contents.h_tpx_chargeStep_s20 = new TH1D("h_tpx_chargeStep_s20","TPC adc vs time sector#20",512,0,512);
  contents.h_tpx_chargeStep_s21 = new TH1D("h_tpx_chargeStep_s21","TPC adc vs time sector#21",512,0,512);
  contents.h_tpx_chargeStep_s22 = new TH1D("h_tpx_chargeStep_s22","TPC adc vs time sector#22",512,0,512);
  contents.h_tpx_chargeStep_s23 = new TH1D("h_tpx_chargeStep_s23","TPC adc vs time sector#23",512,0,512);
  contents.h_tpx_chargeStep_s24 = new TH1D("h_tpx_chargeStep_s24","TPC adc vs time sector#24",512,0,512);
 
  contents.h_tpc_drift_vel = new TH1D("h_tpc_drift_vel", "TPC Drift Velocity (cm/us)",400,5.4,5.8);

  contents.h_itpc_phi_charge = new TH1D("h_itpc_phi_charge","Azimuthal Distribution of iTPC Charge",360,-180,180);
  contents.h_itpc_sector_charge = new TH1D("h_itpc_sector_charge","iTPC Charge per Sector",24,0.5,24.5);
  contents.h_tpx_phi_charge = new TH1D("h_tpx_phi_charge","Azimuthal Distribution of TPX Charge",360,-180,180);
  contents.h_tpx_sector_charge = new TH1D("h_tpx_sector_charge","TPX Charge per Sector",24,0.5,24.5);

 
  contents.cl_width_itpc_tb = new TH2D("cl_width_itpc_tb","iTPC clusterwidth (time)",
				       24,0.5,24.5,
				       30,0.5,30.5);
  contents.cl_width_itpc_pad = new TH2D("cl_width_itpc_pad","iTPC clusterwidth (pad)",
					24,0.5,24.5,
					30,0.5,30.5);
  contents.cl_width_tpx_tb = new TH2D("cl_width_tpx_tb","TPX clusterwidth (time)",
				       24,0.5,24.5,
				       30,0.5,30.5);
  contents.cl_width_tpx_tb->GetXaxis()->SetTitle("sector");
  contents.cl_width_tpx_pad = new TH2D("cl_width_tpx_pad","TPX clusterwidth (pad)",
					24,0.5,24.5,
					30,0.5,30.5);
  contents.cl_width_tpx_pad->GetXaxis()->SetTitle("sector");
  
  // cluster based vesions...
  //
  extras.itpc_clpix_occ_physics = new TH1D("itpc_clpix_occ_physics","iTPC Pixel Occupancy (in %) Physics",100,0,2.5);
  extras.itpc_clpix_occ_laser = new TH1D("itpc_clpix_occ_laser","iTPC Pixel Occupancy (in %) Lasers",100,0,1);
  extras.itpc_clpix_occ_pulser = new TH1D("itpc_clpix_occ_pulser","iTPC Pixel Occupancy (in %) Pulsers",100,0,10);

  extras.tpc_clpix_occ_physics = new TH1D("tpc_clpix_occ_physics","TPC Pixel Occupancy (in %) Physics",100,0,2.5);
  extras.tpc_clpix_occ_laser = new TH1D("tpc_clpix_occ_laser","TPC Pixel Occupancy (in %) Lasers",100,0,1);
  extras.tpc_clpix_occ_pulser = new TH1D("tpc_clpix_occ_pulser","TPC Pixel Occupancy (in %) Pulsers",100,0,10);

 


  extras.cl_itpc_chargeStep_s1 = new TH1D("cl_itpc_chargeStep_s1","iTPC adc vs time sector#1",512,0,512);
  extras.cl_itpc_chargeStep_s2 = new TH1D("cl_itpc_chargeStep_s2","iTPC adc vs time sector#2",512,0,512);
  extras.cl_itpc_chargeStep_s3 = new TH1D("cl_itpc_chargeStep_s3","iTPC adc vs time sector#3",512,0,512);
  extras.cl_itpc_chargeStep_s4 = new TH1D("cl_itpc_chargeStep_s4","iTPC adc vs time sector#4",512,0,512);
  extras.cl_itpc_chargeStep_s5 = new TH1D("cl_itpc_chargeStep_s5","iTPC adc vs time sector#5",512,0,512);
  extras.cl_itpc_chargeStep_s6 = new TH1D("cl_itpc_chargeStep_s6","iTPC adc vs time sector#6",512,0,512);
  extras.cl_itpc_chargeStep_s7 = new TH1D("cl_itpc_chargeStep_s7","iTPC adc vs time sector#7",512,0,512);
  extras.cl_itpc_chargeStep_s8 = new TH1D("cl_itpc_chargeStep_s8","iTPC adc vs time sector#8",512,0,512);
  extras.cl_itpc_chargeStep_s9 = new TH1D("cl_itpc_chargeStep_s9","iTPC adc vs time sector#9",512,0,512);
  extras.cl_itpc_chargeStep_s10 = new TH1D("cl_itpc_chargeStep_s10","iTPC adc vs time sector#10",512,0,512);
  extras.cl_itpc_chargeStep_s11 = new TH1D("cl_itpc_chargeStep_s11","iTPC adc vs time sector#11",512,0,512);
  extras.cl_itpc_chargeStep_s12 = new TH1D("cl_itpc_chargeStep_s12","iTPC adc vs time sector#12",512,0,512);
  extras.cl_itpc_chargeStep_s13 = new TH1D("cl_itpc_chargeStep_s13","iTPC adc vs time sector#13",512,0,512);
  extras.cl_itpc_chargeStep_s14 = new TH1D("cl_itpc_chargeStep_s14","iTPC adc vs time sector#14",512,0,512);
  extras.cl_itpc_chargeStep_s15 = new TH1D("cl_itpc_chargeStep_s15","iTPC adc vs time sector#15",512,0,512);
  extras.cl_itpc_chargeStep_s16 = new TH1D("cl_itpc_chargeStep_s16","iTPC adc vs time sector#16",512,0,512);
  extras.cl_itpc_chargeStep_s17 = new TH1D("cl_itpc_chargeStep_s17","iTPC adc vs time sector#17",512,0,512);
  extras.cl_itpc_chargeStep_s18 = new TH1D("cl_itpc_chargeStep_s18","iTPC adc vs time sector#18",512,0,512);
  extras.cl_itpc_chargeStep_s19 = new TH1D("cl_itpc_chargeStep_s19","iTPC adc vs time sector#19",512,0,512);
  extras.cl_itpc_chargeStep_s20 = new TH1D("cl_itpc_chargeStep_s20","iTPC adc vs time sector#20",512,0,512);
  extras.cl_itpc_chargeStep_s21 = new TH1D("cl_itpc_chargeStep_s21","iTPC adc vs time sector#21",512,0,512);
  extras.cl_itpc_chargeStep_s22 = new TH1D("cl_itpc_chargeStep_s22","iTPC adc vs time sector#22",512,0,512);
  extras.cl_itpc_chargeStep_s23 = new TH1D("cl_itpc_chargeStep_s23","iTPC adc vs time sector#23",512,0,512);
  extras.cl_itpc_chargeStep_s24 = new TH1D("cl_itpc_chargeStep_s24","iTPC adc vs time sector#24",512,0,512);

 extras.cl_tpx_chargeStep_s1 = new TH1D("cl_tpx_chargeStep_s1","iTPC adc vs time sector#1",512,0,512);
  extras.cl_tpx_chargeStep_s2 = new TH1D("cl_tpx_chargeStep_s2","iTPC adc vs time sector#2",512,0,512);
  extras.cl_tpx_chargeStep_s3 = new TH1D("cl_tpx_chargeStep_s3","iTPC adc vs time sector#3",512,0,512);
  extras.cl_tpx_chargeStep_s4 = new TH1D("cl_tpx_chargeStep_s4","iTPC adc vs time sector#4",512,0,512);
  extras.cl_tpx_chargeStep_s5 = new TH1D("cl_tpx_chargeStep_s5","iTPC adc vs time sector#5",512,0,512);
  extras.cl_tpx_chargeStep_s6 = new TH1D("cl_tpx_chargeStep_s6","iTPC adc vs time sector#6",512,0,512);
  extras.cl_tpx_chargeStep_s7 = new TH1D("cl_tpx_chargeStep_s7","iTPC adc vs time sector#7",512,0,512);
  extras.cl_tpx_chargeStep_s8 = new TH1D("cl_tpx_chargeStep_s8","iTPC adc vs time sector#8",512,0,512);
  extras.cl_tpx_chargeStep_s9 = new TH1D("cl_tpx_chargeStep_s9","iTPC adc vs time sector#9",512,0,512);
  extras.cl_tpx_chargeStep_s10 = new TH1D("cl_tpx_chargeStep_s10","iTPC adc vs time sector#10",512,0,512);
  extras.cl_tpx_chargeStep_s11 = new TH1D("cl_tpx_chargeStep_s11","iTPC adc vs time sector#11",512,0,512);
  extras.cl_tpx_chargeStep_s12 = new TH1D("cl_tpx_chargeStep_s12","iTPC adc vs time sector#12",512,0,512);
  extras.cl_tpx_chargeStep_s13 = new TH1D("cl_tpx_chargeStep_s13","iTPC adc vs time sector#13",512,0,512);
  extras.cl_tpx_chargeStep_s14 = new TH1D("cl_tpx_chargeStep_s14","iTPC adc vs time sector#14",512,0,512);
  extras.cl_tpx_chargeStep_s15 = new TH1D("cl_tpx_chargeStep_s15","iTPC adc vs time sector#15",512,0,512);
  extras.cl_tpx_chargeStep_s16 = new TH1D("cl_tpx_chargeStep_s16","iTPC adc vs time sector#16",512,0,512);
  extras.cl_tpx_chargeStep_s17 = new TH1D("cl_tpx_chargeStep_s17","iTPC adc vs time sector#17",512,0,512);
  extras.cl_tpx_chargeStep_s18 = new TH1D("cl_tpx_chargeStep_s18","iTPC adc vs time sector#18",512,0,512);
  extras.cl_tpx_chargeStep_s19 = new TH1D("cl_tpx_chargeStep_s19","iTPC adc vs time sector#19",512,0,512);
  extras.cl_tpx_chargeStep_s20 = new TH1D("cl_tpx_chargeStep_s20","iTPC adc vs time sector#20",512,0,512);
  extras.cl_tpx_chargeStep_s21 = new TH1D("cl_tpx_chargeStep_s21","iTPC adc vs time sector#21",512,0,512);
  extras.cl_tpx_chargeStep_s22 = new TH1D("cl_tpx_chargeStep_s22","iTPC adc vs time sector#22",512,0,512);
  extras.cl_tpx_chargeStep_s23 = new TH1D("cl_tpx_chargeStep_s23","iTPC adc vs time sector#23",512,0,512);
  extras.cl_tpx_chargeStep_s24 = new TH1D("cl_tpx_chargeStep_s24","iTPC adc vs time sector#24",512,0,512);
 
  extras.cl_itpc_phi_charge = new TH1D("cl_itpc_phi_charge","Azimuthal Distribution of iTPC Charge",360,-180,180);
  extras.cl_itpc_sector_charge = new TH1D("cl_itpc_sector_charge","iTPC Charge per Sector",24,0.5,24.5);
  extras.cl_tpx_phi_charge = new TH1D("cl_tpx_phi_charge","Azimuthal Distribution of TPX Charge",360,-180,180);
  extras.cl_tpx_sector_charge = new TH1D("cl_tpx_sector_charge","TPX Charge per Sector",24,0.5,24.5);

  extras.clusters_per_bx = new TH1D("clusters_per_bx", "TPC clusters per bunch crossing", 120, -.5, 119.5);
 
  // Add root histograms to Plots
  int np = sizeof(contents) / sizeof(TH1 *);
  JevpPlot *plots[np];

  int n=0;

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

  // tpc  plots

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
  
  plots[++n] = new JevpPlot(extras.clusters_per_bx);
  plots[n]->optstat = 0;


  int pl_q_idx = n+1;
  plots[++n] = new JevpPlot(contents.h_tpc_sec1);
  plots[++n] = new JevpPlot(contents.h_tpc_sec2);
  plots[++n] = new JevpPlot(contents.h_tpc_sec3);
  plots[++n] = new JevpPlot(contents.h_tpc_sec4);
  plots[++n] = new JevpPlot(contents.h_tpc_sec5);
  plots[++n] = new JevpPlot(contents.h_tpc_sec6);
  plots[++n] = new JevpPlot(contents.h_tpc_sec7);
  plots[++n] = new JevpPlot(contents.h_tpc_sec8);
  plots[++n] = new JevpPlot(contents.h_tpc_sec9);
  plots[++n] = new JevpPlot(contents.h_tpc_sec10);
  plots[++n] = new JevpPlot(contents.h_tpc_sec11);
  plots[++n] = new JevpPlot(contents.h_tpc_sec12);
  plots[++n] = new JevpPlot(contents.h_tpc_sec13);
  plots[++n] = new JevpPlot(contents.h_tpc_sec14);
  plots[++n] = new JevpPlot(contents.h_tpc_sec15);
  plots[++n] = new JevpPlot(contents.h_tpc_sec16);
  plots[++n] = new JevpPlot(contents.h_tpc_sec17);
  plots[++n] = new JevpPlot(contents.h_tpc_sec18);
  plots[++n] = new JevpPlot(contents.h_tpc_sec19);
  plots[++n] = new JevpPlot(contents.h_tpc_sec20);
  plots[++n] = new JevpPlot(contents.h_tpc_sec21);
  plots[++n] = new JevpPlot(contents.h_tpc_sec22);
  plots[++n] = new JevpPlot(contents.h_tpc_sec23);
  plots[++n] = new JevpPlot(contents.h_tpc_sec24);

  int pl_qe_idx = n+1;
  plots[++n] = new JevpPlot(contents.e_tpc_sec1);
  plots[++n] = new JevpPlot(contents.e_tpc_sec2);
  plots[++n] = new JevpPlot(contents.e_tpc_sec3);
  plots[++n] = new JevpPlot(contents.e_tpc_sec4);
  plots[++n] = new JevpPlot(contents.e_tpc_sec5);
  plots[++n] = new JevpPlot(contents.e_tpc_sec6);
  plots[++n] = new JevpPlot(contents.e_tpc_sec7);
  plots[++n] = new JevpPlot(contents.e_tpc_sec8);
  plots[++n] = new JevpPlot(contents.e_tpc_sec9);
  plots[++n] = new JevpPlot(contents.e_tpc_sec10);
  plots[++n] = new JevpPlot(contents.e_tpc_sec11);
  plots[++n] = new JevpPlot(contents.e_tpc_sec12);
  plots[++n] = new JevpPlot(contents.e_tpc_sec13);
  plots[++n] = new JevpPlot(contents.e_tpc_sec14);
  plots[++n] = new JevpPlot(contents.e_tpc_sec15);
  plots[++n] = new JevpPlot(contents.e_tpc_sec16);
  plots[++n] = new JevpPlot(contents.e_tpc_sec17);
  plots[++n] = new JevpPlot(contents.e_tpc_sec18);
  plots[++n] = new JevpPlot(contents.e_tpc_sec19);
  plots[++n] = new JevpPlot(contents.e_tpc_sec20);
  plots[++n] = new JevpPlot(contents.e_tpc_sec21);
  plots[++n] = new JevpPlot(contents.e_tpc_sec22);
  plots[++n] = new JevpPlot(contents.e_tpc_sec23);
  plots[++n] = new JevpPlot(contents.e_tpc_sec24);

  // these plots should likely all be logy
  //

  int pl_qs_idx = n+1;
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s1);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s1);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s2);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s2);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s3);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s3);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s4);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s4);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s5);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s5);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s6);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s6);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s7);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s7);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s8);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s8);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s9);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s9);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s10);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s10);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s11);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s11);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s12);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s12);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s13);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s13);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s14);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s14);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s15);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s15);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s16);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s16);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s17);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s17);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s18);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s18);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s19);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s19);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s20);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s20);
  // plots[n]->logy = 1;
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s21);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s21);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s22);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s22);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s23);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s23);
  plots[++n] = new JevpPlot(contents.h_itpc_chargeStep_s24);
  plots[n]->addHisto(extras.cl_itpc_chargeStep_s24);

  int pl_qs_idx_tpx = n+1;
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s1);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s1);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s2);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s2);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s3);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s3);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s4);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s4);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s5);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s5);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s6);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s6);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s7);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s7);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s8);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s8);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s9);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s9);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s10);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s10);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s11);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s11);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s12);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s12);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s13);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s13);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s14);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s14);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s15);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s15);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s16);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s16);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s17);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s17);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s18);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s18);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s19);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s19);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s20);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s20);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s21);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s21);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s22);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s22);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s23);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s23);
  plots[++n] = new JevpPlot(contents.h_tpx_chargeStep_s24);
  plots[n]->addHisto(extras.cl_tpx_chargeStep_s24);
 
  plots[++n] = new JevpPlot(contents.h_tpc_drift_vel);

  plots[++n] = new JevpPlot(contents.h_itpc_phi_charge);
  plots[n]->addHisto(extras.cl_itpc_phi_charge);
  plots[n]->optstat = 0;
  extras.cl_itpc_phi_charge->SetLineColor(kRed);
  contents.h_itpc_phi_charge->SetLineColor(kGreen);
  plots[n]->setLegend(.78,.6,.98,.7);
  plots[n]->getHisto(0)->setLegText("adc's");
  plots[n]->getHisto(1)->setLegText("clusters");
  plots[n]->getHisto(0)->setLegArgs("l");
  plots[n]->getHisto(1)->setLegArgs("l");

  plots[++n] = new JevpPlot(contents.h_itpc_sector_charge);
  plots[n]->addHisto(extras.cl_itpc_sector_charge);
  plots[n]->optstat = 0;
  extras.cl_itpc_sector_charge->SetLineColor(kRed);
  contents.h_itpc_sector_charge->SetLineColor(kGreen);
  contents.h_itpc_sector_charge->SetMinimum(0);
  plots[n]->setLegend(.78,.6,.98,.7);
  plots[n]->getHisto(0)->setLegText("adc's");
  plots[n]->getHisto(1)->setLegText("clusters");
  plots[n]->getHisto(0)->setLegArgs("l");
  plots[n]->getHisto(1)->setLegArgs("l");

  plots[++n] = new JevpPlot(contents.h_tpx_phi_charge);
  plots[n]->addHisto(extras.cl_tpx_phi_charge);
  plots[n]->optstat = 0;
  extras.cl_tpx_phi_charge->SetLineColor(kRed);
  contents.h_tpx_phi_charge->SetLineColor(kGreen);
  plots[n]->setLegend(.78,.6,.98,.7);
  plots[n]->getHisto(0)->setLegText("adc's");
  plots[n]->getHisto(1)->setLegText("clusters");
  plots[n]->getHisto(0)->setLegArgs("l");
  plots[n]->getHisto(1)->setLegArgs("l");

  plots[++n] = new JevpPlot(contents.h_tpx_sector_charge);
  plots[n]->addHisto(extras.cl_tpx_sector_charge);
  plots[n]->optstat = 0;
  extras.cl_tpx_sector_charge->SetLineColor(kRed);
  extras.cl_tpx_sector_charge->SetMinimum(0.0);
  contents.h_tpx_sector_charge->SetLineColor(kGreen);
  contents.h_tpx_sector_charge->SetMinimum(0);
  plots[n]->setLegend(.78,.6,.98,.7);
  plots[n]->getHisto(0)->setLegText("adc's");
  plots[n]->getHisto(1)->setLegText("clusters");
  plots[n]->getHisto(0)->setLegArgs("l");
  plots[n]->getHisto(1)->setLegArgs("l");

  plots[++n] = new JevpPlot(contents.cl_width_itpc_tb);
  plots[n]->setOptStat(0);
  plots[++n] = new JevpPlot(contents.cl_width_itpc_pad);
  plots[n]->setOptStat(0);
  plots[++n] = new JevpPlot(contents.cl_width_tpx_tb);
  plots[n]->setOptStat(0);
  plots[++n] = new JevpPlot(contents.cl_width_tpx_pad);
  plots[n]->setOptStat(0);
  //
  // indices for blocks into the plots[] list
  //
  long q_idx = ((long)&contents.h_tpc_sec1 - (long)contents.array) / (sizeof(TH1 *));
  long qs_idx = ((long)&contents.h_itpc_chargeStep_s1 - (long)contents.array) / (sizeof(TH1 *));
  long qe_idx = ((long)&contents.e_tpc_sec1 - (long)contents.array) / (sizeof(TH1 *));
  long cl_qs_idx  = ((long)&extras.cl_itpc_chargeStep_s1 - (long)extras.array) / (sizeof(TH1 *));
  long qs_idx_tpx = ((long)&contents.h_tpx_chargeStep_s1 - (long)contents.array) / (sizeof(TH1 *)); 
  long cl_qs_idx_tpx  = ((long)&extras.cl_tpx_chargeStep_s1 - (long)extras.array) / (sizeof(TH1 *));

  // The indices should only be used to address offsets into the contents or the extras TH1 arrays
  // They should not be used for plots[..] which may or may not correspond to the offset into

  for(int i=0;i<24;i++) {
    plots[i+pl_q_idx]->setDrawOpts((char *)"colz");
    plots[i+pl_q_idx]->optstat = 0;
    plots[i+pl_qe_idx]->setDrawOpts((char *)"colz");
    plots[i+pl_qe_idx]->optstat = 0;

    plots[i+pl_qs_idx]->optstat = 0;
    plots[i+pl_qs_idx]->setLegend(.7,.8,.95,.95);
    plots[i+pl_qs_idx]->getHisto(0)->setLegText("adc's");
    plots[i+pl_qs_idx]->getHisto(1)->setLegText("clusters");
    plots[i+pl_qs_idx]->getHisto(0)->setLegArgs("l");
    plots[i+pl_qs_idx]->getHisto(1)->setLegArgs("l");

    plots[i+pl_qs_idx_tpx]->optstat = 0;
    plots[i+pl_qs_idx_tpx]->setLegend(.7,.8,.95,.95);
    plots[i+pl_qs_idx_tpx]->getHisto(0)->setLegText("adc's");
    plots[i+pl_qs_idx_tpx]->getHisto(1)->setLegText("clusters");
    plots[i+pl_qs_idx_tpx]->getHisto(0)->setLegArgs("l");
    plots[i+pl_qs_idx_tpx]->getHisto(1)->setLegArgs("l");

    contents.array[i+qs_idx]->SetLineColor(kGreen);
    extras.array[i+cl_qs_idx]->SetLineColor(kRed);
    contents.array[i+qs_idx_tpx]->SetLineColor(kGreen);
    extras.array[i+cl_qs_idx_tpx]->SetLineColor(kRed);
  }

  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    addPlot(plots[i]);
  } 


}
  
void tpcBuilder::startrun(daqReader *rdr) {
  LOG("JEFF", "tpcBuilder starting run #%d",rdr->run);
  resetAllPlots();
  laserReader->resetAll();
  n_cld = 0;
  n_adc = 0;
  nlasers = 0;
  drift_vel = 0;
  event_no=0;
  run = rdr->run;

  tpcDataInThisRun=0;
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void tpcBuilder::event(daqReader *rdr)
{
  int has_adc=0;
  int has_cld=0;
  int bunch7bit=0;
  
  // Get bunch crossing from trigger data..
  StTriggerData *trgd = getStTriggerData(rdr);
  if(trgd) {
    bunch7bit = trgd->bunchId7Bit();
    delete trgd;
  }


  long q_idx = ((long)&contents.h_tpc_sec1 - (long)contents.array) / (sizeof(TH1 *));
  long qe_idx = ((long)&contents.e_tpc_sec1 - (long)contents.array) / (sizeof(TH1 *));
  long qs_idx = ((long)&contents.h_itpc_chargeStep_s1 - (long)contents.array) / (sizeof(TH1 *));
  long cl_qs_idx = ((long)&extras.cl_itpc_chargeStep_s1 - (long)extras.array) / (sizeof(TH1 *));
  long qs_idx_tpx = ((long)&contents.h_tpx_chargeStep_s1 - (long)contents.array) / (sizeof(TH1 *)); 
  long cl_qs_idx_tpx  = ((long)&extras.cl_tpx_chargeStep_s1 - (long)extras.array) / (sizeof(TH1 *));



  double channel_count = 0;
  double charge_count = 0;

  int tpc_max_channels_inner_sector = 3440;
  int tpc_max_channels_outer_sector = 3942;
  double tpc_max_channels = 0.0;
  double cl_max_channels = 0;
  double itpc_max_channels = 0.0;
  double itpc_cl_max_channels = 0;

  double pix_count_cl = 0;
  double itpc_pix_count_cl = 0;
  double pixel_count = 0;
  double itpc_pixel_count=0;
  event_no++;
  //
  // reset event cluster plots
  //
  for(int i=0;i<24;i++) {
    contents.array[i+qe_idx]->Reset();
  }

  int channel_counts[Npads1][Nrows1];
  double charge_counts[Npads1][Nrows1];
  double tb_charge_counts[512];
  double charge_counts_cl[Npads1][Nrows1];
  double tb_charge_counts_cl[512];



  for(int s=1;s<=24;s++) {

#if fvd
    daq_dta *dda = rdr->det("tpx")->get("adc",s) ;
    cout << "tpx adc " << dda << endl;
    daq_dta *ddb = rdr->det("itpc")->get("adc",s) ;
    cout << "itpc adc " << ddb << endl;
#endif

 
    double charge_count_sector = 0;
    memset(channel_counts, 0, sizeof(channel_counts));
    memset(charge_counts, 0,  sizeof(charge_counts));
    memset(tb_charge_counts, 0, sizeof(tb_charge_counts));
    memset(tb_charge_counts_cl, 0, sizeof(tb_charge_counts_cl));

    //
    // get itpc data
    //
    daq_dta *dd = rdr->det("itpc")->get("adc",s) ;
#if fvd
    cout << "itpc adc" << endl;
#endif
    
    if(dd) {   
      if(tpcDataInThisRun==0) addServerTags("tpc");
      tpcDataInThisRun = 1;

      // regular data... Note its always there even if empty
      // e.g. for Run 18 data there are data banks for all sectors?
      //
      has_adc = 1;
      tpc_max_channels += tpc_max_channels_inner_sector;
      itpc_max_channels += tpc_max_channels_inner_sector;
    
      while(dd->iterate()) {
	if (dd->ncontent == 0) continue;
	// skip padrow _tb0
	// These are pins on SAMPA not connected to pads.
	//
	if((dd->pad < 1 ) || 
	   (dd->row < 1))
	  {
	    continue;
	  }
      
      
	pixel_count += dd->ncontent ;
	itpc_pixel_count += dd->ncontent ;
	if(dd->ncontent > 0) {
	  channel_counts[dd->pad][dd->row] = 1;
	}
      
	for(u_int i=0;i<dd->ncontent;i++) {
	  int tb = dd->adc[i].tb;
	  int adc = dd->adc[i].adc;
	  if((dd->pad >= Npads1) ||
	     (dd->row >= Nrows1) ||
	     (tb >= 512)) {
	    LOG(ERR, "event=%d pad=%d row=%d tb=%d out of range.  Ignore.", event_no, dd->pad, dd->row, tb);
	  }
	  else {
	    // this cut was used in run18 due to partial lack of pedetsal sub..
	    // remove timebins where GG osc is important.
	    if(tb>32 && tb<430) {

	      if(s==12){
		if(dd->pad==41 && dd->row==26)
		  continue;
	      }

	      if(s==14){
		if(dd->pad==41 && dd->row==38)
		  continue;
	      }

	      charge_counts[dd->pad][dd->row] += adc;
	    }
	    tb_charge_counts[tb] += adc;
	  }
	}
      }  // end iterate
    
      for(int i=1;i<Npads1;i++) {
	for(int j=1;j<41;j++) {
	  channel_count += channel_counts[i][j];
	  charge_count += charge_counts[i][j];
	  charge_count_sector += charge_counts[i][j];
	
	  if(charge_counts[i][j] > 0 ) {
	    contents.h_itpc_phi_charge->Fill(mPhiAngleMap[s-1][j-1][i-1],charge_counts[i][j]);
	    ((TH2D *)contents.array[s + q_idx - 1])->Fill(i, j, charge_counts[i][j]);
	  }
	}
      }  // end i,j
    
      for(int i=0;i<512;i++) {
	contents.array[s + qs_idx - 1]->Fill(i,tb_charge_counts[i]);
      }
      contents.h_itpc_sector_charge->Fill(s, charge_count_sector);
    
    }  //end if(dd)
 
    //
    //  tpx data
    //     
    memset(tb_charge_counts, 0, sizeof(tb_charge_counts));
    charge_count_sector = 0.0;

    dd = rdr->det("tpx")->get("adc",s) ;
#if fvd
    cout << "tpx " << dd << endl;
#endif
    if(dd) {   // regular data...
      if(tpcDataInThisRun==0) addServerTags("tpc");
      tpcDataInThisRun = 1;
      
      has_adc = 1;
      tpc_max_channels += tpc_max_channels_outer_sector;
      
      while(dd->iterate()){
#if fvd
	cout << "Sector " << s << " " << dd->ncontent << endl;
#endif
#if fvd
	cout << "    " << dd->pad << " " << dd->row << endl;
#endif
	//
	// skip rows < 14 ! should not appear in run 19 data
	//
	if((dd->pad < 1 ) || 
	   (dd->row < 14))
	  {
	    continue;
          }
	
	// 
	// update padrow count for outer sectors
	//
	
	pixel_count += dd->ncontent ;
	
	if(dd->ncontent > 0) {
	  channel_counts[dd->pad][dd->row+27] = 1;
	}
	
	
	for(u_int i=0;i<dd->ncontent;i++) {
	  int tb = dd->adc[i].tb;
	  int adc = dd->adc[i].adc;
	  if((dd->pad >= Npads1) ||
	     (dd->row +27 >= Nrows1) ||
	     (tb >= 512)) {
	    LOG(ERR, "event=%d pad=%d row=%d tb=%d out of range.  Ignore.", event_no, dd->pad, dd->row, tb);
	  }
	  else {
#if fvd
	    cout << dd->pad << " " << dd->row << " " << tb << " " << adc <<endl;
#endif
	    //
	    //  Remove pads that are also hot and cannot be masked
	    //  with pedestals
	    if(tb > 24){
	      if(s==4)
		if(dd->row+27 == 72 && dd->pad==110){
		  continue;
		}
	      if(s==12){
		if(dd->pad==19 && dd->row+27==43)
		  continue;

	      }

	      if(s==22){
		if(dd->pad==71 && dd->row+27==56)
		  continue;


	      }
	      charge_counts[dd->pad][dd->row+27] += adc;
	    }
	    tb_charge_counts[tb] += adc;  
	  }
	  
	}
      }
      
      //
      // All data have been looked at
      //
      
      for(int i=0;i<512;i++) {
	contents.array[s + qs_idx_tpx - 1]->Fill(i,tb_charge_counts[i]);
      }

      for(int i=1;i<Npads1;i++) {
	for(int j=41;j<Nrows1;j++) {
	  channel_count += channel_counts[i][j];
	  charge_count += charge_counts[i][j];
	  charge_count_sector += charge_counts[i][j];

	  if(charge_counts[i][j] > 0 ) {
	    contents.h_tpx_phi_charge->Fill(mPhiAngleMap[s-1][j-1][i-1],charge_counts[i][j]);
	    ((TH2D *)contents.array[s + q_idx - 1])->Fill(i, j, charge_counts[i][j]);
	  }
	}
      }
      contents.h_tpx_sector_charge->Fill(s,charge_count_sector);      
    }  // end dd

    //
    // go to clusters
    //


    double charge_cl=0;
    memset(charge_counts_cl, 0, sizeof(charge_counts_cl));
    memset(tb_charge_counts_cl, 0, sizeof(tb_charge_counts_cl));

    //
    // Itpc data
    //

    dd = rdr->det("itpc")->get("cld",s) ;
    if(dd) {
      if(tpcDataInThisRun==0) addServerTags("tpc");
      tpcDataInThisRun = 1;

      has_cld = 1;
      cl_max_channels += tpc_max_channels_inner_sector;
      itpc_cl_max_channels += tpc_max_channels_inner_sector;

      while(dd->iterate()) {
	for(u_int i=0;i<dd->ncontent;i++) {
	  if((dd->cld[i].flags==0)) {
	    pix_count_cl += (dd->cld[i].t2 - dd->cld[i].t1)*(dd->cld[i].p2 - dd->cld[i].p1);
	    itpc_pix_count_cl += (dd->cld[i].t2 - dd->cld[i].t1)*(dd->cld[i].p2 - dd->cld[i].p1);
	    itpc_pix_count_cl += (dd->cld[i].t2 - dd->cld[i].t1)*(dd->cld[i].p2 - dd->cld[i].p1);
	    charge_counts_cl[(int)dd->cld[i].pad][dd->row] += dd->cld[i].charge;
	    tb_charge_counts_cl[(int)dd->cld[i].tb] += dd->cld[i].charge;
	    charge_cl += dd->cld[i].charge;
	    ((TH2D*)contents.cl_width_itpc_tb)->Fill(s,dd->cld[i].t2 - dd->cld[i].t1);
	    ((TH2D*)contents.cl_width_itpc_pad)->Fill(s,dd->cld[i].p2 - dd->cld[i].p1);
	  }
	}
      }
    

    } // dd itpc

    if(has_cld) {
      for(int i=0;i<512;i++) {
	extras.array[s + cl_qs_idx - 1]->Fill(i,tb_charge_counts_cl[i]);
      }
    }
    for(int i=1;i<Npads1;i++) {
      for(int j=1;j<41;j++) {
	if(charge_counts_cl[i][j]) {
	  ((TH2D *)contents.array[s + qe_idx - 1])->Fill(i, j, charge_counts_cl[i][j]);
	  extras.cl_itpc_phi_charge->Fill(mPhiAngleMap[s-1][j-1][i-1],charge_counts_cl[i][j]);
	}
      }
    }

    extras.cl_itpc_sector_charge->Fill(s,charge_cl);

    charge_cl = 0.0;
    has_cld=0;
    memset(tb_charge_counts_cl, 0, sizeof(tb_charge_counts_cl));
    
    dd = rdr->det("tpx")->get("cld",s) ;
    if(dd) {
      if(tpcDataInThisRun==0) addServerTags("tpc");
      tpcDataInThisRun = 1;

      has_cld = 1;
      cl_max_channels += tpc_max_channels_outer_sector;  // This will not work properly for run18 and earlier data

      while(dd->iterate()) {
	if(dd->row<14) continue; // fix for reading pre run 19 files
	for(u_int i=0;i<dd->ncontent;i++) {
	  if((dd->cld[i].flags==0)) {
	    pix_count_cl += (dd->cld[i].t2 - dd->cld[i].t1)*(dd->cld[i].p2 - dd->cld[i].p1);
	    charge_counts_cl[(int)dd->cld[i].pad][dd->row+27] += dd->cld[i].charge;
	    tb_charge_counts_cl[(int)dd->cld[i].tb] += dd->cld[i].charge;
	    charge_cl += dd->cld[i].charge;
	    ((TH2D*)contents.cl_width_tpx_tb)->Fill(s,dd->cld[i].t2 - dd->cld[i].t1);
	    ((TH2D*)contents.cl_width_tpx_pad)->Fill(s,dd->cld[i].p2 - dd->cld[i].p1);
	  }
	}
      }
    }
    if(has_cld) {
      for(int i=0;i<512;i++) {
	extras.array[s + cl_qs_idx_tpx - 1]->Fill(i,tb_charge_counts_cl[i]);
      }
    }
 
    
    for(int i=1;i<Npads1;i++) {
      for(int j=41;j<Nrows1;j++) {
	if(charge_counts_cl[i][j]) {
	  ((TH2D *)contents.array[s + qe_idx - 1])->Fill(i, j, charge_counts_cl[i][j]);
	  extras.cl_tpx_phi_charge->Fill(mPhiAngleMap[s-1][j-1][i-1],charge_counts_cl[i][j]);
	}
      }
    }
    
    extras.cl_tpx_sector_charge->Fill(s,charge_cl);
    
    
  }  //end s
  
  // Summaries are completed...
  //printf("%d channel counts:   %lf (%lf)\n",rdr->seq, channel_count, pixel_count);  
  //
  // Need scale differently for itpx,itpc addc may not be in same event ???
  // must also scale tpx differently.

  //FV This whole scaling does not make much sense to me. Commented out.
  // It does make some sense since it tries to ensure that adc 
  // and clusters are on approximately the same scale
  //
  /*    
	if(has_adc) {

	n_adc++;
	double adc_scale = (double)(n_adc-1) / (double)n_adc;
	if(n_adc == 1) adc_scale = 1;

	for(int i=1;i<=24;i++) {
	contents.array[i + qs_idx - 1]->Scale(adc_scale);
	}
	contents.h_itpc_phi_charge->Scale(adc_scale);
	contents.h_itpc_sector_charge->Scale(adc_scale);
	}

	if(has_cld) {
	n_cld++;   
	double cld_scale = (double)(n_cld-1) / (double)n_cld;
	if(n_cld == 1) cld_scale = 1;

	for(int i=1;i<=24;i++) {
	extras.array[i + cl_qs_idx - 1]->Scale(cld_scale);
	}
	extras.cl_itpc_phi_charge->Scale(cld_scale);
	extras.cl_itpc_sector_charge->Scale(cld_scale);
	}
  */


  switch(rdr->trgcmd) {
  case 4:
#if 0
    cout << pixel_count << " " << tpc_max_channels << endl;
    cout << itpc_pixel_count << " " << itpc_max_channels << endl;
#endif
    contents.tpc_pix_occ_physics->Fill(100.0 * (double)pixel_count / (tpc_max_channels * 400.0));
    extras.tpc_clpix_occ_physics->Fill(100.0 * (double)pix_count_cl / (cl_max_channels * 400.0));
    contents.itpc_pix_occ_physics->Fill(100.0 * (double)itpc_pixel_count / (itpc_max_channels * 400.0));
    extras.itpc_clpix_occ_physics->Fill(100.0 * (double)itpc_pix_count_cl / (itpc_cl_max_channels * 400.0));

    extras.clusters_per_bx->Fill(bunch7bit, pix_count_cl);

    break;
  case 8:  // Lasers...
  case 9:
#if checklaser
    {
      
      LOG(DBG, "Got a laser...");

      contents.tpc_pix_occ_laser->Fill(100.0 * (double)pixel_count / (tpc_max_channels * 400.0));
      extras.tpc_clpix_occ_laser->Fill(100.0 * (double)pix_count_cl / (cl_max_channels * 400.0));
      contents.itpc_pix_occ_laser->Fill(100.0 * (double)itpc_pixel_count / (itpc_max_channels * 400.0));
      extras.itpc_clpix_occ_laser->Fill(100.0 * (double)itpc_pix_count_cl / (itpc_cl_max_channels * 400.0));

      double vDrift = laserReader->Make(rdr);
     
      //LOG("JEFF","%d vDrift = %lf",rdr->event_number, vDrift);

      if((vDrift > 5.4) && (vDrift < 5.8)) {
	nlasers++;
	contents.h_tpc_drift_vel->Fill(vDrift);
      }

      drift_vel = contents.h_tpc_drift_vel->GetMean();

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

    contents.tpc_pix_occ_pulser->Fill(100.0 * (double)pixel_count / (tpc_max_channels * 400.0));
    extras.tpc_clpix_occ_pulser->Fill(100.0 * (double)pix_count_cl / (cl_max_channels * 400.0));
    contents.itpc_pix_occ_pulser->Fill(100.0 * (double)itpc_pixel_count / (itpc_max_channels * 400.0));
    extras.itpc_clpix_occ_pulser->Fill(100.0 * (double)itpc_pix_count_cl / (itpc_cl_max_channels * 400.0));

    break;
  default:
    LOG(WARN, "Trigger command other than 4,8,9,10: evt %d, token=%d, cmd=%d",rdr->seq, rdr->token, rdr->trgcmd);
  }

  //
  // Normalize the charge histos
  //
  // adc vs sector
  // phy disto

#if fv
  printf("End event %d \n",event_no);
#endif
}

  void tpcBuilder::setPhiAngleMap()
  {
    const Int_t NP = 72; // # padrows
    const Float_t DEG = 57.296;//360/2pi

    Double_t Xpads[NP] = { 
      55.8, 57.4, 59, 60.6, 62.2, 63.8, 65.4, 67, 68.6, 70.2, 
      71.8, 73.4, 75, 76.6, 78.2, 79.8, 81.4, 83, 84.6, 86.2, 
      87.8, 89.4, 91, 92.6, 94.2, 95.8, 97.4, 99, 100.6, 102.2, 
      103.8, 105.4, 107, 108.6, 110.2, 111.8, 113.4, 115, 116.6, 118.2,
      127.195, 129.195, 131.195, 133.195, 135.195, //Outer
      137.195, 139.195, 141.195, 143.195, 145.195,
      147.195, 149.195, 151.195, 153.195, 155.195,
      157.195, 159.195, 161.195, 163.195, 165.195,
      167.195, 169.195, 171.195, 173.195, 175.195,
      177.195, 179.195, 181.195, 183.195, 185.195,
      187.195, 189.195
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
	if (j >= 40) pitch = pitches[1];
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

  void tpcBuilder::main(int argc, char *argv[])
  {
    tpcBuilder me;
    me.Main(argc, argv);
  }

