#include <math.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
//#include <TROOT.h>
#include <TStyle.h>
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TCanvas.h>
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "LaserReader.h"
#include <RTS/include/rtsLog.h>

float LaserReader::Make(daqReader *rdr)
{
  static int loLim[7] = {40,  80, 140, 180, 240, 290, 320};
  static int hiLim[7] = {60, 100 ,160, 210, 260, 310, 360};

  resetAll();

  // First store the appropriate hits...
  for(int s=2;s<=24;s += 2) {
    if(s == 10) continue;   // no lasers here...?
    if(s == 16) continue;

    
    daq_dta *dd = rdr->det("tpx")->get("cld",s,6);   // only care about padrow 45!
    if(!dd) continue;
    
    while(dd->iterate()) {
      if(dd->row != 45) continue;
      
      for(unsigned int i=0;i<dd->ncontent;i++) {
	double q = dd->cld[i].charge;
	double tb = dd->cld[i].tb - 15.0;
	double pad = dd->cld[i].pad;
	
	if((pad >= 68) && (pad <= 76)) {  // good pad

	  for(int j=0;j<7;j++) {

	    if((tb >= loLim[j]) && (tb <= hiLim[j])) {  // log the hit...
	      
	      int bin = 7*((s/2)-1) + j + 1;
	      double x = crossingHitsHist->GetBinContent(bin);
	      if(x == 0) {
		crossingHitsHist->SetBinContent(bin, tb);
	      }
	      else {    // multiple hits -->  bad rms!
		crossingHitsHist->SetBinContent(bin, -1.0);
	      }
	      break;
	    }
	  }
	}
      }
    } 
  }


  // Now calculate the drift velocities...
  //const double TPC_DELTAT = 0.106574;
  //const double TPC_DELTAT = 0.108508;
  const double TPC_DELTAT = 0.106576; //200GeV d+Au
  //const double TPC_DELTAT = .10982996;   // 3.875 GeV fixed:   f=9.104984

  static double LaserPosition[12][7] =
    { {-179.353, -151.665, -120.698, -90.8549, -59.3999, -32.1487, 0.0},
      {-179.207, -151.674, -120.563, -90.6271, -59.6521, -32.0164, 0.0},
      {-179.089, -151.571, -120.368, -90.553,  -59.5709, -31.9249, 0.0},
      {-179.153, -151.63,  -120.537, -90.6864, -59.6839, -31.9726, 0.0},
      {-179.219, -151.566, -120.598, -90.6391, -59.6167, -32.011,  0.0},
      {-179.198, -151.572, -120.411, -90.5936, -59.4754, -31.951,  0.0},
      { 179.211,  151.567,  120.522,  90.8314,  59.7034,  32.126,  0.0},
      { 179.291,  151.72,   120.616,  90.7793,  59.73,    32.0686, 0.0},
      { 179.079,  151.591,  120.479,  90.5401,  59.5061,  31.8956, 0.0},
      { 179.264,  151.672,  120.632,  90.7614,  59.7441,  32.1511, 0.0},
      { 179.247,  151.597,  120.589,  90.6547,  59.6163,  32.0717, 0.0},
      { 179.182,  151.769,  120.625,  90.7889,  59.7514,  31.9764, 0.0} };
  
  for(int i=0;i<12;i++) {   
    for(int l=0;l<6;l++) {   // loop over each pair of mirror positions...
      for(int m=0;m<7-l;m++) {
	int bin = i*7 + l + 1;
	double t1 = crossingHitsHist->GetBinContent(bin);
	double t2 = crossingHitsHist->GetBinContent(bin+m);
	
	if((t1 <= 0) || (t2 <= 0)) continue;

	double time = fabs(t2-t1) * TPC_DELTAT;
	double dist = fabs(LaserPosition[i][l+m] - LaserPosition[i][l]);
	
	if(time == 0) continue;
	double v = dist/time;

	driftVelocityHist->Fill(v);
      }
    }
  }

  double vel = driftVelocityHist->GetMean();
  return vel;

  //if(vel<5.4 || vel>5.8) vel = 1972.;//flag as bad value to trigger reset in HistoHandler.cxx

}

LaserReader::LaserReader()
{
  crossingHitsHist = new TH1D("hitStorage", "hitStorage", 84, 0, 84);
  driftVelocityHist = new TH1D("driftVelocityHist","driftVelocityHist",200,4.0,8.0);
}



void LaserReader::resetAll()
{
  crossingHitsHist->Reset();
  driftVelocityHist->Reset();
}


