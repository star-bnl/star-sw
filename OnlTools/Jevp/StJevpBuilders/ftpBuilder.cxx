#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_FTP/daq_ftp.h"

#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "ftpBuilder.h"
#include <RTS/include/rtsLog.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(ftpBuilder);
  

ftpBuilder::ftpBuilder() {
  plotsetname = (char *)"ftp";

  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
}

ftpBuilder::~ftpBuilder() {

  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(contents.array[i]) delete contents.array[i];
  }
}

void ftpBuilder::initialize(int argc, char *argv[]) {

  // Initialization of histograms.

  contents.h49_ftp = new TH1D("h49_ftp","FTPC Occupancy (in %)",100,0,10);
  contents.h51_ftp_OccLaser = new TH1D("h51_ftp_OccLaser","FTPC Occupancy (in %) Lasers",100,0,10);
  contents.h48_ftp_charge = new TH1D("h48_ftp_charge","FTPC Charge",30,0,1e7);
  contents.h50_ftp_OccPulser = new TH1D("h50_ftp_OccPulser","FTPC Occupancy (in %) Pulsers",100,0,10);
  contents.h109_ftp_west_time = new TH1D("h109_ftp_west_time","FTPC West timebins",256,-0.5,255.5);
  contents.h110_ftp_east_time = new TH1D("h110_ftp_east_time","FTPC East timebins",256,-0.5,255.5);
  contents.h338_ftp_west = new TH2D("h338_ftp_west","FTPC West pad charge: pad vs row",10,0,10,960,0,960);
  contents.h339_ftp_east = new TH2D("h339_ftp_east","FTPC East pad charge: pad vs row",10,0,10,960,0,960);
  
  // Add root histograms to Plots
  int np = sizeof(contents) / sizeof(TH1 *);
  JevpPlot *plots[np];

  int n=0;
  plots[n] = new JevpPlot(contents.h49_ftp);
  plots[++n] = new JevpPlot(contents.h51_ftp_OccLaser);
  plots[++n] = new JevpPlot(contents.h48_ftp_charge);
  plots[n]->logx = 1;
  plots[++n] = new JevpPlot(contents.h50_ftp_OccPulser);
  plots[++n] = new JevpPlot(contents.h109_ftp_west_time);
  plots[++n] = new JevpPlot(contents.h110_ftp_east_time);
  plots[++n] = new JevpPlot(contents.h338_ftp_west);
  plots[n]->setDrawOpts("colz");
  plots[n]->optstat = 0;
  plots[++n] = new JevpPlot(contents.h339_ftp_east);
  plots[n]->setDrawOpts("colz");
  plots[n]->optstat = 0;
  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }
}
  
void ftpBuilder::startrun(daqReader *rdr) {
  LOG("JEFF", "ftpBuilder starting run #%d",rdr->run);
  resetAllPlots();
}

#define safelog(x) ((x > 0) ? log10(x) : 0)
#define MAX_L3_SZ 1000000

void ftpBuilder::event(daqReader *rdr)
{
  daq_dta *dd = rdr->det("ftp")->get("legacy") ;

  double charge = 0;
  double hits = 0;
 
  while(dd && dd->iterate()) {	
    ftp_t *ftp = (ftp_t *) dd->Void ;
		
    for(int ss=0;ss<2;ss++) {
      for(int rb=0;rb<10;rb++) {
	for(int pad=0;pad<960;pad++) {
	  for(int tbi=0;tbi<ftp->counts[ss][rb][pad];tbi++) {

	    int tb = ftp->timebin[ss][rb][pad][tbi];
	    int adc = ftp->adc[ss][rb][pad][tbi];

	    hits += 1;
	    charge += adc;
	    
	    if(ss == 0) {
	      contents.h109_ftp_west_time->Fill((double)tb, (double)adc);
	      contents.h338_ftp_west->Fill(rb,pad,adc);
	    }
	    else {
	      contents.h110_ftp_east_time->Fill((double)tb, (double)adc);
	      contents.h339_ftp_east->Fill(rb,pad,adc);
	    }
	  }
	}
      }
    }

    contents.h48_ftp_charge->Fill(charge);
    
    double occ = 100.0 * hits / ((double)(2.0 * 10.0 * 960.0 * 196.0));   // 196 timebins...
    
    switch (rdr->trgcmd) {
    case 10:  // pulser
      contents.h50_ftp_OccPulser->Fill(occ);
      break;
    case 8:   // laser
    case 9:
      contents.h51_ftp_OccLaser->Fill(occ);
      break;
    default:  // physics...
      contents.h49_ftp->Fill(occ);
    }
  }
  
}

void ftpBuilder::stoprun(daqReader *rdr) {
}

void ftpBuilder::main(int argc, char *argv[])
{
  ftpBuilder me;
  
  me.Main(argc, argv);
}

