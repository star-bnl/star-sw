#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "trgStructures.h"
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "upcBuilder.h"
#include <RTS/include/rtsLog.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(upcBuilder);
  

upcBuilder::upcBuilder() {
  plotsetname = (char *)"upc";

  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
}

upcBuilder::~upcBuilder() {

  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(contents.array[i]) delete contents.array[i];
  }
}

void upcBuilder::initialize(int argc, char *argv[]) {

  contents.h_zdce_sum = new TH1D( "h_zdce_sum", "ZDC East Unattenuated Sum", 150, -0.5, 149.5);
  contents.h_zdce_sum->SetXTitle("Sum [ADC Ch.]");
  contents.h_zdce_sum->SetYTitle("Count");
  contents.h_zdcw_sum = new TH1D( "h_zdcw_sum", "ZDC West Unattenuated Sum", 150, -0.5, 149.5);
  contents.h_zdcw_sum->SetXTitle("Sum [ADC Ch.]");
  contents.h_zdcw_sum->SetYTitle("Count");
  contents.h_zdce_sum_vs_ctb_sum = new TH2D( "h_zdce_sum_vs_ctb_sum", "ZDC East Unattenuated Sum vs. CTB ADC Sum",
					      210, -0.5, 209.5, 150, -0.5, 149.5);
  contents.h_zdce_sum_vs_ctb_sum->SetXTitle("CTB Sum [ADC Ch.]");
  contents.h_zdce_sum_vs_ctb_sum->SetYTitle("ZDC Sum [ADC Ch.]");
  contents.h_zdcw_sum_vs_ctb_sum = new TH2D( "h_zdcw_sum_vs_ctb_sum", "ZDC West Unattenuated Sum vs. CTB ADC Sum",
					      210, -0.5, 209.5, 150, -0.5, 149.5);
  contents.h_zdcw_sum_vs_ctb_sum->SetXTitle("CTB Sum [ADC Ch.]");
  contents.h_zdcw_sum_vs_ctb_sum->SetYTitle("ZDC Sum [ADC Ch.]");
  

  // Add root histograms to Plots
  int np = sizeof(contents) / sizeof(TH1 *);
  JevpPlot *plots[np];
  
  // Add Plots to plot set...
  for(int i=0;i<np;i++) {
    plots[i] = new JevpPlot(contents.array[i]);
    addPlot(plots[i]);
  }

}
  
void upcBuilder::startrun(daqReader *rdr) {
  LOG("JEFF", "upcBuilder starting run #%d",rdr->run);
  resetAllPlots();
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void upcBuilder::event(daqReader *rdr)
{
  unsigned short adc, tac;
  
  StTriggerData *trgd = getStTriggerData(rdr);
  if(!trgd) return;

  if(trgd){
    unsigned int zdcEastUnattSum = trgd->zdcUnAttenuated(east);
    unsigned int zdcWestUnattSum = trgd->zdcUnAttenuated(west);
    unsigned int ctbAdcSum       = trgd->lastDSM(3);
    
    contents.h_zdce_sum->Fill(zdcEastUnattSum);
    contents.h_zdcw_sum->Fill(zdcWestUnattSum);
    contents.h_zdce_sum_vs_ctb_sum->Fill(ctbAdcSum, zdcEastUnattSum);
    contents.h_zdcw_sum_vs_ctb_sum->Fill(ctbAdcSum, zdcWestUnattSum);
  }

  if(trgd) 
    delete trgd;
}

void upcBuilder::stoprun(daqReader *rdr) {
}

void upcBuilder::main(int argc, char *argv[])
{
  upcBuilder me;
  
  me.Main(argc, argv);
}

