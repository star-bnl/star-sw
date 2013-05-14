#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "pxlBuilder.h"
#include <RTS/include/rtsLog.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(pxlBuilder);
  

pxlBuilder::pxlBuilder(JevpServer *parent) : JevpPlotSet(parent) {
  plotsetname = (char *)"pxl";

  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
}

pxlBuilder::~pxlBuilder() {

  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(contents.array[i]) delete contents.array[i];
  }
}

void pxlBuilder::initialize(int argc, char *argv[]) {

  // Initialization of histograms.
  contents.myhisto = new TH1F("myhisto","My Histogram",50,0,10);


  // Add root histograms to Plots
  int np = sizeof(contents) / sizeof(TH1 *);
  JevpPlot *plots[np];

  int n=0;
  plots[n] = new JevpPlot(contents.myhisto);
  //plots[n]->setDrawOpts("col");
  plots[n]->optstat = 0;

  //plots[++n] = new JevpPlot(contents.h0_evt_size);
  //plots[n]->optstat = 0;

  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }
}
  
void pxlBuilder::startrun(daqReader *rdr) {
  LOG(NOTE, "pxlBuilder starting run #%d",rdr->run);
  resetAllPlots();
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void pxlBuilder::event(daqReader *rdr)
{
  int pxl_size = rdr->getDetectorSize("pxl");
  contents.myhisto->Fill(safelog(pxl_size));
  // End Fill Histograms...
}

void pxlBuilder::stoprun(daqReader *rdr) {
  
}

void pxlBuilder::main(int argc, char *argv[])
{
  pxlBuilder me;
  
  me.Main(argc, argv);
}

