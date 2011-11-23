#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "StDaqLib/TRG/trgStructures2009.h"
#include "StEvent/StTriggerData2009.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "fgtBuilder.h"
#include <RTS/include/rtsLog.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(fgtBuilder);
  

fgtBuilder::fgtBuilder() {
  plotsetname = (char *)"fgt";
  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
}

fgtBuilder::~fgtBuilder() {

  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(contents.array[i]) delete contents.array[i];
  }
}

void fgtBuilder::initialize(int argc, char *argv[]) {

  // Initialization of histograms.
  contents.h2_tst = new TH1F("h2_tst","tst",50,0,10);

  // Add root histograms to Plots
  int np = sizeof(contents) / sizeof(TH1 *);
  JevpPlot *plots[np];

  int n=0;
  plots[n] = new JevpPlot(contents.h2_tmp);
  //  plots[++n] = new JevpPlot(contents.h0_evt_size);

  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }
}
  
void fgtBuilder::startrun(daqReader *rdr) {
  LOG(NOTE, "fgtBuilder starting run #%d",rdr->run);
  resetAllPlots();

  t_2min = time(NULL);
  t_10min = time(NULL);
  t_120min = time(NULL);


}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void fgtBuilder::event(daqReader *rdr)
{
  contents.h2_tmp->Fill(tRnd.Rndm(0));
  // StTriggerData2009 *trgd2009;
  // int run = rdr->run;

  // Fill Histograms...
  //  int tpc_size = rdr->getDetectorSize("tpx");
  //  contents.h2_tpc->Fill(safelog(tpc_size));
  
  // Reset rolling histos if necessary..
  //  int tm = time(NULL);
  //  if(tm > t_2min + 120) {
  //    t_2min = tm;
  //    contents.h155_time_size_2min->Reset();
  //  }

  //  contents.h155_time_size_2min->Fill(tm-t_2min, safelog(sz));
  // End Fill Histograms...
}

void fgtBuilder::stoprun(daqReader *rdr) {
  
}

void fgtBuilder::main(int argc, char *argv[])
{
  fgtBuilder me;
  me.Main(argc, argv);
}

