#include <stdio.h>
#include <stdlib.h>

#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "fpdBuilder.h"
#include <RTS/include/rtsLog.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(fpdBuilder);
  
typedef JevpPlot * ptrJevpPlot;

fpdBuilder::fpdBuilder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"fpd";
  
  int np = sizeof(contents) / sizeof(TH1 *);

  plots = new ptrJevpPlot[np]();
  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
  memset(plots, 0, sizeof(JevpPlot *) * np);
}

fpdBuilder::~fpdBuilder() {
  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(plots[i]) delete plots[i];
  }
  
  delete plots;
}

void fpdBuilder::initialize(int argc, char *argv[]) {

  // Build Root Histograms...

  contents.h206_fpd_EN_adcsum = new TH1F("h206_fpd_EN_adcsum","FPD EN AdcSum",128, 0.,256);
  contents.h207_fpd_ES_adcsum = new TH1F("h207_fpd_ES_adcsum","FPD ES AdcSum",128, 0.,256);
  contents.h208_fpd_ET_adcsum = new TH1F("h208_fpd_ET_adcsum","FPD ET AdcSum",128, 0.,256);
  contents.h209_fpd_EB_adcsum = new TH1F("h209_fpd_EB_adcsum","FPD EB AdcSum",128, 0.,256);
  contents.h210_fpd_WN_adcsum = new TH1F("h210_fpd_WN_adcsum","FPD WN AdcSum",128, 0.,256);
  contents.h211_fpd_WS_adcsum = new TH1F("h211_fpd_WS_adcsum","FPD WS AdcSum",128, 0.,256);
  contents.h212_fpd_WT_adcsum = new TH1F("h212_fpd_WT_adcsum","FPD WT AdcSum",128, 0.,256);
  contents.h213_fpd_WB_adcsum = new TH1F("h213_fpd_WB_adcsum","FPD WB AdcSum",128, 0.,256);
  contents.h214_fpd_EN_hitmap = new TH1F("h214_fpd_EN_hitmap","FPD EN Hitmap",49, 0.5,49.5);
  contents.h215_fpd_ES_hitmap = new TH1F("h215_fpd_ES_hitmap","FPD ES Hitmap",49, 0.5,49.5);
  contents.h216_fpd_ET_hitmap = new TH1F("h216_fpd_ET_hitmap","FPD ET Hitmap",25, 0.5,25.5);
  contents.h217_fpd_EB_hitmap = new TH1F("h217_fpd_EB_hitmap","FPD EB Hitmap",25, 0.5,25.5);
  contents.h218_fpd_WN_hitmap = new TH1F("h218_fpd_WN_hitmap","FPD WN Hitmap",49, 0.5,49.5);
  contents.h219_fpd_WS_hitmap = new TH1F("h219_fpd_WS_hitmap","FPD WS Hitmap",49, 0.5,49.5);
  contents.h220_fpd_WT_hitmap = new TH1F("h220_fpd_WT_hitmap","FPD WT Hitmap",25, 0.5,25.5);
  contents.h221_fpd_WB_hitmap = new TH1F("h221_fpd_WB_hitmap","FPD WB Hitmap",25, 0.5,25.5);
  contents.h222_fpd_EN_weighted_hitmap = new TH1F("h222_fpd_EN_weighted_hitmap","FPD EN weighted Hitmap",49, 0.5,49.5);
  contents.h223_fpd_ES_weighted_hitmap = new TH1F("h223_fpd_ES_weighted_hitmap","FPD ES weighted Hitmap",49, 0.5,49.5);
  contents.h224_fpd_ET_weighted_hitmap = new TH1F("h224_fpd_ET_weighted_hitmap","FPD ET weighted Hitmap",25, 0.5,25.5);
  contents.h225_fpd_EB_weighted_hitmap = new TH1F("h225_fpd_EB_weighted_hitmap","FPD EB weighted Hitmap",25, 0.5,25.5);
  contents.h226_fpd_WN_weighted_hitmap = new TH1F("h226_fpd_WN_weighted_hitmap","FPD WN weighted Hitmap",49, 0.5,49.5);
  contents.h227_fpd_WS_weighted_hitmap = new TH1F("h227_fpd_WS_weighted_hitmap","FPD WS weighted Hitmap",49, 0.5,49.5);
  contents.h228_fpd_WT_weighted_hitmap = new TH1F("h228_fpd_WT_weighted_hitmap","FPD WT weighted Hitmap",25, 0.5,25.5);
  contents.h229_fpd_WB_weighted_hitmap = new TH1F("h229_fpd_WB_weighted_hitmap","FPD WB weighted Hitmap",25, 0.5,25.5);

  // Add root histograms to Plots
  JevpPlot *plots[100];
  int n=0;

  plots[n] = new JevpPlot(contents.h206_fpd_EN_adcsum);
  plots[++n] = new JevpPlot(contents.h207_fpd_ES_adcsum);
  plots[++n] = new JevpPlot(contents.h208_fpd_ET_adcsum);
  plots[++n] = new JevpPlot(contents.h209_fpd_EB_adcsum);
  plots[++n] = new JevpPlot(contents.h210_fpd_WN_adcsum);
  plots[++n] = new JevpPlot(contents.h211_fpd_WS_adcsum);
  plots[++n] = new JevpPlot(contents.h212_fpd_WT_adcsum);
  plots[++n] = new JevpPlot(contents.h213_fpd_WB_adcsum);
  plots[++n] = new JevpPlot(contents.h214_fpd_EN_hitmap);
  plots[++n] = new JevpPlot(contents.h215_fpd_ES_hitmap);
  plots[++n] = new JevpPlot(contents.h216_fpd_ET_hitmap);
  plots[++n] = new JevpPlot(contents.h217_fpd_EB_hitmap);
  plots[++n] = new JevpPlot(contents.h218_fpd_WN_hitmap);
  plots[++n] = new JevpPlot(contents.h219_fpd_WS_hitmap);
  plots[++n] = new JevpPlot(contents.h220_fpd_WT_hitmap);
  plots[++n] = new JevpPlot(contents.h221_fpd_WB_hitmap);
  plots[++n] = new JevpPlot(contents.h222_fpd_EN_weighted_hitmap);
  plots[++n] = new JevpPlot(contents.h223_fpd_ES_weighted_hitmap);
  plots[++n] = new JevpPlot(contents.h224_fpd_ET_weighted_hitmap);
  plots[++n] = new JevpPlot(contents.h225_fpd_EB_weighted_hitmap);
  plots[++n] = new JevpPlot(contents.h226_fpd_WN_weighted_hitmap);
  plots[++n] = new JevpPlot(contents.h227_fpd_WS_weighted_hitmap);
  plots[++n] = new JevpPlot(contents.h228_fpd_WT_weighted_hitmap);
  plots[++n] = new JevpPlot(contents.h229_fpd_WB_weighted_hitmap);

  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);

    contents.array[i]->SetFillColor(30);
  }
  
}
  
void fpdBuilder::startrun(daqReader *rdr) {
  LOG("JEFF", "TriggerPlotBuilder starting run #%d",rdr->run);
  resetAllPlots();
}

void fpdBuilder::event(daqReader *rdr)
{
    //int run = rdr->run;

  StTriggerData *trgd = getStTriggerData(rdr);
  if(!trgd) return;

  // No code! available...
  //
  

  if(trgd) delete trgd;
}

void fpdBuilder::stoprun(daqReader *rdr) {
//   printf("Stopping run #%d\n",run);
//   status.setEndOfRun(1);
//   send((TObject *)&status);
}

void fpdBuilder::main(int argc, char *argv[])
{
  fpdBuilder me;
  
  me.Main(argc, argv);
}

