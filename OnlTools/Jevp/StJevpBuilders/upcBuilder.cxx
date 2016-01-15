#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
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
  

upcBuilder::upcBuilder(JevpServer *parent) : JevpBuilder(parent) {
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

  contents.h_zdce_sum = new TH1D( "h_zdce_sum", "ZDC East Unattenuated Sum", 150, -0.5, 1800.5);
  contents.h_zdce_sum->SetXTitle("Sum [ADC Ch.]");
  contents.h_zdce_sum->SetYTitle("Count");
  contents.h_zdcw_sum = new TH1D( "h_zdcw_sum", "ZDC West Unattenuated Sum", 150, -0.5, 1800.5);
  contents.h_zdcw_sum->SetXTitle("Sum [ADC Ch.]");
  contents.h_zdcw_sum->SetYTitle("Count");
  contents.h_zdce_sum_vs_zdcw_sum = new TH2D( "h_zdce_sum_vs_zdcw_sum", "ZDC East Unattenuated Sum vs. ZDC West Unatt.  ADC Sum",
					      150, -0.5, 1800.5, 150, -0.5, 1800.5);


  contents.h_zdce_sum_vs_zdcw_sum->SetXTitle("ZDC West Sum [ADC Ch.]");
  contents.h_zdce_sum_vs_zdcw_sum->SetYTitle("ZDC East Sum [ADC Ch.]");

  contents.h_bbce_adc = new TH1D( "h_bbce_adc", "BBC East ADC", 150, -0.5, 600.5);
  contents.h_bbce_adc->SetXTitle("Sum [ADC Ch.]");
  contents.h_bbcw_adc = new TH1D( "h_bbcw_adc", "BBC West ADC", 150, -0.5, 600.5);
  contents.h_bbcw_adc->SetXTitle("Sum [ADC Ch.]");

  contents.upcTOF_L1mult_vs_ZDCadcsum=new TH2F("upcTOF_L1_vs_ZDCadcsum","TOF_L1_vs_ZDCadcsum",144,0.5,1800.5,150,0,3000);
  

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

    unsigned int bbcADCeast       = trgd->bbcADCSum(east);
    unsigned int bbcADCwest       = trgd->bbcADCSum(west);
    unsigned int bbcLargeTileEast = trgd->bbcADCSumLargeTile(east);
    unsigned int bbcLargeTileWest = trgd->bbcADCSumLargeTile(west);
    
    contents.h_zdce_sum->Fill(zdcEastUnattSum);
    contents.h_zdcw_sum->Fill(zdcWestUnattSum);
    contents.h_zdce_sum_vs_zdcw_sum->Fill(zdcWestUnattSum, zdcEastUnattSum);


    contents.h_bbce_adc->Fill(bbcADCeast);
    contents.h_bbcw_adc->Fill(bbcADCwest);
    float TOF_L1mult = (float)trgd->tofMultiplicity(0);

    contents.upcTOF_L1mult_vs_ZDCadcsum->Fill(TOF_L1mult, trgd->zdcHardwareSum());

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

