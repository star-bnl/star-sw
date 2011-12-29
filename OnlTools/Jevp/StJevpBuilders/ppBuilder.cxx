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
#include "ppBuilder.h"
#include <RTS/include/rtsLog.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(ppBuilder);
  

ppBuilder::ppBuilder(JevpServer *parent) : JevpPlotSet(parent) {
  plotsetname = (char *)"pp";

  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
}

ppBuilder::~ppBuilder() {

  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(contents.array[i]) delete contents.array[i];
  }
}

void ppBuilder::initialize(int argc, char *argv[]) {

  const char *hist_name[32] = { "RPEVU1_ADC", "RPEVU2_ADC", "RPEVD1_ADC", "RPEVD2_ADC", "RPWVU1_ADC", "RPWVU2_ADC", "RPWVD1_ADC", "RPWVD2_ADC",
				"RPEHO1_ADC", "RPEHO2_ADC", "RPEHI1_ADC", "RPEHI2_ADC", "RPWHO1_ADC", "RPWHO2_ADC", "RPWHI1_ADC", "RPWHI2_ADC",
				"RPEVU1_TAC", "RPEVU2_TAC", "RPEVD1_TAC", "RPEVD2_TAC", "RPWVU1_TAC", "RPWVU2_TAC", "RPWVD1_TAC", "RPWVD2_TAC",
				"RPEHO1_TAC", "RPEHO2_TAC", "RPEHI1_TAC", "RPEHI2_TAC", "RPWHO1_TAC", "RPWHO2_TAC", "RPWHI1_TAC", "RPWHI2_TAC" };

  // Initialization of histograms.
  for(int i=0;i<32;i++) {
    char nn[256];
    sprintf(nn, "h_pp2pp_%s",hist_name[i]);

    contents.h_P2P[i] = new TH1D(nn, nn, 256,0,256);
  }

  // Add root histograms to Plots
  int np = sizeof(contents) / sizeof(TH1 *);
  JevpPlot *plots[np];

  for(int i=0;i<32;i++) {
    plots[i] = new JevpPlot(contents.h_P2P[i]);
  }


  // Add Plots to plot set...
  for(int i=0;i<32;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }

}
  
void ppBuilder::startrun(daqReader *rdr) {
  LOG("JEFF", "ppBuilder starting run #%d",rdr->run);
  resetAllPlots();
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void ppBuilder::event(daqReader *rdr)
{
  unsigned short adc, tac;
  
  StTriggerData *trgd = getStTriggerData(rdr);
  if(!trgd) return;

  if(trgd){
    int i=0;
    for(int vh=0; vh<2; vh++){
      for(int ew=0; ew<2; ew++){
        for(int udio=0; udio<2; udio++){
          for(int ch=0; ch<2; ch++) {
	    adc = trgd->pp2ppADC((StBeamDirection)ew,vh,udio,ch);
	    contents.h_P2P[i]->Fill( double(adc) );

	    tac = trgd->pp2ppTAC((StBeamDirection)ew,vh,udio,ch);
	    contents.h_P2P[i+16]->Fill( double(tac) );

            i++;
          }
        }
      }
    }
  }
}

void ppBuilder::stoprun(daqReader *rdr) {
}

void ppBuilder::main(int argc, char *argv[])
{
  ppBuilder me;
  
  me.Main(argc, argv);
}

