#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "DAQ_PMD/daq_pmd.h"

#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "pmdBuilder.h"
#include <RTS/include/rtsLog.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(pmdBuilder);
  

pmdBuilder::pmdBuilder() {
  plotsetname = (char *)"pmd";

  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
}

pmdBuilder::~pmdBuilder() {

  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(contents.array[i]) delete contents.array[i];
  }
}

void pmdBuilder::initialize(int argc, char *argv[]) {

  // Initialization of histograms.

  //contents.h49_pmd = new TH1D("h49_pmd","PMDC Occupancy (in %)",100,0,10);
  for(int i=0;i<48;i++) {
    char name[256];
    char title[256];
    int tchain = i+1;
    if(tchain == 36) tchain = 6;
    else if (tchain == 6) tchain = 36;
   
    sprintf(name, "h%d_pmd_Chain_%d", i+382, i+1);
    sprintf(title, "Channel vs ADC for %s Chain %d",(tchain >=25) ? "PreShower" : "CPV", tchain);

    contents.pmd_chains[i] = new TH1F(name,title,1728,-.5,1727.5);
  }

  contents.h430_pmd_Chain_vs_channel = new TH2F("h430_pmd_Chain_vs_channel","Chain vs Channel 2D", 1728,-0.5,1727.5,48,0.5,48.5);
  contents.h431_pmd_Chain_vs_channel_adc = new TH2F("h431_pmd_Chain_vs_channel_adc", "Chain vs Channel 2D (ADC weighted)",1728,-0.5,1727.5,48,0.5,48.5);

  // Add root histograms to Plots
  int np = sizeof(contents) / sizeof(TH1 *);
  JevpPlot *plots[np];

  int n=0;
  for(int i=0;i<48;i++) {
    plots[i] = new JevpPlot(contents.pmd_chains[i]);
  }
  n=47;
  plots[++n] = new JevpPlot(contents.h430_pmd_Chain_vs_channel);
  plots[n]->setDrawOpts("colz");
  plots[++n] = new JevpPlot(contents.h431_pmd_Chain_vs_channel_adc);
  plots[n]->setDrawOpts("colz");

  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }
}
  
void pmdBuilder::startrun(daqReader *rdr) {
  LOG("JEFF", "pmdBuilder starting run #%d",rdr->run);
  resetAllPlots();
}

#define safelog(x) ((x > 0) ? log10(x) : 0)
#define MAX_L3_SZ 1000000

void pmdBuilder::event(daqReader *rdr)
{
  static const int Crate = 2;			// 2 Crates max
  static const int CRAM=12; 			// Max no of Crams
  static const int BLOCK=2; 			// Max no of blocks in each cram
  static const int CHANNEL=1728; 		//Max no of channels in eack block


  daq_dta *dd = rdr->det("pmd")->get("legacy") ;

  double charge = 0;
  double hits = 0;
 
  while(dd && dd->iterate()) {	
    pmd_t *pmd = (pmd_t *) dd->Void ;
    
    if(pmd->mode == 0) {     // normal event
      for( int sec=0; sec < Crate; sec++)//<2
	{
	  for(int rb=0; rb < CRAM; rb++)// < 12
	    {
	      for(int mz=0; mz < BLOCK; mz++)//BLK
		{
		  for(int channel=0; channel < CHANNEL; channel++)
		    {//<1728
		      Int_t Chain_No=(rb+1)+(sec*12)+(mz*24);
		      Int_t index=Chain_No - 1;
		   
		      if(channel >= 1)
		      contents.pmd_chains[index]->Fill(channel, pmd->adc[sec][rb][mz][channel]);

		      if(pmd->adc[sec][rb][mz][channel] > 0) {
			((TH2F *)contents.h430_pmd_Chain_vs_channel)->Fill(channel, Chain_No);
		      }
		      ((TH2F *)contents.h431_pmd_Chain_vs_channel_adc)->Fill(channel, Chain_No, pmd->adc[sec][rb][mz][channel]);
		    }
		} //mz(BLK)
	    } //rb (CRAM)
	}//sec
    }
  }  
}

void pmdBuilder::stoprun(daqReader *rdr) {
}

void pmdBuilder::main(int argc, char *argv[])
{
  pmdBuilder me;
  
  me.Main(argc, argv);
}

