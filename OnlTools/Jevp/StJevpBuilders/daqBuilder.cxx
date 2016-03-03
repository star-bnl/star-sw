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
#include "daqBuilder.h"
#include <RTS/include/rtsLog.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(daqBuilder);
  

daqBuilder::daqBuilder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"daq";

  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
}

daqBuilder::~daqBuilder() {

  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(contents.array[i]) delete contents.array[i];
  }
}

void daqBuilder::initialize(int argc, char *argv[]) {

  // Initialization of histograms.
  contents.h2_tpc = new TH1F("h2_tpc","Log of TPC Buffer Size",50,0,10);
  contents.h0_evt_size = new TH1F("h0_evt_size","Log of Event Size",50,1,10);
  //contents.h10_bemc_evsize = new TH1F("h10_bemc_evsize","log of BEMC Buffer Size",30,0,6);
  //contents.h11_ftp_evsize = new TH1F("h11_ftp_evsize","log of FTPC Buffer Size",50,0,10);
  //contents.h12_l3_evsize = new TH1F("h12_l3_evsize","log of L3 Buffer Size",30,0,6);
  //contents.h14_tof_evsize = new TH1F("h14_tof_evsize","log of TOF Buffer Size",30,0.,6);
  contents.h155_time_size_2min = new TH2F("h155_time_size_2min","Log10(event size) vs time", 60,0,120,60,0,8);
  contents.h156_time_size_10min = new TH2F("h156_time_size_10min","Log10(event size) vs time", 60,0,600,60,0,8);
  contents.h157_time_size_2hour = new TH2F("h157_time_size_2hour","Log10(event size) vs time", 60,0,7200,60,0,8);
  // contents.h337_ftp_time_size_2hour = new TH2F("h337_ftp_time_size_2hour","FTPC Event Size vs time(sec)",60,0,600,60,0,8);
  contents.h103_tpc_frac = new TH1F("h103_tpc_frac","TPC Event Size Fraction (%)",50,0,100);
  contents.h106_bemc_frac = new TH1F("h106_bemc_frac","BEMC Event Size Fraction (%)",50,0,100);
  //contents.h105_ftp_frac = new TH1F("h105_ftp_frac","FTP Event Size Fraction (%)",50,0,100);
  //contents.h108_l3_frac = new TH1F("h108_l3_frac","L3 Event Fraction (%)",50,0,100);
  contents.h107_tof_frac = new TH1F("h107_tof_frac","TOFp Event Fraction (%)",50,0,100);

  // Add root histograms to Plots
  int np = sizeof(contents) / sizeof(TH1 *);
  JevpPlot *plots[np];

  int n=0;
  plots[n] = new JevpPlot(contents.h2_tpc);
  plots[++n] = new JevpPlot(contents.h0_evt_size);
  //plots[++n] = new JevpPlot(contents.h10_bemc_evsize);
  //plots[++n] = new JevpPlot(contents.h11_ftp_evsize);
  // plots[++n] = new JevpPlot(contents.h12_l3_evsize);
  //plots[++n] = new JevpPlot(contents.h14_tof_evsize);
  plots[++n] = new JevpPlot(contents.h155_time_size_2min);
  plots[n]->setDrawOpts("col");
  plots[n]->optstat = 0;
  plots[++n] = new JevpPlot(contents.h156_time_size_10min);  
  plots[n]->setDrawOpts("col");
  plots[n]->optstat = 0;
  plots[++n] = new JevpPlot(contents.h157_time_size_2hour);   
  plots[n]->setDrawOpts("col");
  plots[n]->optstat = 0;
  //plots[++n] = new JevpPlot(contents.h337_ftp_time_size_2hour);
  plots[++n] = new JevpPlot(contents.h103_tpc_frac);
  plots[++n] = new JevpPlot(contents.h106_bemc_frac);
  //plots[++n] = new JevpPlot(contents.h105_ftp_frac);
  //plots[++n] = new JevpPlot(contents.h108_l3_frac);
  plots[++n] = new JevpPlot(contents.h107_tof_frac);

  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }
}
  
void daqBuilder::startrun(daqReader *rdr) {
  LOG(NOTE, "daqBuilder starting run #%d",rdr->run);
  resetAllPlots();

  t_2min = time(NULL);
  t_10min = time(NULL);
  t_120min = time(NULL);
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void daqBuilder::event(daqReader *rdr)
{
  // Fill Histograms...
  
  int tpc_size = rdr->getDetectorSize("tpx");
  int bemc_size = rdr->getDetectorSize("btow");
  //  int eemc_size = rdr->getDetectorSize("etow");
  //  int bsmd_size = rdr->getDetectorSize("bsmd");
  // int esmd_size = rdr->getDetectorSize("esmd");
  //int ftp_size = rdr->getDetectorSize("ftp");
  //int l3_size = rdr->getDetectorSize("hlt");
  int tof_size = rdr->getDetectorSize("tof");
  int sz = rdr->getDetectorSize("/");
  
  //printf("rdr->getDetectorSize(): %d  evtSize : %d  (diff=%d)\n", sz, rdr->event_size,rdr->event_size-sz);

  contents.h2_tpc->Fill(safelog(tpc_size));
  contents.h0_evt_size->Fill(safelog(sz));
  //contents.h10_bemc_evsize->Fill(safelog(bemc_size));
  //contents.h11_ftp_evsize->Fill(safelog(ftp_size));
  //contents.h12_l3_evsize->Fill(safelog(l3_size));
  //contents.h14_tof_evsize->Fill(safelog(tof_size));
  
  // Reset rolling histos if necessary..
  int tm = time(NULL);
  if(tm > t_2min + 120) {
    t_2min = tm;
    contents.h155_time_size_2min->Reset();
  }
  if(tm > t_10min + 600) {
    t_10min = tm;
    contents.h156_time_size_10min->Reset();
  }
  if(tm > t_120min + 60*120) {
    t_120min = tm;
    contents.h157_time_size_2hour->Reset();
    //contents.h337_ftp_time_size_2hour->Reset();
  }


  contents.h155_time_size_2min->Fill(tm-t_2min, safelog(sz));
  contents.h156_time_size_10min->Fill(tm-t_10min, safelog(sz));
  contents.h157_time_size_2hour->Fill(tm-t_120min, safelog(sz));
  //contents.h337_ftp_time_size_2hour->Fill(tm-t_120min, safelog(ftp_size));

  contents.h103_tpc_frac->Fill(100.0 * (double)tpc_size / (double)sz);
  contents.h106_bemc_frac->Fill(100.0 * (double)bemc_size / (double)sz);
  //contents.h105_ftp_frac->Fill(100.0 * (double)ftp_size / (double)sz);
  //contents.h108_l3_frac->Fill(100.0 * (double)l3_size / (double)sz);
  contents.h107_tof_frac->Fill(100.0 * (double)tof_size / (double)sz);
  // End Fill Histograms...
}

void daqBuilder::stoprun(daqReader *rdr) {
  
}

void daqBuilder::main(int argc, char *argv[])
{
  daqBuilder me;
  
  me.Main(argc, argv);
}

