#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include "RTS/EventTracker/eventTrackerLib.hh"

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "l3Builder.h"
#include <RTS/include/rtsLog.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(l3Builder);
  

l3Builder::l3Builder(JevpServer *parent) : JevpBuilder(parent) {
  plotsetname = (char *)"l3";

  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
}

l3Builder::~l3Builder() {

  // Delete any existing histograms...
  int n = sizeof(contents) / sizeof(TH1 *);
  for(int i=0;i<n;i++) {
    if(contents.array[i]) delete contents.array[i];
  }
}

void l3Builder::initialize(int argc, char *argv[]) {

  // Initialization of histograms.
  contents.h88_l3_tracks = new TH1D("h88_l3_tracks","L3 Number of tracks",200,0,800);
  contents.h89_l3_Xvertex = new TH1D("h89_l3_Xvertex","L3 X vertex",100,-4,4);
  contents.h90_l3_Yvertex = new TH1D("h90_l3_Yvertex","L3 Y vertex",100,-4,4);
  contents.h91_l3_Zvertex = new TH1D(",h91_l3_Zvertex","L3 Z vertex",100,-200,200);
  contents.h100_l3_zdc_zvertex = new TH2D("h100_l3_zdc_zvertex","ZDC Vertex vs L3 Vertex",100,-200,200,100,-100,100);
  contents.h230_l3_bbc_zvertex = new TH2D("h230_l3_bbc_zvertex","BBC Vertex vs L3 Vertex",100,-100,100,100,-100,100);
  contents.h101_l3_x_y = new TH2D("h101_l3_x_y","L3 Vertex Y vs X",100,-10,10,100,-10,10);
  contents.h112_l3_vertex_zdctimediff = new TH2D("h112_l3_vertex_zdctimediff","L3 Vertex-Z vs ZDC Timing difference",100,-200,200,100,-100,100);
  contents.h62_l3_pt = new TH1D("h62_l3_pt","L3 Track Pt",100,0,2);
  contents.h63_l3_phi0 = new TH1D("h63_l3_phi0","L3 Track Phi0",100,0,6.29);
  contents.h64_l3_psi = new TH1D("h64_l3_psi","L3 Track Psi",100,-3.15,3.15);
  contents.h65_l3_trk_vertex = new TH1D("h65_l3_trk_vertex","L3 Track  Z0",200,-220.,220);
  
  // Add root histograms to Plots
  int np = sizeof(contents) / sizeof(TH1 *);
  JevpPlot *plots[np];

  int n=0;
  plots[n] = new JevpPlot(contents.h88_l3_tracks);
  plots[++n] = new JevpPlot(contents.h89_l3_Xvertex);
  plots[++n] = new JevpPlot(contents.h90_l3_Yvertex);
  plots[++n] = new JevpPlot(contents.h91_l3_Zvertex);
  plots[++n] = new JevpPlot(contents.h100_l3_zdc_zvertex);
  plots[n]->setDrawOpts((char *)"colz");
  plots[++n] = new JevpPlot(contents.h230_l3_bbc_zvertex);
  plots[n]->setDrawOpts((char *)"colz");
  plots[++n] = new JevpPlot(contents.h101_l3_x_y);
  plots[n]->setDrawOpts((char *)"colz");
  plots[++n] = new JevpPlot(contents.h112_l3_vertex_zdctimediff);
  plots[n]->setDrawOpts((char *)"colz");
  plots[++n] = new JevpPlot(contents.h62_l3_pt);
  plots[++n] = new JevpPlot(contents.h63_l3_phi0);
  plots[++n] = new JevpPlot(contents.h64_l3_psi);
  plots[++n] = new JevpPlot(contents.h65_l3_trk_vertex);

  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }
}
  
void l3Builder::startrun(daqReader *rdr) {
  LOG("JEFF", "l3Builder starting run #%d",rdr->run);
  resetAllPlots();
}

#define safelog(x) ((x > 0) ? log10(x) : 0)
#define MAX_L3_SZ 1000000

void l3Builder::event(daqReader *rdr)
{
  static EventTracker *evtTracker = new EventTracker();
  static L3_P *l3p = (L3_P *)malloc(MAX_L3_SZ);  
  static l3_t l3t;
  int ret;
 

  // Get the trigger data...
  StTriggerData *trgd = getStTriggerData(rdr);
  if(!trgd) return;

  int te = trgd->zdcPmtTDC(east,1);
  int tw = trgd->zdcPmtTDC(west,1);

  double zdcTimeDiff = tw-te;
  double zdcVertex = (zdcTimeDiff / 2.0) * .02*30;

  int bbctd = trgd->bbcTimeDifference()-4096;
  double bbcVertex = (bbctd / 2.0) * .02 * 30;
  
  // Track the tpc events...
  ret = evtTracker->trackEvent(rdr, (char *)rdr, l3p, MAX_L3_SZ);
  if(ret < 0) return;
  ret = evtTracker->copyl3_t(l3t, l3p);
  if(ret < 0) return;

  // Fill Histos for this event...
  if(rdr->trgcmd == 4) {
    contents.h88_l3_tracks->Fill(l3t.tracks_num);
    
    if((l3t.xVertex != 0) &&
       (l3t.yVertex != 0) &&
       (l3t.zVertex != 0)) {
      
      contents.h89_l3_Xvertex->Fill(l3t.xVertex);
      contents.h90_l3_Yvertex->Fill(l3t.yVertex);
      contents.h91_l3_Zvertex->Fill(l3t.zVertex);
      
      contents.h100_l3_zdc_zvertex->Fill(l3t.zVertex, zdcVertex);
      contents.h230_l3_bbc_zvertex->Fill(l3t.zVertex, bbcVertex);
      contents.h101_l3_x_y->Fill(l3t.xVertex, l3t.yVertex);
      contents.h112_l3_vertex_zdctimediff->Fill(l3t.zVertex, zdcTimeDiff);

      for(u_int i = 0; i < l3t.tracks_num ; i++) {
	contents.h62_l3_pt->Fill(l3t.track[i].pt);
	contents.h63_l3_phi0->Fill(l3t.track[i].phi0);
	contents.h64_l3_psi->Fill(l3t.track[i].psi);
	contents.h65_l3_trk_vertex->Fill(l3t.track[i].z0);
      }
    }
  }
  
  if(trgd) delete trgd;
}

void l3Builder::stoprun(daqReader *rdr) {
}

void l3Builder::main(int argc, char *argv[])
{
  l3Builder me;
  
  me.Main(argc, argv);
}

