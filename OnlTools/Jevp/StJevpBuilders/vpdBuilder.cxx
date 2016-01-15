#include <stdio.h>
#include <stdlib.h>

#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "Jevp/StJevpPlot/RunStatus.h"

#include <TH1I.h>
#include <TH2F.h>

#include <math.h>
#include "vpdBuilder.h"
#include <RTS/include/rtsLog.h>
#include <rtsSystems.h>
// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...
//
// It has no plots (currently)
//


ClassImp(vpdBuilder);
  
void vpdBuilder::initialize(int argc, char *argv[]) {

  char tmp[256];
  char tmp1[256];
  

  // VPD lo

  sprintf(tmp,"vpd_east_ADClo");
  sprintf(tmp1,"VPD-vtx east ADC");
  contents.cdb[0] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
  contents.cdb[0]->SetXTitle("Channel # (east)");
  contents.cdb[0]->SetYTitle("Low-Th ADC (east)");

  sprintf(tmp,"vpd_east_TAClo");
  sprintf(tmp1,"VPD-vtx east TAC");
  contents.cdb[1] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
  contents.cdb[1]->SetXTitle("Channel # (east)");
  contents.cdb[1]->SetYTitle("Low-Th TAC (east)");

  sprintf(tmp,"vpd_west_ADClo");
  sprintf(tmp1,"VPD-vtx west ADC");
  contents.cdb[2] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
  contents.cdb[2]->SetXTitle("Channel # (west)");
  contents.cdb[2]->SetYTitle("Low-Th ADC (west)");

  sprintf(tmp,"vpd_west_TAClo");
  sprintf(tmp1,"VPD-vtx west TAC");
  contents.cdb[3] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
  contents.cdb[3]->SetXTitle("Channel # (west)");
  contents.cdb[3]->SetYTitle("Low-Th TAC (west)");
  
  sprintf(tmp,"vpd_tac_east_vs_tac_west");
  contents.tac_east_vs_tac_west = new TH2D(tmp,"VPD-vtx TAC East vs. TAC West", 256, -1.5, 4094.5, 256, -1.5, 4094.5);
  contents.tac_east_vs_tac_west->SetXTitle("TAC West");
  contents.tac_east_vs_tac_west->SetYTitle("TAC East");
  

  //sprintf(tmp,"vpd_vertex_vs_l3_vertex");
  //contents.vertex_vs_l3_vertex = new TH2D(tmp,"VPDlo TAC Difference vs. L3 Vertex z-Position", 200, -200, 200, 200, 3600,4600);
  //contents.vertex_vs_l3_vertex->SetXTitle("L3 Vertex z-Position [cm]");
  //contents.vertex_vs_l3_vertex->SetYTitle("VPD TAC Difference");

  sprintf(tmp,"vpd_earliestTAC_vs_chan_east");
  contents.earliestTAC_vs_eastchan = new TH2D(tmp,"VPD-vtx EarliestTAC vs chan east", 16, -0.5, 15.5, 256, -1.5, 4094.5);
  contents.earliestTAC_vs_eastchan->SetXTitle("Chan#(east)");
  contents.earliestTAC_vs_eastchan->SetYTitle("Earliest TAC");


  sprintf(tmp,"vpd_earliestTAC_vs_chan_west");
  contents.earliestTAC_vs_westchan = new TH2D(tmp,"VPD-vtx EarliestTAC vs chan west", 16, -0.5, 15.5, 256, -1.5, 4094.5);
  contents.earliestTAC_vs_westchan->SetXTitle("Chan#(west)");
  contents.earliestTAC_vs_westchan->SetYTitle("Earliest TAC");
  

  // VPD Hi


  sprintf(tmp,"vpd_east_ADChi");
  sprintf(tmp1,"VPD-mtd east ADC");
  contents.hi_cdb[0] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
  contents.hi_cdb[0]->SetXTitle("Channel # (east)");
  contents.hi_cdb[0]->SetYTitle("High-Th ADC (east)");

  sprintf(tmp,"vpd_east_TAChi");
  sprintf(tmp1,"VPD-mtd east TAC");
  contents.hi_cdb[1] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
  contents.hi_cdb[1]->SetXTitle("Channel # (east)");
  contents.hi_cdb[1]->SetYTitle("High-Th TAC (east)");

  sprintf(tmp,"vpd_west_ADChi");
  sprintf(tmp1,"VPD-mtd west ADC");
  contents.hi_cdb[2] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
  contents.hi_cdb[2]->SetXTitle("Channel # (west)");
  contents.hi_cdb[2]->SetYTitle("High-Th ADC (west)");

  sprintf(tmp,"vpd_west_TAChi");
  sprintf(tmp1,"VPD-mtd west TAC");
  contents.hi_cdb[3] = new TH2D(tmp,tmp1,16,-0.5,15.5,400,-0.5,4095.5);
  contents.hi_cdb[3]->SetXTitle("Channel # (west)");
  contents.hi_cdb[3]->SetYTitle("High-Th TAC (west)");
  
  sprintf(tmp,"vpd_hi_tac_east_vs_tac_west");
  contents.hi_tac_east_vs_tac_west = new TH2D(tmp,"VPD-mtd TAC East vs. TAC West", 256, -1.5, 4094.5, 256, -1.5, 4094.5);
  contents.hi_tac_east_vs_tac_west->SetXTitle("TAC West");
  contents.hi_tac_east_vs_tac_west->SetYTitle("TAC East");
  

  //sprintf(tmp,"vpd_hi_vertex_vs_l3_vertex");
  //contents.hi_vertex_vs_l3_vertex = new TH2D(tmp,"VPDhi TAC Difference vs. L3 Vertex z-Position", 200, -200, 200, 200, 3600,4600);
  //contents.hi_vertex_vs_l3_vertex->SetXTitle("L3 Vertex z-Position [cm]");
  //contents.hi_vertex_vs_l3_vertex->SetYTitle("VPD TAC Difference");

  sprintf(tmp,"vpd_hi_earliestTAC_vs_chan_east");
  contents.hi_earliestTAC_vs_eastchan = new TH2D(tmp,"VPD-mtd EarliestTAC vs chan east", 16, -0.5, 15.5, 256, -1.5, 4094.5);
  contents.hi_earliestTAC_vs_eastchan->SetXTitle("Chan#(east)");
  contents.hi_earliestTAC_vs_eastchan->SetYTitle("Earliest TAC");


  sprintf(tmp,"vpd_hi_earliestTAC_vs_chan_west");
  contents.hi_earliestTAC_vs_westchan = new TH2D(tmp,"VPD-mtd EarliestTAC vs chan west", 16, -0.5, 15.5, 256, -1.5, 4094.5);
  contents.hi_earliestTAC_vs_westchan->SetXTitle("Chan#(west)");
  contents.hi_earliestTAC_vs_westchan->SetYTitle("Earliest TAC");
  
  // Add root histograms to Plots
  int np = sizeof(contents) / sizeof(TH1 *);
  JevpPlot *plots[np];

  int n=0;
  plots[n] = new JevpPlot(contents.cdb[0]);
  plots[++n] = new JevpPlot(contents.cdb[1]);
  plots[++n] = new JevpPlot(contents.cdb[2]);
  plots[++n] = new JevpPlot(contents.cdb[3]);
  plots[++n] = new JevpPlot(contents.tac_east_vs_tac_west);
  //plots[++n] = new JevpPlot(contents.vertex_vs_l3_vertex);
  plots[++n] = new JevpPlot(contents.earliestTAC_vs_eastchan);
  plots[++n] = new JevpPlot(contents.earliestTAC_vs_westchan);  
  plots[++n] = new JevpPlot(contents.hi_cdb[0]);
  plots[++n] = new JevpPlot(contents.hi_cdb[1]);
  plots[++n] = new JevpPlot(contents.hi_cdb[2]);
  plots[++n] = new JevpPlot(contents.hi_cdb[3]);
  plots[++n] = new JevpPlot(contents.hi_tac_east_vs_tac_west);
  //plots[++n] = new JevpPlot(contents.hi_vertex_vs_l3_vertex);
  plots[++n] = new JevpPlot(contents.hi_earliestTAC_vs_eastchan);
  plots[++n] = new JevpPlot(contents.hi_earliestTAC_vs_westchan);  


  // Add Plots to plot set...
  for(int i=0;i<=n;i++) {
    LOG(DBG, "Adding plot %d",i);
    
    contents.array[i]->GetXaxis()->SetLabelSize(0.055);
    contents.array[i]->GetYaxis()->SetLabelSize(0.045);
    
    addPlot(plots[i]);
  }
}
  
void vpdBuilder::startrun(daqReader *rdr) {
  resetAllPlots();
}

void vpdBuilder::stoprun(daqReader *rdr) {
}

void vpdBuilder::event(daqReader *rdr) {
  LOG(DBG, "event #%d",rdr->seq);

  StTriggerData *trgd = getStTriggerData(rdr);
  if(!trgd) {
    LOG(DBG, "No trigger data");
    return;
  }

//  int maxTacEast = trgd->vpdEarliestTDC((StBeamDirection)0);
//  int maxTacWest = trgd->vpdEarliestTDC((StBeamDirection)1);
  int maxTacEast = -1;
  int maxTacWest = -1;
  int earliestchan_east=-1;
  int earliestchan_west=-1; 
  int nlitlo[2]={0};
  for(int i=0;i<2;i++) {   //
    for(int ich=0;ich<16;ich++){
      int adc_lo = trgd->vpdADC((StBeamDirection)i,ich+1);
      int tdc_lo = trgd->vpdTDC((StBeamDirection)i,ich+1);
      if(tdc_lo>200 && i==0 && maxTacEast<tdc_lo){ earliestchan_east=ich; maxTacEast=tdc_lo; }
      if(tdc_lo>200 && i==1 && maxTacWest<tdc_lo){ earliestchan_west=ich; maxTacWest=tdc_lo; }
      if (tdc_lo>200){
          nlitlo[i]++;
	      contents.cdb[2*i+0]->Fill(ich, adc_lo);
	      contents.cdb[2*i+1]->Fill(ich, tdc_lo);
	  }
    }
  }
  contents.tac_east_vs_tac_west->Fill(maxTacWest, maxTacEast);
  if (maxTacEast>200){
      contents.earliestTAC_vs_eastchan->Fill(earliestchan_east,maxTacEast);
  }
  if (maxTacWest>200){
      contents.earliestTAC_vs_westchan->Fill(earliestchan_west,maxTacWest);
  }

	//---- debug.....
//  	if(maxTacEast>200||maxTacWest>200){
//  		for(int i=0;i<2;i++) {   //
//  			cout<<nlitlo[i]<<" .... ";
//  			for(int ich=0;ich<16;ich++){
//  				int adc_lo = trgd->vpdADC((StBeamDirection)i,ich+1);
//  				int tdc_lo = trgd->vpdTDC((StBeamDirection)i,ich+1);
//  				if (tdc_lo>200){
//  					cout<<"["<<ich<<","<<adc_lo<<","<<tdc_lo<<"]";
//  				}
//  			} cout<<endl;
//  		}
//  		cout<<"max E,W = "<<earliestchan_east<<" "<<maxTacEast<<" ... "<<earliestchan_west<<" "<<maxTacWest<<endl;
//  		cout<<"--------------"<<endl;
//  	}
	//---- debug.....

	
//   int ret = evtTracker->trackEvent(evp, datap, l3p, sizL3_max);
//   if (!(ret<0)) ret = evtTracker->copyl3_t(l3,l3p);
//   if(ret < 0){
//       fprintf(stderr,"L3: problems in data (%d) - continuing...",ret) ;
//       cout<<"Error tracking event: "<<evp->seq<<endl;
//       return false;
//   }
//   unsigned int tacDiff=trgd->vpdTimeDifference();
//   contents.vertex_vs_l3_vertex->Fill(l3.zVertex, tacDiff);

//  int maxTacEastHigh = trgd->vpdEarliestTDCHighThr((StBeamDirection)0);
//  int maxTacWestHigh = trgd->vpdEarliestTDCHighThr((StBeamDirection)1);
  int maxTacEastHigh = -1;
  int maxTacWestHigh = -1;
  int earliestchan_east_hi=-1;
  int earliestchan_west_hi=-1; 
  int nlithi[2]={0};
  for(int i=0;i<2;i++) {   //
    for(int ich=0;ich<16;ich++){
      int adc_hi = trgd->vpdADCHighThr((StBeamDirection)i,ich+1);
      int tdc_hi = trgd->vpdTDCHighThr((StBeamDirection)i,ich+1);
      if(tdc_hi>200 && i==0 && maxTacEastHigh<tdc_hi){ earliestchan_east_hi=ich; maxTacEastHigh=tdc_hi; }
      if(tdc_hi>200 && i==1 && maxTacWestHigh<tdc_hi){ earliestchan_west_hi=ich; maxTacWestHigh=tdc_hi; }
      if (tdc_hi>200){
          nlithi[i]++;
	      contents.hi_cdb[2*i+0]->Fill(ich, adc_hi);
	      contents.hi_cdb[2*i+1]->Fill(ich, tdc_hi);
	  }
    }
  }
  contents.hi_tac_east_vs_tac_west->Fill(maxTacWestHigh, maxTacEastHigh);
  if (maxTacEastHigh>200){
      contents.hi_earliestTAC_vs_eastchan->Fill(earliestchan_east_hi,maxTacEastHigh);
  }
  if (maxTacWestHigh>200){
      contents.hi_earliestTAC_vs_westchan->Fill(earliestchan_west_hi,maxTacWestHigh);
  }

  if(trgd) delete trgd;
}

void vpdBuilder::main(int argc, char *argv[])
{
  vpdBuilder me;  
  me.Main(argc, argv);
}

