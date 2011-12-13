#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include <DAQ_READER/daq_dta.h>
#include "DAQ_READER/daq_det.h"
#include <DAQ_FGT/daq_fgt.h>

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
  

fgtBuilder::fgtBuilder():evtCt(0) {
  plotsetname = (char *)"fgt";
  // start with histograms undefined...
  memset(&contents, 0, sizeof(contents));
}

fgtBuilder::~fgtBuilder() {

  // Delete any existing histograms...

    int n = sizeof(contents) / sizeof(TH2 *);
  cout <<" deleting " <<n<<" histos"  << endl;
  for(int i=0;i<n;i++) {
        if(contents.array[i]) delete contents.array[i];
  }
  cout <<"done.." <<endl;
}

void fgtBuilder::initialize(int argc, char *argv[]) {
  
  // Initialization of histograms.
  //could run a loop...
  contents.q1=new TH2F("assembly1","Disc 1, Assembly 1",3000,0,3000,100,0,5000);
  contents.q2=new TH2F("assembly2","Disc 1, Assembly 2",3000,0,3000,100,0,5000);
  contents.q3=new TH2F("assembly3","Disc 1, Assembly 3",3000,0,3000,100,0,5000);
  contents.q4=new TH2F("assembly4","Disc 1, Assembly 4",3000,0,3000,100,0,5000);
  contents.q5=new TH2F("assembly5","Disc 2, Assembly 1",3000,0,3000,100,0,5000);
  contents.q6=new TH2F("assembly6","Disc 2, Assembly 2",3000,0,3000,100,0,5000);
  contents.q7=new TH2F("assembly7","Disc 2, Assembly 3",3000,0,3000,100,0,5000);
  contents.q8=new TH2F("assembly8","Disc 2, Assembly 4",3000,0,3000,100,0,5000);
  contents.q9=new TH2F("assembly9","Disc 3, Assembly 1",3000,0,3000,100,0,5000);
  contents.q10=new TH2F("assembly10","Disc 3, Assembly 2",3000,0,3000,100,0,5000);
  contents.q11=new TH2F("assembly11","Disc 3, Assembly 3",3000,0,3000,100,0,5000);
  contents.q12=new TH2F("assembly12","Disc 3, Assembly 4",3000,0,3000,100,0,5000);
  contents.q13=new TH2F("assembly13","Disc 4, Assembly 1",3000,0,3000,100,0,5000);
  contents.q14=new TH2F("assembly14","Disc 4, Assembly 2",3000,0,3000,100,0,5000);
  contents.q15=new TH2F("assembly15","Disc 4, Assembly 3",3000,0,3000,100,0,5000);
  contents.q16=new TH2F("assembly16","Disc 4, Assembly 4",3000,0,3000,100,0,5000);
  contents.q17=new TH2F("assembly17","Disc 5, Assembly 1",3000,0,3000,100,0,5000);
  contents.q18=new TH2F("assembly18","Disc 5, Assembly 2",3000,0,3000,100,0,5000);
  contents.q19=new TH2F("assembly19","Disc 5, Assembly 3",3000,0,3000,100,0,5000);
  contents.q20=new TH2F("assembly20","Disc 5, Assembly 4",3000,0,3000,100,0,5000);
  contents.q21=new TH2F("assembly21","Disc 6, Assembly 1",3000,0,3000,100,0,5000);
  contents.q22=new TH2F("assembly22","Disc 6, Assembly 2",3000,0,3000,100,0,5000);
  contents.q23=new TH2F("assembly23","Disc 6, Assembly 3",3000,0,3000,100,0,5000);
  contents.q24=new TH2F("assembly24","Disc 6, Assembly 4",3000,0,3000,100,0,5000);
  ////

  // Add root histograms to Plots
    int np = sizeof(contents) / sizeof(TH2 *);
  JevpPlot *plots[np];
  cout <<" we have " << np <<endl;
  
  for(int i=0;i<np;i++)
  {
    contents.array[i]->SetOption("colz");
    plots[i] = new JevpPlot(contents.array[i]);
  }
  
  // Add Plots to plot set...
  for(int i=0;i<np;i++) {
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
  //  contents.h2_tmp->Fill(tRnd.Rndm(0));
  if(!(evtCt %1000))
    LOG(DBG, "Looking at evt %d",evtCt);
  daq_dta *dd=rdr->det("fgt")->get("adc");
  while(dd && dd->iterate()) {
    fgt_adc_t *f = (fgt_adc_t *) dd->Void ;
    for(u_int i=0;i<dd->ncontent;i++)
      {

	//		  printf("FGT ADC: RDO %d, ARM %d, APV %d: %d values\n",dd->rdo,dd->sec,dd->pad,dd->ncontent) ;
	//	dd->rdo;
	//the arm
	//	dd->sec;// two arms per disc
	//	int disc=dd->rdo*3+dd->sec/2;
	int quad=(dd->rdo-1)*12+dd->sec*2;
	if(dd->pad>10)
	  quad+=1;
	int channel;
	if(quad<24)
	  {
	    //	    channel=dd->pad*128+f[i].ch;
	    //	    if(channel>1280)
	    //	      channel-=(2*128);
	    channel=(dd->pad%12)+f[i].ch;
	    contents.array[quad]->Fill(channel,f[i].adc);
	  }
	else
	  cout <<"quad too large: " << quad <<endl;
	//	cout <<" filling with : " << dd->pad*128+f[i].ch <<" and " << f[i].adc <<endl;
	/*if(dd->pad <= APVEnd)
	  {
	  value=f[i].adc-pedestals[dd->pad-APVStart][f[i].ch];
	  vals[dd->pad-APVStart][f[i].ch]+=value;
	  //timbin
	  f[i].tb;
	  }*/
	//quad should be 0-23
      }
  }
  evtCt++;


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
  cout <<"stopping run " <<endl;  
}

void fgtBuilder::main(int argc, char *argv[])
{
  fgtBuilder me;
  cout <<"starting main" << endl;
  me.Main(argc, argv);
  cout <<"ending main" << endl;
}

