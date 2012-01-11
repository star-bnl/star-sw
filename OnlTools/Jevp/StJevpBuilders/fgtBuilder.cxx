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

#include <stdio.h>
#include <stdlib.h>
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
  

fgtBuilder::fgtBuilder(JevpServer *parent):JevpPlotSet(parent),evtCt(0) {
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
  // Add root histograms to Plots
  for(int i=0;i<maxC*maxA;i++)
    {
      meanVals[i]=0;
      aVals[i]=0;
      rmsVals[i]=0;
      numVals[i]=0;
      isChannelBad[i]=false;
  }

  //////////////////////////////////add bad channels here///////////////////////
  ///////////////////isChannelBad[numAssembly*maxC+channel]=true;






  ////////////////////////////////////
  np = sizeof(contents) / sizeof(TH2 *);
  hNp=sizeof(hContents)/sizeof(TH1 *);
  char buffer[50];
  for(int gid=0;gid<np;gid++)
    {
      sprintf(buffer,"FGTassembly%d",gid);
      contents.array[gid]=new TH2F(buffer,Gid2Label[gid].c_str(),maxC,0,maxC,100,0,4000);
    }

  hContents.h1=new TH1F("MeanPeds","Mean Pedestal Values",500,minPedVal,maxPedVal);
  hContents.h2=new TH1F("MeanStdDev","Mean StdDev",500,0,maxRMSVal);

  
  //  JevpPlot *plots[np+hNp];
  plots=new JevpPlot*[np+hNp];
  for(int i=0;i<np;i++)
    {
      contents.array[i]->SetOption("colz");
      plots[i] = new JevpPlot(contents.array[Indx2Gid[i]]);
      //    plots[i] = new JevpPlot(contents.array[i]);
    }
  plots[np]=new JevpPlot(hContents.h1);
  plots[np+1]=new JevpPlot(hContents.h2);

  //  cout <<"adding plots... " <<endl;  
  // Add Plots to plot set...
  for(int i=0;i<np+hNp;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }

  //  cout <<" done " <<endl;
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
  char buffer[200];
  //  contents.h2_tmp->Fill(tRnd.Rndm(0));
  if(!(evtCt %1000))
    LOG(DBG, "Looking at evt %d",evtCt);
  daq_dta *dd=rdr->det("fgt")->get("adc");
  while(dd && dd->iterate()) {
    fgt_adc_t *f = (fgt_adc_t *) dd->Void ;
    for(u_int i=0;i<dd->ncontent;i++)
      {
	//	if(evtCt <1000)
	//	  printf("FGT ADC: RDO %d, ARM %d, APV %d: %d values\n",dd->rdo,dd->sec,dd->pad,dd->ncontent) ;
	//	dd->rdo;
	//the arm
	//	dd->sec;// two arms per disc
	//	int disc=dd->rdo*3+dd->sec/2;
	
	//see ben's spreadsheet, first rdo has 10, second 9 assemblies attached
	int gid=(dd->rdo-1)*10+dd->sec*2;
	if(dd->pad>10)
	  gid+=1;
	if(gid>18)
	  cout <<"gid: " << gid <<" to high "<<endl;
	int quad=(dd->rdo-1)*12+dd->sec*2;
	if(dd->pad>10)
	  quad+=1;
	int channel;
	if(gid<20)
	  {
	    channel=(dd->pad%12)*128+f[i].ch;
	    if(isChannelBad[gid*maxC+channel])
	      continue;
	    //apvs go 0-9 then 12-...
	    contents.array[gid]->Fill(channel,f[i].adc);
	    rmsVals[gid*maxC+channel]+=(meanVals[gid*maxC+channel]-f[i].adc)*(meanVals[gid*maxC+channel]-f[i].adc);
	    aVals[gid*maxC+channel]+=f[i].adc;
	    numVals[gid*maxC+channel]++;
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
    int tm = time(NULL);
    if((tm > t_10min + 600) || (!(evtCt%5000))) {
      t_10min = tm;
      hContents.h1->Reset();
      hContents.h2->Reset();
      int numBad=0;
      for(int i=0;i<maxC*maxA;i++)
	{
	  bool isBad=false;
	  if(aVals[i]>0 && numVals[i] > 0)
	    {
	      hContents.h1->Fill(aVals[i]/numVals[i]);
	      meanVals[i]=aVals[i]/numVals[i];
	      if(meanVals[i]<250 || meanVals[i]>maxPedVal)
		isBad=true;
	    }
	  if(numVals[i]>1)
	    {
	      //cout <<"numVals: " << numVals[i] <<" rms val: " << rmsVals[i] << " filling with : " << sqrt(rmsVals[i]/(numVals[i]-1)) <<endl;
	      double rms=sqrt(rmsVals[i]/(numVals[i]-1));
	      hContents.h2->Fill(rms);
	      if(rms<5 || maxRMSVal)
		isBad=true;
	    }
	  aVals[i]=0;
	  numVals[i]=0;
	  rmsVals[i]=0;
	  if(isBad)
	    numBad++;
	}
      sprintf(buffer,"You seem to have %d bad channels that are not masked", numBad);
      plots[np]->setRefComment(buffer);
    }

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


//const string fgtBuilder::Gid2Label[19]={"1AB","1BC","1CD","1DA","2AB","2BC","2DA","3AB","3BC","3DA","4AB","4BC","4DA","5AB","5BC","5DA","6AB","6BC","6DA"};
const string fgtBuilder::Gid2Label[19]={"1DA","1AB","2DA","2AB","3AB","3BC","4BC","5DA","6DA","6AB","1BC","1CD","2BC","3DA","4DA","4AB","5AB","5BC","6BC"};
const int fgtBuilder::Indx2Gid[19]={1,10,11,0,3,12,2,4,5,13,15,6,14,16,17,7,9,18,8};
const int fgtBuilder::maxA=19;
const int fgtBuilder::maxC=1400;
const int fgtBuilder::maxPedVal=1500;
const int fgtBuilder::maxRMSVal=100;
const int fgtBuilder::minPedVal=100;
const int fgtBuilder::minRMSVal=0;
