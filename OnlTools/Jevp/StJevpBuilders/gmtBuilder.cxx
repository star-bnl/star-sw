
///////////////////////////////////////////////////
//// Sabita Das: Run13 GMT Online QA       ////////
//// using Jeff's FrameWork (January 2013) ////////
///////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>

#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include <DAQ_READER/daq_dta.h>
#include "DAQ_READER/daq_det.h"
#include <DAQ_FGT/daq_fgt.h>
#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2F.h>
#include <TFile.h>
#include <TPaveStats.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "gmtBuilder.h"
#include <RTS/include/rtsLog.h>

// This is the one PlotSet that is guarenteed to always exist
// It's main purpose is to provide the run information 
// To the server...


ClassImp(gmtBuilder);


gmtBuilder::gmtBuilder(JevpServer *parent):JevpBuilder(parent),evtCount(0) {
 plotsetname = (char *)"gmt";

  // start with histograms undefined...
  memset(&hPedContents, 0, sizeof(hPedContents));
  memset(&hSignalContents,0,sizeof(hSignalContents));
  memset(&hTimebinContents,0,sizeof(hTimebinContents));
  memset(&hSumContents,0,sizeof(hSumContents));
  memset(&hSigtbContents,0,sizeof(hSigtbContents));
}

gmtBuilder::~gmtBuilder() {

  int nped = sizeof(hPedContents) / sizeof(TH2 *);
  int nsig = sizeof(hSignalContents) / sizeof(TH1 *);
  int ntb = sizeof(hTimebinContents) / sizeof(TH1 *);
  int nsum = sizeof(hSumContents) / sizeof(TH1 *);
  int nsigtb = sizeof(hSigtbContents) / sizeof(TH2 *);

  for(int i=0;i<nped;i++) {
    if(hPedContents.hPedArray[i]) delete hPedContents.hPedArray[i];
  }
  
  for(int i=0;i<nsig;i++) {
    if(hSignalContents.hSignalArray[i]) delete hSignalContents.hSignalArray[i];
  }
  
  for(int i=0;i<ntb;i++) {
    if(hTimebinContents.hTimebinArray[i]) delete hTimebinContents.hTimebinArray[i];
  }
  
  for(int i=0;i<nsum;i++) {
    if(hSumContents.hSumArray[i]) delete hSumContents.hSumArray[i];
  }
  
  for(int i=0;i<nsigtb;i++) {
    if(hSigtbContents.hSigtb[i]) delete hSigtbContents.hSigtb[i];
  }
}

void gmtBuilder::initialize(int argc, char *argv[]) {
  
  // Initialization of histograms.
  //could run a loop...
  // Add root histograms to Plots
  //  cout <<"init " << endl;
  ////////////////////////////////////
  nped = sizeof(hPedContents) / sizeof(TH2 *);
  //printf("total no of ped histograms %d \n", nped);
  
  char tmp[256];
  char tmp1[256];
  for(int i=0;i<nped;i++)
    {
      int iarm;
      if(i<=7)
	iarm = 0;
      else
	iarm = 1;
      
      sprintf(tmp,"pedestalsAPV%d",i);  //0-7 ARM =0 and 8-15 ARM =1
      sprintf(tmp1,"pulse height for all channels in ARM:%d, APV%d",iarm,i);
      hPedContents.hPedArray[i]=new TH2F(tmp,tmp1,128,-0.5,127.5,512,-0.5,4095.5);
      hPedContents.hPedArray[i]->GetXaxis()->SetTitle("X = Channel+timebin/numTimebins");
      hPedContents.hPedArray[i]->GetYaxis()->SetTitle("ADC value");
      hPedContents.hPedArray[i]->GetXaxis()->SetNdivisions(8,false);
      hPedContents.hPedArray[i]->SetStats(false);
      hPedContents.hPedArray[i]->GetYaxis()->SetTitleOffset(1.3);

    }
  
  nsig = sizeof(hSignalContents) / sizeof(TH1 *);
  //printf("total no of histograms for Signals in each Chamber: %d \n", nsig);
  
  char sg[256];
  char sg1[256];
  for(int i=0;i<nsig;i++)
    {
      
      sprintf(sg,"Signals in Chamber%d",i);
      sprintf(sg1,"ADC signals in Chamber%d",i);
      hSignalContents.hSignalArray[i]=new TH1D(sg,sg1,4000,0,40000);
      hSignalContents.hSignalArray[i]->GetXaxis()->SetTitle("");
      hSignalContents.hSignalArray[i]->GetYaxis()->SetTitle("");
      hSignalContents.hSignalArray[i]->SetStats(false);
      hSignalContents.hSignalArray[i]->GetYaxis()->SetRangeUser(0,40);
      hSignalContents.hSignalArray[i]->GetYaxis()->SetTitleOffset(1.2);
      hSignalContents.hSignalArray[i]->SetLineColor(4);
      hSignalContents.hSignalArray[i]->SetLineWidth(4);
      
    }
  
  ntb = sizeof(hTimebinContents) / sizeof(TH1 *);
  //printf("total no of histograms for Timebins in each Chamber: %d \n", ntb);

  char TBbuffer[256];
  char TBbuffer1[256];
  for(int i=0;i<ntb;i++)
    {
            
      sprintf(TBbuffer,"Timebins in Chamber%d",i);
      sprintf(TBbuffer1,"Timebin Charge Distributions for Chamber%d",i);
      hTimebinContents.hTimebinArray[i]=new TH1D(TBbuffer,TBbuffer1,15,0,15);
      hTimebinContents.hTimebinArray[i]->GetXaxis()->SetTitle("");
      hTimebinContents.hTimebinArray[i]->GetYaxis()->SetTitle("");
      hTimebinContents.hTimebinArray[i]->GetXaxis()->SetNdivisions(15,false);
      hTimebinContents.hTimebinArray[i]->SetStats(false);
      hTimebinContents.hTimebinArray[i]->GetYaxis()->SetTitleOffset(1.2);
      hTimebinContents.hTimebinArray[i]->SetBarWidth(1);
      hTimebinContents.hTimebinArray[i]->SetBarOffset(0.005);
      hTimebinContents.hTimebinArray[i]->SetFillColor(50);
      
    }
  
  hSumContents.h1SumAllsignals = new TH1D("SumAllsignals","Sum of signals from all chambers per time bin",15,0,15);
  hSumContents.h1SumAllsignals->GetXaxis()->SetNdivisions(15,false);
  hSumContents.h1SumAllsignals->SetBarWidth(1);
  hSumContents.h1SumAllsignals->SetBarOffset(0.005);
  hSumContents.h1SumAllsignals->SetFillColor(50);
  
  hSigtbContents.h2SignalTimebins = new TH2D("SignalTimebins","Signal vs. Timebin",15,0,15,4096,0,4096);
  
  // Add root histograms to Plots
  JevpPlot *plots[100];
  
  int n=0;
  plots[n] = new JevpPlot( hSumContents.h1SumAllsignals);
  plots[++n] = new JevpPlot(hSigtbContents.h2SignalTimebins);
  for(int i=0;i<numAPVs;i++)plots[++n] = new JevpPlot(hPedContents.hPedArray[i]);
  for(int i=0;i<numLayers;i++)plots[++n]=new JevpPlot(hSignalContents.hSignalArray[i]);
  for(int i=0;i<numLayers;i++)plots[++n]=new JevpPlot(hTimebinContents.hTimebinArray[i]);
  //plots[i]->setDrawOpts("colz");
  //plots[n]->setOptStat(11111);
  
  for(int i=0;i<=n;i++) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }
  
}


void gmtBuilder::startrun(daqReader *rdr) {
  LOG(NOTE, "gmtBuilder starting run #%d",rdr->run);
  resetAllPlots();
  
  
  
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

void gmtBuilder::event(daqReader *rdr)
{
  //Used for eventcut for test
  //if(evtCount >= 0){
  
  //=======initialization ======================
  /*
  for ( int i_arm = 0; i_arm < numARMs; i_arm++ ) {
    for ( int i_port = 0; i_port < numPORTs; i_port++ ) {
      for ( int i_apv = 0; i_apv < numAPVs; i_apv++ ) {
	for(int i_ch =0; i_ch<numChannels; i_ch++){
	  for(int i_tb=0;i_tb<numTimebins; i_tb++){
	    SignalPedCorrected[i_arm][i_port][i_apv][i_ch][i_tb] = 9999.00;
	  }
	}
      }
    }
  }
  */
  
  for( int ilayer = 0; ilayer<numLayers; ilayer++){
      	sumSignal_AllTimebins[ilayer] = 0.0;
	for (int tb = 0; tb<numTimebins; tb++){
	  SumSignalPedCorrected[tb][ilayer] = 0.0;
	  sumSignal_AllChambers[tb] = 0.0;
	}
  }
  
  daq_dta *dd=rdr->det("gmt")->get("adc");
  while(dd && dd->iterate()) {
    
    int rdo = dd->rdo;
    int arm = dd->sec;
    int port = -999;
    int apv = dd->pad;
    
    if(apv >=0 && apv<=3){
      port =0;
    }
    else if(apv >=12 && apv<= 15){
      port = 1;
      apv = apv -12;
    }
    //if(evtCount%1000)printf("GMT ADC: RDO %d, ARM %d, PORT %d, APV %d \n", rdo,arm,port, apv) ;
    
    if(usedAPV(rdo,arm,port,apv)){
      
      fgt_adc_t *f = (fgt_adc_t *) dd->Void ;
      
      for(u_int ii=0;ii<dd->ncontent;ii++)
	{
	  adc = f[ii].adc;
	  timebin = f[ii].tb;
	  channel = f[ii].ch;
	  ch_seq = channel+1.0*timebin/numTimebins;
	  //if(evtCount%1000)printf("GMT ADC: RDO %d, ARM %d, APV %d: %d values\n", rdo,arm,apv,dd->ncontent) ;
	  //cout << " "<< channel << "\t tb =  "<<timebin <<" \t nTB = "<<numTimebins<<"\t ch_seq = "<<ch_seq<<endl;
	  //============= Filling pedestals including hits   ========================
	  int apvs = getAPV(arm, port, apv);
	  if(apvs >=0 && apvs <numAPVs){                           
	    hPedContents.hPedArray[apvs]->Fill(ch_seq,adc); 
	  }
	  
	}  //dd-> cnotent
    }  //usedAPV  loop
  } //1st dd->iterate
  
  //======================  Zero suppressed data =====================================================
  daq_dta *dd1=rdr->det("gmt")->get("zs");
  while(dd1 && dd1->iterate()) {
    
    int rdo = dd1->rdo;
    int arm = dd1->sec;
    int port = -999;
    int apv = dd1->pad;
    
    if(apv >=0 && apv<=3){
      port =0;
    }
    else if(apv >=12 && apv<= 15){
      port = 1;
      apv = apv -12;
    }
    //printf("evntno %d,GMT ADC: RDO %d, ARM %d, PORT %d, APV %d \n", evtCount, rdo,arm,port,apv) ;
    int channel, ch_seq, timebin;
    double adc;
    
    if(usedAPV(rdo,arm,port,apv)){
      
      
      fgt_adc_t *f = (fgt_adc_t *) dd1->Void ;
      for(u_int ii=0; ii<dd1->ncontent;ii++)
	{
	  
	  timebin = f[ii].tb;
	  adc = f[ii].adc;
	  channel = f[ii].ch;
	  
	  SignalPedCorrected[arm][port][apv][channel][timebin] = adc ;
	  int layer=getLayer(arm,port,apv);
	  if(layer>=0 && layer<numLayers){
	    hSigtbContents.h2SignalTimebins->Fill(timebin,SignalPedCorrected[arm][port][apv][channel][timebin]);  //signal vs timebin
	    if(SignalPedCorrected[arm][port][apv][channel][timebin] > ADCcut ){
	      SumSignalPedCorrected[timebin][layer] += SignalPedCorrected[arm][port][apv][channel][timebin];
	    } //ADCcut loop
	  }	//layers loop
	} // dd->content loop
    }	// usedAPV loop
    
  }//dd1 iterate loop
  
  
  for(int ilayer=0; ilayer<numLayers; ilayer++){
    for (int tb =0; tb< numTimebins; tb++){
      sumSignal_AllTimebins[ilayer] = sumSignal_AllTimebins[ilayer] + SumSignalPedCorrected[tb][ilayer];
    }
    if(sumSignal_AllTimebins[ilayer]>0.){
      hSignalContents.hSignalArray[ilayer]->Fill(sumSignal_AllTimebins[ilayer]);  
      //cout<< "ilayer = "<<ilayer<<"\t SumSignal entries = "<<hSignalContents.hSignalArray[ilayer]->GetEntries()<<endl;
    }
  }
  
  for (int tb =0; tb<numTimebins; tb++){
    for(int ilayer=0; ilayer<numLayers; ilayer++){
      sumSignal_AllChambers[tb] = sumSignal_AllChambers[tb] + SumSignalPedCorrected[tb][ilayer];
      hTimebinContents.hTimebinArray[ilayer]->Fill(tb,SumSignalPedCorrected[tb][ilayer]);
    }
    hSumContents.h1SumAllsignals->Fill(tb,sumSignal_AllChambers[tb]);
  }
  evtCount++;
  //} //event cut lop

  //cout<< "Total no. of events processed= " <<evtCount<<endl;
}  //event loop


void gmtBuilder::stoprun(daqReader *rdr) {
  
}

void gmtBuilder::main(int argc, char *argv[])
{
  gmtBuilder me;
  //  cout <<"starting main" << endl;
  me.Main(argc, argv);
  //  cout <<"ending main" << endl;
}


