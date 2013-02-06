
///////////////////////////////////////////////////
//// Sabita Das: Run13 GMT Online QA       ////////
//// using Jeff's FrameWork (January 2013) ////////
///////////////////////////////////////////////////


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


gmtBuilder::gmtBuilder(JevpServer *parent):JevpPlotSet(parent),evtCount(0) {
 plotsetname = (char *)"gmt";

  // start with histograms undefined...
  memset(&hPedContents, 0, sizeof(hPedContents));
  memset(&hSignalContents,0,sizeof(hSignalContents));
  memset(&hTimebinContents,0,sizeof(hTimebinContents));
  memset(&hSumContents,0,sizeof(hSumContents));
}

gmtBuilder::~gmtBuilder() {

  int nped = sizeof(hPedContents) / sizeof(TH2 *);
  int nsig = sizeof(hSignalContents) / sizeof(TH1 *);
  int ntb = sizeof(hTimebinContents) / sizeof(TH1 *);
  int nsum = sizeof(hSumContents) / sizeof(TH1 *);

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
}

void gmtBuilder::initialize(int argc, char *argv[]) {
  
  // Initialization of histograms.
  //could run a loop...
  // Add root histograms to Plots
  //  cout <<"init " << endl;
  ////////////////////////////////////
  nped = sizeof(hPedContents) / sizeof(TH2 *);
  printf("total no of ped histograms %d \n", nped);
  
  char tmp[256];
  char tmp1[256];
  for(int i=0;i<nped;i++)
    {
      int iarm;
      if(i<=7)
	iarm = 0;
      else
	iarm = 1;
      
      sprintf(tmp,"pedestalsAPV%d",i);  //0-7 ARM =0 and 8-15 ARM =2
      sprintf(tmp1,"pulse height for all channels in ARM:%d, APV%d",iarm,i);
      hPedContents.hPedArray[i]=new TH2F(tmp,tmp1,128*numTimebins,0,128,4096,0,4096);
      hPedContents.hPedArray[i]->GetXaxis()->SetTitle("X = Channel+timebin/numTimebins");
      hPedContents.hPedArray[i]->GetYaxis()->SetTitle("ADC value");
      hPedContents.hPedArray[i]->GetXaxis()->SetNdivisions(8,false);
      hPedContents.hPedArray[i]->SetStats(false);
      //hPedContents.hPedArray[i]->GetXaxis()->SetTitleOffset(0.9);
      //hPedContents.hPedArray[i]->GetXaxis()->SetTitleSize(0.06);
      //hPedContents.hPedArray[i]->GetXaxis()->SetLabelSize(0.06);
      hPedContents.hPedArray[i]->GetYaxis()->SetTitleOffset(1.3);
      //hPedContents.hPedArray[i]->GetYaxis()->SetTitleSize(0.06);
      //hPedContents.hPedArray[i]->GetYaxis()->SetLabelSize(0.06);
    }
  
  nsig = sizeof(hSignalContents) / sizeof(TH1 *);
  printf("total no of histograms for Signals in each Chamber: %d \n", nsig);

  char sg[256];
  char sg1[256];
  for(int i=0;i<nsig;i++)
    {
      sprintf(sg,"Signals in Chamber%d",i);
      sprintf(sg1,"ADC signals in Chamber%d",i);
      hSignalContents.hSignalArray[i]=new TH1D(sg,sg1,4000,0,40000);
      hSignalContents.hSignalArray[i]->GetXaxis()->SetTitle("");
      hSignalContents.hSignalArray[i]->GetYaxis()->SetTitle("");
      //hSignalContents.hSignalArray[i]->GetXaxis()->SetNdivisions(10,false);
      hSignalContents.hSignalArray[i]->SetStats(false);
      hSignalContents.hSignalArray[i]->GetYaxis()->SetRangeUser(0,40);
      //hSignalContents.hSignalArray[i]->GetXaxis()->SetTitleOffset(0.9);
      //hSignalContents.hSignalArray[i]->GetXaxis()->SetTitleSize(0.06);
      //hSignalContents.hSignalArray[i]->GetXaxis()->SetLabelSize(0.06);
      hSignalContents.hSignalArray[i]->GetYaxis()->SetTitleOffset(1.2);
      //hSignalContents.hSignalArray[i]->GetYaxis()->SetTitleSize(0.06);
      //hSignalContents.hSignalArray[i]->GetYaxis()->SetLabelSize(0.06);
      //hSignalContents.hSignalArray[i]->GetYaxis()->SetLogy();
    }
  
  ntb = sizeof(hTimebinContents) / sizeof(TH1 *);
  printf("total no of histograms for Timebins in each Chamber: %d \n", ntb);

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
      //hTimebinContents.hTimebinArray[i]->GetXaxis()->SetTitleOffset(0.9);
      //hTimebinContents.hTimebinArray[i]->GetXaxis()->SetTitleSize(0.06);
      //hTimebinContents.hTimebinArray[i]->GetXaxis()->SetLabelSize(0.06);
      hTimebinContents.hTimebinArray[i]->GetYaxis()->SetTitleOffset(1.2);
      //hTimebinContents.hTimebinArray[i]->GetYaxis()->SetTitleSize(0.06);
      //hTimebinContents.hTimebinArray[i]->GetYaxis()->SetLabelSize(0.06);
    }
  
  hSumContents.h1SumAllsignals = new TH1D("SumAllsignals","Sum of signals from all chambers per time bin",15,0,15);
  hSumContents.h1SumAllsignals->GetXaxis()->SetNdivisions(15,false);

  // Add root histograms to Plots
  JevpPlot *plots[100];

  int n=0;
  plots[n] = new JevpPlot( hSumContents.h1SumAllsignals);
  for(int i=0;i<numAPVs;i++)plots[++n] = new JevpPlot(hPedContents.hPedArray[i]);
  for(int i=0;i<numLayers;i++) plots[++n]=new JevpPlot(hSignalContents.hSignalArray[i]);
  for(int i=0;i<numLayers;i++) plots[++n]=new JevpPlot(hTimebinContents.hTimebinArray[i]);
  //plots[i]->setDrawOpts("colz");
  //plots[n]->setOptStat(0);
  // Add Plots to plot set...
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
  if(evtCount >=0){
  

     //  Just initializing the variables.
    for ( int g = 0; g < numARMs;  g++ ) {
      for ( int h = 0; h < numPORTs; h++ ) {
	for ( int i = 0; i < numAPVs;  i++ ) {
	  for( int j = 0; j < numChannels; j++ ) {
	    counters[g][h][i][j] = 0;
	    sumADC[g][h][i][j]= 0.0;
	  }
	}
      }
    }
        
    //=======initialization ======================
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
   
    
    for( int ilayer = 0; ilayer<numLayers; ilayer++){
      	sumSignal_AllTimebins[ilayer] = 0.0;
	SumSignalPedCorrected1[ilayer] = 0.0;
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
	
	//if(arm==3) arm = 0;
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
	      //	      ch_seq = timebin+channel*numTimebins;
	      ch_seq = channel+1.0*timebin/numTimebins;
	      //if(evtCount%1000)printf("GMT ADC: RDO %d, ARM %d, APV %d: %d values\n", rdo,arm,apv,dd->ncontent) ;
	      //cout << " "<< channel << "\t tb =  "<<timebin <<" \t nTB = "<<numTimebins<<"\t ch_seq = "<<ch_seq<<endl;
	      //============= Filling pedestals including hits   ========================
	      int apvs = getAPV(arm, port, apv);
	      //cout<<"arm" << arm <<"\t port "<< port<<"\t apv "<<apv<<"\t apvs "<<apvs<<endl;
	      
	      if(apvs >=0 && apvs <numAPVs){                           
		hPedContents.hPedArray[apvs]->Fill(ch_seq,adc); 
	      }
	      
	      //======================= calculation of pedestals ========================
	      
	      if(timebin < 4){
	      double adc_tb3 = f[ii].adc;
		int ch_tb3 = f[ii].ch;
		sumADC[arm][port][apv][ch_tb3] += adc_tb3;
		counters[arm][port][apv][ch_tb3]++ ;
		} //tb
	      
	    }  //dd-> cnotent
	}  //usedAPV  loop
	//================ start added for signal ===========================

      } //1st dd->iterate
	double pedestals[numARMs][numPORTs][numAPVs][numChannels];
	
	for (int iarm=0; iarm<numARMs; iarm++){
	  for (int iport=0; iport<numPORTs; iport++){
	    for (int iapv=0; iapv<numAPVs; iapv++){
	      for (int ichannel = 0;ichannel<numChannels;ichannel++){
		pedestals[iarm][iport][iapv][ichannel] = sumADC[iarm][iport][iapv][ichannel]/counters[iarm][iport][iapv][ichannel];
		//cout << "sumADC = "<<sumADC[iarm][iport][iapv][ichannel]<<"\t counters = "<<counters[iarm][iport][iapv][ichannel]<<"\t avg. pedestals = "<<pedestals[iarm][iport][iapv][ichannel]<<endl;
	      }
	    }
	  }
	}
	
      //======================  2nd usedAPV =====================================================

	
      daq_dta *dd1=rdr->det("fgt")->get("adc");
      while(dd1 && dd1->iterate()) {
	
	int rdo = dd1->rdo;
	int arm = dd1->sec;
	int port = -999;
	int apv = dd1->pad;
	
	if(arm==3) arm = 0;
	if(apv >=0 && apv<=3){
	  port =0;
	}
	else if(apv >=12 && apv<= 15){
	  port = 1;
	  apv = apv -12;
	}

	int channel, ch_seq, timebin;
	double adc;
	
      if(usedAPV(rdo,arm,port,apv)){


	fgt_adc_t *f = (fgt_adc_t *) dd->Void ;
	for(u_int ii=0; ii<dd->ncontent;ii++)
	  {
	    
	    timebin = f[ii].tb;
	    adc = f[ii].adc;
	    channel = f[ii].ch;

	    if(timebin>=4){
	      SignalPedCorrected[arm][port][apv][channel][timebin] = adc - pedestals[arm][port][apv][channel];
	      int layer=getLayer(arm,port,apv);
	      if(layer>=0 && layer<numLayers){
	      
		if(SignalPedCorrected[arm][port][apv][channel][timebin] > ADCcut ){
	
		SumSignalPedCorrected[timebin][layer] += SignalPedCorrected[arm][port][apv][channel][timebin];
				
		} //ADCcut loop
	      }	//layers loop
	    }  //timebin loop

      //if(evtCount%1 == 0)printf("evntno %d,GMT ADC: RDO %d, ARM %d, PORT %d, APV %d \n", evtCount, rdo,arm,port,apv) ;
      //================ end added for signal===========================
      } // dd->content loop
      }	// usedAPV loop
	
      }//dd iterate loop
      
      int nlayerhits =0;
      for(int tb=0; tb<numTimebins; tb++){
      for(int ilayer=0; ilayer<numLayers; ilayer++){
	if(SumSignalPedCorrected[tb][ilayer]>0.0)nlayerhits ++;
      }
      }

      //if(nlayerhits>=2){
	      
	for(int ilayer=0; ilayer<numLayers; ilayer++){
	for (int tb =3; tb<numTimebins; tb++){
	  
	  sumSignal_AllTimebins[ilayer] = sumSignal_AllTimebins[ilayer] + SumSignalPedCorrected[tb][ilayer];
	  
	}
	
	if(sumSignal_AllTimebins[ilayer]>0)hSignalContents.hSignalArray[ilayer]->Fill(sumSignal_AllTimebins[ilayer]);  
      }
      
       for (int tb =0; tb<numTimebins; tb++){
      for(int ilayer=0; ilayer<numLayers; ilayer++){
	  sumSignal_AllChambers[tb] = sumSignal_AllChambers[tb] + SumSignalPedCorrected[tb][ilayer];
	  hTimebinContents.hTimebinArray[ilayer]->Fill(tb,SumSignalPedCorrected[tb][ilayer]);
	}
      hSumContents.h1SumAllsignals->Fill(tb,sumSignal_AllChambers[tb]);
      }
       //}  //nLayerhits loop

     //====================================================================================== 
   
  evtCount++;

  //if(evtCount%1000 == 0.)cout <<"evnts = "<< evtCount<<endl;
  } //event cut lop

  cout<< "Total no. of events = " <<evtCount<<endl;
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


