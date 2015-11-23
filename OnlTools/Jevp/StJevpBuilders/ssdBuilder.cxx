#include <stdio.h>
#include <stdlib.h>

#include "Jevp/StJevpPlot/JevpPlotSet.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include "DAQ_READER/daq_det.h"
#include <DAQ_SST/daq_sst.h> 

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TH1I.h>
#include <TH2I.h>
#include <TFile.h>
#include <TPaveStats.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "ssdBuilder.h"
#include "TStyle.h"
#include <RTS/include/rtsLog.h>
#include "TTree.h"
#define  OFFSET  375
ClassImp(ssdBuilder);

ssdBuilder::ssdBuilder(JevpServer *parent):JevpPlotSet(parent),evtCt(0) {
  plotsetname = (char *)"ssd";
}

ssdBuilder::~ssdBuilder() 
{
  for (int ns=0;ns<nSide;ns++) 
    {
      for(int nl=0;nl<nLadderPerSide;nl++)
	{
	  if(hRawAdcStrip[ns][nl])   delete hRawAdcStrip[ns][nl];
	  if(hZSAdcStrip[ns][nl])    delete hZSAdcStrip[ns][nl];
	  if(hPedStrip[ns][nl])      delete hPedStrip[ns][nl];
	  if(hRmsStrip[ns][nl])      delete hRmsStrip[ns][nl];
	}    		
      if(hRawLadderWafer[ns]) 	 delete hRawLadderWafer[ns];
      if(hZSLadderWafer[ns])  	 delete hZSLadderWafer[ns];
      if(hFailedLadderChip[ns])  delete hFailedLadderChip[ns];
    }

  for(int nr=0;nr<nRdo;nr++)
    {
      if(hErrorFiber[nr])      delete hErrorFiber[nr];
      //if(hErrorRatioRdo[nr])   delete hErrorRatioRdo[nr];
      //if(hErrorRatioFiber[nr]) delete hErrorRatioRdo[nr];
    }
  if(hErrorRdo) delete hErrorRdo;
  if(hModeCounter) delete hModeCounter;
}

//-------------------------
void ssdBuilder::initialize(int argc, char *argv[]) 
{
  errorMsg=0;
  char buffer[100];
  char buffer1[100];
  char buffer2[100];
  int merge   = 128;
  int nBinsX  = nWaferPerLadder*nStripPerWafer;
  int nBinsY  = 1024;
  int nHists  = 0;
  mSector     = 0;
  mRDO        = 0;
  mSide       = 0;
  mFiber      = 0;
  mLadder     = 0;
  mWafer      = 0;
  mChip       = 0;
  mStrip      = 0;
  mAdc        = 0;
  mAdcLength  = 0;
  mOutPutTree = 0;
  // gStyle->SetOptLogz(1);
  // gStyle->SetGridWidth(0.001);
  // gStyle->SetGridStyle(2);

  // --------------------------
  // Load Global Style Settings
  // globleStyle();

  // --------------------------
  // Load my style 
  // setstyle();

  if(mOutPutTree)
    {
      mTree = new TTree("mTree","Adc bank output ");
      mTree->Branch("Sector",&mSector,"mSector/I");
      mTree->Branch("Rdo",&mRDO,"mRDO/I");
      mTree->Branch("Side",&mSide,"mSide/I");
      mTree->Branch("Fiber",&mFiber,"mFiber/I");
      mTree->Branch("Ladder",&mLadder,"mLadder/I");
      mTree->Branch("Wafer",&mWafer,"mWafer/I");
      mTree->Branch("Strip",&mStrip,"mStrip/I");
      mTree->Branch("Adc",&mAdc,"mAdc/I");
      mTree->Branch("AdcLength",&mAdcLength,"mAdcLength/I");
    }
  //---------------------
  for ( int ns=0; ns<nSide; ns++ ) 
    {
      for(int nl=0;nl<nLadderPerSide;nl++)
	{
	  //---------------------------
	  //Define ADC Strip histograms 
	  sprintf( buffer, "RawADCStrip_%d_%d",ns,nl);
	  sprintf( buffer1, "ZSADCStrip_%d_%d",ns,nl);
	  if(ns==0)
	    sprintf( buffer2, "RAW/East-P-%d",nl+1);// east p-side
	  else 
	    sprintf( buffer2, "RAW/West-N-%d",nl+1);//west n-side
    
	  hRawAdcStrip[ns][nl] = new TH2I(buffer, buffer2,nBinsX/merge,0,nBinsX,nBinsY/4,0,nBinsY);
	  hRawAdcStrip[ns][nl]->GetXaxis()->SetTitle("Strip #");
	  hRawAdcStrip[ns][nl]->GetYaxis()->SetTitle("ADC value");
	  //hAdcStrip[ns][nl]->GetXaxis()->SetNdivisions(4,0,0,false);
	  //hAdcStrip[ns][nl]->GetXaxis()->SetNdivisions(16,0,0,false);
	  //hAdcStrip[ns][nl]->GetYaxis()->SetNdivisions(0,0,0,false);
	  hRawAdcStrip[ns][nl]->SetStats(false);//true
	  nHists += 1;

	  if(ns==0)
	    sprintf( buffer2, "ZS/East-P-%d",nl+1);// east p-side
	  else 
	    sprintf( buffer2, "ZS/West-N-%d",nl+1);//west n-side
	  hZSAdcStrip[ns][nl] = new TH2I(buffer1, buffer2,nBinsX/merge,0,nBinsX,nBinsY/4,0,nBinsY);
	  hZSAdcStrip[ns][nl]->GetXaxis()->SetTitle("Strip #");
	  hZSAdcStrip[ns][nl]->GetYaxis()->SetTitle("ADC value");
	  hZSAdcStrip[ns][nl]->SetStats(false);//true
	  nHists += 1;
	  
	  //set labele
	  for ( int index=0; index<nWaferPerLadder; index++ ) {         
	    char label[nWaferPerLadder];
	    sprintf(label, "Wafer%d", index);
	    //////hAdcStrip[ns][nl]->GetXaxis()->SetBinLabel((index*nStripPerWafer+448)/merge, label);  
	  }

	  //---------------------------
	  //Define PedStrip histograms 
	  sprintf( buffer, "hPedStrip_%d_%d",ns,nl);
	  if(ns==0)
	    sprintf( buffer2, "Ped/East-P-%d",nl+1);
	  else 
	    sprintf( buffer2, "Ped/West-N-%d",nl+1);

	  hPedStrip[ns][nl] = new TH1I(buffer, buffer2,nBinsX,0,nBinsX);
	  hPedStrip[ns][nl]->GetXaxis()->SetTitle("Physical Strip Number");
	  hPedStrip[ns][nl]->GetYaxis()->SetTitle("Pedestal");
	  hPedStrip[ns][nl]->SetMaximum(nBinsY);
	  hPedStrip[ns][nl]->SetFillColor(4);
	  //hPedStrip[ns][nl]->SetFillStyle(3144);
	  hPedStrip[ns][nl]->SetFillStyle(3013);
	  hPedStrip[ns][nl]->SetStats(false);//true
	  nHists += 1;

	  //---------------------------
	  //Define RmsStrip histograms 
	  sprintf( buffer, "hRmsStrip_%d_%d",ns,nl);
	  if(ns==0)
	    sprintf( buffer2, "RMS/East-P-%d",nl+1);
	  else 
	    sprintf( buffer2, "RMS/West-N-%d",nl+1);

	  hRmsStrip[ns][nl] = new TH1I(buffer, buffer2,nBinsX,0,nBinsX);
	  hRmsStrip[ns][nl]->GetXaxis()->SetTitle("Physical Strip Number");
	  hRmsStrip[ns][nl]->GetYaxis()->SetTitle("Rms");
	  hRmsStrip[ns][nl]->SetMaximum(0,100);
	  hRmsStrip[ns][nl]->SetFillColor(4);
	  hRmsStrip[ns][nl]->SetFillStyle(3013);
	  hRmsStrip[ns][nl]->SetStats(false);//true
	  nHists += 1;
	}
    }

  //------------------------------
  //Define Hit Map distribution 
  hRawLadderWafer[0] = new TH2I("hRawLadderWaferP","Raw data/P-side Hit Map",20,0,20,16,0,16);
  hRawLadderWafer[0]->SetName("hRawLadderWaferP");
  hRawLadderWafer[0]->GetXaxis()->SetTitle("Ladder #");
  hRawLadderWafer[0]->GetYaxis()->SetTitle("Wafer #");
  hRawLadderWafer[0]->GetYaxis()->SetNdivisions(16,0,0,false);
  hRawLadderWafer[0]->SetStats(false);
  hRawLadderWafer[0]->GetXaxis()->SetRangeUser(0,20);
  hRawLadderWafer[0]->GetXaxis()->SetNdivisions(20,0,0,false);
  nHists += 1;

  hRawLadderWafer[1] = new TH2I("hRawLadderWaferN","Raw data/N-side Hit Map",20,0,20,16,0,16);
  hRawLadderWafer[1]->SetName("hRawLadderWaferN");
  hRawLadderWafer[1]->GetXaxis()->SetTitle("Ladder #");
  hRawLadderWafer[1]->GetYaxis()->SetTitle("Wafer #");
  hRawLadderWafer[1]->GetYaxis()->SetNdivisions(16,0,0,false);
  hRawLadderWafer[1]->SetStats(false);
  hRawLadderWafer[1]->GetXaxis()->SetRangeUser(0,20);
  hRawLadderWafer[1]->GetXaxis()->SetNdivisions(20,0,0,false);
  nHists += 1;

  // ZS mode 
  hZSLadderWafer[0] = new TH2I("hZSLadderWaferP","ZS/CMN data/P-side Hit Map",20,0,20,16,0,16);
  hZSLadderWafer[0]->SetName("hZSLadderWaferP");
  hZSLadderWafer[0]->GetXaxis()->SetTitle("Ladder #");
  hZSLadderWafer[0]->GetYaxis()->SetTitle("Wafer #");
  hZSLadderWafer[0]->GetYaxis()->SetNdivisions(16,0,0,false);
  hZSLadderWafer[0]->SetStats(false);
  hZSLadderWafer[0]->GetXaxis()->SetRangeUser(0,20);
  hZSLadderWafer[0]->GetXaxis()->SetNdivisions(20,0,0,false);
  nHists += 1;

  hZSLadderWafer[1] = new TH2I("hZSLadderWaferN","ZS/CMN data/N-side Hit Map",20,0,20,16,0,16);
  hZSLadderWafer[1]->SetName("hZSLadderWaferN");
  hZSLadderWafer[1]->GetXaxis()->SetTitle("Ladder #");
  hZSLadderWafer[1]->GetYaxis()->SetTitle("Wafer #");
  hZSLadderWafer[1]->GetYaxis()->SetNdivisions(16,0,0,false);
  hZSLadderWafer[1]->SetStats(false);
  hZSLadderWafer[1]->GetXaxis()->SetRangeUser(0,20);
  hZSLadderWafer[1]->GetXaxis()->SetNdivisions(20,0,0,false);
  nHists += 1;

  //-----------------------------------
  // CMN Algorithm Faild Map 
  hFailedLadderChip[0] = new TH2I("hFailedLadderChipP","P-Side CMN Failed Chip Map",20,0,20,96,0,96);
  hFailedLadderChip[0]->SetName("hFailedLadderChipP");
  hFailedLadderChip[0]->GetXaxis()->SetTitle("Ladder #");
  hFailedLadderChip[0]->GetYaxis()->SetTitle("Wafer #");
  // hFailedLadderChip[0]->GetYaxis()->SetNdivisions(96,0,0,true);
  hFailedLadderChip[0]->SetStats(false);
  hFailedLadderChip[0]->GetXaxis()->SetRangeUser(0,20);
  hFailedLadderChip[0]->GetXaxis()->SetNdivisions(20,0,0,false);
  nHists += 1;

  hFailedLadderChip[1] = new TH2I("hFailedLadderChipN","N-side CMN Failed Chip Map",20,0,20,96,0,96);
  hFailedLadderChip[1]->SetName("hFailedLadderChipN");
  hFailedLadderChip[1]->GetXaxis()->SetTitle("Ladder #");
  hFailedLadderChip[1]->GetYaxis()->SetTitle("Wafer #");
  // hFailedLadderChip[1]->GetYaxis()->SetNdivisions(96,0,0,true);
  hFailedLadderChip[1]->SetStats(false);
  hFailedLadderChip[1]->GetXaxis()->SetRangeUser(0,20);
  hFailedLadderChip[1]->GetXaxis()->SetNdivisions(20,0,0,false);
  nHists += 1;

  //-----------------------------------
  //Error code distribution in each RDO
  sprintf( buffer, "hErrorRdo");
  sprintf( buffer2, "Error Code Distribution in Each RDO");
  hErrorRdo = new TH2I(buffer,buffer2,5,0,5,5,0,5);
  hErrorRdo->GetXaxis()->SetTitle("RDO #");
  hErrorRdo->GetYaxis()->SetTitle("Error Code #");
  //hErrorRdo->GetZaxis()->SetTitle("Ratio");
  hErrorRdo->GetXaxis()->SetNdivisions(5,0,0,false);
  hErrorRdo->GetYaxis()->SetNdivisions(5,0,0,false);
  hErrorRdo->GetYaxis()->SetLabelSize(0.03);
  hErrorRdo->GetYaxis()->SetLabelOffset(0.003);
  hErrorRdo->SetStats(false);
  nHists += 1;

  //------------------------------
  //Define monitor FPGA histograms
  for(int nr=0;nr<nRdo;nr++)
    {
      //-------------------------------------
      //Error code distribution in each Fiber
      sprintf( buffer, "hErrorFiber_%d",nr);
      sprintf( buffer2, "Fiber Error Code Distribution in RDO %d",nr);
      hErrorFiber[nr] = new TH2I(buffer,buffer2,8,0,8,8,0,8);
      hErrorFiber[nr]->GetXaxis()->SetTitle("Fiber #");
      hErrorFiber[nr]->GetYaxis()->SetTitle("Error Code #");
      hErrorFiber[nr]->GetYaxis()->SetLabelSize(0.03);
      hErrorFiber[nr]->GetYaxis()->SetLabelOffset(0.003);
      //hErrorFiber[nr]->GetZaxis()->SetTitle("Ratio");
      //hErrorFiber[nr]->GetXaxis()->SetNdivisions(8,0,0,false);
      hErrorFiber[nr]->SetStats(false);
      nHists += 1;

      ////-----------------------------------
      ////Error code ratio per event in each RDO
      //sprintf( buffer, "hErrorRatioRdo_%d",nr);
      //sprintf( buffer2, "Error Ratio per Event in RDO %d",nr);
      //hErrorRatioRdo[nr] = new TH1I(buffer,buffer2,10,0,10);
      //hErrorRatioRdo[nr]->GetXaxis()->SetTitle("Error Code #");
      //hErrorRatioRdo[nr]->GetYaxis()->SetTitle("Ratio per Event");
      //hErrorRatioRdo[nr]->SetStats(false);
      //nHists += 1;
    }

  //-----------------------------------
  // Data mode counter
  sprintf( buffer, "hModeCounter");
  sprintf( buffer2, "Data Mode Counter");
  hModeCounter = new TH1I(buffer,buffer2,3,0,3);
  hModeCounter->GetXaxis()->SetTitle("Mode ");
  hModeCounter->GetYaxis()->SetTitle("Counts");
  // hModeCounte->GetZaxis()->SetTitle("Ratio");
  hModeCounter->GetXaxis()->SetNdivisions(3,0,0,false);
  hModeCounter->GetYaxis()->SetNdivisions(3,0,0,false);
  hModeCounter->GetYaxis()->SetLabelSize(0.03);
  hModeCounter->GetYaxis()->SetLabelOffset(-0.003);
  hModeCounter->SetStats(false);
  hModeCounter->SetFillColor(3);
  nHists += 1;

  // ------------------
  // JEVP plots setting
  // ------------------
  plots = new JevpPlot*[nHists];  
  Int_t histcounter = 0;

  for( int i=0;i<nSide;i++) 
    {
    for(int j=0;j<nLadderPerSide;j++)
      {
	// -----------------------------------
	// PLEASE BE CAREFUL THE INDEX ISSUES
	// -----------------------------------

	//Raw AdcStrip
	plots[nLadderPerSide*i+j] = new JevpPlot(hRawAdcStrip[i][j]);
	plots[nLadderPerSide*i+j]->optlogz=true;
	plots[nLadderPerSide*i+j]->setDrawOpts("COLZ");
	histcounter += 1;

	// ZSAdcStrip
	plots[nSide*nLadderPerSide+nLadderPerSide*i+j] = new JevpPlot(hZSAdcStrip[i][j]);
	plots[nSide*nLadderPerSide+nLadderPerSide*i+j]->optlogz=true;
	plots[nSide*nLadderPerSide+nLadderPerSide*i+j]->setDrawOpts("COLZ");
	histcounter += 1;
	
	//PedStrip
	plots[2*nSide*nLadderPerSide+nLadderPerSide*i+j] = new JevpPlot(hPedStrip[i][j]);
	plots[2*nSide*nLadderPerSide+nLadderPerSide*i+j]->optlogz=true;
	//plots[2*nSide*nLadderPerSide+nLadderPerSide*i+j]->setDrawOpts("*H");
	histcounter += 1;

	//RmsStrip
	plots[3*nSide*nLadderPerSide+nLadderPerSide*i+j] = new JevpPlot(hRmsStrip[i][j]);
	plots[3*nSide*nLadderPerSide+nLadderPerSide*i+j]->optlogz=true;
	//plots[3*nSide*nLadderPerSide+nLadderPerSide*i+j]->setDrawOpts("*H");
	histcounter += 1;
      }
    }
  
  for(int ns=0;ns<nSide;ns++)
    {
      //Raw LadderWafer
      plots[histcounter] = new JevpPlot(hRawLadderWafer[ns]);
      // plots[histcounter]->optlogz=true;
      // plots[histcounter]->optlogz=false;
      plots[histcounter]->setDrawOpts("COLZ");  
      histcounter += 1;

      //ZS LadderWafer
      plots[histcounter] = new JevpPlot(hZSLadderWafer[ns]);
      // plots[histcounter]->optlogz=true;
      // plots[histcounter]->optlogz=false;
      plots[histcounter]->setDrawOpts("COLZ");  
      histcounter += 1;

      //FailedLadderChip
      plots[histcounter] = new JevpPlot(hFailedLadderChip[ns]);
      plots[histcounter]->optlogz=true;
      plots[histcounter]->setDrawOpts("COLZ");  
      histcounter += 1;
    }

  for(int nr=0;nr<nRdo;nr++)
    {  
      //ErrorRatioRdo
      plots[histcounter] = new JevpPlot(hErrorFiber[nr]);
      plots[histcounter]->optlogz=true;
      plots[histcounter]->setDrawOpts("COLZTEXT");  
      histcounter += 1;
    }

  //ErrorRdo
  plots[histcounter] = new JevpPlot(hErrorRdo);
  plots[histcounter]->setDrawOpts("COLZTEXT");  
  plots[histcounter]->logy=0;
  histcounter += 1;

  //Mode Counter
  plots[histcounter] = new JevpPlot(hModeCounter);
  plots[histcounter]->setDrawOpts("HISTTEXT");  
  plots[histcounter]->logy=0;
  histcounter += 1;

  LOG(WARN,"Number of Hist %d , Added number of hist %d",nHists,histcounter);
  //---------
  //add plots to plot set
  for ( int i=0; i<histcounter;i++ )
    {
      LOG(DBG, "Adding plot %d",i);
      addPlot(plots[i]);
    }
}

//-------------------------
void ssdBuilder::startrun(daqReader *rdr) 
{
  LOG ( DBG, "ssdBuilder starting run #%d", rdr->run );
  resetAllPlots();
 
  mSector = 0;
  mRDO    = 0;
  mSide   = 0;
  mFiber  = 0;
  mLadder = 0;
  mWafer  = 0;
  mChip   = 0;
  mStrip  = 0;
  evtCt   = 0; 
  //errorMsg->SetText("No Error Message");    
}

#define safelog(x) ((x > 0) ? log10(x) : 0)

//-------------------------

void ssdBuilder::event(daqReader *rdr) {

  if ( !(evtCt %1000) )     LOG(DBG, "Looking at evt %d",evtCt);
  daq_dta *dd ;
  
  //--------------------
  //Decode Raw Data Bank
  //--------------------

  dd = rdr->det("sst")->get("raw");
  
  while(dd && dd->iterate()) 
    { 
      Int_t AdcLength = 0;
      Int_t index = 0;
      u_int *d = (u_int *)dd->Void;
      int FiberError[8] = {0}; //000--Fiber Token Error,001--No Data,002--OverFlow,003--Early Abort,004--Not Supported Format
      int RdoError; //0-Header Token Error, 1-TCD Token Error,2-TCD end Token Error, 3-End Token Error 
      int maxI = (int)dd->ncontent/4;    

      mSector = dd->sec;
      
      if(mSector==1)
  	mRDO = dd->rdo;
      else 
  	mRDO = 3 + dd->rdo;
      
      if ( mSector < 1 || mSector > 2 )  continue;  //sector 1-2
      if ( mRDO < 1 || mRDO > 5 )        continue;  //RDO 1-5

      LOG(DBG,"SST ADC: Sector %d , RDO %d , Fiber %d",mSector,mRDO,mFiber);
      
      if(d[index] != HEADER_TOKEN) {
        printf("ssdBuilder : SST DATA HEADER_TOKEN not correct : 0x%08X",d[index]);

        // LOG("ssdBuilder","SST DATA HEADER_TOKEN not correct");

      	RdoError = 0; //header Token issue
	hErrorRdo->Fill(mRDO-1,"HT Err",1);
      	LOG(WARN,"SST DAQ DATA HEADER_TOKEN is not correct!! header : 0x%08X",d[index]);
      }
      
      index += HEADER_LENGTH;
      mDataMode = Mid(DATAMODE_START, DATAMODE_END, d[index+1]);
      if(mDataMode==RAWMODE) hModeCounter->Fill("RAW",1);
      if(mDataMode==COMPRESSEDMODE) hModeCounter->Fill("ZS",1);
      if(mDataMode==CMNSMODE) hModeCounter->Fill("CMN",1);

      for(int i=0;i<8;i++)
      	{	 
      	  if(d[index] != FIBER_LINK_TOKEN) 
      	    {
	      hErrorFiber[mRDO-1]->Fill(i,"Fiber Token Error",1);
	      break;
      	    }

	  FiberError[i] = Mid(FLAG_START, FLAG_END, d[index+1]);
	  if(FiberError[i]!=0) 
	    {
	      // hErrorFiber[mRDO-1]->Fill(i,"Normal",1);
	      if(FiberError[i]==1) hErrorFiber[mRDO-1]->Fill(i,"No Data",1);
	      else if(FiberError[i]==2) hErrorFiber[mRDO-1]->Fill(i,"Over Flow",1);
	      else if(FiberError[i]==3) hErrorFiber[mRDO-1]->Fill(i,"Early Abort",1);
	      else hErrorFiber[mRDO-1]->Fill(i,"Not Support",1);
	    }
	  AdcLength = Mid(ADC_START,ADC_END,d[index+1]);
      	  index += AdcLength;
      	}
      
      if(d[index] != TCD_TOKEN)       {RdoError = 1; hErrorRdo->Fill(mRDO-1,"TT Err",1); LOG(WARN,"SST DAQ DATA TCD_TOKEN is not correct!! RDO : %i TCD_TOKEN : 0x%08X",mRDO,d[index]);}//TCD Token Error 
      if(d[index+2] != TCD_END_TOKEN) {RdoError = 2; hErrorRdo->Fill(mRDO-1,"TET Err",1);LOG(WARN,"SST DAQ DATA TCD_END_TOKEN is not correct!! RDO : %i TCD_END_TOKEN : 0x%08X",mRDO,d[index+3]);}//TCD End Token Error 
      if(d[index+4] != END_TOKEN)     {RdoError = 3; hErrorRdo->Fill(mRDO-1,"ET Err",1); LOG(WARN,"SST DAQ DATA END_TOKEN is not correct!! RDO : %i END_TOKEN : 0x%08X",mRDO,d[index+5]);}//End Token Error 
      // if(d[index] != TCD_TOKEN || d[index+2] != TCD_END_TOKEN || d[index+4] != END_TOKEN) 
      // 	{
      // 	  for(int ie=0;ie<5;ie++) 
      // 	    {
      // 	      LOG(WARN,"Evt : %i MODE : %i RDO : %i DAQ Words : 0x%08X",evtCt,mDataMode,mRDO,d[index+ie]);
      // 	    }
      // 	}

      index = 0;
    }
  
  //-------------------------------
  //Directly read Tonko's Adc Bank
  //-------------------------------

  dd = rdr->det("sst")->get("adc");

  while(dd && dd->iterate()) 
    { 
    
      daq_sst_data_t *sst = (daq_sst_data_t *)dd->Void;
           
      mSector = dd->sec;
      
      if(mSector==1)
	mRDO = dd->rdo;
      else 
	mRDO = 3 + dd->rdo;
      
      mFiber = dd->pad;
     
      if ( mFiber < 0 || mFiber > 7 )    continue;  //fiber 0-7
      if ( mSector < 1 || mSector > 2 )  continue;  //sector 1-2
      if ( mRDO < 1 || mRDO > 5 )        continue;  //RDO 1-5
      
      LOG(DBG,"SST ADC: Sector %d , RDO %d , Fiber %d",mSector,mRDO,mFiber);
      u_int maxI = dd->ncontent;    
      FindLadderSide(mRDO,mFiber,mLadder,mSide);   

      LOG(DBG,"SST ADC: Ladder %d , side %d",mLadder,mSide);

      int ChipFlag = 0;		// Chip Status 0-good, 1-bad
      int OldChip  = 0;      	// Chip index buffer
      
      for ( u_int i=0; i<maxI; i++ ) {
	mWafer = sst[i].hybrid;
	mStrip = sst[i].strip;
	mAdc   = sst[i].adc;
	mChip = mWafer*nChipPerWafer + mStrip/128;
	
	if ( mStrip<0 || mStrip>767 )    continue; //strip 0-767
	if ( mWafer<0 || mWafer>15 )     continue; //wafer 0 15
	if ( mAdc > 1024 )               continue; //adc 0-1024
	if ( mChip > 96 )                continue; //Chip 0-95

	if (OldChip!=mChip) ChipFlag = 0;
	
	if(mDataMode==CMNSMODE && mStrip==0 && mAdc==0xB)
	  {
	    // When CMN Algorithm failed, the first channel will be 0xB.
	    hFailedLadderChip[mSide]->Fill(mLadder,mWafer*nChipPerWafer+mStrip/128);
	    ChipFlag = 1;
	  }

	if (ChipFlag) continue;	// Discard Bad Chips
	
	//====================
	if(mOutPutTree) mTree->Fill();     
	//====================
	FindStripNumber(mStrip);
	LOG(DBG,"##Strip %d , hybrid %d , Adc %d",mStrip,mWafer,mAdc);
	if(mSide==0)
	  {
	    if(mDataMode==RAWMODE) hRawLadderWafer[0]->Fill(mLadder,mWafer);
	    if(mDataMode==COMPRESSEDMODE || mDataMode==CMNSMODE) hZSLadderWafer[0]->Fill(mLadder,mWafer);
	  }
	if(mSide==1)
	  {
	    if(mDataMode==RAWMODE) hRawLadderWafer[1]->Fill(mLadder,mWafer);
	    if(mDataMode==COMPRESSEDMODE || mDataMode==CMNSMODE) hZSLadderWafer[1]->Fill(mLadder,mWafer);
	  }

	if(mDataMode==RAWMODE) hRawAdcStrip[mSide][mLadder]->Fill((mStrip+mWafer*nStripPerWafer), (mAdc+OFFSET)%1024);
	if(mDataMode==COMPRESSEDMODE || mDataMode==CMNSMODE) hZSAdcStrip[mSide][mLadder]->Fill((mStrip+mWafer*nStripPerWafer), (mAdc+OFFSET)%1024);
       
	OldChip = mChip;       
      }//end all RDO,Fiber,Ladder loop
    }

  //---------------------
  //For Pedestal Run only
  //---------------------

  dd = rdr->det("sst")->get("pedrms");

  while(dd && dd->iterate()) 
    { 
      daq_sst_pedrms_t *sst = (daq_sst_pedrms_t *)dd->Void;
           
      mSector = dd->sec;
      
      if(mSector==1)
	mRDO = dd->rdo;
      else 
	mRDO = 3 + dd->rdo;
      
      mFiber = dd->pad;
      u_int maxI = dd->ncontent;
      
      if ( mFiber < 0 || mFiber > 7 )   continue;  //fiber 0-7
      if ( mSector < 1 || mSector > 2 ) continue;  //sector 1-2
      if ( mRDO < 1 || mRDO > 5 )       continue;  //RDO 1-5
      LOG(DBG,"##SST PEDRMS: Sector %d , RDO %d , Fiber %d",mSector,mRDO,mFiber);
          
      FindLadderSide(mRDO,mFiber,mLadder,mSide);   
      LOG(DBG,"##SST ADC: Ladder %d , side %d",mLadder,mSide);
           
      for ( u_int i=0; i<maxI; i++ )
	{
	  for(int h=0;h<nWaferPerLadder;h++)
	    {
	    for(int c=0;c<nStripPerWafer;c++)
	      {
		int s = c;
		FindStripNumber(s);
		mPed = sst->ped[h][c];
		mRms = sst->rms[h][c]/16.0;
		
		if(mSide==0)
		  hRawLadderWafer[0]->Fill(mLadder,h);
		if(mSide==1)
		  hRawLadderWafer[1]->Fill(mLadder,h);
		hPedStrip[mSide][mLadder]->SetBinContent((s+h*nStripPerWafer+1),mPed);
	        hRmsStrip[mSide][mLadder]->SetBinContent((s+h*nStripPerWafer+1),mRms);
	      }
          }  
      }
    }
  if(dd)
    evtCt++;
}
//-------------------------------

void ssdBuilder::stoprun(daqReader *rdr) 
{
  mSector = 0;
  mRDO    = 0;
  mSide   = 0;
  mFiber  = 0;
  mLadder = 0;
  mWafer  = 0;
  mStrip  = 0;
  evtCt   = 0;
  if(mOutPutTree)
    {
      LOG(DBG,"Writing Histogram !");
      TFile *output = new TFile("AdcBank.root","RECREATE");
      output->cd();
      mTree->Write();
      output->Close();
    }
}
//-----------------------------
void ssdBuilder::main(int argc, char *argv[])
{
  ssdBuilder myssd;
  
  myssd.Main(argc, argv);
  
}

//---------------------------
void ssdBuilder::FindLadderSide(int RDO,int channel,int &ladder,int &side)
{
  ladder = RDO2LADDER[RDO-1][channel]-1;//ladder [0-19];
  if(ladder>20) LOG(DBG,"RDO Number is :%d, channel number :%d",RDO,channel);
  if(RDO<3) side = 1;
  else if(RDO>3) side = 0;
  else if(channel<4) side = 1;
  else side = 0;
}
//----------------------------
void ssdBuilder::FindStripNumber(int &strip)
{
  int temp = (strip/128)*128 + ReadOutMap[strip%128];
  strip = temp-1;
}
//----------------------------
void ssdBuilder::UpdateRatio(Float_t &ratio,Int_t Nevent,Int_t value)
{
  Float_t temp = (Nevent-1)*ratio;
  temp += value;
  ratio = temp/(Float_t)Nevent;
}
//-------------------------------------------------
Int_t ssdBuilder::Mid(UInt_t start, UInt_t end, UInt_t input)
{
  Int_t buf;
  buf = input << (32 - end);
  buf = buf >> (32 - end);
  return buf >> start;
}

void ssdBuilder::globleStyle() 
{
  cout<<"gStyle mode requested!!!"<<endl;

  int font = 22;

  gStyle->SetOptTitle(1);
  gStyle->SetOptDate(0);
  gStyle->SetOptStat(0);
  gStyle->SetStatColor(10);
  //gStyle->SetOptFit(0);
  gStyle->SetStatH(0.17);
  gStyle->SetStatW(0.17);
  gStyle->SetPalette(1,0);
  gStyle->SetTextFont(font);
  gStyle->SetTextSize(0.055);
  //gStyle->SetErrorX(1);
  gStyle->SetEndErrorSize(4);
  gStyle->SetDrawBorder(0);

  gStyle->SetCanvasDefH(600);
  gStyle->SetCanvasDefW(800);
  gStyle->SetCanvasColor(10);
  gStyle->SetCanvasBorderMode(0);
  gStyle->SetCanvasBorderSize(2);
  gStyle->SetPadColor(10);
  gStyle->SetPadBorderMode(0);
  gStyle->SetPadBorderSize(0);
  gStyle->SetPadBottomMargin(0.12);
  gStyle->SetPadLeftMargin(0.12);
  gStyle->SetPadRightMargin(0.10);
  gStyle->SetPadTopMargin(0.08);
  gStyle->SetPadTickX(1);
  gStyle->SetPadTickY(1);
  gStyle->SetTickLength(0.02,"X");
  gStyle->SetTickLength(0.02,"Y");
  gStyle->SetPadGridX(0);
  gStyle->SetPadGridY(0);
  gStyle->SetGridColor(18);
  gStyle->SetFrameFillStyle(4000);
  gStyle->SetFrameLineWidth(2);
  gStyle->SetFrameBorderSize(2);
  gStyle->SetFrameBorderMode(0);
  gStyle->SetFrameFillColor(10);
  //gStyle->SetFrameLineStyle(1);

  gStyle->SetNdivisions(510,"X");
  gStyle->SetNdivisions(510,"Y");
  gStyle->SetLabelSize(0.04,"X");
  gStyle->SetLabelSize(0.04,"Y");
  gStyle->SetLabelFont(font,"X");
  gStyle->SetLabelFont(font,"Y");
  gStyle->SetLabelOffset(0.01,"X");
  gStyle->SetLabelOffset(0.01,"Y");
  gStyle->SetTitleOffset(1.0,"X");
  gStyle->SetTitleOffset(1.2,"Y");
  gStyle->SetTitleOffset(1.2,"Z");
  gStyle->SetTitleSize(0.05,"X");
  gStyle->SetTitleSize(0.05,"Y");
  gStyle->SetTitleSize(0.05,"Z");
  gStyle->SetTitleFont(font,"X");
  gStyle->SetTitleFont(font,"Y");
  gStyle->SetTitleFont(font,"Z");
  gStyle->SetTitleColor(1);
}

// My Style 
void ssdBuilder::setstyle()
{
  TStyle* myStyle = new TStyle("myStyle","Styles");
  myStyle->SetPalette(1,0); // avoid horrible default color scheme
  myStyle->SetOptStat("e");
  myStyle->SetOptTitle(1);
  myStyle->SetOptDate(0);
  myStyle->SetLabelSize(0.045,"xyz"); // size of axis value font
  myStyle->SetTitleX(0.2f);
  myStyle->SetTitleY(0.96f);
  myStyle->SetTitleW(0.5f);
  myStyle->SetTickLength(0.01,"xyz");
  myStyle->SetTitleFont(62,"xyz"); // font option 
  myStyle->SetLabelFont(62,"xyz");
  myStyle->SetTitleOffset(0.8,"z");
  myStyle->SetTitleOffset(1.2,"y");
  myStyle->SetTitleFillColor(10);
  myStyle->SetLineWidth(2);
  myStyle->SetCanvasDefW(700);
  myStyle->SetCanvasDefH(600);
  myStyle->SetCanvasColor(0);// canvas...
  myStyle->SetCanvasBorderMode(0);
  myStyle->SetCanvasBorderSize(0);
  myStyle->SetPadColor(0);
  myStyle->SetPadBorderSize(1);
  myStyle->SetPadBorderMode(-1);
  myStyle->SetPadBottomMargin(0.14); //margins...
  myStyle->SetPadTopMargin(0.06);
  myStyle->SetPadLeftMargin(0.14);
  myStyle->SetPadRightMargin(0.04);
  myStyle->SetPadGridX(0); // grids, tickmarks
  myStyle->SetPadGridY(0);
  myStyle->SetPadTickX(1);
  myStyle->SetPadTickY(1);
  myStyle->SetFrameBorderSize(1);
  myStyle->SetFrameBorderMode(-1);
  myStyle->SetFrameFillColor(0);
  myStyle->SetFrameLineWidth(1.2);
  myStyle->SetPaperSize(20,24); // US letter size
  gROOT->SetStyle("myStyle");
  cout << "Styles are Set!" << endl;
}
