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
#include "istBuilder.h"
#include <RTS/include/rtsLog.h>

ClassImp(istBuilder);

istBuilder::istBuilder(JevpServer *parent):JevpPlotSet(parent),evtCt(0) {
  plotsetname = (char *)"ist";
  // start with histograms undefined...
  memset( &hAdcContents,      0, sizeof(hAdcContents) );
  memset( &hMultContents,     0, sizeof(hMultContents) );
  memset( &hHitMapContents,   0, sizeof(hHitMapContents) );
  memset( &hTbVsAdcContents,  0, sizeof(hTbVsAdcContents) );
  memset( &hEventSumContents, 0, sizeof(hEventSumContents) );
  memset( &hMipContents,      0, sizeof(hMipContents) );
  memset( &hSumContents,      0, sizeof(hSumContents) );
  memset( &hCmnTemp,	      0, sizeof(hCmnTemp) );
}

istBuilder::~istBuilder() {
  // Delete any existing histograms...  
  int nAdcHist      = sizeof(hAdcContents) / sizeof(TH2 *);
  int nMultHist     = sizeof(hMultContents) / sizeof(TH1 *);
  int nHitMapHist   = sizeof(hHitMapContents) / sizeof(TH2 *);
  int nTbVsAdcHist  = sizeof(hTbVsAdcContents) / sizeof(TH2 *);
  int nEventSumHist = sizeof(hEventSumContents) / sizeof(TH1 *);
  int nMipHist      = sizeof(hMipContents) / sizeof(TH1 *);
  int nSumHist      = sizeof(hSumContents) / sizeof(TH2 *);  
  
  for ( int i=0; i<nAdcHist; i++ )      {    if(hAdcContents.adcArray[i])          delete hAdcContents.adcArray[i];    		}
  for ( int i=0; i<nMultHist; i++ )     {    if(hMultContents.multArray[i])        delete hMultContents.multArray[i];    	}
  for ( int i=0; i<nHitMapHist; i++ )   {    if(hHitMapContents.hitMapArray[i])    delete hHitMapContents.hitMapArray[i];    	}
  for ( int i=0; i<nTbVsAdcHist; i++ )  {    if(hTbVsAdcContents.tbVsAdcArray[i])  delete hTbVsAdcContents.tbVsAdcArray[i];    	}
  for ( int i=0; i<nEventSumHist; i++ ) {    if(hEventSumContents.eventSumArray[i])delete hEventSumContents.eventSumArray[i];   }
  for ( int i=0; i<nMipHist; i++ )      {    if(hMipContents.mipArray[i])          delete hMipContents.mipArray[i];    		}
  for ( int i=0; i<nSumHist; i++ )      {    if(hSumContents.sumArray[i])          delete hSumContents.sumArray[i];    		}
  for ( int i=0; i<totAPV; i++ )        {    if(hCmnTemp.hCmnPerChip[i])	   delete hCmnTemp.hCmnPerChip[i];		}
}

// **********IST INITIALIZE*****************
// -----------------------------------------
void istBuilder::initialize(int argc, char *argv[]) {
  // Initialization of histograms.
  // could run a loop...
  // Add root histograms to Plots
  
  errorMsg=0;

  for( int i=0; i<totCh; i++ ) {
    meanVals[i]        = 0;
    aVals[i]           = 0;
    numVals[i]         = 0;
    numOverOneSig[i]   = 0;
    oldStdDevs[i]      = 0;
    isChannelBad[i]    = false;
    runningAvg[i]      = 0;
    runningStdDevSq[i] = 0;
  }
  for ( int i=0; i<totCh; i++ )         {    maxAdc[i]          = 0;   }
  for ( int i=0; i<totCh; i++ )         {    maxTimeBin[i]      = 0;   }
  for ( int i=0; i<totAPV; i++ )        {    cmNoise[i]         = 0;   }

  // //////////////////////////////////add bad channels here///////////////////////
  // ///////////////////isChannelBad[numAssembly*ChPerSec+channel]=true;

  //filling IST mapping
  Int_t channelId=-1, ladder=-1, sensor=-1, column=-1, row=-1;
  for(int rdo=1; rdo<=numRDO; rdo++) {
        for(int arm=0; arm<numARM; arm++) {
            for(int apv=0; apv<numAPV; apv++) {
                for(int chan=0; chan<ChPerApv; chan++) {
                    channelId = (rdo-1)*numARM*numAPV*ChPerApv+arm*numAPV*ChPerApv+apv*ChPerApv+chan;
                    // the following mapping refered from IST readout channel datasheet
		    if(channelId<101376)
                        ladder = 22 - channelId/ChPerLadder;
                    else
                        ladder = 46 - channelId/ChPerLadder;

		    //Update 03/03/14 Yaping: sections B and C swapped on ladder 13
		    if(channelId>=43008 && channelId<44544) //section C
                        sensor = 4 - (channelId%ChPerLadder)/ChPerSensor;
                    else if(channelId>=44544 && channelId<46080) //section B
                        sensor = 8 - (channelId%ChPerLadder)/ChPerSensor;
                    else
                    	sensor = 6 - (channelId%ChPerLadder)/ChPerSensor;

                    Int_t pad = (channelId%ChPerLadder)%ChPerSensor;
                    column = 12 - pad/numRow;
                    if(column%2)
                        row = 64 - pad%numRow;
                    else
                        row = 1 + pad%numRow;

                    istMapping[channelId] = (ladder-1)*ChPerLadder + (sensor-1)*ChPerSensor + (column-1)*numRow + row;
                }
            }
        }
  }

  mAdcHist      = sizeof(hAdcContents) / sizeof(TH2 *);
  mMultHist     = sizeof(hMultContents) / sizeof(TH1 *);
  mHitMapHist   = sizeof(hHitMapContents) / sizeof(TH2 *);
  mTbVsAdcHist  = sizeof(hTbVsAdcContents) / sizeof(TH2 *);
  mEventSumHist = sizeof(hEventSumContents) / sizeof(TH1 *);
  mMipHist      = sizeof(hMipContents) / sizeof(TH1 *);
  mSumHist      = sizeof(hSumContents) / sizeof(TH2 *);
  
  char buffer[100];
  char buffer2[100];

  int nBins       = 50;
  int nBinsAPV    = 864;
  int nBinsCh     = 864;
  int nBinsTB     = 32;
  double ADCMax   = 4100.;
  double ADCMin   = -100.;
  double TBMax    = 32.;
  double PedMin   = -100.;
  double PedMax   = 4100.;
  double SigMin   = -10.;
  double SigMax   = 100.;
  double CmnMin   = -10.;
  double CmnMax   = 40.;

  //////////////////////
  for ( int index=0; index<mAdcHist; index++ ) {
    sprintf( buffer, "ADC_Vs_Channel_Ladder_%d", index+1 );
    sprintf( buffer2, "IST - ADC vs Channel Id, Ladder: %d", index+1 );
    
    hAdcContents.adcArray[index] = new TH2F(buffer, buffer2, 48, 1, ChPerLadder+1, nBins, ADCMin, ADCMax); //48*50 bins
    hAdcContents.adcArray[index]->GetXaxis()->SetTitle("Channel Index");
    hAdcContents.adcArray[index]->GetYaxis()->SetTitle("ADC value");
    hAdcContents.adcArray[index]->GetXaxis()->SetNdivisions(numSensor,false);
    hAdcContents.adcArray[index]->SetStats(false);
    hAdcContents.adcArray[index]->GetYaxis()->SetTitleOffset(1.1);
  }

  /////////////////////
  for ( int index=0; index<mMultHist; index++ ) {
    sprintf( buffer, "HitMult_Ladder_%d", index+1 );
    sprintf( buffer2, "IST - Hit Multiplicity, Ladder: %d", index+1 );

    hMultContents.multArray[index] = new TH1F(buffer, buffer2, nBins, 0, ChPerLadder); // 50 bins
    hMultContents.multArray[index]->GetXaxis()->SetTitle("Number of Hits");
    hMultContents.multArray[index]->GetYaxis()->SetTitle("Counts");
    hMultContents.multArray[index]->SetStats(true);
    hMultContents.multArray[index]->GetYaxis()->SetTitleOffset(1.1);
  }

  /////////////////////
  for ( int index=0; index<mHitMapHist; index++ ) {
    sprintf( buffer, "HitMap_Ladder_%d", index+1 );
    sprintf( buffer2, "IST - Hit Map (density), Ladder: %d", index+1 );

    hHitMapContents.hitMapArray[index] = new TH2F(buffer, buffer2, numRow, 1, numRow+1, numColumn*numSensor, 1, numColumn*numSensor+1); //64*72 bins
    hHitMapContents.hitMapArray[index]->GetXaxis()->SetTitle("Row Index (#phi)");
    hHitMapContents.hitMapArray[index]->GetYaxis()->SetTitle("Column Index (Z)");
    hHitMapContents.hitMapArray[index]->GetYaxis()->SetNdivisions(numSensor,false);
    hHitMapContents.hitMapArray[index]->SetStats(false);
  }

  ////////////////////
  for ( int index=0; index<mTbVsAdcHist; index++ ) {
    sprintf( buffer, "ADC_Vs_Tb_Ladder_%d", index+1 );
    sprintf( buffer2, "IST - ADC vs Timebin, Ladder: %d", index+1 );

    hTbVsAdcContents.tbVsAdcArray[index] = new TH2F(buffer, buffer2, numTimeBin, 0, numTimeBin, nBins, ADCMin, ADCMax); //9*50 bins
    hTbVsAdcContents.tbVsAdcArray[index]->GetXaxis()->SetTitle("Time Bin Index");
    hTbVsAdcContents.tbVsAdcArray[index]->GetYaxis()->SetTitle("ADC value");
    hTbVsAdcContents.tbVsAdcArray[index]->GetXaxis()->SetNdivisions(numTimeBin,false);
    hTbVsAdcContents.tbVsAdcArray[index]->SetStats(false);
    hTbVsAdcContents.tbVsAdcArray[index]->GetYaxis()->SetTitleOffset(1.1);
    for ( int iTB=0; iTB<numTimeBin; iTB++ ) {
      sprintf( buffer, "TB%d", iTB );
      hTbVsAdcContents.tbVsAdcArray[index]->GetXaxis()->SetBinLabel(iTB+1,buffer);
    }
  }

  ///////////////////
  hEventSumContents.hMeanPed = new TH1F("MeanPeds", "IST - <Mean Pedestal>", nBins*2, PedMin, PedMax); //100 bins
  hEventSumContents.hMeanPed->GetXaxis()->SetTitle("Mean Pedestal [ADC counts]");
  hEventSumContents.hMeanPed->SetFillColor(kYellow-9);
  hEventSumContents.hMeanPed->SetStats(true);

  hEventSumContents.hMeanRMS = new TH1F("MeanRMS", "IST - <RMS Pedestal>", nBins*2, SigMin, SigMax); //100 bins
  hEventSumContents.hMeanRMS->GetXaxis()->SetTitle("RMS pedestal [ADC counts]");
  hEventSumContents.hMeanRMS->SetFillColor(kYellow-9);
  hEventSumContents.hMeanRMS->SetStats(true);

  hEventSumContents.hSumTB = new TH1F("NumberOfTB", "IST - Number of Time Bins", nBinsTB, 0, TBMax); //15 bins
  hEventSumContents.hSumTB->SetFillColor(kYellow-9);
  hEventSumContents.hSumTB->SetStats(true);
  hEventSumContents.hSumTB->GetXaxis()->SetTitle("No. of Time Bin");

  hEventSumContents.hMaxTimeBin = new TH1F("MaxTimeBin", "IST - Max ADC Time Bin", numTimeBin, 0, numTimeBin); //9 bins
  hEventSumContents.hMaxTimeBin->SetFillColor(kYellow-9);
  hEventSumContents.hMaxTimeBin->SetStats(true);
  hEventSumContents.hMaxTimeBin->GetXaxis()->SetTitle("Time Bin Index");

  hEventSumContents.hSumBad = new TH1F("NumberOfGoodChannelsPerAPV", "IST - Good Channels per APV", totAPV, 0, totAPV); //864 bins
  hEventSumContents.hSumBad->SetNdivisions(-numLadder,"X");
  hEventSumContents.hSumBad->SetFillColor(kYellow-9);
  hEventSumContents.hSumBad->GetXaxis()->SetTitle("APV Index");
  hEventSumContents.hSumBad->SetStats(false);
  hEventSumContents.hSumBad->SetLabelSize(0.03);
  for(int iLad=0; iLad<numLadder; iLad++) {
    sprintf( buffer, "L%d", 1+iLad );
    hEventSumContents.hSumBad->GetXaxis()->SetBinLabel(iLad*ApvPerLadder+ApvPerLadder/2,buffer);
  }

  hEventSumContents.hApvCorpt = new TH1F("VisibleAPV", "IST - Visible APVs Frequency", nBins*2, 0, totAPV+1); //100 bins
  hEventSumContents.hApvCorpt->GetXaxis()->SetTitle("Number of visible APVs");
  hEventSumContents.hApvCorpt->SetFillColor(kYellow-9);
  hEventSumContents.hApvCorpt->SetStats(true);

  hEventSumContents.hEventSize = new TH1F("ISTEventSize", "IST - Event Size", nBins, 0, 6000); //50 bins
  hEventSumContents.hEventSize->GetXaxis()->SetTitle("Unpacked Event size [kB]");
  hEventSumContents.hEventSize->SetFillColor(kYellow-9);
  hEventSumContents.hEventSize->SetStats(true);

  hEventSumContents.hMIPvsSensor = new TH1F("MIPvsSensor", "IST - MPV vs Sensor Id", totSensor, 1, totSensor+1); //144 bins
  hEventSumContents.hMIPvsSensor->GetXaxis()->SetTitle("Sensor Index");
  hEventSumContents.hMIPvsSensor->GetXaxis()->SetTitle("<MIP> [ADC Counts]");
  hEventSumContents.hMIPvsSensor->SetFillColor(kYellow-9);
  hEventSumContents.hMIPvsSensor->SetStats(false);

  ///////////////////
  for ( int index=0; index<mMipHist; index++ ) {
    sprintf( buffer, "MIP_Sensor_%d", index+1 );
    sprintf( buffer2,"IST - MipPerSensor, Ladder %d Sensor %d", index/6+1, index%6+1 );

    hMipContents.mipArray[index] = new TH1F(buffer, buffer2, nBins*2, 0, 4096); //100 bins
    hMipContents.mipArray[index]->GetXaxis()->SetTitle("Pedestal Subtracted ADC [ADC Counts]");
    hMipContents.mipArray[index]->SetFillColor(kYellow-9);
    hMipContents.mipArray[index]->SetStats(true);
  }

  //////////////////
  hSumContents.hHitMap = new TH2F("HitMapOfIST", "IST - Hit map (density)", numRow*numLadder, 1, numRow*numLadder+1, numColumn*numSensor, 1, numColumn*numSensor+1);//1536*72 bins
  hSumContents.hHitMap->GetXaxis()->SetNdivisions(-numLadder, false);
  hSumContents.hHitMap->GetYaxis()->SetNdivisions(-numSensor, false);
  hSumContents.hHitMap->SetStats(false);
  hSumContents.hHitMap->GetXaxis()->SetTitle("Row Index in #phi");
  hSumContents.hHitMap->GetYaxis()->SetTitle("Column Index in Z");
  hSumContents.hHitMap->SetLabelSize(0.02);

  hSumContents.hHitMapVsAPV = new TH2F("HitMapPerAPV", "IST - Hit map in Ladder vs APV", 36, 1, 37, 24, 1, 25);
  hSumContents.hHitMapVsAPV->SetStats(false);
  hSumContents.hHitMapVsAPV->GetXaxis()->SetTitle("APV geometry ID");
  hSumContents.hHitMapVsAPV->GetYaxis()->SetTitle("Ladder geometry ID");

  hSumContents.hMultVsLadder = new TH2F("HitMultVsLadder", "IST - Hit Multiplicity vs Ladder Id", numLadder, 1, numLadder+1, 72, 0, ChPerLadder);//24*72 bins
  hSumContents.hMultVsLadder->GetXaxis()->SetNdivisions(-numLadder, false);
  hSumContents.hMultVsLadder->SetStats(false);
  hSumContents.hMultVsLadder->GetXaxis()->SetTitle("Ladder Index");
  hSumContents.hMultVsLadder->GetYaxis()->SetTitle("Number of Hits");
  hSumContents.hMultVsLadder->SetLabelSize(0.03);
  for(int iLad=0; iLad<numLadder; iLad++) {
    sprintf( buffer, "L%d", 1+iLad );
    hSumContents.hMultVsLadder->GetXaxis()->SetBinLabel(iLad+1,buffer);
  }

  hSumContents.hSumPed = new TH2F("PedestalPerChannel", "IST - Pedestal vs Channel", nBinsCh, 1, totCh+1, nBins, PedMin, PedMax); //864*50 bins
  hSumContents.hSumPed->GetXaxis()->SetNdivisions(-numLadder,false);
  hSumContents.hSumPed->SetStats(false);
  hSumContents.hSumPed->GetXaxis()->SetTitle("Channel Index");
  hSumContents.hSumPed->GetYaxis()->SetTitle("Mean Pedestal [ADC counts]");
  hSumContents.hSumPed->GetYaxis()->SetTitleOffset(1.1);

  hSumContents.hSumSig = new TH2F("PedestalRmsPerChannel", "IST - RMS vs Channel", nBinsCh, 1, totCh+1, nBins, SigMin, SigMax); //864*50 bins
  hSumContents.hSumSig->GetXaxis()->SetNdivisions(-numLadder,false);
  hSumContents.hSumSig->SetStats(false);
  hSumContents.hSumSig->GetXaxis()->SetTitle("Channel Index");
  hSumContents.hSumSig->GetYaxis()->SetTitle("Pedestal RMS [ADC counts]");

  hSumContents.hCommonModeNoise = new TH2F("CommonModeNoisePerAPV", "IST - Common Mode Noise vs APV", nBinsAPV, 1, totAPV+1, nBins, CmnMin, CmnMax);//864*50 bins
  hSumContents.hCommonModeNoise->GetXaxis()->SetNdivisions(-numLadder, false);
  hSumContents.hCommonModeNoise->SetStats(false);
  hSumContents.hCommonModeNoise->GetXaxis()->SetTitle("APV Index");
  hSumContents.hCommonModeNoise->GetYaxis()->SetTitle("Common Mode Noise [ADC counts]");
  hSumContents.hCommonModeNoise->SetLabelSize(0.03);
  for(int iLad=0; iLad<numLadder; iLad++) {
    sprintf( buffer, "L%d", 1+iLad );
    hSumContents.hCommonModeNoise->GetXaxis()->SetBinLabel(iLad*ApvPerLadder+ApvPerLadder/2,buffer);
  }

  //label setting
  for ( int index=0; index<numLadder; index++ ) {         
    char label[100];
    sprintf(label, "L%d", index+1);
    hSumContents.hSumPed->GetXaxis()->SetBinLabel(index*ApvPerLadder+ApvPerLadder/2, label);  
    hSumContents.hSumSig->GetXaxis()->SetBinLabel(index*ApvPerLadder+ApvPerLadder/2, label);
  }

  //JEVP plots setting
  int totPlots = mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+mEventSumHist+mMipHist+mSumHist;
  plots = new JevpPlot*[totPlots];
  
  for ( int i=0; i<mAdcHist; i++ ) {
    hAdcContents.adcArray[i]->SetOption("colz");
    plots[i] = new JevpPlot(hAdcContents.adcArray[i]);
  }

  for ( int i=0; i<mMultHist; i++ ) {
    plots[mAdcHist+i] = new JevpPlot(hMultContents.multArray[i]);
    plots[mAdcHist+i]->logy=true;
  }

  for ( int i=0; i<mHitMapHist; i++ ) {
    hHitMapContents.hitMapArray[i]->SetOption("colz");
    plots[mAdcHist+mMultHist+i] = new JevpPlot(hHitMapContents.hitMapArray[i]);
  }

  for ( int i=0; i<mTbVsAdcHist; i++ ) {
    hTbVsAdcContents.tbVsAdcArray[i]->SetOption("colz");
    plots[mAdcHist+mMultHist+mHitMapHist+i] = new JevpPlot(hTbVsAdcContents.tbVsAdcArray[i]);
    plots[mAdcHist+mMultHist+mHitMapHist+i]->optlogz=true;
  }
  
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist]   = new JevpPlot(hEventSumContents.hMeanPed);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+1] = new JevpPlot(hEventSumContents.hMeanRMS);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+2] = new JevpPlot(hEventSumContents.hSumTB);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+3] = new JevpPlot(hEventSumContents.hMaxTimeBin);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+4] = new JevpPlot(hEventSumContents.hSumBad);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+5] = new JevpPlot(hEventSumContents.hApvCorpt);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+6] = new JevpPlot(hEventSumContents.hEventSize);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+7] = new JevpPlot(hEventSumContents.hMIPvsSensor);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+5]->logy=true;
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+5]->setOptStat(10);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+6]->logy=true;
  JLine* line = new JLine(0, 128, totAPV, 128);
  line->SetLineColor(kRed);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+4]->addElement(line);

  for ( int i=0; i<mMipHist; i++ ) {
    plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+mEventSumHist+i] = new JevpPlot(hMipContents.mipArray[i]);
  }

  for ( int i=0; i<mSumHist; i++ ) {
    hSumContents.sumArray[i]->SetOption("colz");
    plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+mEventSumHist+mMipHist+i] = new JevpPlot(hSumContents.sumArray[i]);
    hSumContents.sumArray[i]->SetStats(false);
  }
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+mEventSumHist+mMipHist+2]->logy=true;

  // Add Plots to plot set...
  for ( int i=0; i<totPlots ;i++ ) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }

  //red would be [2]
  errorMsg = new JLatex(.25, .12, "#color[4]{No Error Message}");
  errorMsg->SetTextSize(0.035);
  errorMsg->SetTextAlign(13);
  errorMsg->SetTextAngle(45);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist]->addElement(errorMsg);

  //temp histograms for all APV chips' dynamical CM noise distribution
  for(int i=0; i<totAPV; i++) {
    sprintf( buffer, "APV%d", i );
    hCmnTemp.hCmnPerChip[i] = new TH1F(buffer, "Common mode noise per APV chip", 128, 0, 4096);
  }
}

// ************IST START RUN*****************
// ------------------------------------------
void istBuilder::startrun(daqReader *rdr) {
  LOG ( NOTE, "istBuilder starting run #%d", rdr->run );
  resetAllPlots();
  
  for ( int i=0; i<totCh; i++ ) {
    meanVals[i]        = 0;
    aVals[i]           = 0;
    //      rmsVals[i] = 0;
    numVals[i]         = 0;
    numOverOneSig[i]   = 0;
    oldStdDevs[i]      = 0;
    isChannelBad[i]    = false;
    runningAvg[i]      = 0;
    runningStdDevSq[i] = 0;
  }

  errorMsg->SetText("No Error Message");    
  sumHistogramsFilled  = 0;  
  t_2min               = time(NULL);
  t_10min              = time(NULL);
  t_120min             = time(NULL);
}

#define safelog(x) ((x > 0) ? log10(x) : 0)


// ************IST EVENT******************
// ---------------------------------------
void istBuilder::event(daqReader *rdr) {
  // arrays to calculate dynamical common mode noise contribution to this chip in current event
  float sumAdcPerEvent[totAPV];
  int counterAdcPerEvent[totAPV];

  int HitCount[numLadder]; // for each ladder per event

  for ( int i=0; i<totCh; i++ ) 	{    maxAdc[i]          = 0;   }
  for ( int i=0; i<totCh; i++ )         {    maxTimeBin[i]      = 0;   }
  for ( int i=0; i<numLadder; i++ )     {    HitCount[i]        = 0;   }
  for ( int i=0; i<totAPV; i++ )        {    cmNoise[i]         = 0;   }

  Int_t numTb = numTimeBin;        //default: 9 timebins
  memset( chCntDaq, 0, sizeof(chCntDaq) );                     //Jan's request

  if ( !(evtCt %1000) )     LOG(DBG, "Looking at evt %d",evtCt);
  daq_dta *dd = rdr->det("ist")->get("adc");
    
  if ( dd && dd->meta ) {
    apv_meta_t *meta = (apv_meta_t *) dd->meta;

    for ( int r=1; r<=numRDO; r++ ) {                  //1--6 ARCs (ARM Readout Controllers)
      if ( meta->arc[r].present == 0 ) continue ;
      for ( int arm=0; arm<numARM; arm++ ) {           //0--5 ARMs (APV Readout Modules) per ARC
	if ( meta->arc[r].arm[arm].present == 0 ) continue ;
	for ( int apv=0; apv<numAPV; apv++ ) {        //0--23 APV chips per ARM
	  if ( meta->arc[r].arm[arm].apv[apv].present == 0 ) continue ;
	  int Tb = meta->arc[r].arm[arm].apv[apv].ntim;

	  if( numTb != 0 && Tb != 0 && numTb != Tb ) {
		printf("Different number of timebins in different APV!!! Taking real one!!!\n");
		numTb = Tb; //update by Yaping: 03/25/2014
	  }
	  //update by Yaping: 3/25/2014
	  //if( numTb < Tb )	    numTb = Tb;
	  hEventSumContents.hSumTB->Fill(numTb);
	}
      }
    }   
  }

   // check if we have to look for the zs data
  size_t evtSize = 0;
  daq_dta *ddZS = rdr->det("ist")->get("zs");
  if ( ddZS ) {
    while( ddZS && ddZS->iterate() ) {
      evtSize += ddZS->ncontent * sizeof(fgt_adc_t);
    }
  }

  // don't use zs data to fill histos...  
  while(dd && dd->iterate()) { 
    fgt_adc_t *f = (fgt_adc_t *) dd->Void ;
    evtSize += dd->ncontent * sizeof(fgt_adc_t);
        
    if ( dd->pad < 0 || dd->pad > 23 )        continue;      //valid APV numbering: 0, 1, ..., 23
    if ( dd->sec < 0 || dd->sec > 5 )         continue;      //valid ARM numbering: 0, 1, ..., 5
    if ( dd->rdo < 1 || dd->rdo > 6 )         continue;      //valid ARC numbering: 1, 2, ..., 6

    Int_t currentAPV = -1;
    for (int apvIdx=0; apvIdx<totAPV; apvIdx++ ){
    	sumAdcPerEvent[apvIdx]     = 0.;
    	counterAdcPerEvent[apvIdx] = 0 ;
    }

    u_int maxI = dd->ncontent;    // 128ch*7tb = 896 measured points per APV
    //loop current APV chip
    for ( u_int i=0; i<maxI; i++ ) {
      //not zs data
      if ( f[i].ch  < 0 || f[i].ch  > 127 )       continue;      //valid Channel numbering: 0, 1, ..., 127 
      if ( f[i].tb  < 0 || f[i].tb  > numTb )     continue;      //valid Time bin numbering: 0, 1, ..., numTb-1 (default 9 time bins, or 4, 5)
      if ( f[i].adc < 0 || f[i].adc > 4095 )      continue;      //valid ADC counts from 0 to 4095
 
      //count the found channel (only taking time bin 0 to avoid double counting)
      if ( f[i].tb == 0 ) {	chCntDaq[dd->rdo-1][dd->sec][dd->pad]++;  }
      
      Int_t channelId = (dd->rdo-1)*numARM*numAPV*ChPerApv + dd->sec*numAPV*ChPerApv + dd->pad*ChPerApv + f[i].ch;
      if(channelId < 0 || channelId >= totCh)	continue;
      Int_t geoId    = istMapping[channelId];     //numbering from 1 to 110592
      Int_t ladder   = 1 + (geoId-1)/ChPerLadder; //numbering from 1 to 24
      Int_t apvId    = (geoId-1) / ChPerApv;      //numbering from 0 to 863
      Int_t channel  = (geoId-1)%ChPerLadder;     //use to fill "per Ladder" histos
      currentAPV     = apvId;	

      if ( isChannelBad[geoId-1] )     continue;  // No bad channels have been added so far
	
      //fill ADC value vs channel index
      hAdcContents.adcArray[ladder-1]->Fill(channel, f[i].adc);

      //calculate mean pedestal and RMS
      aVals[geoId-1] += f[i].adc;
      numVals[geoId-1]++;
      runningAvg[geoId-1] += (f[i].adc-runningAvg[geoId-1]) / numVals[geoId-1];
      runningStdDevSq[geoId-1] += ((float)numVals[geoId-1]-1)/(numVals[geoId-1]) * (f[i].adc-runningAvg[geoId-1]) * (f[i].adc-runningAvg[geoId-1]);
      oldStdDevs[geoId-1]       = sqrt(runningStdDevSq[geoId-1] / numVals[geoId-1]);

      //channel status decision
      Bool_t isBad = false;
      if ( runningAvg[geoId-1] < minPedVal || runningAvg[geoId-1] > maxPedVal )    isBad = true;
      if ( oldStdDevs[geoId-1] < minRMSVal || oldStdDevs[geoId-1] > maxRMSVal )    isBad = true;

      //fill ADC vs time bin index
      if ( !isBad )     hTbVsAdcContents.tbVsAdcArray[ladder-1]->Fill(f[i].tb, f[i].adc);
	
      //count channel whose pedestal subtracted ADC yield one RMS
      if ( (f[i].adc-runningAvg[geoId-1])>oldStdDevs[geoId-1] && oldStdDevs[geoId-1]>0 ) 
	  numOverOneSig[geoId-1]++; 

      //max ADC and its time bin index decision
      if ( (f[i].adc - runningAvg[geoId-1] )> maxAdc[geoId-1] ) {
          maxAdc[geoId-1]     = f[i].adc - runningAvg[geoId-1];
          maxTimeBin[geoId-1] = f[i].tb;
      }

      //counts for dynamical common mode noise calculation
      if ( f[i].tb==(numTb-1) ) {       //only take last time bin
	  //exclude signal-related channels for common mode noise calculation
	  if ( oldStdDevs[geoId-1]>0 && abs(maxAdc[geoId-1] < cmnCut*oldStdDevs[geoId-1]) ) {
	      sumAdcPerEvent[apvId] += (maxAdc[geoId-1]+runningAvg[geoId-1]);
              counterAdcPerEvent[apvId]++;
	   }
        }
    } //end current APV chip loops

    //calculate dynamical common mode noise for current event
    if ( counterAdcPerEvent[currentAPV] > 0 && currentAPV > -1) {
	cmNoise[currentAPV] = sumAdcPerEvent[currentAPV] / counterAdcPerEvent[currentAPV];
        hCmnTemp.hCmnPerChip[currentAPV]->Fill(cmNoise[currentAPV]);
    }
  }//end all RDO, ARM, APV chips loops

  //do for zs and non-zs data
  //fill with num kB
  hEventSumContents.hEventSize->Fill(evtSize/1024);

  //makes only sense for non zero suppressed data....
  if ( dd ) {
    int goodAPVs = 0;
    for ( int iRdo=0; iRdo<numRDO; iRdo++ ) {
      for ( int iArm=0; iArm<numARM; iArm++ ) {
	for ( int iApv=0; iApv<numAPV; iApv++ ) {
	  if ( chCntDaq[iRdo][iArm][iApv] > goodChCut ) 		    
		goodAPVs++;
	}
      }
    }
    hEventSumContents.hApvCorpt->Fill(goodAPVs);
    evtCt++;
  }
  
  //count hit per Ladder per Event and fill hit map and MIP
  for(int geoIdx=1; geoIdx<=totCh; geoIdx++) {
      int ladderIdx = 1 + (geoIdx-1)/ChPerLadder;               //ladder index 1 to 24
      int sensorIdx = 1 + ((geoIdx-1)%ChPerLadder)/ChPerSensor; //sensor index 1 to 6 per ladder
      int padIdx    = ((geoIdx-1)%ChPerLadder)%ChPerSensor;
      int columnIdx = 1 + padIdx/numRow;  //column index 1 to 12
      int rowIdx    = 1 + padIdx%numRow;  //row index 1 to 64
      int sensorIndex = (ladderIdx-1)*numSensor + sensorIdx;  // sensor index 1 to 144 in all 
      int apvGeoIdx = (sensorIdx-1)*6+(columnIdx-1)/2 + 1;    // apv index 1 to 36 

      //float pedestal   = runningAvg[geoIdx-1];
      float rms        = oldStdDevs[geoIdx-1];
      int adc_max      = maxAdc[geoIdx-1];
      int tb_max       = maxTimeBin[geoIdx-1];

      if( (adc_max>hitCut*rms) && (rms>rmsMin) ) {
	  HitCount[ladderIdx-1]++;
	  hHitMapContents.hitMapArray[ladderIdx-1]->Fill(rowIdx, (sensorIdx-1)*numColumn+columnIdx);
	  hSumContents.hHitMap->Fill((ladderIdx-1)*numRow+rowIdx, (sensorIdx-1)*numColumn+columnIdx);
	  hSumContents.hHitMapVsAPV->Fill(apvGeoIdx, ladderIdx);
          hMipContents.mipArray[sensorIndex-1]->Fill(adc_max);
	  hEventSumContents.hMaxTimeBin->Fill(tb_max);
      }
  }

  //fill hit multiplicity per ladder per event
  for ( int i=0; i<numLadder; i++) { 
    hMultContents.multArray[i]->Fill(HitCount[i]);
    hSumContents.hMultVsLadder->Fill(i+1, HitCount[i]);
  }

  //getting MPV value and CM noise every 50 evts for each sensor
  if(!(evtCt%50)) {
    for(int j=0; j<totSensor; j++) {
	hMipContents.mipArray[j]->GetXaxis()->SetRangeUser(200., 1000.);
	hEventSumContents.hMIPvsSensor->SetBinContent(j+1, hMipContents.mipArray[j]->GetMean());
    }
    for( int k=0; k<totAPV; k++ ) {
	hSumContents.hCommonModeNoise->Fill(k+1, hCmnTemp.hCmnPerChip[k]->GetRMS());
    }
  }

  // Reset rolling histos if necessary..
  int tm = time(NULL);
  if ( (tm > t_10min + 10) || (!(evtCt%50)) ) {
    t_10min = tm;
    fillSumHistos();
  }
  // End Fill Histograms...
}

//right now all time bins are summed, so what is shown in mean and std dev is the mean over all tb for a channel

// ********IST FILL SUM HISTOS****************
// -------------------------------------------
void istBuilder::fillSumHistos() {
  char buffer[200];
  hEventSumContents.hMeanPed->Reset();  // mean ped
  hEventSumContents.hMeanRMS->Reset();  // sigma
  hEventSumContents.hSumBad->Reset();   // #goodapv

  int numGood[totAPV];
  for(int i=0; i<totAPV; i++)  numGood[i] = 0;

  int numBadAll =0;
  sumHistogramsFilled++;

  for ( int geoIdx=1; geoIdx<=totCh; geoIdx++ ) {
    int   apvIdx   = (geoIdx-1)/ChPerApv;
    bool  isBad    = false;
    float pedestal = runningAvg[geoIdx-1];
    float rmsPed   = oldStdDevs[geoIdx-1];

    if ( rmsPed > 0 ) {
      hEventSumContents.hMeanRMS->Fill(rmsPed);
      hSumContents.hSumSig->Fill(geoIdx, rmsPed);
    }

    if ( pedestal > 0 ) {
      hEventSumContents.hMeanPed->Fill(pedestal);
      hSumContents.hSumPed->Fill(geoIdx, pedestal);
    }

    if ( numVals[geoIdx-1] > 0 ) {
      if ( pedestal < minPedVal || pedestal > maxPedVal || rmsPed < minRMSVal || rmsPed > maxRMSVal ) {
	isBad = true;
        numBadAll++;
      }
      else
        numGood[apvIdx]++;
    }
  }
  
  for(int idx=0; idx<totAPV; idx++)
      hEventSumContents.hSumBad->SetBinContent(idx+1, numGood[idx]);

  sprintf(buffer, "#color[4]{You seem to have %d bad channels that are not masked}", numBadAll);  
  errorMsg->SetText(buffer);    
}

// ***********IST STOP RUN*************************
// ------------------------------------------------
void istBuilder::stoprun(daqReader *rdr) {
  for ( int i=0; i<totCh; i++ )    {
    numVals[i]         = 0;
    numOverOneSig[i]   = 0;
    runningAvg[i]      = 0;
    runningStdDevSq[i] = 0;
    oldStdDevs[i]      = 0;
    meanVals[i]        = 0;
    aVals[i]           = 0;
    //rmsVals[i]         = 0;
    isChannelBad[i]    =false;
  }
  for ( int i=0; i<totCh; i++ )         {    maxAdc[i]          = 0;   }
  for ( int i=0; i<totCh; i++ )         {    maxTimeBin[i]      = 0;   }
  for ( int i=0; i<totAPV; i++ )        {    cmNoise[i]         = 0;   }
}

void istBuilder::main(int argc, char *argv[])
{
  istBuilder me;
  //  cout <<"starting main" << endl;
  me.Main(argc, argv);
  //  cout <<"ending main" << endl;
}
