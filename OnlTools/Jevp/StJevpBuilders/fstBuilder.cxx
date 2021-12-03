/* This builder is developed for FST online monitoring
 * Author: Xu Sun
 */
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include "JevpBuilder.h"
#include "DAQ_READER/daqReader.h"
#include "DAQ_READER/daq_dta.h"
#include <DAQ_READER/daq_dta.h>
#include "DAQ_READER/daq_det.h"
#include <DAQ_FGT/daq_fgt.h> 

#include "Jevp/StJevpPlot/RunStatus.h"
#include "StEvent/StTriggerData.h"
#include <TF1.h>
#include <TFile.h>
#include <TPaveStats.h>
#include <TString.h>
#include <TMath.h>
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "fstBuilder.h"
#include <RTS/include/rtsLog.h>

ClassImp(fstBuilder);


const float fstBuilder::minMipMpv_ZS      = 450;
const float fstBuilder::minMipMpv_nonZS   = 400;
const float fstBuilder::maxMipMpv         = 800;
const float fstBuilder::minMipSigma_ZS    = 80;
const float fstBuilder::minMipSigma_nonZS = 60;
const float fstBuilder::maxMipSigma       = 200;
const float fstBuilder::maxTbFracOK       = 0.9;
const float fstBuilder::landauFit_dn      = 400.0;
const float fstBuilder::landauFit_up      = 2000.0;
const float fstBuilder::cmnCut            = 3.0;
const float fstBuilder::hitCut            = 3.0;
const float fstBuilder::noiseChipCut      = 10.0;
const int   fstBuilder::hitOccupancyCut  = 25;

// constant used for FST Geometry Hit Map
// all values are defined by inner direction
const int fstBuilder::zFilp[totDisk]         = {1,-1,1};
const int fstBuilder::zDirct[ModPerDisk]     = {1,-1,1,-1,1,-1,1,-1,1,-1,1,-1};
const float fstBuilder::phiStart[ModPerDisk] = {2.0, 2.0, 0.0, 12.0, 10.0, 10.0, 8.0, 8.0, 6.0, 6.0, 4.0, 4.0}; // * pi/6
const float fstBuilder::phiStop[ModPerDisk]  = {3.0, 1.0, 1.0, 11.0, 11.0,  9.0, 9.0, 7.0, 7.0, 5.0, 5.0, 3.0}; // * pi/6
const float fstBuilder::phiDelta             = TMath::TwoPi()/(PhiSegPerMod*ModPerDisk); // 2pi/1536 in rad
const float fstBuilder::rStart[RstripPerMod] = {50.00, 78.75, 107.50, 136.25, 165.00, 193.75, 222.50, 251.25}; // in mm
const float fstBuilder::rStop[RstripPerMod]  = {78.75, 107.5, 136.25, 165.00, 193.75, 222.50, 251.25, 280.00}; // in mm
const float fstBuilder::rDelta               = 28.75; // in mm

fstBuilder::fstBuilder(JevpServer *parent):JevpBuilder(parent),evtCt(0) 
{
  plotsetname = (char *)"fst";
  // start with histograms undefined...
  memset( &hAdcContents,      	0, sizeof(hAdcContents) );
  memset( &hMultContents,     	0, sizeof(hMultContents) );
  memset( &hHitMapContents,   	0, sizeof(hHitMapContents) );
  memset( &hTbVsAdcContents,  	0, sizeof(hTbVsAdcContents) );
  memset( &hEventSumContents, 	0, sizeof(hEventSumContents) );
  memset( &hMipContents,      	0, sizeof(hMipContents) );
  memset( &hMaxTimeBinContents, 0, sizeof(hMaxTimeBinContents) );
  memset( &hSumContents,      	0, sizeof(hSumContents) );
  memset( &hCmnTemp,	      	0, sizeof(hCmnTemp) );
}

fstBuilder::~fstBuilder() 
{
  // Delete any existing histograms...  
  int nAdcHist        = sizeof(hAdcContents) / sizeof(TH2 *);
  int nMultHist       = sizeof(hMultContents) / sizeof(TH1 *);
  int nHitMapHist     = sizeof(hHitMapContents) / sizeof(TH2 *);
  int nTbVsAdcHist    = sizeof(hTbVsAdcContents) / sizeof(TH2 *);
  int nEventSumHist   = sizeof(hEventSumContents) / sizeof(TH1 *);
  int nMipHist        = sizeof(hMipContents) / sizeof(TH1 *);
  int nMaxTimeBinHist = sizeof(hMaxTimeBinContents) / sizeof(TH1 *);
  int nSumHist        = sizeof(hSumContents) / sizeof(TH2 *);

  for ( int i=0; i<nAdcHist; i++ )      {    if(hAdcContents.adcArray[i])          delete hAdcContents.adcArray[i];    		}
  for ( int i=0; i<nMultHist; i++ )     {    if(hMultContents.multArray[i])        delete hMultContents.multArray[i];    	}
  for ( int i=0; i<nHitMapHist; i++ )   {    if(hHitMapContents.hitMapArray[i])    delete hHitMapContents.hitMapArray[i];    	}
  for ( int i=0; i<nTbVsAdcHist; i++ )  {    if(hTbVsAdcContents.tbVsAdcArray[i])  delete hTbVsAdcContents.tbVsAdcArray[i];    	}
  for ( int i=0; i<nEventSumHist; i++ ) {    if(hEventSumContents.eventSumArray[i])delete hEventSumContents.eventSumArray[i];   }
  for ( int i=0; i<nMipHist; i++ )      {    if(hMipContents.mipArray[i])          delete hMipContents.mipArray[i];    		}
  for ( int i=0; i<nMaxTimeBinHist; i++){    if(hMaxTimeBinContents.maxTimeBinArray[i])   delete hMaxTimeBinContents.maxTimeBinArray[i];             }
  for ( int i=0; i<nSumHist; i++ )      {    if(hSumContents.sumArray[i])          delete hSumContents.sumArray[i];    		}
  for ( int i=0; i<totAPV; i++ )        
  {    
    for(int iRstrip = 0; iRstrip< 4; ++iRstrip) 
    {
      if(hCmnTemp.hCmnPerChip[i][iRstrip]) delete hCmnTemp.hCmnPerChip[i][iRstrip];		
    }
  }
}

// **********FST INITIALIZE*****************
// -----------------------------------------
void fstBuilder::initialize(int argc, char *argv[]) 
{
  // Initialization of histograms.
  // could run a loop...
  // Add root histograms to Plots

  errorMsg=0;

  for( int i=0; i<totCh; i++ ) 
  {
    meanVals[i]        = 0;
    aVals[i]           = 0;
    numVals[i]         = 0;
    numOverOneSig[i]   = 0;
    oldStdDevs[i]      = 0;
    isChannelBad[i]    = false;
    runningAvg[i]      = 0;
    runningStdDevSq[i] = 0;
    fstRanNoise[i]      = 0;
  }
  for ( int i=0; i<totCh; i++ )
  {
    maxAdc[i]        = 0;
    maxAdc_zs[i]     = 0;
    maxTimeBin[i]    = -1;
    maxTimeBin_zs[i] = -1;
  }
  for ( int i=0; i<totAPV; i++ )
  {
    for(int iRstrip = 0; iRstrip < 4; ++iRstrip) { cmNoise[i][iRstrip] = 0; }
    isNoisyApv[i] = false;
  }

  // //////////////////////////////////add bad channels here///////////////////////
  // ///////////////////isChannelBad[numAssembly*ChPerSec+channel]=true;
  //expected chips per section
  // for(int i=0; i<72; i++) {nExpectedChip_Sec[i] = 12;}
  // nExpectedChip_Sec[67] = 11;

  //initializing FST mapping
  for(int rdoIdx=1; rdoIdx<=totRdo; rdoIdx++) 
  {
    for(int armIdx=0; armIdx<ArmPerRdo; armIdx++) 
    {
      for(int apvIdx=0; apvIdx<ApvPerArm; apvIdx++) 
      {
	int portIdx   = apvIdx/ApvPerPort;                                  // 0-1
	int lclApvIdx = apvIdx-portIdx*ApvPerPort;                          // 0-7
	int lclSensorIdx = lclApvIdx/ApvPerSensor;                          // 0-3

	int diskIdx   = (rdoIdx-1)/2+1;                                     // 1-3
	int moduleIdx = (rdoIdx-1)%2*ModPerRdo+armIdx*PortPerArm+portIdx+1; // 1-12
	for(int chanIdx=0; chanIdx<ChPerApv; chanIdx++) 
	{
	  int glbElecChanId = (rdoIdx-1)*ChPerRdo + armIdx*ChPerArm + apvIdx*ChPerApv + chanIdx; // 0-36863
	  int lclRstripIdx = -1; // 0-7
	  int lclPhiSegIdx = -1; // 0-127

	  // Inner Section: APV 0-1 for sensor 0 | APV 2-3 for sensor 1
	  // Outer Section: APV 4-5 for sensor 2 | APV 6-7 for sensor 3
	  if(lclApvIdx == 0 || lclApvIdx == 1 || lclApvIdx == 4 || lclApvIdx == 6) lclRstripIdx = chanIdx%4;
	  if(lclApvIdx == 2 || lclApvIdx == 3 || lclApvIdx == 5 || lclApvIdx == 7) lclRstripIdx = 3-chanIdx%4;
	  if(lclSensorIdx == 2 || lclSensorIdx == 3) lclRstripIdx = lclRstripIdx+4; // outer

	  if(lclSensorIdx == 0 || lclSensorIdx == 1) lclPhiSegIdx = lclApvIdx*32+chanIdx/4; // inner
	  if(lclSensorIdx == 2 || lclSensorIdx == 3) lclPhiSegIdx = (lclApvIdx-ApvPerSec)*32+chanIdx/4; // outer

	  int glbGeomChanId = (diskIdx-1)*ChPerDisk + (moduleIdx-1)*ChPerMod + lclRstripIdx*PhiSegPerMod + lclPhiSegIdx; // 0-36863

	  fstGeomMapping[glbElecChanId] = glbGeomChanId;
	  fstElecMapping[glbGeomChanId] = glbElecChanId;
	}
      }
    }
  }

  mAdcHist        = sizeof(hAdcContents) / sizeof(TH2 *);
  mMultHist       = sizeof(hMultContents) / sizeof(TH1 *);
  mHitMapHist     = sizeof(hHitMapContents) / sizeof(TH2 *);
  mTbVsAdcHist    = sizeof(hTbVsAdcContents) / sizeof(TH2 *);
  mEventSumHist   = sizeof(hEventSumContents) / sizeof(TH1 *);
  mMipHist        = sizeof(hMipContents) / sizeof(TH1 *);
  mMaxTimeBinHist = sizeof(hMaxTimeBinContents) / sizeof(TH1 *);
  mSumHist        = sizeof(hSumContents) / sizeof(TH2 *);

  char buffer[100];
  char buffer1[100];
  char buffer2[100];

  int nBins    = 50;
  int nBinsAPV = 288;
  int nBinsTB  = 32;
  int ADCMax   = 4100;
  int ADCMin   = -100;
  int TBMax    = 32;
  int PedMin   = -100;
  int PedMax   = 4100;
  int SigMin   = -10;
  int SigMax   = 190;
  int CmnMin   = -10;
  int CmnMax   = 90;

  //////////////////////
  for(int index = 0; index < mAdcHist; index++) 
  { // per module
    int rdoIdx    = index/ModPerRdo + 1;         //1-6
    int armIdx    = (index%ModPerRdo)/ModPerArm; //0-2
    int portIdx   = index%PortPerArm;	         //0-1

    int diskIdx   = (rdoIdx-1)/2+1;                                           // 1-3
    int moduleIdx = (rdoIdx-1)%2*ModPerRdo + armIdx*PortPerArm + portIdx + 1; // 1-12

    sprintf( buffer, "ADC_Vs_Channel_Disk%d_Module%d", diskIdx, moduleIdx);
    sprintf( buffer2, "FST - ADC vs Channel Id: Disk%d Module%d", diskIdx, moduleIdx);

    hAdcContents.adcArray[index] = new TH2S(buffer, buffer2, PhiSegPerMod, 0, ChPerMod, 100, ADCMin, ADCMax); // 128 (2*64) columns (8 strps per column) * 100 bins
    hAdcContents.adcArray[index]->GetXaxis()->SetTitle("Channel Electronics Index");
    hAdcContents.adcArray[index]->GetYaxis()->SetTitle("ADC value");
    hAdcContents.adcArray[index]->GetXaxis()->SetNdivisions(-3, false);
    hAdcContents.adcArray[index]->SetStats(false);
    hAdcContents.adcArray[index]->GetYaxis()->SetTitleOffset(1.1);
    hAdcContents.adcArray[index]->SetLabelSize(0.03);
    for(int iSec=0; iSec<SecPerMod; iSec++) 
    {
      sprintf( buffer1, "RDO%d_ARM%d_PORT%d_SEC%d", rdoIdx, armIdx, portIdx, iSec);
      hAdcContents.adcArray[index]->GetXaxis()->SetBinLabel((2*iSec+1)*PhiSegPerMod/4, buffer1);
      hAdcContents.adcArray[index]->GetXaxis()->LabelsOption("h");
    }
  }

  /////////////////////
  for(int index = 0; index < mMultHist; index++) 
  { // per module
    int rdoIdx    = index/ModPerRdo + 1;         //1-6
    int armIdx    = (index%ModPerRdo)/ModPerArm; //0-2
    int portIdx   = index%PortPerArm;	         //0-1

    int diskIdx   = (rdoIdx-1)/2+1;                                           // 1-3
    int moduleIdx = (rdoIdx-1)%2*ModPerRdo + armIdx*PortPerArm + portIdx + 1; // 1-12

    sprintf( buffer, "HitMult_Disk%d_Module%d", diskIdx, moduleIdx);
    sprintf( buffer2, "FST - Hit Multiplicity: Disk%d Module%d ", diskIdx, moduleIdx);

    hMultContents.multArray[index] = new TH1S(buffer, buffer2, 100, 0, 500); // 100 bins
    hMultContents.multArray[index]->GetXaxis()->SetTitle("Number of Hits");
    hMultContents.multArray[index]->GetYaxis()->SetTitle("Counts");
    hMultContents.multArray[index]->SetStats(true);
    hMultContents.multArray[index]->GetYaxis()->SetTitleOffset(1.1);
  }

  /////////////////////
  for(int index = 0; index < mHitMapHist; index++) 
  { // per module
    int rdoIdx    = index/ModPerRdo + 1;         //1-6
    int armIdx    = (index%ModPerRdo)/ModPerArm; //0-2
    int portIdx   = index%PortPerArm;	         //0-1

    int diskIdx   = (rdoIdx-1)/2+1;                                           // 1-3
    int moduleIdx = (rdoIdx-1)%2*ModPerRdo + armIdx*PortPerArm + portIdx + 1; // 1-12

    sprintf( buffer, "HitMap_Disk%d_Module%d", diskIdx, moduleIdx);
    sprintf( buffer2, "FST - Hit Map (density): Disk%d Module%d", diskIdx, moduleIdx);

    hHitMapContents.hitMapArray[index] = new TH2S(buffer, buffer2, PhiSegPerMod, -0.5, PhiSegPerMod-0.5, RstripPerMod, -0.5, RstripPerMod-0.5); //128 phi(row) * 8 r(col)
    hHitMapContents.hitMapArray[index]->GetXaxis()->SetTitle("#phi Index");
    hHitMapContents.hitMapArray[index]->GetYaxis()->SetTitle("r Index");
    hHitMapContents.hitMapArray[index]->GetYaxis()->SetNdivisions(RstripPerMod,false);
    hHitMapContents.hitMapArray[index]->SetStats(false);
  }

  ////////////////////
  for(int index = 0; index < mTbVsAdcHist; index++) 
  { // per section
    sprintf( buffer, "ADC_Vs_Tb_Section_%d", index);
    int rdoIdx    = index/SecPerRdo + 1;          //1-6
    int armIdx    = (index%SecPerRdo)/SecPerArm;  //0-2
    int portIdx   = (index/SecPerMod)%PortPerArm; //0-1
    int secIdx    = index%SecPerMod;              //0-1

    int diskIdx   = (rdoIdx-1)/2+1;                                           // 1-3
    int moduleIdx = (rdoIdx-1)%2*ModPerRdo + armIdx*PortPerArm + portIdx + 1; // 1-12

    sprintf( buffer2,"FST - ADC vs Timebin (non-ZS), Disk%d Module%d: RDO%d_ARM%d_PORT%d_SEC%d", diskIdx, moduleIdx, rdoIdx, armIdx, portIdx, secIdx);

    hTbVsAdcContents.tbVsAdcArray[index] = new TH2S(buffer, buffer2, numTimeBin, 0, numTimeBin, 100, ADCMin, ADCMax); //9*50 bins
    hTbVsAdcContents.tbVsAdcArray[index]->GetXaxis()->SetTitle("Time Bin Index");
    hTbVsAdcContents.tbVsAdcArray[index]->GetYaxis()->SetTitle("Pedestal-subtracted ADC value");
    hTbVsAdcContents.tbVsAdcArray[index]->GetXaxis()->SetNdivisions(numTimeBin,false);
    hTbVsAdcContents.tbVsAdcArray[index]->SetStats(false);
    hTbVsAdcContents.tbVsAdcArray[index]->GetYaxis()->SetTitleOffset(1.1);
    for (int iTB=0; iTB<numTimeBin; iTB++) 
    {
      sprintf(buffer, "TB%d", iTB);
      hTbVsAdcContents.tbVsAdcArray[index]->GetXaxis()->SetBinLabel(iTB+1,buffer);
    }
  }

  ///////////////////
  hEventSumContents.hMeanPed = new TH1S("MeanPeds", "FST - <Mean Pedestal>", nBins*2, PedMin, PedMax); //100 bins
  hEventSumContents.hMeanPed->GetXaxis()->SetTitle("Mean Pedestal [ADC counts]");
  hEventSumContents.hMeanPed->SetFillColor(kYellow-9);
  hEventSumContents.hMeanPed->SetStats(true);

  hEventSumContents.hMeanRMS = new TH1S("MeanRMS", "FST - <RMS Pedestal>", nBins*2, SigMin, SigMax); //100 bins
  hEventSumContents.hMeanRMS->GetXaxis()->SetTitle("RMS pedestal [ADC counts]");
  hEventSumContents.hMeanRMS->SetFillColor(kYellow-9);
  hEventSumContents.hMeanRMS->SetStats(true);

  hEventSumContents.hMeanRan = new TH1S("MeanRandomRMS", "FST - <Random RMS>", nBins*2, SigMin, SigMax); //100 bins
  hEventSumContents.hMeanRan->GetXaxis()->SetTitle("Random RMS [ADC counts]");
  hEventSumContents.hMeanRan->SetFillColor(kYellow-9);
  hEventSumContents.hMeanRan->SetStats(true);

  hEventSumContents.hSumTB = new TH1I("NumberOfTB", "FST - Number of Time Bins", nBinsTB, 0, TBMax); //15 bins
  hEventSumContents.hSumTB->SetFillColor(kYellow-9);
  hEventSumContents.hSumTB->SetStats(true);
  hEventSumContents.hSumTB->GetXaxis()->SetTitle("No. of Time Bin");

  hEventSumContents.hMaxTimeBin = new TH1I("MaxTimeBin_nonZS", "FST - Max ADC Time Bin (non-ZS)", numTimeBin, 0, numTimeBin); //9 bins
  hEventSumContents.hMaxTimeBin->SetFillColor(kYellow-9);
  hEventSumContents.hMaxTimeBin->SetStats(true);
  hEventSumContents.hMaxTimeBin->GetXaxis()->SetTitle("Time Bin Index");

  hEventSumContents.hMaxTimeBin_ZS = new TH1I("MaxTimeBin_ZS", "FST - Max ADC Time Bin (ZS)", numTimeBin, 0, numTimeBin); //9 bins
  hEventSumContents.hMaxTimeBin_ZS->SetFillColor(kYellow-9);
  hEventSumContents.hMaxTimeBin_ZS->SetStats(true);
  hEventSumContents.hMaxTimeBin_ZS->GetXaxis()->SetTitle("Time Bin Index");

  hEventSumContents.hSumBad = new TH1S("NumberOfGoodChannelsPerAPV", "FST - Good Channels per APV", totAPV, 0, totAPV); //288 bins
  hEventSumContents.hSumBad->SetNdivisions(-ModPerDisk*totDisk,"X");
  hEventSumContents.hSumBad->SetFillColor(kYellow-9);
  hEventSumContents.hSumBad->GetXaxis()->SetTitle("APV Geometry ID");
  hEventSumContents.hSumBad->SetStats(false);
  hEventSumContents.hSumBad->SetLabelSize(0.03);
  for(int iDisk = 0; iDisk < totDisk; iDisk++)
  {
    for(int iModule = 0; iModule < ModPerDisk; iModule++) 
    {
      sprintf( buffer, "D%dM%d",iDisk+1,iModule+1);
      hEventSumContents.hSumBad->GetXaxis()->SetBinLabel(iDisk*ModPerDisk*ApvPerMod+iModule*ApvPerMod+ApvPerMod/2,buffer);
    }
  }

  hEventSumContents.hApvCorpt = new TH1S("VisibleAPV", "FST - Visible APVs Frequency", nBins*2, 0, 500); //100 bins
  hEventSumContents.hApvCorpt->SetFillColor(kYellow-9);
  hEventSumContents.hApvCorpt->SetStats(true);
  hEventSumContents.hApvCorpt->GetXaxis()->SetTitle("Number of visible APVs");

  hEventSumContents.hEventSize = new TH1S("FSTEventSize", "FST - Event Size", nBins, 0, 6000); //50 bins
  hEventSumContents.hEventSize->GetXaxis()->SetTitle("Unpacked Event size [kB]");
  hEventSumContents.hEventSize->SetFillColor(kYellow-9);
  hEventSumContents.hEventSize->SetStats(true);

  hEventSumContents.hMipMPVvsSection = new TH1S("mipMPVvsSection_nonZS", "FST - MPV vs Section Id (non-ZS)", totSec, 0, totSec); //72 bins
  hEventSumContents.hMipMPVvsSection->GetXaxis()->SetTitle("Section ID");
  hEventSumContents.hMipMPVvsSection->GetYaxis()->SetTitle("MPV_{non-ZS} [ADC Counts]");
  hEventSumContents.hMipMPVvsSection->SetFillColor(kYellow-9);
  hEventSumContents.hMipMPVvsSection->SetStats(false);

  hEventSumContents.hMipMPVvsSection_ZS = new TH1S("mipMPVvsSection_ZS", "FST - MPV vs Section Id (ZS)", totSec, 0, totSec); //72 bins
  hEventSumContents.hMipMPVvsSection_ZS->GetXaxis()->SetTitle("Section ID");
  hEventSumContents.hMipMPVvsSection_ZS->GetYaxis()->SetTitle("MPV_{ZS} [ADC Counts]");
  hEventSumContents.hMipMPVvsSection_ZS->SetFillColor(kYellow-9);
  hEventSumContents.hMipMPVvsSection_ZS->SetStats(false);

  hEventSumContents.hMipSIGMAvsSection = new TH1S("mipSIGMAvsSection_nonZS", "FST - Sigma vs Section Id (non-ZS)", totSec, 0, totSec); //72 bins
  hEventSumContents.hMipSIGMAvsSection->GetXaxis()->SetTitle("Section ID");
  hEventSumContents.hMipSIGMAvsSection->GetYaxis()->SetTitle("Sigma_{non-ZS} [ADC Counts]");
  hEventSumContents.hMipSIGMAvsSection->SetFillColor(kYellow-9);
  hEventSumContents.hMipSIGMAvsSection->SetStats(false);

  hEventSumContents.hMipSIGMAvsSection_ZS = new TH1S("mipSIGMAvsSection_ZS", "FST - Sigma vs Section Id (ZS)", totSec, 0, totSec); //72 bins
  hEventSumContents.hMipSIGMAvsSection_ZS->GetXaxis()->SetTitle("Section ID");
  hEventSumContents.hMipSIGMAvsSection_ZS->GetYaxis()->SetTitle("Sigma_{ZS} [ADC Counts]");
  hEventSumContents.hMipSIGMAvsSection_ZS->SetFillColor(kYellow-9);
  hEventSumContents.hMipSIGMAvsSection_ZS->SetStats(false);

  hEventSumContents.hMaxTBfractionVsSection_ZS = new TH1F("maxTBfractionVsSection_ZS", "FST - maxTB fraction vs Section Id (ZS)", totSec, 0, totSec); //72 bins
  hEventSumContents.hMaxTBfractionVsSection_ZS->GetXaxis()->SetTitle("Section ID");
  hEventSumContents.hMaxTBfractionVsSection_ZS->GetYaxis()->SetTitle("N_{0<maxTB<numTB}/N_{0<=maxTB<=numTB}");
  hEventSumContents.hMaxTBfractionVsSection_ZS->SetFillColor(kYellow-9);
  hEventSumContents.hMaxTBfractionVsSection_ZS->SetStats(false);

  ///////////////////
  for(int index = 0; index < mMipHist; index++) 
  { // per section
    if(index<72) 
    {
      sprintf(buffer, "MIP_nonZS_Section_%d", index);
      int rdoIdx   = index/SecPerRdo + 1;          //1-6
      int armIdx   = (index%SecPerRdo)/SecPerArm;  //0-2
      int portIdx  = (index/SecPerMod)%PortPerArm; //0-1
      int secIdx   = index%SecPerMod;              //0-1
      sprintf(buffer2,"FST - MIP peak non-ZS, RDO%d_ARM%d_PORT%d_SEC%d", rdoIdx, armIdx, portIdx, secIdx);

      hMipContents.mipArray[index] = new TH1S(buffer, buffer2, 128, 0, 4096); //256 bins
      hMipContents.mipArray[index]->GetXaxis()->SetTitle("Pedestal Subtracted ADC [ADC Counts]");
      hMipContents.mipArray[index]->SetFillColor(kYellow-9);
      hMipContents.mipArray[index]->SetStats(true);
    }
    else 
    {
      sprintf( buffer, "MIP_ZS_Section_%d", index-72 );
      int rdoIdx   = (index-totSec)/SecPerRdo + 1;          //1-6
      int armIdx   = ((index-totSec)%SecPerRdo)/SecPerArm;  //0-2
      int portIdx  = ((index-totSec)/SecPerMod)%PortPerArm; //0-1
      int secIdx   = (index-totSec)%SecPerMod;              //0-1
      sprintf(buffer2,"FST - MIP peak ZS, RDO%d_ARM%d_PORT%d_SEC%d", rdoIdx, armIdx, portIdx, secIdx);

      hMipContents.mipArray[index] = new TH1S(buffer, buffer2, 128, 0, 4096); //256 bins
      hMipContents.mipArray[index]->GetXaxis()->SetTitle("Pedestal Subtracted ADC [ADC Counts]");
      hMipContents.mipArray[index]->SetFillColor(kYellow-9);
      hMipContents.mipArray[index]->SetStats(true);
    }
  }

  for(int index = 0; index < mMaxTimeBinHist; index++) 
  { // per section
    sprintf( buffer, "maxTB_ZS_Section_%d", index );
    int rdoIdx   = index/SecPerRdo + 1;          //1-6
    int armIdx   = (index%SecPerRdo)/SecPerArm;  //0-2
    int portIdx  = (index/SecPerMod)%PortPerArm; //0-1
    int secIdx   = index%SecPerMod;              //0-1
    sprintf( buffer2,"FST - Max time bin (ZS), RDO%d_ARM%d_PORT%d_SEC%d", rdoIdx, armIdx, portIdx, secIdx);

    hMaxTimeBinContents.maxTimeBinArray[index] = new TH1S(buffer, buffer2, numTimeBin, 0, numTimeBin); //9 bins
    hMaxTimeBinContents.maxTimeBinArray[index]->GetXaxis()->SetTitle("Time Bin Index");
    hMaxTimeBinContents.maxTimeBinArray[index]->SetFillColor(kYellow-9);
    hMaxTimeBinContents.maxTimeBinArray[index]->SetStats(true);
  }

  //////////////////
  for(int iDisk = 0; iDisk < totDisk; ++iDisk)
  {
    sprintf(buffer,"VisibleAPVperMoudleDisk%d", iDisk+1);
    sprintf(buffer2,"FST - Visible APVs per Module for Disk%d", iDisk+1);
    hSumContents.hVisibleApv[iDisk] = new TH2S(buffer, buffer2, SecPerDisk, 0.5, SecPerDisk+0.5, ApvPerSec+1, -0.5, ApvPerSec+0.5); //12*9 bins
    hSumContents.hVisibleApv[iDisk]->SetFillColor(kYellow-9);
    hSumContents.hVisibleApv[iDisk]->SetStats(true);
    hSumContents.hVisibleApv[iDisk]->GetXaxis()->SetTitle("Section ID [per Disk]");
    hSumContents.hVisibleApv[iDisk]->GetYaxis()->SetTitle("Number of APV [per Event]");

    sprintf(buffer,"HitMapOfFSTDisk%d", iDisk+1);
    sprintf(buffer2,"FST - Hit map (non-ZS) for Disk%d", iDisk+1);
    hSumContents.hHitMap[iDisk] = new TH2S(buffer, buffer2, PhiSegPerMod*ModPerDisk, -0.5, PhiSegPerMod*ModPerDisk-0.5, RstripPerMod, -0.5, RstripPerMod-0.5);//1536*8 bins
    hSumContents.hHitMap[iDisk]->GetXaxis()->SetNdivisions(-ModPerDisk, false); // 12
    hSumContents.hHitMap[iDisk]->GetYaxis()->SetNdivisions(-RstripPerMod, false); // 8
    hSumContents.hHitMap[iDisk]->SetStats(false);
    hSumContents.hHitMap[iDisk]->GetXaxis()->SetTitle("#phi Index");
    hSumContents.hHitMap[iDisk]->GetXaxis()->SetLabelSize(0.02);
    hSumContents.hHitMap[iDisk]->GetYaxis()->SetTitle("R Index");
    hSumContents.hHitMap[iDisk]->GetYaxis()->SetLabelSize(0.02);

    sprintf(buffer,"DisplayHitMapOfFSTDisk%d", iDisk+1);
    sprintf(buffer2,"FST - Hit Map Display (non-ZS) for Disk%d", iDisk+1);
    hSumContents.hDummyPolyHitMap[iDisk] = new TH2S(buffer, buffer2, 200, -300.0, 300.0, 200, -300.0, 300.0);
    // hSumContents.hDummyPolyHitMap[iDisk]->GetXaxis()->SetNdivisions(-ModPerDisk, false); // 12
    // hSumContents.hDummyPolyHitMap[iDisk]->GetYaxis()->SetNdivisions(-RstripPerMod, false); // 8
    hSumContents.hDummyPolyHitMap[iDisk]->SetStats(false);
    hSumContents.hDummyPolyHitMap[iDisk]->GetXaxis()->SetTitle("x (mm)");
    hSumContents.hDummyPolyHitMap[iDisk]->GetXaxis()->SetLabelSize(0.02);
    hSumContents.hDummyPolyHitMap[iDisk]->GetYaxis()->SetTitle("y (mm)");
    hSumContents.hDummyPolyHitMap[iDisk]->GetYaxis()->SetLabelSize(0.02);

    sprintf(buffer,"PolyHitMapOfFSTDisk%d", iDisk+1);
    sprintf(buffer2,"FST - Poly Hit map (non-ZS) for Disk%d", iDisk+1);
    hSumContents.hPolyHitMap[iDisk] = new TH2S(buffer, buffer2, PhiSegPerMod*ModPerDisk/8, 0, TMath::TwoPi(), RstripPerMod, 50.0, 280.0);// (4*4*12)*8 bins
    hSumContents.hPolyHitMap[iDisk]->GetXaxis()->SetNdivisions(-ModPerDisk, false); // 12
    hSumContents.hPolyHitMap[iDisk]->GetYaxis()->SetNdivisions(-RstripPerMod, false); // 8
    hSumContents.hPolyHitMap[iDisk]->SetStats(false);
    // hSumContents.hPolyHitMap[iDisk]->GetXaxis()->SetTitle("#phi (Rad)");
    // hSumContents.hPolyHitMap[iDisk]->GetYaxis()->SetTitle("R (mm)");
    // hSumContents.hPolyHitMap[iDisk]->SetLabelSize(0.02);

    sprintf(buffer,"HitMapPerAPVDisk%d", iDisk+1);
    sprintf(buffer2,"FST - Hit map in Module vs APV (non-ZS) for Disk%d", iDisk+1);
    hSumContents.hHitMapVsAPV[iDisk] = new TH2S(buffer, buffer2, 12, 0.5, 12.5, 8, -0.5, 7.5);
    hSumContents.hHitMapVsAPV[iDisk]->SetStats(false);
    hSumContents.hHitMapVsAPV[iDisk]->GetXaxis()->SetTitle("Module geometry ID");
    hSumContents.hHitMapVsAPV[iDisk]->GetYaxis()->SetTitle("APV geometry ID");

    sprintf(buffer,"HitMapOfFSTDisk%d_ZS", iDisk+1);
    sprintf(buffer2,"FST - Hit map (ZS) for Disk%d", iDisk+1);
    hSumContents.hHitMap_ZS[iDisk] = new TH2S(buffer, buffer2, PhiSegPerMod*ModPerDisk, -0.5, PhiSegPerMod*ModPerDisk, RstripPerMod-0.5, -0.5, RstripPerMod-0.5);//1536*8 bins
    hSumContents.hHitMap_ZS[iDisk]->GetXaxis()->SetNdivisions(-ModPerDisk, false);
    hSumContents.hHitMap_ZS[iDisk]->GetYaxis()->SetNdivisions(-RstripPerMod, false);
    hSumContents.hHitMap_ZS[iDisk]->SetStats(false);
    hSumContents.hHitMap_ZS[iDisk]->GetXaxis()->SetTitle("#phi Index");
    hSumContents.hHitMap_ZS[iDisk]->GetXaxis()->SetLabelSize(0.02);
    hSumContents.hHitMap_ZS[iDisk]->GetYaxis()->SetTitle("R Index");
    hSumContents.hHitMap_ZS[iDisk]->GetYaxis()->SetLabelSize(0.02);

    sprintf(buffer,"DisplayHitMapOfFSTDisk%d_ZS", iDisk+1);
    sprintf(buffer2,"FST - Hit Map Display (ZS) for Disk%d", iDisk+1);
    hSumContents.hDummyPolyHitMap_ZS[iDisk] = new TH2S(buffer, buffer2, 200, -300.0, 300.0, 200, -300.0, 300.0);
    // hSumContents.hDummyPolyHitMap_ZS[iDisk]->GetXaxis()->SetNdivisions(-ModPerDisk, false); // 12
    // hSumContents.hDummyPolyHitMap_ZS[iDisk]->GetYaxis()->SetNdivisions(-RstripPerMod, false); // 8
    hSumContents.hDummyPolyHitMap_ZS[iDisk]->SetStats(false);
    hSumContents.hDummyPolyHitMap_ZS[iDisk]->GetXaxis()->SetTitle("x (mm)");
    hSumContents.hDummyPolyHitMap_ZS[iDisk]->GetXaxis()->SetLabelSize(0.02);
    hSumContents.hDummyPolyHitMap_ZS[iDisk]->GetYaxis()->SetTitle("y (mm)");
    hSumContents.hDummyPolyHitMap_ZS[iDisk]->GetYaxis()->SetLabelSize(0.02);

    sprintf(buffer,"PolyHitMapOfFSTDisk%d_ZS", iDisk+1);
    sprintf(buffer2,"FST - Poly Hit map (ZS) for Disk%d", iDisk+1);
    hSumContents.hPolyHitMap_ZS[iDisk] = new TH2S(buffer, buffer2, PhiSegPerMod*ModPerDisk/8, 0, TMath::TwoPi(), RstripPerMod, 50.0, 280.0);// (4*4*12)*8 bins
    hSumContents.hPolyHitMap_ZS[iDisk]->GetXaxis()->SetNdivisions(-ModPerDisk, false); // 12
    hSumContents.hPolyHitMap_ZS[iDisk]->GetYaxis()->SetNdivisions(-RstripPerMod, false); // 8
    hSumContents.hPolyHitMap_ZS[iDisk]->SetStats(false);
    // hSumContents.hPolyHitMap_ZS[iDisk]->GetXaxis()->SetTitle("#phi (Rad)");
    // hSumContents.hPolyHitMap_ZS[iDisk]->GetYaxis()->SetTitle("R (cm)");
    // hSumContents.hPolyHitMap_ZS[iDisk]->SetLabelSize(0.02);

    sprintf(buffer,"HitMapPerAPVDisk%d_ZS", iDisk+1);
    sprintf(buffer2,"FST - Hit map in Module vs APV (ZS) for Disk%d", iDisk+1);
    hSumContents.hHitMapVsAPV_ZS[iDisk] = new TH2S(buffer, buffer2, 12, 0.5, 12.5, 8, -0.5, 7.5);
    hSumContents.hHitMapVsAPV_ZS[iDisk]->SetStats(false);
    hSumContents.hHitMapVsAPV_ZS[iDisk]->GetXaxis()->SetTitle("Module Geometry ID");
    hSumContents.hHitMapVsAPV_ZS[iDisk]->GetYaxis()->SetTitle("APV Geometry ID");

    sprintf(buffer,"HitMultVsModuleDisk%d", iDisk+1);
    sprintf(buffer2,"FST - Hit Multiplicity vs Module Id for Disk%d", iDisk+1);
    hSumContents.hMultVsModule[iDisk] = new TH2S(buffer, buffer2, ModPerDisk, 0.5, ModPerDisk+0.5, 4096, 0, 4096);//
    hSumContents.hMultVsModule[iDisk]->GetXaxis()->SetNdivisions(-ModPerDisk, false);
    hSumContents.hMultVsModule[iDisk]->SetStats(false);
    hSumContents.hMultVsModule[iDisk]->GetXaxis()->SetTitle("Module Geometry ID");
    hSumContents.hMultVsModule[iDisk]->GetYaxis()->SetTitle("Number of Hits");
    hSumContents.hMultVsModule[iDisk]->GetYaxis()->SetRangeUser(0, 4096);
    hSumContents.hMultVsModule[iDisk]->SetLabelSize(0.03);
    for(int iModule=0; iModule<ModPerDisk; iModule++) {
      sprintf( buffer, "M%d", 1+iModule);
      hSumContents.hMultVsModule[iDisk]->GetXaxis()->SetBinLabel(iModule+1,buffer);
    }

    sprintf(buffer,"PedestalPerChannelDisk%d", iDisk+1);
    sprintf(buffer2,"FST - Pedestal vs Channel for Disk%d", iDisk+1);
    hSumContents.hSumPed[iDisk] = new TH2S(buffer, buffer2, ApvPerDisk, 0, ChPerDisk, nBins*2, PedMin, PedMax); //96*100 bins
    hSumContents.hSumPed[iDisk]->GetXaxis()->SetNdivisions(-ModPerDisk,false);
    hSumContents.hSumPed[iDisk]->SetStats(false);
    hSumContents.hSumPed[iDisk]->GetXaxis()->SetTitle("Channel Geometry ID");
    hSumContents.hSumPed[iDisk]->GetYaxis()->SetTitle("Mean Pedestal [ADC counts]");
    hSumContents.hSumPed[iDisk]->GetYaxis()->SetTitleOffset(1.1);

    sprintf(buffer,"PedestalRmsPerChannelDisk%d", iDisk+1);
    sprintf(buffer2,"FST - RMS vs Channel for Disk%d", iDisk+1);
    hSumContents.hSumSig[iDisk] = new TH2S(buffer, buffer2, ApvPerDisk, 0, ChPerDisk, nBins*2, SigMin, SigMax); //96*100 bins
    hSumContents.hSumSig[iDisk]->GetXaxis()->SetNdivisions(-ModPerDisk,false);
    hSumContents.hSumSig[iDisk]->SetStats(false);
    hSumContents.hSumSig[iDisk]->GetXaxis()->SetTitle("Channel Geometry ID");
    hSumContents.hSumSig[iDisk]->GetYaxis()->SetTitle("Pedestal RMS [ADC counts]");

    sprintf(buffer,"RandomRmsPerChannelDisk%d", iDisk+1);
    sprintf(buffer2,"FST - Random RMS vs Channel for Disk%d", iDisk+1);
    hSumContents.hSumRan[iDisk] = new TH2S(buffer, buffer2, ApvPerDisk, 0, ChPerDisk, nBins*2, SigMin, SigMax); //96*100 bins
    hSumContents.hSumRan[iDisk]->GetXaxis()->SetNdivisions(-ModPerDisk,false);
    hSumContents.hSumRan[iDisk]->SetStats(false);
    hSumContents.hSumRan[iDisk]->GetXaxis()->SetTitle("Channel Geometry ID");
    hSumContents.hSumRan[iDisk]->GetYaxis()->SetTitle("Random RMS [ADC counts]");

    for(int index=0; index<ModPerDisk; index++ )
    {
      char label[100];
      sprintf(label, "M%d", index+1);
      hSumContents.hSumPed[iDisk]->GetXaxis()->SetBinLabel(index*ApvPerMod+ApvPerMod/2, label);  
      hSumContents.hSumSig[iDisk]->GetXaxis()->SetBinLabel(index*ApvPerMod+ApvPerMod/2, label);
      hSumContents.hSumRan[iDisk]->GetXaxis()->SetBinLabel(index*ApvPerMod+ApvPerMod/2, label);
    }

    sprintf(buffer,"CommonModeNoisePerAPVDisk%d", iDisk+1);
    sprintf(buffer2,"FST - Common Mode Noise vs APV for Disk%d", iDisk+1);
    hSumContents.hCommonModeNoise[iDisk] = new TH2S(buffer, buffer2, ApvPerDisk*4, 0, ApvPerDisk*4, nBins*2, CmnMin, CmnMax);//384*100 bins
    hSumContents.hCommonModeNoise[iDisk]->GetXaxis()->SetNdivisions(-ModPerDisk, false);
    hSumContents.hCommonModeNoise[iDisk]->SetStats(false);
    hSumContents.hCommonModeNoise[iDisk]->GetXaxis()->SetTitle("APV Geometry ID");
    hSumContents.hCommonModeNoise[iDisk]->GetXaxis()->SetLabelSize(0.03);
    hSumContents.hCommonModeNoise[iDisk]->GetYaxis()->SetTitle("Common Mode Noise [ADC counts]");
    hSumContents.hCommonModeNoise[iDisk]->GetYaxis()->SetLabelSize(0.03);
    //label setting
    for(int iModule=0; iModule<ModPerDisk; iModule++) 
    {
      sprintf( buffer, "M%d", 1+iModule );
      hSumContents.hCommonModeNoise[iDisk]->GetXaxis()->SetBinLabel(iModule*ApvPerMod*4+ApvPerMod*2,buffer);
    }
  }

  //JEVP plots setting
  int totPlots = mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+mEventSumHist+mMipHist+mMaxTimeBinHist+mSumHist;
  plots = new JevpPlot*[totPlots];

  JLine* line1 = new JLine(1536, -100, 1536, 4000);
  line1->SetLineColor(kGreen);
  line1->SetLineWidth(2.0);
  JLine* line2 = new JLine(3072, -100, 3072, 4000);
  line2->SetLineColor(kGreen);
  line2->SetLineWidth(2.0); 
  for ( int i=0; i<mAdcHist; i++ ) {
    hAdcContents.adcArray[i]->SetOption("colz");
    plots[i] = new JevpPlot(hAdcContents.adcArray[i]);
    plots[i]->addElement(line1);
    plots[i]->addElement(line2);
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
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+2] = new JevpPlot(hEventSumContents.hMeanRan);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+3] = new JevpPlot(hEventSumContents.hSumTB);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+4] = new JevpPlot(hEventSumContents.hMaxTimeBin);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+5] = new JevpPlot(hEventSumContents.hMaxTimeBin_ZS);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+6] = new JevpPlot(hEventSumContents.hSumBad);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+7] = new JevpPlot(hEventSumContents.hApvCorpt);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+8] = new JevpPlot(hEventSumContents.hEventSize);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+9] = new JevpPlot(hEventSumContents.hMipMPVvsSection);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+10] = new JevpPlot(hEventSumContents.hMipMPVvsSection_ZS);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+11] = new JevpPlot(hEventSumContents.hMipSIGMAvsSection);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+12] = new JevpPlot(hEventSumContents.hMipSIGMAvsSection_ZS);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+13] = new JevpPlot(hEventSumContents.hMaxTBfractionVsSection_ZS);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+6]->logy=true;
  // plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+6]->setOptStat(10);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+7]->logy=true;

  JLine* line = new JLine(0, goodChCut, totAPV, goodChCut);
  line->SetLineColor(kRed);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+5]->addElement(line);

  //JLine* line3_nonZS = new JLine(0, minMipMpv_nonZS, totSec, minMipMpv_nonZS);
  //line3_nonZS->SetLineColor(kRed);
  JLine* line3_ZS = new JLine(0, minMipMpv_ZS, totSec, minMipMpv_ZS);
  line3_ZS->SetLineColor(kRed);
  //plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+8]->addElement(line3_nonZS);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+9]->addElement(line3_ZS);

  //JLine* line5_nonZS = new JLine(0, minMipSigma_nonZS, totSec, minMipSigma_nonZS);
  //line5_nonZS->SetLineColor(kRed);
  JLine* line5_ZS = new JLine(0, minMipSigma_ZS, totSec, minMipSigma_ZS);
  line5_ZS->SetLineColor(kRed);
  //plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+10]->addElement(line5_nonZS);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+11]->addElement(line5_ZS);

  JLine* line7 = new JLine(0, maxTbFracOK, totSec, maxTbFracOK);
  line7->SetLineColor(kRed);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+12]->addElement(line7);

  for ( int i=0; i<mMipHist; i++ ) {
    plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+mEventSumHist+i] = new JevpPlot(hMipContents.mipArray[i]);
  }

  for ( int i=0; i<mMaxTimeBinHist; i++ ) {
    plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+mEventSumHist+mMipHist+i] = new JevpPlot(hMaxTimeBinContents.maxTimeBinArray[i]);
  }

  // Add Plots to plot set...
  for ( int i=0; i<totPlots-mSumHist ;i++ ) {
    LOG(DBG, "Adding plot %d",i);
    addPlot(plots[i]);
  }

  int nPlots = mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+mEventSumHist+mMipHist+mMaxTimeBinHist;
  plots[nPlots] = new JevpPlot(hSumContents.hVisibleApv[0]);
  plots[nPlots+1] = new JevpPlot(hSumContents.hVisibleApv[1]);
  plots[nPlots+2] = new JevpPlot(hSumContents.hVisibleApv[2]);
  plots[nPlots+3] = new JevpPlot(hSumContents.hHitMap[0]);
  plots[nPlots+4] = new JevpPlot(hSumContents.hHitMap[1]);
  plots[nPlots+5] = new JevpPlot(hSumContents.hHitMap[2]);
  plots[nPlots+6] = new JevpPlot(); // Polar Hit Map
  PlotHisto *ph = new PlotHisto(hSumContents.hDummyPolyHitMap[0]);
  plots[nPlots+6]->addHisto(ph);
  ph = new PlotHisto(hSumContents.hPolyHitMap[0]);
  plots[nPlots+6]->addHisto(ph);
  plots[nPlots+7] = new JevpPlot(); // Polar Hit Map
  ph = new PlotHisto(hSumContents.hDummyPolyHitMap[1]);
  plots[nPlots+7]->addHisto(ph);
  ph = new PlotHisto(hSumContents.hPolyHitMap[1]);
  plots[nPlots+7]->addHisto(ph);
  plots[nPlots+8] = new JevpPlot(); // Polar Hit Map
  ph = new PlotHisto(hSumContents.hDummyPolyHitMap[2]);
  plots[nPlots+8]->addHisto(ph);
  ph = new PlotHisto(hSumContents.hPolyHitMap[2]);
  plots[nPlots+8]->addHisto(ph);
  plots[nPlots+9]  = new JevpPlot(hSumContents.hHitMapVsAPV[0]);
  plots[nPlots+10] = new JevpPlot(hSumContents.hHitMapVsAPV[1]);
  plots[nPlots+11] = new JevpPlot(hSumContents.hHitMapVsAPV[2]);
  plots[nPlots+12] = new JevpPlot(hSumContents.hHitMap_ZS[0]);
  plots[nPlots+13] = new JevpPlot(hSumContents.hHitMap_ZS[1]);
  plots[nPlots+14] = new JevpPlot(hSumContents.hHitMap_ZS[2]);
  plots[nPlots+15] = new JevpPlot(); // Polar Hit Map
  ph = new PlotHisto(hSumContents.hDummyPolyHitMap_ZS[0]);
  plots[nPlots+15]->addHisto(ph);
  ph = new PlotHisto(hSumContents.hPolyHitMap_ZS[0]);
  plots[nPlots+15]->addHisto(ph);
  plots[nPlots+16] = new JevpPlot(); // Polar Hit Map
  ph = new PlotHisto(hSumContents.hDummyPolyHitMap_ZS[1]);
  plots[nPlots+16]->addHisto(ph);
  ph = new PlotHisto(hSumContents.hPolyHitMap_ZS[1]);
  plots[nPlots+16]->addHisto(ph);
  plots[nPlots+17] = new JevpPlot(); // Polar Hit Map
  ph = new PlotHisto(hSumContents.hDummyPolyHitMap_ZS[2]);
  plots[nPlots+17]->addHisto(ph);
  ph = new PlotHisto(hSumContents.hPolyHitMap_ZS[2]);
  plots[nPlots+17]->addHisto(ph);
  plots[nPlots+18] = new JevpPlot(hSumContents.hHitMapVsAPV_ZS[0]);
  plots[nPlots+19] = new JevpPlot(hSumContents.hHitMapVsAPV_ZS[1]);
  plots[nPlots+20] = new JevpPlot(hSumContents.hHitMapVsAPV_ZS[2]);
  plots[nPlots+21] = new JevpPlot(hSumContents.hMultVsModule[0]);
  plots[nPlots+22] = new JevpPlot(hSumContents.hMultVsModule[1]);
  plots[nPlots+23] = new JevpPlot(hSumContents.hMultVsModule[2]);
  plots[nPlots+24] = new JevpPlot(hSumContents.hSumPed[0]);
  plots[nPlots+25] = new JevpPlot(hSumContents.hSumPed[1]);
  plots[nPlots+26] = new JevpPlot(hSumContents.hSumPed[2]);
  plots[nPlots+27] = new JevpPlot(hSumContents.hSumSig[0]);
  plots[nPlots+28] = new JevpPlot(hSumContents.hSumSig[1]);
  plots[nPlots+29] = new JevpPlot(hSumContents.hSumSig[2]);
  plots[nPlots+30] = new JevpPlot(hSumContents.hSumRan[0]);
  plots[nPlots+31] = new JevpPlot(hSumContents.hSumRan[1]);
  plots[nPlots+32] = new JevpPlot(hSumContents.hSumRan[2]);
  plots[nPlots+33] = new JevpPlot(hSumContents.hCommonModeNoise[0]);
  plots[nPlots+34] = new JevpPlot(hSumContents.hCommonModeNoise[1]);
  plots[nPlots+35] = new JevpPlot(hSumContents.hCommonModeNoise[2]);

  for(int iPlots = nPlots; iPlots < nPlots+36; ++iPlots)
  {
    LOG(DBG, "Adding plot %d",iPlots);
    addPlot(plots[iPlots]);
    plots[iPlots]->setDrawOpts("colz");
  }
  plots[nPlots+6]->setDrawOpts("colz POL");
  plots[nPlots+7]->setDrawOpts("colz POL");
  plots[nPlots+8]->setDrawOpts("colz POL");
  plots[nPlots+15]->setDrawOpts("colz POL");
  plots[nPlots+16]->setDrawOpts("colz POL");
  plots[nPlots+17]->setDrawOpts("colz POL");
  plots[nPlots+6]->optlogz=true;
  plots[nPlots+7]->optlogz=true;
  plots[nPlots+8]->optlogz=true;
  plots[nPlots+15]->optlogz=true;
  plots[nPlots+16]->optlogz=true;
  plots[nPlots+17]->optlogz=true;
  // plots[nPlots+6]->setOptStat(0);
  // plots[nPlots+7]->setOptStat(0);
  // plots[nPlots+8]->setOptStat(0);
  // plots[nPlots+15]->setOptStat(0);
  // plots[nPlots+16]->setOptStat(0);
  // plots[nPlots+17]->setOptStat(0);

  // module divisions
  JLine *polyLine[ModPerDisk];
  for(int iModule = 0; iModule < ModPerDisk; ++iModule)
  {
    polyLine[iModule] = new JLine(50*TMath::Cos((iModule+1)*TMath::TwoPi()/12.0), 50*TMath::Sin((iModule+1)*TMath::TwoPi()/12.0), 280*TMath::Cos((iModule+1)*TMath::TwoPi()/12.0), 280*TMath::Sin((iModule+1)*TMath::TwoPi()/12.0)); // every 30 degree
    polyLine[iModule]->SetLineColor(kRed);
    polyLine[iModule]->SetLineWidth(3);
    polyLine[iModule]->SetLineStyle(2);
    polyLine[iModule]->SetNDC_x(0);
    polyLine[iModule]->SetNDC_y(0);
  }

  for(int iModule = 0; iModule < ModPerDisk; ++iModule)
  {
    plots[nPlots+6]->addElement(polyLine[iModule]);
    plots[nPlots+7]->addElement(polyLine[iModule]);
    plots[nPlots+8]->addElement(polyLine[iModule]);
    plots[nPlots+15]->addElement(polyLine[iModule]);
    plots[nPlots+16]->addElement(polyLine[iModule]);
    plots[nPlots+17]->addElement(polyLine[iModule]);
  }


  //red would be [2]
  errorMsg = new JLatex(.25, .12, "#color[4]{No Error Message}");
  errorMsg->SetTextSize(0.035);
  errorMsg->SetTextAlign(13);
  errorMsg->SetTextAngle(45);
  plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist]->addElement(errorMsg);

  //temp histograms for all APV chips' dynamical CM noise distribution
  for(int i=0; i<totAPV; i++) {
    for(int iRstrip = 0; iRstrip < 4; ++iRstrip) {
      sprintf( buffer, "APV%d Group%d", i, iRstrip);
      hCmnTemp.hCmnPerChip[i][iRstrip] = new TH1S(buffer, "Common mode noise per APV chip per Rstrip", 256, 0, 4096);
    }
  }
}

// ************FST START RUN*****************
// ------------------------------------------
void fstBuilder::startrun(daqReader *rdr) 
{
  LOG ( NOTE, "fstBuilder starting run #%d", rdr->run );
  resetAllPlots();
  run = rdr->run; 

  for ( int i=0; i<totCh; i++ ) 
  {
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

  //load external pedstal/RMS value for all channels
  FILE *file;
  char paraDir[256];
  sprintf(paraDir, "%s/fst_s1_pedestals.txt", clientdatadir);
  // sprintf(paraDir, "/star/data01/pwg/sunxuhit/ForwardSiliconTracker/Data/FstInstallation/daqtest/fst_s1_pedestals.txt");

  file = fopen(paraDir, "r");
  if (file==0) {
    LOG(WARN,"ped::external table can't open input file \"%s\" [%s]", paraDir, strerror(errno));
    tableFound = false;
    sprintf(paraDir, "%s/fst_s1_pedestals_local.txt", clientdatadir);
    file = fopen(paraDir, "r");
    if(file==0){
      LOG(WARN,"ped::external table can't open input file \"%s\" [%s]", paraDir, strerror(errno));
    }else{
      //LOG(U_FST,"loading pedestals from %s ", paraDir);
      while(!feof(file)) {
	int rdoIdxTemp=0, armIdxTemp=0, apvIdxTemp=0, chanIdxTemp=0, tbIdxTemp=0;
	float pp=0., rr=0., nn=0.;
	char buff[256];

	if(fgets(buff,sizeof(buff),file) == 0) continue ;
	switch(buff[0]) {
	  case '#' :
	  case '!' :
	  case '*' :
	  case '/' :
	  case '.' :
	    continue ;
	}
	int ret = sscanf(buff,"%d %d %d %d %d %f %f %f",&rdoIdxTemp,&armIdxTemp,&apvIdxTemp,&chanIdxTemp,&tbIdxTemp,&pp,&rr,&nn);
	if(ret!=8) continue;

	if(tbIdxTemp==2) { //only take time bin 2 as sample
	  int portIdxTemp        = apvIdxTemp/ApvRoPerPort; // 0: 0-7 | 1: 12-19
	  int refApvIdxTemp      = apvIdxTemp - portIdxTemp*ApvNumOffset + portIdxTemp*ApvPerPort; // 0-15
	  int glbElecChanIdxTemp = (rdoIdxTemp-1)*ChPerRdo + armIdxTemp*ChPerArm + refApvIdxTemp*ChPerApv + chanIdxTemp; // 0-36863
	  fstPedestal[glbElecChanIdxTemp] = pp; // pedestal
	  fstRmsNoise[glbElecChanIdxTemp] = rr; // total noise
	  fstRanNoise[glbElecChanIdxTemp] = nn; // random noise
	}
      }
      tableFound = true;
      fclose(file);
    }
  }
  else {
    //LOG(U_FST,"loading pedestals from %s ", paraDir);
    while(!feof(file)) {
      int rdoIdxTemp=0, armIdxTemp=0, apvIdxTemp=0, chanIdxTemp=0, tbIdxTemp=0;
      float pp=0., rr=0., nn=0.;
      char buff[256];

      if(fgets(buff,sizeof(buff),file) == 0) continue ;
      switch(buff[0]) {
	case '#' :
	case '!' :
	case '*' :
	case '/' :
	case '.' :
	  continue ;
      }
      int ret = sscanf(buff,"%d %d %d %d %d %f %f %f",&rdoIdxTemp,&armIdxTemp,&apvIdxTemp,&chanIdxTemp,&tbIdxTemp,&pp,&rr,&nn);
      if(ret!=8) continue;

      if(tbIdxTemp==2) { //only take time bin 2 as sample
	int portIdxTemp        = apvIdxTemp/ApvRoPerPort; // 0: 0-7 | 1: 12-19
	int refApvIdxTemp      = apvIdxTemp - portIdxTemp*ApvNumOffset + portIdxTemp*ApvPerPort; // 0-15
	int glbElecChanIdxTemp = (rdoIdxTemp-1)*ChPerRdo + armIdxTemp*ChPerArm + refApvIdxTemp*ChPerApv + chanIdxTemp; // 0-36863
	fstPedestal[glbElecChanIdxTemp] = pp; // pedestal
	fstRmsNoise[glbElecChanIdxTemp] = rr; // total noise
	fstRanNoise[glbElecChanIdxTemp] = nn; // random noise
	// cout << "glbElecChanIdxTemp = " << glbElecChanIdxTemp << ", rdoIdxTemp = " << rdoIdxTemp << ", armIdxTemp = " << armIdxTemp << ", apvIdxTemp = " << apvIdxTemp << "chanIdxTemp = " << chanIdxTemp << ", nn = " << nn << endl;
      }
    }
    tableFound = true;
    fclose(file);
  }

  sprintf(paraDir, "%s/fst_s2_pedestals.txt", clientdatadir);
  // sprintf(paraDir, "/star/data01/pwg/sunxuhit/ForwardSiliconTracker/Data/FstInstallation/daqtest/fst_s2_pedestals.txt");

  FILE *file0;

  file0 = fopen(paraDir, "r");
  if (file0==0) {
    LOG(WARN,"ped::external table can't open input file \"%s\" [%s]", paraDir, strerror(errno));
    tableFound = false;
    sprintf(paraDir, "%s/fst_s2_pedestals_local.txt", clientdatadir);
    file0 = fopen(paraDir, "r");
    if(file0==0){
      LOG(WARN,"ped::external table can't open input file \"%s\" [%s]", paraDir, strerror(errno));
    }else{
      //LOG(U_FST,"loading pedestals from %s ", paraDir);
      while(!feof(file0)) {
	int rdoIdxTemp=0, armIdxTemp=0, apvIdxTemp=0, chanIdxTemp=0, tbIdxTemp=0;
	float pp=0., rr=0., nn=0.;
	char buff[256];

	if(fgets(buff,sizeof(buff),file0) == 0) continue ;
	switch(buff[0]) {
	  case '#' :
	  case '!' :
	  case '*' :
	  case '/' :
	  case '.' :
	    continue ;
	}
	int ret = sscanf(buff,"%d %d %d %d %d %f %f %f",&rdoIdxTemp,&armIdxTemp,&apvIdxTemp,&chanIdxTemp,&tbIdxTemp,&pp,&rr,&nn);
	if(ret!=8) continue;

	if(tbIdxTemp==2) { //only take time bin 2 as sample
	  int portIdxTemp        = apvIdxTemp/ApvRoPerPort; // 0: 0-7 | 1: 12-19
	  int refApvIdxTemp      = apvIdxTemp - portIdxTemp*ApvNumOffset + portIdxTemp*ApvPerPort; // 0-15
	  int glbElecChanIdxTemp = (rdoIdxTemp-1)*ChPerRdo + armIdxTemp*ChPerArm + refApvIdxTemp*ChPerApv + chanIdxTemp; // 0-36863
	  fstPedestal[glbElecChanIdxTemp] = pp; // pedestal
	  fstRmsNoise[glbElecChanIdxTemp] = rr; // total noise
	  fstRanNoise[glbElecChanIdxTemp] = nn; // random noise
	}
      }
      tableFound = true;
      fclose(file0);
    }
  }
  else {
    //LOG(U_FST,"loading pedestals from %s ", paraDir);
    while(!feof(file0)) {
      int rdoIdxTemp=0, armIdxTemp=0, apvIdxTemp=0, chanIdxTemp=0, tbIdxTemp=0;
      float pp=0., rr=0., nn=0.;
      char buff[256];

      if(fgets(buff,sizeof(buff),file0) == 0) continue ;
      switch(buff[0]) {
	case '#' :
	case '!' :
	case '*' :
	case '/' :
	case '.' :
	  continue ;
      }
      int ret = sscanf(buff,"%d %d %d %d %d %f %f %f",&rdoIdxTemp,&armIdxTemp,&apvIdxTemp,&chanIdxTemp,&tbIdxTemp,&pp,&rr,&nn);
      if(ret!=8) continue;

      if(tbIdxTemp==2) { //only take time bin 2 as sample
	int portIdxTemp        = apvIdxTemp/ApvRoPerPort; // 0: 0-7 | 1: 12-19
	int refApvIdxTemp      = apvIdxTemp - portIdxTemp*ApvNumOffset + portIdxTemp*ApvPerPort; // 0-15
	int glbElecChanIdxTemp = (rdoIdxTemp-1)*ChPerRdo + armIdxTemp*ChPerArm + refApvIdxTemp*ChPerApv + chanIdxTemp; // 0-36863
	fstPedestal[glbElecChanIdxTemp] = pp; // pedestal
	fstRmsNoise[glbElecChanIdxTemp] = rr; // total noise
	fstRanNoise[glbElecChanIdxTemp] = nn; // random noise
	// cout << "glbElecChanIdxTemp = " << glbElecChanIdxTemp << ", rdoIdxTemp = " << rdoIdxTemp << ", armIdxTemp = " << armIdxTemp << ", apvIdxTemp = " << apvIdxTemp << "chanIdxTemp = " << "pp = " << pp << ", rr = " << rr << ", nn = " << nn << endl;
      }
    }
    tableFound = true;
    fclose(file0);
  }

  /*
  // will active once needed
  sprintf(paraDir, "%s/fst/fst_apv_bad.txt", clientdatadir);
  //LOG(U_FST,"Loading file %s",paraDir);
  FILE *file1;
  file1 = fopen(paraDir,"rb");
  if(file1==0){
    LOG(WARN,"ped::misconfigured apv table can't open input file \"%s\" [%s]", paraDir, strerror(errno));
  }else{
    int c=0, ret=-1;
    long offset=0;
    int runTemp=-1,rdoTemp=-1,armTemp=-1,groupTemp=-1,apvTemp=-1,tmp1=-1;
    fseek(file1,0,SEEK_END);
    while(1){
      c = fgetc(file1);
      char buff[256];
      if(c=='\n'){
	offset = ftell(file1);
	fgets(buff,256,file1);
	fseek(file1,offset-2,SEEK_SET);
	ret = sscanf(buff,"%d %d %d %d %d %d",&runTemp,&rdoTemp,&armTemp,&groupTemp,&apvTemp,&tmp1);
	if(ret!=6){ 
	  // LOG(U_FST,"Wrong input:%s",buff);
	}else{
	  if(runTemp<run&&runTemp>10000000) break;
	  else if(runTemp==run){
	    LOG(DBG,"misconfigure mask rdo %d arm %d apv %d", rdoTemp, armTemp, groupTemp*ApvPerPort+apvTemp);
	    int apvId = (rdoTemp-1)*ApvPerRdo + armTemp*ApvPerArm + groupTemp*ApvPerPort + apvTemp;
	    for(int i=0;i<ChPerApv;i++){
	      int chId  = apvId*ChPerApv+i;
	      int geoId = fstGeomMapping[chId];
	      isChannelBad[geoId] = true;
	    }
	  }
	}
      }else if(fseek(file1,-2,SEEK_CUR)==-1){
	fseek(file1,0,SEEK_SET);
	fgets(buff,256,file1);

	ret = sscanf(buff,"%d %d %d %d %d %d",&runTemp,&rdoTemp,&armTemp,&groupTemp,&apvTemp,&tmp1);
	if(ret!=6) LOG(WARN,"Wrong input:%s",buff);
	else{
	  if(runTemp<run&&runTemp>10000000) break;
	  else if(runTemp==run){
	    int apvId = (rdoTemp-1)*ApvPerRdo + armTemp*ApvPerArm + groupTemp*ApvPerPort + apvTemp;
	    LOG(DBG,"misconfigure mask rdo %d arm %d apv %d", rdoTemp, armTemp, groupTemp*ApvPerPort+apvTemp);
	    for(int i=0;i<ChPerApv;i++){
	      int chId  = apvId*ChPerApv+i;
	      int geoId = fstGeomMapping[chId];
	      isChannelBad[geoId] = true;
	    }
	  }
	}
	break;
      }
    }
    fclose(file1);
  }

  sprintf(paraDir, "%s/fst/fst_bad_channels.txt", clientdatadir);
  //LOG(U_FST,"Loading file %s",paraDir);
  FILE *file2;
  file2 = fopen(paraDir,"rb");
  if(file2==0){
    LOG(WARN,"ped::fst bad channel list can't open input file \"%s\" [%s]", paraDir, strerror(errno));
  }else{
    while(!feof(file2)) {
      int r, arm, apv, ch  ;
      int apvId, chId, geoId;
      char buff[256] ;

      if(fgets(buff,sizeof(buff),file2) == 0) continue ;

      switch(buff[0]) {
	case '#' :
	case '!' :
	case '*' :
	case '/' :
	case '.' :
	  continue ;
      }

      int ret = sscanf(buff,"%d %d %d %d",&r,&arm,&apv,&ch) ;
      if(ret != 4) continue ;

      //check for negative 0!
      char ca[4][16] ;
      char n[4] ;
      memset(n,0,sizeof(n)) ;
      sscanf(buff,"%s %s %s %s",ca[0],ca[1],ca[2],ca[3]) ;
      for(int i=0;i<4;i++) {
	int dummy ;
	if(sscanf(ca[i],"%d",&dummy)!=1) continue ;
	if(dummy==0) {
	  if(index(ca[i],'-')) n[i] = '-' ;
	  else n[i] = '+' ;
	}
	else {
	  if(dummy<0) n[i] = '-' ;
	  else n[i] = '+' ;
	}
      }

      if(r<0) r *= -1 ;
      if(arm < 0) arm *= -1 ;
      if(apv < 0) apv *= -1 ;
      if(ch < 0) ch *= -1 ;

      if(n[1]=='-') {	//nix ARM
	for(int a=0;a<ApvPerArm;a++) {
	  for(int c=0;c<ChPerApv;c++) {
	    apvId = (r-1)*ArmPerRdo*ApvPerArm + arm*ApvPerArm + a;
	    chId  = apvId*ChPerApv + c;
	    geoId = fstGeomMapping[chId];
	    isChannelBad[geoId-1] = true;
	  }
	  LOG(DBG,"mask rdo %d arm %d apv %d", r, arm, a);
	}
      }
      else if(n[2]=='-') {	//nix APV
	for(int c=0;c<ChPerApv;c++) {

	  apvId = (r-1)*ArmPerRdo*ApvPerArm + arm*ApvPerArm + apv;
	  chId  = apvId*ChPerApv + c;
	  geoId = fstGeomMapping[chId];
	  isChannelBad[geoId-1] = true;
	}
	LOG(DBG,"mask rdo %d arm %d apv %d", r, arm, apv);
      }
      else {
	apvId = (r-1)*ArmPerRdo*ApvPerArm + arm*ApvPerArm + apv;
	chId  = apvId*ChPerApv + ch;
	geoId = fstGeomMapping[chId];
	isChannelBad[geoId] = true;
	LOG(DBG,"mask rdo %d arm %d apv %d ch %d", r, arm, apv, ch);
      }
    }
    fclose(file2);
  }

  sprintf(paraDir, "%s/fst/fst_noisy_chips.txt", clientdatadir);
  //LOG(U_FST,"Loading file %s",paraDir);
  FILE *file3;
  file3 = fopen(paraDir,"rb");
  if(file3==0){
    LOG(WARN,"ped::fst noisy chip list can't open input file \"%s\" [%s]", paraDir, strerror(errno));
  }else{
    while(!feof(file3)) {
      int r, arm, apv, ch  ;
      int apvId;
      char buff[256] ;

      if(fgets(buff,sizeof(buff),file3) == 0) continue ;

      switch(buff[0]) {
	case '#' :
	case '!' :
	case '*' :
	case '/' :
	case '.' :
	  continue ;
      }

      int ret = sscanf(buff,"%d %d %d",&r,&arm,&apv) ;
      if(ret != 3) continue ;

      apvId = (r-1)*ArmPerRdo*ApvPerArm + arm*ApvPerArm + apv;
      isNoisyApv[apvId] = true;
      LOG(DBG,"mask rdo %d arm %d apv %d", r, arm, apv);

    }
    fclose(file3);
  }
  */

  errorMsg->SetText("No Error Message");    
  sumHistogramsFilled  = 0;  
  t_2min               = time(NULL);
  t_10min              = time(NULL);
  t_120min             = time(NULL);

}

#define safelog(x) ((x > 0) ? log10(x) : 0)
#define MAX_L3_SZ 1000000

// ************FST EVENT******************
// ---------------------------------------
void fstBuilder::event(daqReader *rdr) 
{
  //StTriggerData *trgd = getStTriggerData(rdr);
  //if(!trgd) return;

  /*****      -----bad code
    long long int trgId = rdr->daqbits64;
  //skip zerobias event
  if((trgId>>60) & 0x1){

  if(trgd) delete(trgd);
  return;
  }

  //ZDC vertex
  double mZdcTimeDiff = -9999;
  double mZdcVertex   = -9999;
  int te = trgd->zdcPmtTDC(east,1);
  int tw = trgd->zdcPmtTDC(west,1);
  if(te>20 && te<4000 && tw>20 && tw<4000) { //te=tw=0 if cosmic data, so this vertex cut not applied to cosmic runs
  mZdcTimeDiff = float(tw-te);
  mZdcVertex   = (mZdcTimeDiff / 2.0) * 0.02 * 30;
  mZdcVertex  += (12.88 - .55); //copy from trgBuilder.cxx

  if(fabs(mZdcVertex) > 10.0) {
  //LOG("JEFF", "Skipping evt %d in run %d with vertexZ = %f", trgd->eventNumber(), run, mZdcVertex);
  if(trgd) delete trgd;
  return;  //skip current event if its vertex Z was outside of +-10.0 cm
  }
  }
  if(trgd) delete trgd;  
  */

  //if(trgd) delete trgd;
  // arrays to calculate dynamical common mode noise contribution to this chip in current event
  float sumAdcPerEvent[totAPV][4];
  int counterAdcPerEvent[totAPV][4];
  int counterGoodHitPerEvent[totAPV];
  int counterGoodHitPerEvent_zs[totAPV];
  memset(counterGoodHitPerEvent,0,sizeof(counterGoodHitPerEvent));
  memset(counterGoodHitPerEvent_zs,0,sizeof(counterGoodHitPerEvent_zs));

  int HitCount[totMod]; // for each module per event

  for ( int i=0; i<totCh; i++ )
  {
    maxAdc[i] = 0; maxAdc_zs[i] = 0;  
    maxTimeBin[i] = -1; maxTimeBin_zs[i] = -1; 
  }
  for ( int i=0; i<totMod; i++ )     {    HitCount[i] = 0;   }
  for ( int i=0; i<totAPV; i++ )
  {    
    for(int iRstrip = 0; iRstrip < 4; ++iRstrip)
    {
      cmNoise[i][iRstrip]  = 0;   
    }
  }

  numTb = numTimeBin;        		//default: 9 timebins
  memset( chCntDaq,  0, sizeof(chCntDaq) );
  memset( apvCntDaq, 0, sizeof(apvCntDaq) );

  if( !(evtCt %1000) )     LOG(DBG, "Looking at evt %d",evtCt);

  daq_dta *dd = rdr->det("fst")->get("adc");    
  if ( dd && dd->meta ) {
    apv_meta_t *meta = (apv_meta_t *) dd->meta;

    for ( int r=1; r<=totRdo; r++ ) {                  //1--6 ARCs (ARM Readout Controllers)
      if ( meta->arc[r].present == 0 ) continue ;
      for ( int arm=0; arm<ArmPerRdo; arm++ ) {           //0--2 ARMs (APV Readout Modules) per ARC
	if ( meta->arc[r].arm[arm].present == 0 ) continue ;
	for ( int apv=0; apv<ApvRoPerArm; apv++ ) {         //0--7 & 12--19 APV chips per ARM
	  if ( meta->arc[r].arm[arm].apv[apv].present == 0 ) continue ;

	  int Tb = meta->arc[r].arm[arm].apv[apv].ntim;
	  if( numTb != 0 && Tb != 0 && numTb != Tb ) {
	    //printf("Different number of timebins in different APV!!! Taking real one!!!\n");
	    numTb = Tb;
	  }
	  hEventSumContents.hSumTB->Fill(numTb);
	}
      }
    }   
  }

  // check if we have to look for the zs data
  size_t evtSize = 0;
  daq_dta *ddZS = rdr->det("fst")->get("zs");
  while( ddZS && ddZS->iterate() ) {
    fgt_adc_t *f_zs = (fgt_adc_t *) ddZS->Void ;
    evtSize += ddZS->ncontent * sizeof(fgt_adc_t);

    // if ( ddZS->pad < 0 || ddZS->pad > 23 )        continue;      //valid APV numbering: 0, 1, ..., 23
    // if ( ddZS->sec < 0 || ddZS->sec > 5 )         continue;      //valid ARM numbering: 0, 1, ..., 5
    // if ( ddZS->rdo < 1 || ddZS->rdo > 6 )         continue;      //valid ARC numbering: 1, 2, ..., 6
    if ( ddZS->pad < 0 || (ddZS->pad > 7 && ddZS->pad < 12) || ddZS->pad > 19) continue; //valid APV numbering: 0-7 & 12-19
    if ( ddZS->sec < 0 || ddZS->sec > 2 ) continue; //valid ARM numbering: 0, 1, 2
    if ( ddZS->rdo < 1 || ddZS->rdo > 6 ) continue; //valid ARC numbering: 1, 2, ..., 6

    // int elecApvId = (ddZS->rdo-1)*ArmPerRdo*ApvPerArm + ddZS->sec*ApvPerArm + ddZS->pad;
    int rdoIdx        = ddZS->rdo; // 1-6
    int armIdx        = ddZS->sec; // 0-2
    int apvIdx        = ddZS->pad; // 0-23
    int portIdx       = apvIdx/ApvRoPerPort; // 0: 0-7 | 1: 12-19
    int refApvIdx     = apvIdx - portIdx*ApvNumOffset + portIdx*ApvPerPort; // 0-15
    int glbElecApvIdx = (rdoIdx-1)*ApvPerRdo + armIdx*ApvPerArm + refApvIdx; // 0-287

    int cou_zs[ChPerApv];
    memset(cou_zs,0,sizeof(cou_zs));
    //loop current APV chip
    for ( uint32_t i=0; i<ddZS->ncontent; i++ ) {
      if ( f_zs[i].ch  < 0 || f_zs[i].ch  > 127 )    continue;//valid Channel numbering: 0, 1, ..., 127
      if ( f_zs[i].tb  < 0 || f_zs[i].tb  > numTb )  continue;//valid Time bin numbering: 0, 1, ..., numTb
      if ( f_zs[i].adc > 4095 )   continue;//valid ADC counts from 0 to 4095

      // Int_t channelId_zs = (ddZS->rdo-1)*ArmPerRdo*ApvPerArm*ChPerApv + ddZS->sec*ApvPerArm*ChPerApv + ddZS->pad*ChPerApv + f_zs[i].ch;
      int glbElecChanId_zs = (rdoIdx-1)*ChPerRdo + armIdx*ChPerArm + refApvIdx*ChPerApv + f_zs[i].ch; // 0-36863
      if(glbElecChanId_zs < 0 || glbElecChanId_zs >= totCh)   continue;
      int glbGeomChanId_zs    = fstGeomMapping[glbElecChanId_zs]; // 0-36863
      if ( isChannelBad[glbGeomChanId_zs] )     continue;

      //max ADC and its time bin index decision
      if ( f_zs[i].adc > maxAdc_zs[glbGeomChanId_zs] ) {
	maxAdc_zs[glbGeomChanId_zs]     = f_zs[i].adc;
	maxTimeBin_zs[glbGeomChanId_zs] = f_zs[i].tb;
      }
      // if ( f_zs[i].adc > hitCut * ranStdDevs[glbGeomChanId_zs]) {
      if ( f_zs[i].adc > hitCut * fstRanNoise[glbElecChanId_zs]) {
	cou_zs[f_zs[i].ch]++;
      }
    }//end current APV loop

    // zero out hits less than 2 TBs
    for(int i=0;i<ChPerApv;i++){
      if(cou_zs[i]<2){
	int glbElecChanId         = (rdoIdx-1)*ChPerRdo + armIdx*ChPerArm + refApvIdx*ChPerApv + i; // 0-36863
	int glbGeomChanId         = fstGeomMapping[glbElecChanId];                                  // 0-36863
	maxAdc_zs[glbGeomChanId]     = 0;
	maxTimeBin_zs[glbGeomChanId] = -1;
      }else{
	counterGoodHitPerEvent_zs[glbElecApvIdx]++;
      }
    }
  }//end all RDO, ARM, APV loops

  //do for zs data and fill with num kB  
  if(ddZS) {
    hEventSumContents.hEventSize->Fill(short(evtSize/1024));
    evtSize = 0;
  }


  // don't use zs data to fill histos...
  while(dd && dd->iterate()) { 
    fgt_adc_t *f = (fgt_adc_t *) dd->Void ;
    evtSize += dd->ncontent * sizeof(fgt_adc_t);

    // if ( dd->pad < 0 || dd->pad > 23 )        continue;      //valid APV numbering: 0, 1, ..., 23
    // if ( dd->sec < 0 || dd->sec > 5 )         continue;      //valid ARM numbering: 0, 1, ..., 5
    // if ( dd->rdo < 1 || dd->rdo > 6 )         continue;      //valid ARC numbering: 1, 2, ..., 6
    if ( dd->pad < 0 || (dd->pad > 7 && dd->pad < 12) || dd->pad > 19) continue; //valid APV numbering: 0-7 & 12-19
    if ( dd->sec < 0 || dd->sec > 2 ) continue; //valid ARM numbering: 0, 1, 2
    if ( dd->rdo < 1 || dd->rdo > 6 ) continue; //valid ARC numbering: 1, 2, ..., 6

    Int_t currentAPV = -1;
    for (int apvIdx=0; apvIdx<totAPV; apvIdx++ ){
      for(int iRstrip = 0; iRstrip < 4; ++iRstrip)
      {
	sumAdcPerEvent[apvIdx][iRstrip]     = 0.;
	counterAdcPerEvent[apvIdx][iRstrip] = 0 ;
      }
    }

    // int elecApvId = (dd->rdo-1)*ArmPerRdo*ApvPerArm + dd->sec*ApvPerArm + dd->pad;
    int rdoIdx        = dd->rdo; // 1-6
    int armIdx        = dd->sec; // 0-2
    int apvIdx        = dd->pad; // 0-23
    int portIdx       = apvIdx/ApvRoPerPort;                                               // 0: 0-7 | 1: 12-19
    int refApvIdx     = apvIdx - portIdx*ApvNumOffset + portIdx*ApvPerPort;                // 0-15
    int glbElecApvIdx = (rdoIdx-1)*ApvPerRdo + armIdx*ApvPerArm + refApvIdx;               // 0-287
    int glbSecIdx     = (rdoIdx-1)*SecPerRdo + armIdx*SecPerArm + refApvIdx/ApvPerSec;     // 0-71
    int glbModuleIdx  = (rdoIdx-1)*ModPerRdo + armIdx*ModPerArm + refApvIdx/ApvPerMod + 1; // 1-36

    bool isFilled = false;
    int cou[ChPerApv];
    memset(cou,0,sizeof(cou));
    for ( uint32_t i=0; i<dd->ncontent; i++ ) { //loop current APV chip
      //non-ZS data
      if ( f[i].ch  < 0 || f[i].ch  > 127 )       continue;      //valid Channel numbering: 0, 1, ..., 127 
      if ( f[i].tb  < 0 || f[i].tb  > numTb )     continue;      //valid Time bin numbering: 0, 1, ..., numTb-1 (default 9 time bins, or 4, 5)
      if ( f[i].adc < 0 || f[i].adc > 4095 )      continue;      //valid ADC counts from 0 to 4095

      //count the found channel/APV (only taking time bin 0 to avoid double counting)
      if ( f[i].tb == 0 ) {	
	chCntDaq[dd->rdo-1][dd->sec][refApvIdx]++;  

	if(!isFilled) {
	  apvCntDaq[glbSecIdx]++; //make sure only count one time
	  isFilled = true;
	}
      }

      // Int_t channelId = (dd->rdo-1)*ArmPerRdo*ApvPerArm*ChPerApv + dd->sec*ApvPerArm*ChPerApv + dd->pad*ChPerApv + f[i].ch;
      int glbElecChanId = (rdoIdx-1)*ChPerRdo + armIdx*ChPerArm + refApvIdx*ChPerApv + f[i].ch; // 0-36863
      int refElecChanId = glbElecChanId - (rdoIdx-1)*ChPerRdo - armIdx*ChPerArm;                // 0-2047
      int lclElecChanId = refElecChanId - portIdx*ChPerMod;                                     // 0-1023
      int sigElecChanId = lclElecChanId%ChPerApv;                                               // 0-127
      if(glbElecChanId < 0 || glbElecChanId >= totCh) continue;

      int glbGeomChanId = fstGeomMapping[glbElecChanId];                                  // 0-36863
      int diskIdx       = glbGeomChanId/ChPerDisk + 1;                                    // 1-3
      int moduleIdx     = (glbGeomChanId-(diskIdx-1)*ChPerDisk)/ChPerMod + 1;             // 1-12
      int lclGeomChanId = glbGeomChanId - (diskIdx-1)*ChPerDisk - (moduleIdx-1)*ChPerMod; // 0-1023
      int lclRstripIdx  = lclGeomChanId/PhiSegPerMod;                                     // 0-7
      int lclPhiSegIdx  = lclGeomChanId%PhiSegPerMod;                                     // 0-127
      currentAPV        = glbElecApvIdx;

      if ( isChannelBad[glbGeomChanId] ) continue;  //

      //fill ADC value vs lclElecChanId per module
      hAdcContents.adcArray[glbModuleIdx-1]->Fill(lclElecChanId, f[i].adc);

      //calculate mean pedestal and RMS
      if(!tableFound) {
	numVals[glbGeomChanId]++;
	aVals[glbGeomChanId]           += f[i].adc;
	runningAvg[glbGeomChanId]      += (f[i].adc-runningAvg[glbGeomChanId]) / numVals[glbGeomChanId];
	runningStdDevSq[glbGeomChanId] += ((float)numVals[glbGeomChanId]-1)/(numVals[glbGeomChanId]) * (f[i].adc-runningAvg[glbGeomChanId]) * (f[i].adc-runningAvg[glbGeomChanId]);
	oldStdDevs[glbGeomChanId]       = sqrt(runningStdDevSq[glbGeomChanId] / numVals[glbGeomChanId]);
	fstRanNoise[glbElecChanId]      = fstRmsNoise[glbElecChanId];
      }
      else {
	numVals[glbGeomChanId]++;
	runningAvg[glbGeomChanId] = fstPedestal[glbElecChanId];
	oldStdDevs[glbGeomChanId] = fstRmsNoise[glbElecChanId];
      }
      //channel status decision
      Bool_t isBad = false;
      if ( runningAvg[glbGeomChanId] < minPedVal || runningAvg[glbGeomChanId] > maxPedVal ) isBad = true;
      if ( oldStdDevs[glbGeomChanId] < minRMSVal || oldStdDevs[glbGeomChanId] > maxRMSVal ) isBad = true;
      if(isBad) continue;

      //fill pedestal-subtracted ADC vs time bin index
      hTbVsAdcContents.tbVsAdcArray[glbSecIdx]->Fill(f[i].tb, f[i].adc - (int)(runningAvg[glbGeomChanId]+0.5));

      //count channel whose pedestal subtracted ADC yield one RMS
      if ( (f[i].adc-runningAvg[glbGeomChanId])>oldStdDevs[glbGeomChanId] && oldStdDevs[glbGeomChanId]>0 ) 
	numOverOneSig[glbGeomChanId]++; 

      //max ADC and its time bin index decision
      if ( (f[i].adc - runningAvg[glbGeomChanId] )> maxAdc[glbGeomChanId] ) {
	maxAdc[glbGeomChanId]     = f[i].adc - runningAvg[glbGeomChanId];
	maxTimeBin[glbGeomChanId] = f[i].tb;
      }
      if ( f[i].adc > runningAvg[glbGeomChanId] + hitCut * oldStdDevs[glbGeomChanId] ){
	cou[f[i].ch]++;
      }

      //counts for dynamical common mode noise calculation
      if ( f[i].tb==(numTb-1) ) {       //only take last time bin
	//exclude signal-related channels for common mode noise calculation
	if ( oldStdDevs[glbGeomChanId]>0 && abs(maxAdc[glbGeomChanId] < cmnCut*oldStdDevs[glbGeomChanId]) ) {
	  int rIdx = lclRstripIdx < 4 ? lclRstripIdx:lclRstripIdx-4;
	  sumAdcPerEvent[glbElecApvIdx][rIdx] += (maxAdc[glbGeomChanId]+runningAvg[glbGeomChanId]);
	  counterAdcPerEvent[glbElecApvIdx][rIdx]++;
	}
      }
    } //end current APV chip loops

    // zero out hits less than 2 TBs
    for(int i=0;i<ChPerApv;i++){
      if(cou[i]<2){
	int glbElecChanId         = (rdoIdx-1)*ChPerRdo + armIdx*ChPerArm + refApvIdx*ChPerApv + i; // 0-36863
	int glbGeomChanId         = fstGeomMapping[glbElecChanId];                                  // 0-36863
	maxAdc[glbGeomChanId]     = 0;
	maxTimeBin[glbGeomChanId] = -1;
      }else{
	counterGoodHitPerEvent[glbElecApvIdx]++;
      }
    }

    //calculate dynamical common mode noise for current event
    if ( counterAdcPerEvent[currentAPV] > 0 && currentAPV > -1) {
      for(int iRstrip = 0; iRstrip < 4; ++iRstrip)
      {
	cmNoise[currentAPV][iRstrip] = sumAdcPerEvent[currentAPV][iRstrip] / counterAdcPerEvent[currentAPV][iRstrip];
	hCmnTemp.hCmnPerChip[currentAPV][iRstrip]->Fill(short(cmNoise[currentAPV][iRstrip]+0.5));
      }
    }
  }//end all RDO, ARM, APV chips loops

  //makes only sense for non zero suppressed data....
  if ( dd ) {
    int goodAPVs = 0;
    for ( int iRdo=0; iRdo<totRdo; iRdo++ ) {
      for ( int iArm=0; iArm<ArmPerRdo; iArm++ ) {
	for ( int iApv=0; iApv<ApvPerArm; iApv++ ) {
	  if ( chCntDaq[iRdo][iArm][iApv] > goodChCut ) 		    
	    goodAPVs++;
	}
      }
    }
    hEventSumContents.hApvCorpt->Fill(goodAPVs);

    for(int iSec=0; iSec<totSec; iSec++) 
    {
      int rdoIdx  = iSec/SecPerRdo + 1; //1-6
      int diskIdx = (rdoIdx-1)/2 + 1;   // 1-3
      hSumContents.hVisibleApv[diskIdx-1]->Fill(iSec+1-(diskIdx-1)*SecPerDisk, apvCntDaq[iSec]);
    }

    //do for zs and non-zs data and fill with num kB
    hEventSumContents.hEventSize->Fill(short(evtSize/1024));
  }

  //counting analyzed event number
  evtCt++;

  //count hit per module per disk per Event and fill hit map and MIP
  for(int geoIdx=0; geoIdx<totCh; geoIdx++) { // loop over glbGeomChanId
    int diskIdx       = geoIdx/ChPerDisk + 1;                                   // 1-3
    int moduleIdx     = (geoIdx-(diskIdx-1)*ChPerDisk)/ChPerMod + 1;            // 1-12
    int glbModuleIdx  = (diskIdx-1)*ModPerDisk + moduleIdx;                     // 1-36
    int lclGeomChanId = geoIdx- (diskIdx-1)*ChPerDisk - (moduleIdx-1)*ChPerMod; // 0-1023
    int lclRstripIdx  = lclGeomChanId/PhiSegPerMod;                             // 0-7
    int lclPhiSegIdx  = lclGeomChanId%PhiSegPerMod;                             // 0-127
    int glbPhiSegIdx  = (moduleIdx-1)*PhiSegPerMod + lclPhiSegIdx;              // 0-1535

    int glbElecChanId = fstElecMapping[geoIdx];                                        // 0-36863
    int rdoIdx        = glbElecChanId/ChPerRdo + 1;                                    // 1-6
    int armIdx        = (glbElecChanId - (rdoIdx-1)*ChPerRdo)/ChPerArm;                // 0-2
    int refElecChanId = glbElecChanId - (rdoIdx-1)*ChPerRdo - armIdx*ChPerArm;         // 0-2047
    int refApvIdx     = refElecChanId/ChPerApv;                                        // 0-15
    int glbElecApvIdx = (rdoIdx-1)*ApvPerRdo + armIdx*ApvPerArm + refApvIdx;           // 0-287
    int portIdx       = refApvIdx/ApvPerPort;                                          // 0-1
    int lclApvIdx     = refApvIdx-portIdx*ApvPerPort;                                  // 0-7
    int lclElecChanId = refElecChanId - portIdx*ChPerMod;                              // 0-1023
    int sigElecChanId = lclElecChanId%ChPerApv;                                        // 0-127
    int glbSecIdx     = (rdoIdx-1)*SecPerRdo + armIdx*SecPerArm + refApvIdx/ApvPerSec; // 0-71

    //float pedestal   = runningAvg[geoIdx-1];
    float rms   = oldStdDevs[geoIdx];
    float ran   = fstRanNoise[glbElecChanId];
    int adc_max = maxAdc[geoIdx];
    int tb_max  = maxTimeBin[geoIdx];

    // non-ZS data
    if( adc_max>hitCut*rms && rms > minRMSVal && rms < maxRMSVal ){
      if( !isNoisyApv[glbElecApvIdx] || (isNoisyApv[glbElecApvIdx] && adc_max>noiseChipCut*rms)){
	if(counterGoodHitPerEvent[glbElecApvIdx]<=hitOccupancyCut){
	  HitCount[glbModuleIdx-1]++;
	  hSumContents.hHitMapVsAPV[diskIdx-1]->Fill(moduleIdx, lclApvIdx);
	  hMipContents.mipArray[glbSecIdx]->Fill(short(adc_max+0.5));
	  if(tb_max>=0) hEventSumContents.hMaxTimeBin->Fill(tb_max);
	}
	//keep monitoring hot chips
	hHitMapContents.hitMapArray[glbModuleIdx-1]->Fill(lclPhiSegIdx, lclRstripIdx);
	hSumContents.hHitMap[diskIdx-1]->Fill(glbPhiSegIdx, lclRstripIdx);

	// FST Geometry Hit Map
	float phiInner = -999.9;
	float phiOuter = -999.9;
	float phiSeg   = -999.9;
	float rStrip   = -999.9;
	if(diskIdx == 1 || diskIdx == 3)
	{ // Disk 1 & 3
	  phiInner = phiStart[moduleIdx-1]*TMath::Pi()/6.0 + 0.5*zDirct[moduleIdx-1]*phiDelta;
	  phiOuter = phiStop[moduleIdx-1]*TMath::Pi()/6.0  - 0.5*zDirct[moduleIdx-1]*phiDelta;
	}
	if(diskIdx == 2)
	{ // Disk 2
	  phiInner = phiStop[moduleIdx-1]*TMath::Pi()/6.0  - 0.5*zDirct[moduleIdx-1]*phiDelta;
	  phiOuter = phiStart[moduleIdx-1]*TMath::Pi()/6.0 + 0.5*zDirct[moduleIdx-1]*phiDelta;
	}

	if(lclRstripIdx < RstripPerMod/2)
	{ // inner
	  phiSeg = phiInner + zFilp[diskIdx-1]*zDirct[moduleIdx-1]*lclPhiSegIdx*phiDelta;
	  rStrip = rStart[lclRstripIdx] + 0.5*rDelta;
	}
	else
	{
	  // outer
	  phiSeg = phiOuter - zFilp[diskIdx-1]*zDirct[moduleIdx-1]*lclPhiSegIdx*phiDelta;
	  rStrip = rStart[lclRstripIdx] + 0.5*rDelta;
	}
	hSumContents.hPolyHitMap[diskIdx-1]->Fill(phiSeg, rStrip);
      }
    }

    //ZS data
    if( maxAdc_zs[geoIdx] > hitCut*ran && ran > minRMSVal && ran < maxRMSVal ) {//roughly cut
      if( !isNoisyApv[glbElecApvIdx] || (isNoisyApv[glbElecApvIdx] && maxAdc_zs[geoIdx] > noiseChipCut*ran)){
	if(counterGoodHitPerEvent_zs[glbElecApvIdx]<=hitOccupancyCut){
	  hMipContents.mipArray[glbSecIdx+totSec]->Fill(short(maxAdc_zs[geoIdx]+0.5));
	  if(maxTimeBin_zs[geoIdx]>=0){
	    hEventSumContents.hMaxTimeBin_ZS->Fill(maxTimeBin_zs[geoIdx]);
	    hMaxTimeBinContents.maxTimeBinArray[glbSecIdx]->Fill(maxTimeBin_zs[geoIdx]);
	  }
	  hSumContents.hHitMapVsAPV_ZS[diskIdx-1]->Fill(moduleIdx, lclApvIdx);
	}
	//keep monitoring hot chips
	hSumContents.hHitMap_ZS[diskIdx-1]->Fill(glbPhiSegIdx, lclRstripIdx);

	// FST Geometry Hit Map
	float phiInner = -999.9;
	float phiOuter = -999.9;
	float phiSeg   = -999.9;
	float rStrip   = -999.9;
	if(diskIdx == 1 || diskIdx == 3)
	{ // Disk 1 & 3
	  phiInner = phiStart[moduleIdx-1]*TMath::Pi()/6.0 + 0.5*zDirct[moduleIdx-1]*phiDelta;
	  phiOuter = phiStop[moduleIdx-1]*TMath::Pi()/6.0  - 0.5*zDirct[moduleIdx-1]*phiDelta;
	}
	if(diskIdx == 2)
	{ // Disk 2
	  phiInner = phiStop[moduleIdx-1]*TMath::Pi()/6.0  - 0.5*zDirct[moduleIdx-1]*phiDelta;
	  phiOuter = phiStart[moduleIdx-1]*TMath::Pi()/6.0 + 0.5*zDirct[moduleIdx-1]*phiDelta;
	}

	if(lclRstripIdx < RstripPerMod/2)
	{ // inner
	  phiSeg = phiInner + zFilp[diskIdx-1]*zDirct[moduleIdx-1]*lclPhiSegIdx*phiDelta;
	  rStrip = rStart[lclRstripIdx] + 0.5*rDelta;
	}
	else
	{
	  // outer
	  phiSeg = phiOuter - zFilp[diskIdx-1]*zDirct[moduleIdx-1]*lclPhiSegIdx*phiDelta;
	  rStrip = rStart[lclRstripIdx] + 0.5*rDelta;
	}
	hSumContents.hPolyHitMap_ZS[diskIdx-1]->Fill(phiSeg, rStrip);
      }
    }
  }

  //fill hit multiplicity per module per event
  for ( int i=0; i<totMod; i++) { 
    hMultContents.multArray[i]->Fill(HitCount[i]);
    int diskIdx = i/ModPerDisk+1;
    int moduleIdx = i - (diskIdx-1)*ModPerDisk;
    hSumContents.hMultVsModule[diskIdx-1]->Fill(moduleIdx, HitCount[i]<101?HitCount[i]:100.5);
  }

  //getting MPV value and CM noise every 50 evts for each section
  //if(!(evtCt%100)) 
  //	cout << "Analyzing event: " << evtCt << endl;

  // Reset rolling histos if necessary..
  int tm = time(NULL);
  if ( (tm > t_10min + 10) || (!(evtCt%100)) ) {
    t_10min = tm;
    fillSumHistos();
  }
  // End Fill Histograms...
}

//right now all time bins are summed, so what is shown in mean and std dev is the mean over all tb for a channel

// ********FST FILL SUM HISTOS****************
// -------------------------------------------
void fstBuilder::fillSumHistos() 
{
  char buffer[200];
  hEventSumContents.hMeanPed->Reset();  // mean ped
  hEventSumContents.hMeanRMS->Reset();  // sigma
  hEventSumContents.hMeanRan->Reset();  // sigma
  hEventSumContents.hSumBad->Reset();   // #goodapv

  int numGood[totAPV];
  for(int i=0; i<totAPV; i++)  numGood[i] = 0;

  int numBadAll =0;
  sumHistogramsFilled++;

  for ( int geoIdx=0; geoIdx<totCh; geoIdx++ ) {
    int diskIdx       = geoIdx/ChPerDisk + 1;                                   // 1-3
    int glbElecChanId = fstElecMapping[geoIdx];                                 // 0-36863
    int rdoIdx        = glbElecChanId/ChPerRdo + 1;                             // 1-6
    int armIdx        = (glbElecChanId - (rdoIdx-1)*ChPerRdo)/ChPerArm;         // 0-2
    int refElecChanId = glbElecChanId - (rdoIdx-1)*ChPerRdo - armIdx*ChPerArm;  // 0-2047
    int refApvIdx     = refElecChanId/ChPerApv;                                 // 0-15
    int glbElecApvIdx = (rdoIdx-1)*ApvPerRdo + armIdx*ApvPerArm + refApvIdx;    // 0-287

    float pedestal = runningAvg[geoIdx];
    float rmsPed   = oldStdDevs[geoIdx];
    float ranPed   = fstRanNoise[glbElecChanId];
    bool  isBad    = false;

    if ( rmsPed > 0 ) {
      hEventSumContents.hMeanRMS->Fill(short(rmsPed+0.5));
      hSumContents.hSumSig[diskIdx-1]->Fill(geoIdx-(diskIdx-1)*ChPerDisk, short(rmsPed+0.5));
    }

    if ( ranPed > 0 ) {
      hEventSumContents.hMeanRan->Fill(short(ranPed+0.5));
      hSumContents.hSumRan[diskIdx-1]->Fill(geoIdx-(diskIdx-1)*ChPerDisk, short(ranPed+0.5));
    }

    if ( pedestal > 0 ) {
      hEventSumContents.hMeanPed->Fill(short(pedestal+0.5));
      hSumContents.hSumPed[diskIdx-1]->Fill(geoIdx-(diskIdx-1)*ChPerDisk, short(pedestal+0.5));
    }

    if ( numVals[geoIdx] > 0 ) {
      if ( pedestal < minPedVal || pedestal > maxPedVal || rmsPed < minRMSVal || rmsPed > maxRMSVal ) {
	isBad = true;
	numBadAll++;
      }
      else
	numGood[glbElecApvIdx]++;
    }
  }

  for(int idx=0; idx<totAPV; idx++)
    hEventSumContents.hSumBad->SetBinContent(idx, numGood[idx]);

  sprintf(buffer, "#color[4]{You seem to have %d bad channels that are not masked}", numBadAll);  
  errorMsg->SetText(buffer);    
}

// ***********FST STOP RUN*************************
// ------------------------------------------------
void fstBuilder::stoprun(daqReader *rdr) 
{
  //common mode noise
  for( int k=0; k<totAPV; k++ ) {
    int diskIdx = k/ApvPerDisk + 1; // 1-3
    for(int iRstrip = 0; iRstrip < 4; ++iRstrip)
    {
      hSumContents.hCommonModeNoise[diskIdx-1]->Fill(4*(k-(diskIdx-1)*ApvPerDisk)+iRstrip, short(hCmnTemp.hCmnPerChip[k][iRstrip]->GetRMS()+0.5));
    }
  }

  int errCt_visibleAPVperSection = 0, errCt_maxTimeBinFraction = 0, errCt_mipNonZS = 0, errCt_mipZS = 0;
  int errLocation_visibleAPVperSection[totSec], errLocation_maxTimeBinFraction[totSec], errLocation_mipNonZS[totSec], errLocation_mipZS[totSec]; 
  int errNumber_visibleAPVperSection[totSec];
  float errValue_maxTimeBinFraction[totSec], errValue_mipNonZS[totSec], errValue_sigmaNonZS[totSec], errValue_mipZS[totSec], errValue_sigmaZS[totSec]; 
  for(int j=0; j<totSec; j++) {
    errLocation_visibleAPVperSection[j] = 0;
    errNumber_visibleAPVperSection[j] = 0;

    errLocation_maxTimeBinFraction[j] = 0;
    errValue_maxTimeBinFraction[j] = 0.;

    errLocation_mipNonZS[j] = 0;
    errValue_mipNonZS[j] = 0.;
    errValue_sigmaNonZS[j] = 0.;

    errLocation_mipZS[j] = 0;
    errValue_mipZS[j] = 0.;
    errValue_sigmaZS[j] = 0.;
  }

  for(int j=0; j<totSec; j++) {
    // int rdoIdx   = j/12 + 1;  //1, 2, ..., 6
    // int armIdx   = (j%12)/2;  //0, 1, ..., 5
    // int portIdx = j%2;       //0, 1
    int rdoIdx    = j/SecPerRdo + 1;          //1-6
    int armIdx    = (j%SecPerRdo)/SecPerArm;  //0-2
    int portIdx   = (j/SecPerMod)%PortPerArm; //0-1
    int secIdx    = j%SecPerMod;              //0-1
    int diskIdx   = (rdoIdx-1)/2+1;                                           // 1-3
    int moduleIdx = (rdoIdx-1)%2*ModPerRdo + armIdx*PortPerArm + portIdx + 1; // 1-12

    if(hSumContents.hVisibleApv[diskIdx-1]->GetEntries()>0) {
      if(hSumContents.hVisibleApv[diskIdx-1]->GetBinContent(j-(diskIdx-1)*SecPerDisk+1, nExpectedChip_Sec[j]+1) < 1) {
	//LOG(U_FST,"visibleAPVperSection::section RDO%d_ARM%d_GROUP%d has missing APVs!", rdoIdx, armIdx, portIdx);
	errLocation_visibleAPVperSection[errCt_visibleAPVperSection] = rdoIdx*100 + armIdx*10 + portIdx;
	for(int jBin=1; jBin<ApvPerSec+1; jBin++) {
	  if(hSumContents.hVisibleApv[diskIdx-1]->GetBinContent(j+1, jBin)>1) 
	    errNumber_visibleAPVperSection[errCt_visibleAPVperSection] = ApvPerSec + 1 - jBin;
	}
	errCt_visibleAPVperSection++;
      }
    }

    double entriesTB_123=0, entriesTB_all=0, fraction = 1.0;
    if(hMaxTimeBinContents.maxTimeBinArray[j]->GetEntries()>0) {
      entriesTB_123 = hMaxTimeBinContents.maxTimeBinArray[j]->Integral(2, numTb-1);
      entriesTB_all = hMaxTimeBinContents.maxTimeBinArray[j]->Integral(1, numTb);
      fraction = entriesTB_123/entriesTB_all;
      if(j==6) fraction = 1.0;
      if(j<36 && fraction<maxTbFracOK) {
	//LOG(U_FST,"maxTimeBinFraction::section RDO%d_ARM%d_GROUP%d with fraction %f!", rdoIdx, armIdx, portIdx, fraction);
	errLocation_maxTimeBinFraction[errCt_maxTimeBinFraction] = rdoIdx*100 + armIdx*10 + portIdx;
	errValue_maxTimeBinFraction[errCt_maxTimeBinFraction] = fraction;
	errCt_maxTimeBinFraction++;
      }
    }
    hEventSumContents.hMaxTBfractionVsSection_ZS->SetBinContent(j+1, fraction);

    float lowerRange=landauFit_dn, upperRange=landauFit_up, mpvMIP_nonZS=0., sigmaMIP_nonZS=0., mpvMIP_ZS=0., sigmaMIP_ZS=0.;
    if(hMipContents.mipArray[j]->GetEntries()>0) {
      if(j%3==1)
	hMipContents.mipArray[j]->Fit("landau","QR","",lowerRange-50, upperRange);
      else
	hMipContents.mipArray[j]->Fit("landau","QR","",lowerRange, upperRange);
      TF1* fit_nonZS = hMipContents.mipArray[j]->GetFunction("landau");
      if(fit_nonZS) {
	mpvMIP_nonZS    = fit_nonZS->GetParameter("MPV");
	sigmaMIP_nonZS  = fit_nonZS->GetParameter("Sigma");

	if(mpvMIP_nonZS<minMipMpv_nonZS || mpvMIP_nonZS>maxMipMpv || sigmaMIP_nonZS<minMipSigma_nonZS || sigmaMIP_nonZS>maxMipSigma) {
	  //LOG(U_FST,"MIP_nonZS::section RDO%d_ARM%d_GROUP%d with MIP mpv %f, sigma %f!", rdoIdx, armIdx, portIdx, mpvMIP_nonZS, sigmaMIP_nonZS);
	  errLocation_mipNonZS[errCt_mipNonZS] = rdoIdx*100 + armIdx*10 + portIdx;
	  errValue_mipNonZS[errCt_mipNonZS] = mpvMIP_nonZS;
	  errValue_sigmaNonZS[errCt_mipNonZS] = sigmaMIP_nonZS;
	  errCt_mipNonZS++;
	}
      }
      else {
	LOG(WARN, "Bad Fit due to non-enough data (non-ZS) for RDO%d_ARM%d_GROUP%d", rdoIdx, armIdx, portIdx);
      }
    }
    // if(j==6) {
    //   mpvMIP_nonZS = 550.;
    //   sigmaMIP_nonZS = 140.;
    // }
    hEventSumContents.hMipMPVvsSection->SetBinContent(j+1, short(mpvMIP_nonZS+0.5));
    hEventSumContents.hMipSIGMAvsSection->SetBinContent(j+1, short(sigmaMIP_nonZS+0.5));


    if(hMipContents.mipArray[j+72]->GetEntries()>0) {
      if((j+72)%3==1)
	hMipContents.mipArray[j+72]->Fit("landau","QR","",lowerRange-50, upperRange);
      else
	hMipContents.mipArray[j+72]->Fit("landau","QR","",lowerRange, upperRange);
      TF1* fit_ZS = hMipContents.mipArray[j+72]->GetFunction("landau");
      if(fit_ZS) {
	mpvMIP_ZS      = fit_ZS->GetParameter("MPV");
	sigmaMIP_ZS    = fit_ZS->GetParameter("Sigma");

	if(mpvMIP_ZS<minMipMpv_ZS || mpvMIP_ZS>maxMipMpv || sigmaMIP_ZS<minMipSigma_ZS || sigmaMIP_ZS>maxMipSigma)  {
	  //LOG(U_FST,"MIP_ZS::section RDO%d_ARM%d_GROUP%d with MIP mpv %f, sigma %f!", rdoIdx, armIdx, portIdx, mpvMIP_ZS, sigmaMIP_ZS);
	  errLocation_mipZS[errCt_mipZS] = rdoIdx*100 + armIdx*10 + portIdx;
	  errValue_mipZS[errCt_mipZS] = mpvMIP_ZS;
	  errValue_sigmaZS[errCt_mipZS] = sigmaMIP_ZS;
	  errCt_mipZS++;
	}
      }
      else {
	LOG(WARN, "Bad Fit due to non-enough data (ZS) for RDO%d_ARM%d_GROUP%d", rdoIdx, armIdx, portIdx);
      }
    }
    // if(j==6) {
    //   mpvMIP_ZS = 550.;
    //   sigmaMIP_ZS = 140.;
    // }
    hEventSumContents.hMipMPVvsSection_ZS->SetBinContent(j+1, short(mpvMIP_ZS+0.5));
    hEventSumContents.hMipSIGMAvsSection_ZS->SetBinContent(j+1, short(sigmaMIP_ZS+0.5));
  }

  hEventSumContents.hMipMPVvsSection->GetYaxis()->SetRangeUser(300, 800);
  hEventSumContents.hMipSIGMAvsSection->GetYaxis()->SetRangeUser(40, 200);
  hEventSumContents.hMipMPVvsSection_ZS->GetYaxis()->SetRangeUser(300, 800);
  hEventSumContents.hMipSIGMAvsSection_ZS->GetYaxis()->SetRangeUser(40, 200);

  TString buffer_Err = "";
  if(errCt_visibleAPVperSection>0) {
    for(int i=0; i<errCt_visibleAPVperSection; i++)
      buffer_Err += Form("RDO%d_ARM%d_GROUP%d (%d missing chips),\t", errLocation_visibleAPVperSection[i]/100, (errLocation_visibleAPVperSection[i]%100)/10, errLocation_visibleAPVperSection[i]%10, errNumber_visibleAPVperSection[i]);
    // LOG(U_FST,"FST visibleAPVperSection:: In #%d, %d sections have missing APV chips, they are %s", run, errCt_visibleAPVperSection, buffer_Err.Data());
  }

  buffer_Err = "";
  if(errCt_maxTimeBinFraction>0) {
    for(int i=0; i<errCt_maxTimeBinFraction; i++)
      buffer_Err += Form("RDO%d_ARM%d_GROUP%d (fraction = %f),\t", errLocation_maxTimeBinFraction[i]/100, (errLocation_maxTimeBinFraction[i]%100)/10, errLocation_maxTimeBinFraction[i]%10, errValue_maxTimeBinFraction[i]);
    // LOG(U_FST,"FST maxTimeBinFraction:: In #%d, %d sections have max time bin fraction less than %f, they are %s", run, errCt_maxTimeBinFraction, maxTbFracOK, buffer_Err.Data());
  }

  buffer_Err = "";
  Int_t entries_ZS = (int)hEventSumContents.hEventSize->Integral(1, 5);
  Int_t entries_nonZS = (int)hEventSumContents.hEventSize->Integral(17, 25);
  //cout << "non-ZS events " << entries_nonZS << "; ZS events " << entries_ZS << endl;
  //if(entries_nonZS>500 && errCt_mipNonZS>0) {
  if(errCt_mipNonZS>0) {
    for(int i=0; i<errCt_mipNonZS; i++)
      buffer_Err += Form("RDO%d_ARM%d_GROUP%d (MPV=%f, Sigma=%f),\t", errLocation_mipNonZS[i]/100, (errLocation_mipNonZS[i]%100)/10, errLocation_mipNonZS[i]%10, errValue_mipNonZS[i], errValue_sigmaNonZS[i]);
    //LOG(U_FST,"FST MIP_nonZS:: In #%d, %d sections have unnormal MIP mpv or sigma for non-ZS data, they are %s", run, errCt_mipNonZS, buffer_Err.Data());
    if(entries_nonZS<500)
      LOG(WARN,"FST MIP_nonZS (limited statistics):: In #%d, %d sections have unnormal MIP mpv or sigma for non-ZS data, they are %s", run, errCt_mipNonZS, buffer_Err.Data());
    else
      LOG(WARN,"FST MIP_nonZS (statistics>=500):: In #%d, %d sections have unnormal MIP mpv or sigma for non-ZS data, they are %s", run, errCt_mipNonZS, buffer_Err.Data());
  }

  buffer_Err = "";
  if(entries_ZS>500 && errCt_mipZS>0) {
    for(int i=0; i<errCt_mipZS; i++)
      buffer_Err += Form("RDO%d_ARM%d_GROUP%d (MPV=%f, Sigma=%f),\t", errLocation_mipZS[i]/100, (errLocation_mipZS[i]%100)/10, errLocation_mipZS[i]%10, errValue_mipZS[i], errValue_sigmaZS[i]);
    // LOG(U_FST,"FST MIP_ZS:: In #%d, %d sections have unnormal MIP mpv or sigma for ZS data, they are %s", run, errCt_mipZS, buffer_Err.Data());
  }

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
  for ( int i=0; i<totCh; i++ )         {    maxAdc[i]          = 0; maxAdc_zs[i]          = 0;  }
  for ( int i=0; i<totCh; i++ )         {    maxTimeBin[i]      = -1; maxTimeBin_zs[i]      = -1;  }
  for ( int i=0; i<totAPV; i++ )
  {    
    for(int iRstrip = 0; iRstrip < 4; ++iRstrip)
    {
      cmNoise[i][iRstrip] = 0;   
    }
  }
}

void fstBuilder::main(int argc, char *argv[])
{
	fstBuilder me;
	me.Main(argc, argv);
}
