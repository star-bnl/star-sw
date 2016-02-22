/* This builder is developed for IST online monitoring
 * Author: Yaping Wang, Zillay Khan
 * Latest updates  4/8/2014  Yaping
 * Latest updates  5/9/2014  Yaping
 * update fit range for MIP peak distribution of center sections  5/12/2014 Yaping 
 * update vertexZ cut for event selection: -10.0 cm < vertexZ < 10.0 cm   6/16/2014 Yaping
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
#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "istBuilder.h"
#include <RTS/include/rtsLog.h>

ClassImp(istBuilder);


const float istBuilder::minMipMpv_ZS      = 450;
const float istBuilder::minMipMpv_nonZS   = 400;
const float istBuilder::maxMipMpv         = 800;
const float istBuilder::minMipSigma_ZS    = 80;
const float istBuilder::minMipSigma_nonZS = 60;
const float istBuilder::maxMipSigma       = 200;
const float istBuilder::maxTbFracOK       = 0.9;
const float istBuilder::landauFit_dn      = 400.0;
const float istBuilder::landauFit_up      = 2000.0;
const float istBuilder::cmnCut            = 3.0;
const float istBuilder::hitCut            = 5.0;
const float istBuilder::noiseChipCut      = 10.0;
const int   istBuilder::hitOccupancyCut  = 25;


istBuilder::istBuilder(JevpServer *parent):JevpBuilder(parent),evtCt(0) {
	plotsetname = (char *)"ist";
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

istBuilder::~istBuilder() {
	// Delete any existing histograms...  
	int nAdcHist      	= sizeof(hAdcContents) / sizeof(TH2 *);
	int nMultHist     	= sizeof(hMultContents) / sizeof(TH1 *);
	int nHitMapHist   	= sizeof(hHitMapContents) / sizeof(TH2 *);
	int nTbVsAdcHist  	= sizeof(hTbVsAdcContents) / sizeof(TH2 *);
	int nEventSumHist 	= sizeof(hEventSumContents) / sizeof(TH1 *);
	int nMipHist      	= sizeof(hMipContents) / sizeof(TH1 *);
	int nMaxTimeBinHist   = sizeof(hMaxTimeBinContents) / sizeof(TH1 *);
	int nSumHist      	= sizeof(hSumContents) / sizeof(TH2 *);  

	for ( int i=0; i<nAdcHist; i++ )      {    if(hAdcContents.adcArray[i])          delete hAdcContents.adcArray[i];    		}
	for ( int i=0; i<nMultHist; i++ )     {    if(hMultContents.multArray[i])        delete hMultContents.multArray[i];    	}
	for ( int i=0; i<nHitMapHist; i++ )   {    if(hHitMapContents.hitMapArray[i])    delete hHitMapContents.hitMapArray[i];    	}
	for ( int i=0; i<nTbVsAdcHist; i++ )  {    if(hTbVsAdcContents.tbVsAdcArray[i])  delete hTbVsAdcContents.tbVsAdcArray[i];    	}
	for ( int i=0; i<nEventSumHist; i++ ) {    if(hEventSumContents.eventSumArray[i])delete hEventSumContents.eventSumArray[i];   }
	for ( int i=0; i<nMipHist; i++ )      {    if(hMipContents.mipArray[i])          delete hMipContents.mipArray[i];    		}
	for ( int i=0; i<nMaxTimeBinHist; i++){    if(hMaxTimeBinContents.maxTimeBinArray[i])   delete hMaxTimeBinContents.maxTimeBinArray[i];             }
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
	for ( int i=0; i<totCh; i++ ){    
      maxAdc[i] = 0; maxAdc_zs[i] = 0;  
      maxTimeBin[i] = -1; maxTimeBin_zs[i] = -1;  
   }
	for ( int i=0; i<totAPV; i++ ){    cmNoise[i]         = 0; isNoisyApv[i]         = false; }

	// //////////////////////////////////add bad channels here///////////////////////
	// ///////////////////isChannelBad[numAssembly*ChPerSec+channel]=true;
	//expected chips per section
	for(int i=0; i<72; i++)
		nExpectedChip_Sec[i] = 12;
	nExpectedChip_Sec[67] = 11;
	//nExpectedChip_Sec[72] = {12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 12, 11, 12, 12, 12, 12};

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

					int geomChannelId = (ladder-1)*ChPerLadder + (sensor-1)*ChPerSensor + (column-1)*numRow + row;
					istMapping[channelId] = geomChannelId;
					istElecMapping[geomChannelId-1] = channelId;
				}
			}
		}
	}

	mAdcHist      	= sizeof(hAdcContents) / sizeof(TH2 *);
	mMultHist     	= sizeof(hMultContents) / sizeof(TH1 *);
	mHitMapHist   	= sizeof(hHitMapContents) / sizeof(TH2 *);
	mTbVsAdcHist  	= sizeof(hTbVsAdcContents) / sizeof(TH2 *);
	mEventSumHist 	= sizeof(hEventSumContents) / sizeof(TH1 *);
	mMipHist      	= sizeof(hMipContents) / sizeof(TH1 *);
	mMaxTimeBinHist 	= sizeof(hMaxTimeBinContents) / sizeof(TH1 *);
	mSumHist      	= sizeof(hSumContents) / sizeof(TH2 *);

	char buffer[100];
	char buffer1[100];
	char buffer2[100];

	int nBins    = 50;
	int nBinsAPV = 864;
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
	for ( int index=0; index<mAdcHist; index++ ) {
		sprintf( buffer, "ADC_Vs_Channel_Ladder_%d", index+1 );
		sprintf( buffer2, "IST - ADC vs Channel Id, Ladder: %d", index+1 );
		int ladderElecId = -1;
		int ladderGeomId = index + 1;
		if(ladderGeomId<23)	ladderElecId = 22 - ladderGeomId;
		else		ladderElecId = 46 - ladderGeomId;

		hAdcContents.adcArray[index] = new TH2S(buffer, buffer2, 72, 1, ChPerLadder+1, 100, ADCMin, ADCMax); //72 columns * 100 bins
		hAdcContents.adcArray[index]->GetXaxis()->SetTitle("Channel Electronics Index");
		hAdcContents.adcArray[index]->GetYaxis()->SetTitle("ADC value");
		hAdcContents.adcArray[index]->GetXaxis()->SetNdivisions(-3, false);
		hAdcContents.adcArray[index]->SetStats(false);
		hAdcContents.adcArray[index]->GetYaxis()->SetTitleOffset(1.1);
		hAdcContents.adcArray[index]->SetLabelSize(0.03);
		for(int iSec=0; iSec<3; iSec++) {
			int secElecId = ladderElecId * 3 + iSec; //geometry ID is reversed to electroncis ID
			int tempIdx = secElecId;
			if(secElecId==28) tempIdx += 1; // sections B & C sawpping on ladder 13
			if(secElecId==29) tempIdx -= 1;
			int rdoIndex   = tempIdx/12 + 1;  //1, 2, ..., 6
			int armIndex   = (tempIdx%12)/2;  //0, 1, ..., 5
			int groupIndex = tempIdx%2;       //0, 1
			sprintf( buffer1, "RDO%d_ARM%d_GROUP%d", rdoIndex, armIndex, groupIndex);
			hAdcContents.adcArray[index]->GetXaxis()->SetBinLabel(iSec*24+12, buffer1);
			hAdcContents.adcArray[index]->GetXaxis()->LabelsOption("h");
		}
	}

	/////////////////////
	for ( int index=0; index<mMultHist; index++ ) {
		sprintf( buffer, "HitMult_Ladder_%d", index+1 );
		sprintf( buffer2, "IST - Hit Multiplicity, Ladder: %d", index+1 );

		hMultContents.multArray[index] = new TH1S(buffer, buffer2, 100, 0, 500); // 100 bins
		hMultContents.multArray[index]->GetXaxis()->SetTitle("Number of Hits");
		hMultContents.multArray[index]->GetYaxis()->SetTitle("Counts");
		hMultContents.multArray[index]->SetStats(true);
		hMultContents.multArray[index]->GetYaxis()->SetTitleOffset(1.1);
	}

	/////////////////////
	for ( int index=0; index<mHitMapHist; index++ ) {
		sprintf( buffer, "HitMap_Ladder_%d", index+1 );
		sprintf( buffer2, "IST - Hit Map (density), Ladder: %d", index+1 );

		hHitMapContents.hitMapArray[index] = new TH2S(buffer, buffer2, numRow, 1, numRow+1, numColumn*numSensor, 1, numColumn*numSensor+1); //64 rows *72 columns
		hHitMapContents.hitMapArray[index]->GetXaxis()->SetTitle("Row Index in r-#phi");
		hHitMapContents.hitMapArray[index]->GetYaxis()->SetTitle("Column Index in Z");
		hHitMapContents.hitMapArray[index]->GetYaxis()->SetNdivisions(numSensor,false);
		hHitMapContents.hitMapArray[index]->SetStats(false);
	}

	////////////////////
	for ( int index=0; index<mTbVsAdcHist; index++ ) {
		sprintf( buffer, "ADC_Vs_Tb_Section_%d", index );
		int ladderGeomId = -1;
		int rdoIndex   = index/12 + 1;  //1, 2, ..., 6
		int armIndex   = (index%12)/2;  //0, 1, ..., 5
		int groupIndex = index%2;       //0, 1
		int ladderElecId = index/3;	    //0, 1, ..., 23
		//y2014
		if(ladderElecId<22) ladderGeomId = 22 - ladderElecId;
		else		ladderGeomId = 46 - ladderElecId;

		sprintf( buffer2,"IST - ADC vs Timebin (non-ZS), Ladder %d: RDO%d_ARM%d_GROUP%d", ladderGeomId, rdoIndex, armIndex, groupIndex );
		hTbVsAdcContents.tbVsAdcArray[index] = new TH2S(buffer, buffer2, numTimeBin, 0, numTimeBin, 100, ADCMin, ADCMax); //9*50 bins
		hTbVsAdcContents.tbVsAdcArray[index]->GetXaxis()->SetTitle("Time Bin Index");
		hTbVsAdcContents.tbVsAdcArray[index]->GetYaxis()->SetTitle("Pedestal-subtracted ADC value");
		hTbVsAdcContents.tbVsAdcArray[index]->GetXaxis()->SetNdivisions(numTimeBin,false);
		hTbVsAdcContents.tbVsAdcArray[index]->SetStats(false);
		hTbVsAdcContents.tbVsAdcArray[index]->GetYaxis()->SetTitleOffset(1.1);
		for ( int iTB=0; iTB<numTimeBin; iTB++ ) {
			sprintf( buffer, "TB%d", iTB );
			hTbVsAdcContents.tbVsAdcArray[index]->GetXaxis()->SetBinLabel(iTB+1,buffer);
		}
	}

	///////////////////
	hEventSumContents.hMeanPed = new TH1S("MeanPeds", "IST - <Mean Pedestal>", nBins*2, PedMin, PedMax); //100 bins
	hEventSumContents.hMeanPed->GetXaxis()->SetTitle("Mean Pedestal [ADC counts]");
	hEventSumContents.hMeanPed->SetFillColor(kYellow-9);
	hEventSumContents.hMeanPed->SetStats(true);

	hEventSumContents.hMeanRMS = new TH1S("MeanRMS", "IST - <RMS Pedestal>", nBins*2, SigMin, SigMax); //100 bins
	hEventSumContents.hMeanRMS->GetXaxis()->SetTitle("RMS pedestal [ADC counts]");
	hEventSumContents.hMeanRMS->SetFillColor(kYellow-9);
	hEventSumContents.hMeanRMS->SetStats(true);

	hEventSumContents.hSumTB = new TH1S("NumberOfTB", "IST - Number of Time Bins", nBinsTB, 0, TBMax); //15 bins
	hEventSumContents.hSumTB->SetFillColor(kYellow-9);
	hEventSumContents.hSumTB->SetStats(true);
	hEventSumContents.hSumTB->GetXaxis()->SetTitle("No. of Time Bin");

	hEventSumContents.hMaxTimeBin = new TH1S("MaxTimeBin_nonZS", "IST - Max ADC Time Bin (non-ZS)", numTimeBin, 0, numTimeBin); //9 bins
	hEventSumContents.hMaxTimeBin->SetFillColor(kYellow-9);
	hEventSumContents.hMaxTimeBin->SetStats(true);
	hEventSumContents.hMaxTimeBin->GetXaxis()->SetTitle("Time Bin Index");

	hEventSumContents.hMaxTimeBin_ZS = new TH1S("MaxTimeBin_ZS", "IST - Max ADC Time Bin (ZS)", numTimeBin, 0, numTimeBin); //9 bins
	hEventSumContents.hMaxTimeBin_ZS->SetFillColor(kYellow-9);
	hEventSumContents.hMaxTimeBin_ZS->SetStats(true);
	hEventSumContents.hMaxTimeBin_ZS->GetXaxis()->SetTitle("Time Bin Index");

	hEventSumContents.hSumBad = new TH1S("NumberOfGoodChannelsPerAPV", "IST - Good Channels per APV", totAPV, 0, totAPV); //864 bins
	hEventSumContents.hSumBad->SetNdivisions(-numLadder,"X");
	hEventSumContents.hSumBad->SetFillColor(kYellow-9);
	hEventSumContents.hSumBad->GetXaxis()->SetTitle("APV Geometry ID");
	hEventSumContents.hSumBad->SetStats(false);
	hEventSumContents.hSumBad->SetLabelSize(0.03);
	for(int iLad=0; iLad<numLadder; iLad++) {
		sprintf( buffer, "L%d", 1+iLad );
		hEventSumContents.hSumBad->GetXaxis()->SetBinLabel(iLad*ApvPerLadder+ApvPerLadder/2,buffer);
	}

	hEventSumContents.hApvCorpt = new TH1S("VisibleAPV", "IST - Visible APVs Frequency", nBins*2, 0, 900); //100 bins
	hEventSumContents.hApvCorpt->SetFillColor(kYellow-9);
	hEventSumContents.hApvCorpt->SetStats(true);
	hEventSumContents.hApvCorpt->GetXaxis()->SetTitle("Number of visible APVs");

	hEventSumContents.hEventSize = new TH1S("ISTEventSize", "IST - Event Size", nBins, 0, 6000); //50 bins
	hEventSumContents.hEventSize->GetXaxis()->SetTitle("Unpacked Event size [kB]");
	hEventSumContents.hEventSize->SetFillColor(kYellow-9);
	hEventSumContents.hEventSize->SetStats(true);

	hEventSumContents.hMipMPVvsSection = new TH1S("mipMPVvsSection_nonZS", "IST - MPV vs Section Id (non-ZS)", totSec, 0, totSec); //72 bins
	hEventSumContents.hMipMPVvsSection->GetXaxis()->SetTitle("Section ID [(RDO-1)*6*2+ARM*2+GROUP]");
	hEventSumContents.hMipMPVvsSection->GetYaxis()->SetTitle("MPV_{non-ZS} [ADC Counts]");
	hEventSumContents.hMipMPVvsSection->SetFillColor(kYellow-9);
	hEventSumContents.hMipMPVvsSection->SetStats(false);

	hEventSumContents.hMipMPVvsSection_ZS = new TH1S("mipMPVvsSection_ZS", "IST - MPV vs Section Id (ZS)", totSec, 0, totSec); //72 bins
	hEventSumContents.hMipMPVvsSection_ZS->GetXaxis()->SetTitle("Section ID [(RDO-1)*6*2+ARM*2+GROUP]");
	hEventSumContents.hMipMPVvsSection_ZS->GetYaxis()->SetTitle("MPV_{ZS} [ADC Counts]");
	hEventSumContents.hMipMPVvsSection_ZS->SetFillColor(kYellow-9);
	hEventSumContents.hMipMPVvsSection_ZS->SetStats(false);

	hEventSumContents.hMipSIGMAvsSection = new TH1S("mipSIGMAvsSection_nonZS", "IST - Sigma vs Section Id (non-ZS)", totSec, 0, totSec); //72 bins
	hEventSumContents.hMipSIGMAvsSection->GetXaxis()->SetTitle("Section ID [(RDO-1)*6*2+ARM*2+GROUP]");
	hEventSumContents.hMipSIGMAvsSection->GetYaxis()->SetTitle("Sigma_{non-ZS} [ADC Counts]");
	hEventSumContents.hMipSIGMAvsSection->SetFillColor(kYellow-9);
	hEventSumContents.hMipSIGMAvsSection->SetStats(false);

	hEventSumContents.hMipSIGMAvsSection_ZS = new TH1S("mipSIGMAvsSection_ZS", "IST - Sigma vs Section Id (ZS)", totSec, 0, totSec); //72 bins
	hEventSumContents.hMipSIGMAvsSection_ZS->GetXaxis()->SetTitle("Section ID [(RDO-1)*6*2+ARM*2+GROUP]");
	hEventSumContents.hMipSIGMAvsSection_ZS->GetYaxis()->SetTitle("Sigma_{ZS} [ADC Counts]");
	hEventSumContents.hMipSIGMAvsSection_ZS->SetFillColor(kYellow-9);
	hEventSumContents.hMipSIGMAvsSection_ZS->SetStats(false);

	hEventSumContents.hMaxTBfractionVsSection_ZS = new TH1F("maxTBfractionVsSection_ZS", "IST - maxTB fraction vs Section Id (ZS)", totSec, 0, totSec); //72 bins
	hEventSumContents.hMaxTBfractionVsSection_ZS->GetXaxis()->SetTitle("Section ID [(RDO-1)*6*2+ARM*2+GROUP]");
	hEventSumContents.hMaxTBfractionVsSection_ZS->GetYaxis()->SetTitle("N_{0<maxTB<numTB}/N_{0<=maxTB<=numTB}");
	hEventSumContents.hMaxTBfractionVsSection_ZS->SetFillColor(kYellow-9);
	hEventSumContents.hMaxTBfractionVsSection_ZS->SetStats(false);

	///////////////////
	for ( int index=0; index<mMipHist; index++ ) {
		if(index<72) {
			sprintf( buffer, "MIP_nonZS_Section_%d", index );
			int rdoIndex   = index/12 + 1; 	//1, 2, ..., 6
			int armIndex   = (index%12)/2; 	//0, 1, ..., 5
			int groupIndex = index%2;	//0, 1
			sprintf( buffer2,"IST - MIP peak non-ZS, RDO%d_ARM%d_GROUP%d", rdoIndex, armIndex, groupIndex );

			hMipContents.mipArray[index] = new TH1S(buffer, buffer2, 128, 0, 4096); //256 bins
			hMipContents.mipArray[index]->GetXaxis()->SetTitle("Pedestal Subtracted ADC [ADC Counts]");
			hMipContents.mipArray[index]->SetFillColor(kYellow-9);
			hMipContents.mipArray[index]->SetStats(true);
		}
		else {
			sprintf( buffer, "MIP_ZS_Section_%d", index-72 );
			int rdoIndex   = (index-72)/12 + 1;  //1, 2, ..., 6
			int armIndex   = ((index-72)%12)/2;  //0, 1, ..., 5
			int groupIndex = (index-72)%2;       //0, 1
			sprintf( buffer2,"IST - MIP peak ZS, RDO%d_ARM%d_GROUP%d", rdoIndex, armIndex, groupIndex );

			hMipContents.mipArray[index] = new TH1S(buffer, buffer2, 128, 0, 4096); //256 bins
			hMipContents.mipArray[index]->GetXaxis()->SetTitle("Pedestal Subtracted ADC [ADC Counts]");
			hMipContents.mipArray[index]->SetFillColor(kYellow-9);
			hMipContents.mipArray[index]->SetStats(true);
		}
	}

	for ( int index=0; index<mMaxTimeBinHist; index++ ) {
		sprintf( buffer, "maxTB_ZS_Section_%d", index );
		int rdoIndex   = index/12 + 1;  //1, 2, ..., 6
		int armIndex   = (index%12)/2;  //0, 1, ..., 5
		int groupIndex = index%2;       //0, 1
		sprintf( buffer2,"IST - Max time bin (ZS), RDO%d_ARM%d_GROUP%d", rdoIndex, armIndex, groupIndex );
		hMaxTimeBinContents.maxTimeBinArray[index] = new TH1S(buffer, buffer2, numTimeBin, 0, numTimeBin); //9 bins
		hMaxTimeBinContents.maxTimeBinArray[index]->GetXaxis()->SetTitle("Time Bin Index");
		hMaxTimeBinContents.maxTimeBinArray[index]->SetFillColor(kYellow-9);
		hMaxTimeBinContents.maxTimeBinArray[index]->SetStats(true);
	}

	//////////////////
	hSumContents.hVisibleApv = new TH2S("VisibleAPVperSection", "IST - Visible APVs per Section", totSec, 0, totSec, ApvPerSec+1, 0, ApvPerSec+1); //72*13 bins
	hSumContents.hVisibleApv->SetFillColor(kYellow-9);
	hSumContents.hVisibleApv->SetStats(true);
	hSumContents.hVisibleApv->GetXaxis()->SetTitle("Section ID [(RDO-1)*6*2+ARM*2+GROUP]");
	hSumContents.hVisibleApv->GetYaxis()->SetTitle("Number of APV per event");

	hSumContents.hHitMap = new TH2S("HitMapOfIST", "IST - Hit map (non-ZS)", numRow*numLadder, 1, numRow*numLadder+1, numColumn*numSensor, 1, numColumn*numSensor+1);//1536*72 bins
	hSumContents.hHitMap->GetXaxis()->SetNdivisions(-numLadder, false);
	hSumContents.hHitMap->GetYaxis()->SetNdivisions(-numSensor, false);
	hSumContents.hHitMap->SetStats(false);
	hSumContents.hHitMap->GetXaxis()->SetTitle("Row Index in r-#phi");
	hSumContents.hHitMap->GetYaxis()->SetTitle("Column Index in Z");
	hSumContents.hHitMap->SetLabelSize(0.02);

	hSumContents.hHitMapVsAPV = new TH2S("HitMapPerAPV", "IST - Hit map in Ladder vs APV (non-ZS)", 24, 1, 25, 36, 1, 37);
	hSumContents.hHitMapVsAPV->SetStats(false);
	hSumContents.hHitMapVsAPV->GetXaxis()->SetTitle("Ladder geometry ID");
	hSumContents.hHitMapVsAPV->GetYaxis()->SetTitle("APV geometry ID");

	hSumContents.hHitMap_ZS = new TH2S("HitMapOfIST_ZS", "IST - Hit map (ZS)", numRow*numLadder, 1, numRow*numLadder+1, numColumn*numSensor, 1, numColumn*numSensor+1);//1536*72 bins
	hSumContents.hHitMap_ZS->GetXaxis()->SetNdivisions(-numLadder, false);
	hSumContents.hHitMap_ZS->GetYaxis()->SetNdivisions(-numSensor, false);
	hSumContents.hHitMap_ZS->SetStats(false);
	hSumContents.hHitMap_ZS->GetXaxis()->SetTitle("Row Index in r-#phi");
	hSumContents.hHitMap_ZS->GetYaxis()->SetTitle("Column Index in Z");
	hSumContents.hHitMap_ZS->SetLabelSize(0.02);

	hSumContents.hHitMapVsAPV_ZS = new TH2S("HitMapPerAPV_ZS", "IST - Hit map in Ladder vs APV (ZS)", 24, 1, 25, 36, 1, 37);
	hSumContents.hHitMapVsAPV_ZS->SetStats(false);
	hSumContents.hHitMapVsAPV_ZS->GetXaxis()->SetTitle("Ladder geometry ID");
	hSumContents.hHitMapVsAPV_ZS->GetYaxis()->SetTitle("APV geometry ID");

	//hSumContents.hMultVsLadder = new TH2S("HitMultVsLadder", "IST - Hit Multiplicity vs Ladder Id", numLadder, 1, numLadder+1, 72, 0, ChPerLadder);//24*72 bins
	hSumContents.hMultVsLadder = new TH2S("HitMultVsLadder", "IST - Hit Multiplicity vs Ladder Id", numLadder, 1, numLadder+1, 101, 1, 101);//
	hSumContents.hMultVsLadder->GetXaxis()->SetNdivisions(-numLadder, false);
	hSumContents.hMultVsLadder->SetStats(false);
	hSumContents.hMultVsLadder->GetXaxis()->SetTitle("Ladder Geometry ID");
	hSumContents.hMultVsLadder->GetYaxis()->SetTitle("Number of Hits");
	hSumContents.hMultVsLadder->GetYaxis()->SetRangeUser(1, 4608);
	hSumContents.hMultVsLadder->SetLabelSize(0.03);
	for(int iLad=0; iLad<numLadder; iLad++) {
		sprintf( buffer, "L%d", 1+iLad );
		hSumContents.hMultVsLadder->GetXaxis()->SetBinLabel(iLad+1,buffer);
	}

	hSumContents.hSumPed = new TH2S("PedestalPerChannel", "IST - Pedestal vs Channel", nBinsAPV, 1, totCh+1, nBins*2, PedMin, PedMax); //864*100 bins
	hSumContents.hSumPed->GetXaxis()->SetNdivisions(-numLadder,false);
	hSumContents.hSumPed->SetStats(false);
	hSumContents.hSumPed->GetXaxis()->SetTitle("Channel Geometry ID");
	hSumContents.hSumPed->GetYaxis()->SetTitle("Mean Pedestal [ADC counts]");
	hSumContents.hSumPed->GetYaxis()->SetTitleOffset(1.1);

	hSumContents.hSumSig = new TH2S("PedestalRmsPerChannel", "IST - RMS vs Channel", nBinsAPV, 1, totCh+1, nBins*2, SigMin, SigMax); //864*100 bins
	hSumContents.hSumSig->GetXaxis()->SetNdivisions(-numLadder,false);
	hSumContents.hSumSig->SetStats(false);
	hSumContents.hSumSig->GetXaxis()->SetTitle("Channel Geometry ID");
	hSumContents.hSumSig->GetYaxis()->SetTitle("Pedestal RMS [ADC counts]");

	hSumContents.hCommonModeNoise = new TH2S("CommonModeNoisePerAPV", "IST - Common Mode Noise vs APV", nBinsAPV, 1, totAPV+1, nBins*2, CmnMin, CmnMax);//864*100 bins
	hSumContents.hCommonModeNoise->GetXaxis()->SetNdivisions(-numLadder, false);
	hSumContents.hCommonModeNoise->SetStats(false);
	hSumContents.hCommonModeNoise->GetXaxis()->SetTitle("APV Geometry ID");
	hSumContents.hCommonModeNoise->GetYaxis()->SetTitle("Common Mode Noise [ADC counts]");
	hSumContents.hCommonModeNoise->SetLabelSize(0.03);
	//label setting
	for(int iLad=0; iLad<numLadder; iLad++) {
		sprintf( buffer, "L%d", 1+iLad );
		hSumContents.hCommonModeNoise->GetXaxis()->SetBinLabel(iLad*ApvPerLadder+ApvPerLadder/2,buffer);
	}
	for ( int index=0; index<numLadder; index++ ) {         
		char label[100];
		sprintf(label, "L%d", index+1);
		hSumContents.hSumPed->GetXaxis()->SetBinLabel(index*ApvPerLadder+ApvPerLadder/2, label);  
		hSumContents.hSumSig->GetXaxis()->SetBinLabel(index*ApvPerLadder+ApvPerLadder/2, label);
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
	plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+2] = new JevpPlot(hEventSumContents.hSumTB);
	plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+3] = new JevpPlot(hEventSumContents.hMaxTimeBin);
	plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+4] = new JevpPlot(hEventSumContents.hMaxTimeBin_ZS);
	plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+5] = new JevpPlot(hEventSumContents.hSumBad);
	plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+6] = new JevpPlot(hEventSumContents.hApvCorpt);
	plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+7] = new JevpPlot(hEventSumContents.hEventSize);
	plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+8] = new JevpPlot(hEventSumContents.hMipMPVvsSection);
	plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+9] = new JevpPlot(hEventSumContents.hMipMPVvsSection_ZS);
	plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+10] = new JevpPlot(hEventSumContents.hMipSIGMAvsSection);
	plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+11] = new JevpPlot(hEventSumContents.hMipSIGMAvsSection_ZS);
	plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+12] = new JevpPlot(hEventSumContents.hMaxTBfractionVsSection_ZS);
	plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+6]->logy=true;
	plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+6]->setOptStat(10);
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

	for ( int i=0; i<mSumHist; i++ ) {
		hSumContents.sumArray[i]->SetOption("colz");
		plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+mEventSumHist+mMipHist+mMaxTimeBinHist+i] = new JevpPlot(hSumContents.sumArray[i]);
		hSumContents.sumArray[i]->SetStats(false);
	}
	plots[mAdcHist+mMultHist+mHitMapHist+mTbVsAdcHist+mEventSumHist+mMipHist+mMaxTimeBinHist+5]->logy=true;

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
		hCmnTemp.hCmnPerChip[i] = new TH1S(buffer, "Common mode noise per APV chip", 256, 0, 4096);
	}
}

// ************IST START RUN*****************
// ------------------------------------------
void istBuilder::startrun(daqReader *rdr) {
	LOG ( NOTE, "istBuilder starting run #%d", rdr->run );
	resetAllPlots();
	run = rdr->run; 

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

	//load external pedstal/RMS value for all channels
	FILE *file;
	char paraDir[256];
	sprintf(paraDir, "%s/ist/pedestals.txt", clientdatadir);

	file = fopen(paraDir, "r");
	if (file==0) {
		LOG(WARN,"ped::external table can't open input file \"%s\" [%s]", paraDir, strerror(errno));
		tableFound = false;
		sprintf(paraDir, "%s/ist/pedestals_local.txt", clientdatadir);
		file = fopen(paraDir, "r");
		if(file==0){
			LOG(WARN,"ped::external table can't open input file \"%s\" [%s]", paraDir, strerror(errno));
		}else{
			//LOG(U_IST,"loading pedestals from %s ", paraDir);
			while(!feof(file)) {
				int rdoIdxTemp=0, armIdxTemp=0, apvIdxTemp=0, chanIdxTemp=0, tbIdxTemp=0;
				float pp=0., rr=0.;
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
				int ret = sscanf(buff,"%d %d %d %d %d %f %f",&rdoIdxTemp,&armIdxTemp,&apvIdxTemp,&chanIdxTemp,&tbIdxTemp,&pp,&rr);
				if(ret!=7) continue;

				if(tbIdxTemp==2) { //only take time bin 2 as sample
					int channelIdxTemp = (rdoIdxTemp-1)*numARM*numAPV*ChPerApv + armIdxTemp*numAPV*ChPerApv + apvIdxTemp*ChPerApv + chanIdxTemp;
					istPedestal[channelIdxTemp] = pp;
					istRmsNoise[channelIdxTemp] = rr;
				}
			}
			tableFound = true;
			fclose(file);
		}
	}
	else {
		//LOG(U_IST,"loading pedestals from %s ", paraDir);
		while(!feof(file)) {
			int rdoIdxTemp=0, armIdxTemp=0, apvIdxTemp=0, chanIdxTemp=0, tbIdxTemp=0;
			float pp=0., rr=0.;
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
			int ret = sscanf(buff,"%d %d %d %d %d %f %f",&rdoIdxTemp,&armIdxTemp,&apvIdxTemp,&chanIdxTemp,&tbIdxTemp,&pp,&rr);
			if(ret!=7) continue;

			if(tbIdxTemp==2) { //only take time bin 2 as sample
				int channelIdxTemp = (rdoIdxTemp-1)*numARM*numAPV*ChPerApv + armIdxTemp*numAPV*ChPerApv + apvIdxTemp*ChPerApv + chanIdxTemp;
				istPedestal[channelIdxTemp] = pp;
				istRmsNoise[channelIdxTemp] = rr;
			}
		}
		tableFound = true;
		fclose(file);
	}


	sprintf(paraDir, "%s/ist/ist_apv_bad.txt", clientdatadir);
	//LOG(U_IST,"Loading file %s",paraDir);
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
					LOG(U_IST,"Wrong input:%s",buff);
				}else{
					if(runTemp<run&&runTemp>10000000) break;
					else if(runTemp==run){
						LOG(DBG,"misconfigure mask rdo %d arm %d apv %d", rdoTemp, armTemp, groupTemp*numAPV/2+apvTemp);
						int apvId = (rdoTemp-1)*numARM*numAPV + armTemp*numAPV + groupTemp*numAPV/2+apvTemp;
						for(int i=0;i<ChPerApv;i++){
							int chId = apvId*ChPerApv+i;
							int geoId    = istMapping[chId];
							isChannelBad[geoId-1] = true;
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
						int apvId = (rdoTemp-1)*numARM*numAPV + armTemp*numAPV + groupTemp*numAPV/2+apvTemp;
						LOG(DBG,"misconfigure mask rdo %d arm %d apv %d", rdoTemp, armTemp, groupTemp*numAPV/2+apvTemp);
						for(int i=0;i<ChPerApv;i++){
							int chId = apvId*ChPerApv+i;
							int geoId    = istMapping[chId];
							isChannelBad[geoId-1] = true;
						}
					}
				}
				break;
			}
		}
		fclose(file1);
	}

	sprintf(paraDir, "%s/ist/ist_bad_channels.txt", clientdatadir);
	//LOG(U_IST,"Loading file %s",paraDir);
	FILE *file2;
	file2 = fopen(paraDir,"rb");
	if(file2==0){
		LOG(WARN,"ped::ist bad channel list can't open input file \"%s\" [%s]", paraDir, strerror(errno));
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
				for(int a=0;a<numAPV;a++) {
				for(int c=0;c<ChPerApv;c++) {
					apvId = (r-1)*numARM*numAPV + arm*numAPV + a;
					chId  = apvId*ChPerApv + c;
					geoId = istMapping[chId];
					isChannelBad[geoId-1] = true;
				}
				LOG(DBG,"mask rdo %d arm %d apv %d", r, arm, a);
				}
			}
			else if(n[2]=='-') {	//nix APV
				for(int c=0;c<ChPerApv;c++) {

					apvId = (r-1)*numARM*numAPV + arm*numAPV + apv;
					chId  = apvId*ChPerApv + c;
					geoId = istMapping[chId];
					isChannelBad[geoId-1] = true;
				}
				LOG(DBG,"mask rdo %d arm %d apv %d", r, arm, apv);
			}
			else {
					apvId = (r-1)*numARM*numAPV + arm*numAPV + apv;
					chId  = apvId*ChPerApv + ch;
					geoId = istMapping[chId];
					isChannelBad[geoId-1] = true;
					LOG(DBG,"mask rdo %d arm %d apv %d ch %d", r, arm, apv, ch);
			}
		}
		fclose(file2);
	}

	sprintf(paraDir, "%s/ist/ist_noisy_chips.txt", clientdatadir);
	//LOG(U_IST,"Loading file %s",paraDir);
	FILE *file3;
	file3 = fopen(paraDir,"rb");
	if(file3==0){
		LOG(WARN,"ped::ist noisy chip list can't open input file \"%s\" [%s]", paraDir, strerror(errno));
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

			apvId = (r-1)*numARM*numAPV + arm*numAPV + apv;
			isNoisyApv[apvId] = true;
			LOG(DBG,"mask rdo %d arm %d apv %d", r, arm, apv);

		}
		fclose(file3);
	}

	errorMsg->SetText("No Error Message");    
	sumHistogramsFilled  = 0;  
	t_2min               = time(NULL);
	t_10min              = time(NULL);
	t_120min             = time(NULL);

}

#define safelog(x) ((x > 0) ? log10(x) : 0)
#define MAX_L3_SZ 1000000

// ************IST EVENT******************
// ---------------------------------------
void istBuilder::event(daqReader *rdr) {
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
	float sumAdcPerEvent[totAPV];
	int   counterAdcPerEvent[totAPV];
	int counterGoodHitPerEvent[totAPV];
	int counterGoodHitPerEvent_zs[totAPV];
   memset(counterGoodHitPerEvent,0,sizeof(counterGoodHitPerEvent));
   memset(counterGoodHitPerEvent_zs,0,sizeof(counterGoodHitPerEvent_zs));

	int HitCount[numLadder]; // for each ladder per event

	for ( int i=0; i<totCh; i++ ){
      maxAdc[i] = 0; maxAdc_zs[i] = 0;  
      maxTimeBin[i] = -1; maxTimeBin_zs[i] = -1; 
   }
	for ( int i=0; i<numLadder; i++ )     {    HitCount[i]        = 0;   }
	for ( int i=0; i<totAPV; i++ )        {    cmNoise[i]         = 0;   }

	numTb = numTimeBin;        		//default: 9 timebins
	memset( chCntDaq,  0, sizeof(chCntDaq) );
	memset( apvCntDaq, 0, sizeof(apvCntDaq) );

	if( !(evtCt %1000) )     LOG(DBG, "Looking at evt %d",evtCt);

	daq_dta *dd = rdr->det("ist")->get("adc");    
	if ( dd && dd->meta ) {
		apv_meta_t *meta = (apv_meta_t *) dd->meta;

		for ( int r=1; r<=numRDO; r++ ) {                  //1--6 ARCs (ARM Readout Controllers)
			if ( meta->arc[r].present == 0 ) continue ;
			for ( int arm=0; arm<numARM; arm++ ) {           //0--5 ARMs (APV Readout Modules) per ARC
				if ( meta->arc[r].arm[arm].present == 0 ) continue ;
				for ( int apv=0; apv<numAPV; apv++ ) {         //0--23 APV chips per ARM
					if ( meta->arc[r].arm[arm].apv[apv].present == 0 ) continue ;

					int Tb = meta->arc[r].arm[arm].apv[apv].ntim;
					if( numTb != 0 && Tb != 0 && numTb != Tb ) {
						//printf("Different number of timebins in different APV!!! Taking real one!!!\n");
						numTb = Tb; //update by Yaping: 03/25/2014
					}
					hEventSumContents.hSumTB->Fill(numTb);
				}
			}
		}   
	}

	// check if we have to look for the zs data
	size_t evtSize = 0;
	daq_dta *ddZS = rdr->det("ist")->get("zs");
	while( ddZS && ddZS->iterate() ) {
		fgt_adc_t *f_zs = (fgt_adc_t *) ddZS->Void ;
		evtSize += ddZS->ncontent * sizeof(fgt_adc_t);

		if ( ddZS->pad < 0 || ddZS->pad > 23 )        continue;      //valid APV numbering: 0, 1, ..., 23
		if ( ddZS->sec < 0 || ddZS->sec > 5 )         continue;      //valid ARM numbering: 0, 1, ..., 5
		if ( ddZS->rdo < 1 || ddZS->rdo > 6 )         continue;      //valid ARC numbering: 1, 2, ..., 6

      int elecApvId = (ddZS->rdo-1)*numARM*numAPV + ddZS->sec*numAPV + ddZS->pad;
      int cou_zs[ChPerApv];
		memset(cou_zs,0,sizeof(cou_zs));
		//loop current APV chip
		for ( u_int i=0; i<ddZS->ncontent; i++ ) {
			if ( f_zs[i].ch  < 0 || f_zs[i].ch  > 127 )    continue;//valid Channel numbering: 0, 1, ..., 127
			if ( f_zs[i].tb  < 0 || f_zs[i].tb  > numTb )  continue;//valid Time bin numbering: 0, 1, ..., numTb
			if ( f_zs[i].adc > 4095 )   continue;//valid ADC counts from 0 to 4095

			Int_t channelId_zs = (ddZS->rdo-1)*numARM*numAPV*ChPerApv + ddZS->sec*numAPV*ChPerApv + ddZS->pad*ChPerApv + f_zs[i].ch;
			if(channelId_zs < 0 || channelId_zs >= totCh)   continue;
			Int_t geoId_zs    = istMapping[channelId_zs];     //numbering from 1 to 110592
			if ( isChannelBad[geoId_zs-1] )     continue;  // 

			//max ADC and its time bin index decision
			if ( f_zs[i].adc > maxAdc_zs[geoId_zs-1] ) {
				maxAdc_zs[geoId_zs-1]     = f_zs[i].adc;
				maxTimeBin_zs[geoId_zs-1] = f_zs[i].tb;
			}
			if ( f_zs[i].adc > hitCut * oldStdDevs[geoId_zs-1]) {
            cou_zs[f_zs[i].ch]++;
         }
		}//end current APV loop

		// zero out hits less than 3 TBs
		for(int i=0;i<ChPerApv;i++){
			if(cou_zs[i]>=3){
            counterGoodHitPerEvent_zs[elecApvId]++;
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

		if ( dd->pad < 0 || dd->pad > 23 )        continue;      //valid APV numbering: 0, 1, ..., 23
		if ( dd->sec < 0 || dd->sec > 5 )         continue;      //valid ARM numbering: 0, 1, ..., 5
		if ( dd->rdo < 1 || dd->rdo > 6 )         continue;      //valid ARC numbering: 1, 2, ..., 6

		Int_t currentAPV = -1;
		for (int apvIdx=0; apvIdx<totAPV; apvIdx++ ){
			sumAdcPerEvent[apvIdx]     = 0.;
			counterAdcPerEvent[apvIdx] = 0 ;
		}
         
      int elecApvId = (dd->rdo-1)*numARM*numAPV + dd->sec*numAPV + dd->pad;

		int sectionIdx = (dd->rdo-1)*6*2 + dd->sec*2 + dd->pad/12;
		bool isFilled = false;
		int cou[ChPerApv];
		memset(cou,0,sizeof(cou));
		for ( u_int i=0; i<dd->ncontent; i++ ) { //loop current APV chip
			//non-ZS data
			if ( f[i].ch  < 0 || f[i].ch  > 127 )       continue;      //valid Channel numbering: 0, 1, ..., 127 
			if ( f[i].tb  < 0 || f[i].tb  > numTb )     continue;      //valid Time bin numbering: 0, 1, ..., numTb-1 (default 9 time bins, or 4, 5)
			if ( f[i].adc < 0 || f[i].adc > 4095 )      continue;      //valid ADC counts from 0 to 4095

			//count the found channel/APV (only taking time bin 0 to avoid double counting)
			if ( f[i].tb == 0 ) {	
				chCntDaq[dd->rdo-1][dd->sec][dd->pad]++;  

				if(!isFilled) {
					apvCntDaq[sectionIdx]++; //make sure only count one time
					isFilled = true;
				}
			}

			Int_t channelId = (dd->rdo-1)*numARM*numAPV*ChPerApv + dd->sec*numAPV*ChPerApv + dd->pad*ChPerApv + f[i].ch;
			if(channelId < 0 || channelId >= totCh)	continue;

			Int_t geoId    = istMapping[channelId];     //numbering from 1 to 110592
			Int_t ladder   = 1 + (geoId-1)/ChPerLadder; //numbering from 1 to 24
			Int_t apvId    = (geoId-1) / ChPerApv;      //numbering from 0 to 863
			//Int_t channel  = (geoId-1)%ChPerLadder;     //use to fill "per Ladder" histos in geometry Index
			Int_t channel  = channelId%ChPerLadder;     //use to fill "per Ladder" histos in electronics index
			currentAPV     = apvId;	

			if ( isChannelBad[geoId-1] )     continue;  //

			//fill ADC value vs channel index
			hAdcContents.adcArray[ladder-1]->Fill(channel, f[i].adc);

			//calculate mean pedestal and RMS
			if(!tableFound) {
				aVals[geoId-1] += f[i].adc;
				numVals[geoId-1]++;
				runningAvg[geoId-1] += (f[i].adc-runningAvg[geoId-1]) / numVals[geoId-1];
				runningStdDevSq[geoId-1] += ((float)numVals[geoId-1]-1)/(numVals[geoId-1]) * (f[i].adc-runningAvg[geoId-1]) * (f[i].adc-runningAvg[geoId-1]);
				oldStdDevs[geoId-1]       = sqrt(runningStdDevSq[geoId-1] / numVals[geoId-1]);
			}
			else {
				numVals[geoId-1]++;
				runningAvg[geoId-1] = istPedestal[channelId];
				oldStdDevs[geoId-1] = istRmsNoise[channelId];
			}
			//channel status decision
			Bool_t isBad = false;
			if ( runningAvg[geoId-1] < minPedVal || runningAvg[geoId-1] > maxPedVal )    isBad = true;
			if ( oldStdDevs[geoId-1] < minRMSVal || oldStdDevs[geoId-1] > maxRMSVal )    isBad = true;
			if(isBad) continue;

			//fill pedestal-subtracted ADC vs time bin index
			hTbVsAdcContents.tbVsAdcArray[sectionIdx]->Fill(f[i].tb, f[i].adc - (int)(runningAvg[geoId-1]+0.5));

			//count channel whose pedestal subtracted ADC yield one RMS
			if ( (f[i].adc-runningAvg[geoId-1])>oldStdDevs[geoId-1] && oldStdDevs[geoId-1]>0 ) 
				numOverOneSig[geoId-1]++; 

			//max ADC and its time bin index decision
			if ( (f[i].adc - runningAvg[geoId-1] )> maxAdc[geoId-1] ) {
				maxAdc[geoId-1]     = f[i].adc - runningAvg[geoId-1];
				maxTimeBin[geoId-1] = f[i].tb;
			}
			if ( f[i].adc > runningAvg[geoId-1] + hitCut * oldStdDevs[geoId-1] ){
				cou[f[i].ch]++;
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

		// zero out hits less than 3 TBs
		for(int i=0;i<ChPerApv;i++){
			if(cou[i]<3){
				Int_t channelId = (dd->rdo-1)*numARM*numAPV*ChPerApv + dd->sec*numAPV*ChPerApv + dd->pad*ChPerApv + i;
				Int_t geoId    = istMapping[channelId];     //numbering from 1 to 110592
				maxAdc[geoId-1]     = 0;
				maxTimeBin[geoId-1] = -1;
			}else{
            counterGoodHitPerEvent[elecApvId]++;
         }
		}

		//calculate dynamical common mode noise for current event
		if ( counterAdcPerEvent[currentAPV] > 0 && currentAPV > -1) {
			cmNoise[currentAPV] = sumAdcPerEvent[currentAPV] / counterAdcPerEvent[currentAPV];
			hCmnTemp.hCmnPerChip[currentAPV]->Fill(short(cmNoise[currentAPV]+0.5));
		}
	}//end all RDO, ARM, APV chips loops

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

		for(int i=0; i<totSec; i++) 
			hSumContents.hVisibleApv->Fill(i, apvCntDaq[i]);

		//do for zs and non-zs data and fill with num kB
		hEventSumContents.hEventSize->Fill(short(evtSize/1024));
	}

	//counting analyzed event number
	evtCt++;

	//count hit per Ladder per Event and fill hit map and MIP
	for(int geoIdx=1; geoIdx<=totCh; geoIdx++) {
		int ladderIdx = 1 + (geoIdx-1)/ChPerLadder;               //ladder index 1 to 24
		int sensorIdx = 1 + ((geoIdx-1)%ChPerLadder)/ChPerSensor; //sensor index 1 to 6 per ladder
		int padIdx    = ((geoIdx-1)%ChPerLadder)%ChPerSensor;
		int columnIdx = 1 + padIdx/numRow;  //column index 1 to 12
		int rowIdx    = 1 + padIdx%numRow;  //row index 1 to 64
		int apvGeoIdx = (sensorIdx-1)*6+(columnIdx-1)/2 + 1;    // apv index 1 to 36 

		int elecChanId = istElecMapping[geoIdx-1];
		int elecRdo = 1 + elecChanId/(6*3072);
		int elecArm = (elecChanId%(6*3072))/3072;
		int elecApv = ((elecChanId%(6*3072))%3072)/128;
		int elecSec = (elecRdo-1)*6*2 + elecArm*2 + elecApv/12;
		int elecChan = ((elecChanId%(6*3072))%3072)%128;

		//float pedestal   = runningAvg[geoIdx-1];
		float rms        = oldStdDevs[geoIdx-1];
		int adc_max      = maxAdc[geoIdx-1];
		int tb_max       = maxTimeBin[geoIdx-1];
			
		int apvId = (elecRdo-1)*numARM*numAPV + elecArm*numAPV + elecApv;
		if( adc_max>hitCut*rms && rms > minRMSVal && rms < maxRMSVal ){
		if( !isNoisyApv[apvId] || (isNoisyApv[apvId] && adc_max>noiseChipCut*rms)){
         if(counterGoodHitPerEvent[apvId]<=hitOccupancyCut){

			HitCount[ladderIdx-1]++;
			hHitMapContents.hitMapArray[ladderIdx-1]->Fill(rowIdx, (sensorIdx-1)*numColumn+columnIdx);
			hSumContents.hHitMap->Fill((ladderIdx-1)*numRow+rowIdx, (sensorIdx-1)*numColumn+columnIdx);
			hSumContents.hHitMapVsAPV->Fill(ladderIdx, apvGeoIdx);
			hMipContents.mipArray[elecSec]->Fill(short(adc_max+0.5));
			if(tb_max>=0) hEventSumContents.hMaxTimeBin->Fill(tb_max);
         }
		}
		}

		//ZS data
		if( maxAdc_zs[geoIdx-1] > hitCut*rms && rms > minRMSVal && rms < maxRMSVal ) {//roughly cut
			if( !isNoisyApv[apvId] || (isNoisyApv[apvId] && maxAdc_zs[geoIdx-1] > noiseChipCut*rms)){
         if(counterGoodHitPerEvent_zs[apvId]<=hitOccupancyCut){
			hMipContents.mipArray[elecSec+72]->Fill(short(maxAdc_zs[geoIdx-1]+0.5));
         if(maxTimeBin_zs[geoIdx-1]>=0){
			   hEventSumContents.hMaxTimeBin_ZS->Fill(maxTimeBin_zs[geoIdx-1]);
			   hMaxTimeBinContents.maxTimeBinArray[elecSec]->Fill(maxTimeBin_zs[geoIdx-1]);
         }
			hSumContents.hHitMap_ZS->Fill((ladderIdx-1)*numRow+rowIdx, (sensorIdx-1)*numColumn+columnIdx);
			hSumContents.hHitMapVsAPV_ZS->Fill(ladderIdx, apvGeoIdx);
         }
		}
		}
	}

	//fill hit multiplicity per ladder per event
	for ( int i=0; i<numLadder; i++) { 
		hMultContents.multArray[i]->Fill(HitCount[i]);
		hSumContents.hMultVsLadder->Fill(i+1, HitCount[i]<101?HitCount[i]:100.5);
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
			hEventSumContents.hMeanRMS->Fill(short(rmsPed+0.5));
			hSumContents.hSumSig->Fill(geoIdx, short(rmsPed+0.5));
		}

		if ( pedestal > 0 ) {
			hEventSumContents.hMeanPed->Fill(short(pedestal+0.5));
			hSumContents.hSumPed->Fill(geoIdx, short(pedestal+0.5));
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
	//common mode noise
	for( int k=0; k<totAPV; k++ ) {
		hSumContents.hCommonModeNoise->Fill(k+1, short(hCmnTemp.hCmnPerChip[k]->GetRMS()+0.5));
	}

	int errCt_visibleAPVperSection = 0, errCt_maxTimeBinFraction = 0, errCt_mipNonZS = 0, errCt_mipZS = 0;
	int errLocation_visibleAPVperSection[72], errLocation_maxTimeBinFraction[72], errLocation_mipNonZS[72], errLocation_mipZS[72]; 
	int errNumber_visibleAPVperSection[72];
	float errValue_maxTimeBinFraction[72], errValue_mipNonZS[72], errValue_sigmaNonZS[72], errValue_mipZS[72], errValue_sigmaZS[72]; 
	for(int j=0; j<72; j++) {
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

	for(int j=0; j<72; j++) {
		int rdoIndex   = j/12 + 1;  //1, 2, ..., 6
		int armIndex   = (j%12)/2;  //0, 1, ..., 5
		int groupIndex = j%2;       //0, 1

		if(hSumContents.hVisibleApv->GetEntries()>0) {
			if(hSumContents.hVisibleApv->GetBinContent(j+1, nExpectedChip_Sec[j]+1) < 1) {
				//LOG(U_IST,"visibleAPVperSection::section RDO%d_ARM%d_GROUP%d has missing APVs!", rdoIndex, armIndex, groupIndex);
				errLocation_visibleAPVperSection[errCt_visibleAPVperSection] = rdoIndex*100 + armIndex*10 + groupIndex;
				for(int jBin=1; jBin<13; jBin++) {
					if(hSumContents.hVisibleApv->GetBinContent(j+1, jBin)>1) 
						errNumber_visibleAPVperSection[errCt_visibleAPVperSection] = 13 - jBin;
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
				//LOG(U_IST,"maxTimeBinFraction::section RDO%d_ARM%d_GROUP%d with fraction %f!", rdoIndex, armIndex, groupIndex, fraction);
				errLocation_maxTimeBinFraction[errCt_maxTimeBinFraction] = rdoIndex*100 + armIndex*10 + groupIndex;
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
					//LOG(U_IST,"MIP_nonZS::section RDO%d_ARM%d_GROUP%d with MIP mpv %f, sigma %f!", rdoIndex, armIndex, groupIndex, mpvMIP_nonZS, sigmaMIP_nonZS);
					errLocation_mipNonZS[errCt_mipNonZS] = rdoIndex*100 + armIndex*10 + groupIndex;
					errValue_mipNonZS[errCt_mipNonZS] = mpvMIP_nonZS;
					errValue_sigmaNonZS[errCt_mipNonZS] = sigmaMIP_nonZS;
					errCt_mipNonZS++;
				}
			}
			else {
				LOG(WARN, "Bad Fit due to non-enough data (non-ZS) for RDO%d_ARM%d_GROUP%d", rdoIndex, armIndex, groupIndex);
			}
		}
		if(j==6) {
			mpvMIP_nonZS = 550.;
			sigmaMIP_nonZS = 140.;
		}
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
					//LOG(U_IST,"MIP_ZS::section RDO%d_ARM%d_GROUP%d with MIP mpv %f, sigma %f!", rdoIndex, armIndex, groupIndex, mpvMIP_ZS, sigmaMIP_ZS);
					errLocation_mipZS[errCt_mipZS] = rdoIndex*100 + armIndex*10 + groupIndex;
					errValue_mipZS[errCt_mipZS] = mpvMIP_ZS;
					errValue_sigmaZS[errCt_mipZS] = sigmaMIP_ZS;
					errCt_mipZS++;
				}
			}
			else {
				LOG(WARN, "Bad Fit due to non-enough data (ZS) for RDO%d_ARM%d_GROUP%d", rdoIndex, armIndex, groupIndex);
			}
		}
		if(j==6) {
			mpvMIP_ZS = 550.;
			sigmaMIP_ZS = 140.;
		}
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
		LOG(U_IST,"IST visibleAPVperSection:: In #%d, %d sections have missing APV chips, they are %s", run, errCt_visibleAPVperSection, buffer_Err.Data());
	}

	buffer_Err = "";
	if(errCt_maxTimeBinFraction>0) {
		for(int i=0; i<errCt_maxTimeBinFraction; i++)
			buffer_Err += Form("RDO%d_ARM%d_GROUP%d (fraction = %f),\t", errLocation_maxTimeBinFraction[i]/100, (errLocation_maxTimeBinFraction[i]%100)/10, errLocation_maxTimeBinFraction[i]%10, errValue_maxTimeBinFraction[i]);
		LOG(U_IST,"IST maxTimeBinFraction:: In #%d, %d sections have max time bin fraction less than %f, they are %s", run, errCt_maxTimeBinFraction, maxTbFracOK, buffer_Err.Data());
	}

	buffer_Err = "";
	Int_t entries_ZS = (int)hEventSumContents.hEventSize->Integral(1, 5);
	Int_t entries_nonZS = (int)hEventSumContents.hEventSize->Integral(17, 25);
	//cout << "non-ZS events " << entries_nonZS << "; ZS events " << entries_ZS << endl;
	//if(entries_nonZS>500 && errCt_mipNonZS>0) {
	if(errCt_mipNonZS>0) {
		for(int i=0; i<errCt_mipNonZS; i++)
			buffer_Err += Form("RDO%d_ARM%d_GROUP%d (MPV=%f, Sigma=%f),\t", errLocation_mipNonZS[i]/100, (errLocation_mipNonZS[i]%100)/10, errLocation_mipNonZS[i]%10, errValue_mipNonZS[i], errValue_sigmaNonZS[i]);
		//LOG(U_IST,"IST MIP_nonZS:: In #%d, %d sections have unnormal MIP mpv or sigma for non-ZS data, they are %s", run, errCt_mipNonZS, buffer_Err.Data());
		if(entries_nonZS<500)
			LOG(WARN,"IST MIP_nonZS (limited statistics):: In #%d, %d sections have unnormal MIP mpv or sigma for non-ZS data, they are %s", run, errCt_mipNonZS, buffer_Err.Data());
		else
			LOG(WARN,"IST MIP_nonZS (statistics>=500):: In #%d, %d sections have unnormal MIP mpv or sigma for non-ZS data, they are %s", run, errCt_mipNonZS, buffer_Err.Data());
	}

	buffer_Err = "";
	if(entries_ZS>500 && errCt_mipZS>0) {
		for(int i=0; i<errCt_mipZS; i++)
			buffer_Err += Form("RDO%d_ARM%d_GROUP%d (MPV=%f, Sigma=%f),\t", errLocation_mipZS[i]/100, (errLocation_mipZS[i]%100)/10, errLocation_mipZS[i]%10, errValue_mipZS[i], errValue_sigmaZS[i]);
		LOG(U_IST,"IST MIP_ZS:: In #%d, %d sections have unnormal MIP mpv or sigma for ZS data, they are %s", run, errCt_mipZS, buffer_Err.Data());
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
	for ( int i=0; i<totAPV; i++ )        {    cmNoise[i]         = 0;   }
}

void istBuilder::main(int argc, char *argv[])
{
	istBuilder me;
	me.Main(argc, argv);
}
