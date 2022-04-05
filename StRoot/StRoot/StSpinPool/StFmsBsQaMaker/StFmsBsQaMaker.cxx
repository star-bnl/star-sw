#include "StFmsBsQaMaker.h"
ClassImp(StFmsBsQaMaker);

#include "StEnumerations.h"
#include "StEventTypes.h"
#include "StEvent/StEvent.h"
#include "StEvent/StFmsCollection.h"
#include "StEvent/StFmsHit.h"
#include "StEvent/StFmsPoint.h"
#include "StEvent/StFmsPointPair.h"
#include "StEvent/StTriggerData.h"
#include "StEvent/StTriggerId.h"
#include "StFmsDbMaker/StFmsDbMaker.h"
#include "StMessMgr.h"
#include "StMuDSTMaker/COMMON/StMuEvent.h"
#include "StMuDSTMaker/COMMON/StMuTypes.hh"
#include "Stypes.h"

#include <TFile.h>
#include <TH1F.h>
#include <TH2F.h>
#include <TString.h>

#include <iostream>
#include <fstream>
#include <map>
using namespace std;

//=================================================================
StFmsBsQaMaker::StFmsBsQaMaker(const char* name) : StMaker(name) {}

//==========================
Int_t StFmsBsQaMaker::Init()
{
	mFmsDbMk = static_cast<StFmsDbMaker*>(GetMaker("fmsDb"));
	if (!mFmsDbMk) { LOG_ERROR <<"StFmsBsQaMaker::InitRun - !StFmsDbMaker" <<endl; return kStFatal; }

	return kStOk;
}//Init

//======================================
Int_t StFmsBsQaMaker::InitRun(int runNo)
{
	Info("InitRun", "Start run %d...", runNo);

	mFile = new TFile(mOutputName, "RECREATE");
	mFile->SetCompressionLevel(9);
	mFile->cd();

	//-------------------------------------------

	/*
	//Get map of each FMS cell's x/y position for mapping
	ofstream out;
	out.open("fmsCellMap.txt");
	for (int a=0; a<nDet; a++)
	{
		const int detId = a+8;
		const int maxCh = mFmsDbMk->maxChannel(detId);
		for (int b=0; b<maxCh; b++)
		{
			const int ch = b+1;
			const float gain = mFmsDbMk->getGain(detId, ch);
			StThreeVectorF XYZ = mFmsDbMk->getStarXYZ(detId, ch);
			out <<Form("%2i %3i %i %6.2f %6.2f", detId, ch, gain>0?true:false, XYZ.x(), XYZ.y()) <<endl;
		}//b, ch
	}//, detId
	out.close();
	*/

	//-------------------------------------------

	//Get list of "valid" FMS channels
	for (int a=0; a<nDet; a++)
	{
		int index = 0;
		const int detId = a+8;
		const int maxCh = mFmsDbMk->maxChannel(detId);
		for (int b=0; b<maxCh; b++)
		{
			const int ch = b+1;
			const float gain = mFmsDbMk->getGain(detId, ch);
			if (gain <= 0.) continue;

			//cout <<Form("detId = %2i ch = %3i index = %3i", detId, ch, index) <<endl;
			nToCh[a].insert(std::pair<int, int>(index, ch));
			chToN[a].insert(std::pair<int, int>(ch, index));
			index++;
		}//b, ch (all, including invalid ones)
	}//a, detId

	//-------------------------------------------

	//Histograms: ADC
	for (int a=0; a<nDet; a++)
	{
		const int nCh = (a<2)?nChLg:nChSm;
		mH2_adc[a] = new TH2F(Form("Adc_d%i",a+8), Form("detId = %i;ch;ADC",a+8), nCh,0.5,nCh+0.5, 5000,0,5000);
		mH2_adc[a]->Sumw2();
	}

	//-------------------------------------------

	//Histograms: bit shift, DB
	for (int a=0; a<nDet; a++)
	{
		const int nCh = (a<2)?nChLg:nChSm;
		mH2_bs_DB[a] = new TH2F(Form("BitShift_DB_d%i", a+8), "", nCh,0.5,nCh+0.5, nBit,0.5,nBit+0.5);
		mH2_bs_DB[a]->SetTitle(Form("DB, detId = %i;ch;bit", a+8));

		for (unsigned int x=0; x<nToCh[a].size(); x++) //Loop over only valid FMS channels
		for (int y=0; y<nBit; y++)
		{
			const int ch = nToCh[a][x];
			const int bs = mFmsDbMk->getBitShiftGain(a+8, ch);

			//BS = +5: filled y bins will be 1, 2, 3, 4, and 5
			//BS = -5: filled y bins will be 12, 11, 10, 9, and 8
			if ((bs>0 && y<bs) || (bs<0 && y>=(bs+nBit))) mH2_bs_DB[a]->SetBinContent(ch, y+1, 1);
		}
	}//a, detId

	//-------------------------------------------

	//Histograms: BS, data
	for (int a=0; a<nDet; a++)
	{
		const int nCh = (a<2)?nChLg:nChSm;
		mH2_bs_data[a] = new TH2F(Form("BitShift_data_d%i", a+8), "", nCh,0.5,nCh+0.5, nBit,0.5,nBit+0.5);
		mH2_bs_data[a]->SetTitle(Form("Data, detId = %i;ch;bit", a+8));

		//Set all "valid" channels' content to 1
		for (unsigned int x=0; x<nToCh[a].size(); x++)
		for (int y=0; y<nBit; y++)
		{
			const int ch = nToCh[a][x];
			mH2_bs_data[a]->SetBinContent(ch, y+1, 1);
		}
	}//a, detId

	//-------------------------------------------

	//Histograms: chMap
	for (int a=0; a<nDet; a++)
	{
		const int nCh = (a<2)?nChLg:nChSm;
		mH2_chMap[a] = new TH2F(Form("ChMap_d%i", a+8), "", nCh,0.5,nCh+0.5, 1,0,1);
		mH2_chMap[a]->SetTitle(Form("Map of valid channels, detId = %i;ch", a+8));

		for (unsigned int x=0; x<nToCh[a].size(); x++)
		{
			const int ch = nToCh[a][x];
			mH2_chMap[a]->SetBinContent(ch, 1, 1);
		}
	}//a, detId

	return kStOk;
}//InitRun

//==========================
Int_t StFmsBsQaMaker::Make()
{
	mEvent++;
	if (mEvent%1000 == 0) cout <<Form("%5i processed...", mEvent) <<endl;

	StEvent* event = (StEvent*)GetInputDS("StEvent");
	if (!event) { LOG_ERROR <<"StFmsOfflineQaMakerNew::Make - !StEvent" <<endl; return kStErr; }
	else mFmsColl = (StFmsCollection*) event->fmsCollection();

	StSPtrVecFmsHit& hits = mFmsColl->hits();
	const int nHits = mFmsColl->numberOfHits();
	for (int a=0; a<nHits; a++)
	{
		const int adc = hits[a]->adc();
		const int ch  = hits[a]->channel();
		const int det = hits[a]->detectorId();
		if (det<8 || det>11) continue;

		//Fill ADCs (for future crosscheck)
		mH2_adc[det-8]->Fill(ch, adc);

		//Iteration for binary bits
		for (int b=0; b<nBit; b++) { if (adc & (1 << b)) mH2_bs_data[det-8]->SetBinContent(ch, b+1, 0); }
	}//Hit

	return kStOk;
}//Make

//============================
Int_t StFmsBsQaMaker::Finish()
{
	mFile->Write();
	mFile->Close();
    return kStOK;
}//Finish
