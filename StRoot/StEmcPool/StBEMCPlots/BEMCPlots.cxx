#include "BEMCPlots.h"
#include <iostream>
#include <fstream>
using namespace std;
#include <TH1F.h>
#include <TH2F.h>
#include <TFile.h>
#include <TCanvas.h>
#include <TLine.h>
#include <TArrow.h>
#include <TLatex.h>
#include <TString.h>
#include <TBox.h>
#include <TObjArray.h>

// this needs to be always included
#include <RTS/src/DAQ_READER/daqReader.h>
#include <RTS/src/DAQ_READER/daq_dta.h>

// only the detectors we will use need to be included
// for their structure definitions...
#include <RTS/src/DAQ_BSMD/daq_bsmd.h>
#include <RTS/src/DAQ_BTOW/daq_btow.h>
#include <RTS/src/DAQ_EMC/daq_emc.h>
#include <RTS/src/DAQ_TRG/daq_trg.h>

#include "StDaqLib/EMC/StEmcDecoder.h"

#include "BEMC_DSM_decoder.h"
#include "BEMCPlotsNames.h"

BEMCPlots *BEMCPlotsInstance = 0;
StEmcDecoder *BEMCDecoder = 0;

//-------------------------------------------------------------------
void BEMCPlots::initHisto(TObjArray *list, const char *bemcStatus) {
    if (BEMCPlotsInstance) delete BEMCPlotsInstance; BEMCPlotsInstance = 0;
    BEMCPlotsInstance = new BEMCPlots(list);
    if (BEMCPlotsInstance) {
	BEMCPlotsInstance->setDebug(0);
	BEMCPlotsInstance->clear(bemcStatus);
    }
}
//-------------------------------------------------------------------
void BEMCPlots::resetHisto(const char *bemcStatus) {
    if (BEMCPlotsInstance) {
	BEMCPlotsInstance->clear(bemcStatus);
    }
}
//-------------------------------------------------------------------
void BEMCPlots::saveHisto(TFile *hfile) {
    if (BEMCPlotsInstance) {
	BEMCPlotsInstance->saveHistograms(hfile);
    }
}
//-------------------------------------------------------------------
void BEMCPlots::fillHisto(    char *datap
			    , const unsigned char *dsmL0WestInput
                	    , const unsigned char *dsmL0EastInput
                	    , const unsigned short *dsmL1Input
                	    , const unsigned short *dsmL2Input
                	    , const unsigned short *dsmL3Input
                	    ) {
    if (BEMCPlotsInstance) {
	BEMCPlotsInstance->processEvent(datap, dsmL0WestInput, dsmL0EastInput, dsmL1Input, dsmL2Input, dsmL3Input);
    }
}
//-------------------------------------------------------------------
BEMCPlots::BEMCPlots(TObjArray *list)
    : mDebug(0)
{
#define ADDHIST(hist) if (list && (hist)) list->Add(hist);
    this->mHistTot = new TH1F(HistTotName, "Total number of events processed;;Number of events", 1, 0, 1);
    ADDHIST(mHistTot)

    this->mHistDsmL0InputHighTower = new TH2F(HistDsmL0InputHighTowerName, "BEMC DSM L0 Input - HighTower;triggerPatch;HighTower", 300, -0.5, 300 - 0.5, 64, 0, 64);
    ADDHIST(mHistDsmL0InputHighTower)
    this->mHistDsmL0InputPatchSum = new TH2F(HistDsmL0InputPatchSumName, "BEMC DSM L0 Input - PatchSum;triggerPatch;PatchSum", 300, -0.5, 300 - 0.5, 64, 0, 64);
    ADDHIST(mHistDsmL0InputPatchSum)

    this->mHistDsmL1InputHighTowerBits = new TH2F(HistDsmL1InputHighTowerBitsName, "BEMC DSM L1 Input - HighTower bits;DSM Level-1 channels;HighTower bits", 36, -0.5, 36-0.5, 5, 0, 5);
    ADDHIST(mHistDsmL1InputHighTowerBits)
    this->mHistDsmL1InputPatchSum = new TH2F(HistDsmL1InputPatchSumName, "BEMC DSM L1 Input - PatchSum;Channel;PatchSum", 36, -0.5, 36-0.5, 128, 0, 256/*1024*/);
    ADDHIST(mHistDsmL1InputPatchSum)

    this->mHistDsmL2InputHighTowerBits = new TH2F(HistDsmL2InputHighTowerBitsName, "BEMC DSM L2 Input - HighTower bits;JetPatch;HighTower bits", 12, -0.5, 12-0.5, 5, 0, 5);
    ADDHIST(mHistDsmL2InputHighTowerBits)
    this->mHistDsmL2InputPatchSumBits = new TH2F(HistDsmL2InputPatchSumBitsName, "BEMC DSM L2 Input - PatchSum bits;JetPatch;PatchSum bits", 12, -0.5, 12-0.5, 4, 0, 4);
    ADDHIST(mHistDsmL2InputPatchSumBits)
    this->mHistDsmL2InputPatchSum = new TH2F(HistDsmL2InputPatchSumName, "BEMC DSM L2 Input - PatchSum;JetPatch pair;PatchSum", 6, -0.5, 6-0.5, 256, 0, 256);
    ADDHIST(mHistDsmL2InputPatchSum)

    this->mHistDsmL3InputHighTowerBits = new TH1F(HistDsmL3InputHighTowerBitsName, "BEMC DSM L3 Input - HighTower bits;HighTower bits", 4, -0.5, 4-0.5);
    ADDHIST(mHistDsmL3InputHighTowerBits)
    this->mHistDsmL3InputPatchSumBits = new TH1F(HistDsmL3InputPatchSumBitsName, "BEMC DSM L3 Input - PatchSum bits;PatchSum bits", 4, -0.5, 4-0.5);
    ADDHIST(mHistDsmL3InputPatchSumBits)
    this->mHistDsmL3InputBackToBackBit = new TH1F(HistDsmL3InputBackToBackBitName, "BEMC DSM L3 Input - Back-to-Back bit;Back-to-Back bit", 2, -0.5, 2-0.5);
    ADDHIST(mHistDsmL3InputBackToBackBit)
    this->mHistDsmL3InputJPsiTopoBit = new TH1F(HistDsmL3InputJPsiTopoBitName, "BEMC DSM L3 Input - J/Psi topology bit;J/Psi bit", 2, -0.5, 2-0.5);
    ADDHIST(mHistDsmL3InputJPsiTopoBit)
    this->mHistDsmL3InputJetPatchTopoBit = new TH1F(HistDsmL3InputJetPatchTopoBitName, "BEMC DSM L3 Input - JetPatch topology bit;JetPatch bit", 2, -0.5, 2-0.5);
    ADDHIST(mHistDsmL3InputJetPatchTopoBit)

      this->mHistRawAdc1 = new TH2F(HistRawAdc1Name, "BTOW ADC, 1 <= SoftId <= 1220;SoftId;ADC",    1220, 0000.5, 1220.5, 300, -0.5, 1000-0.5);
      this->mHistRawAdc2 = new TH2F(HistRawAdc2Name, "BTOW ADC, 1221 <= SoftId <= 2400;SoftId;ADC", 1180, 1220.5, 2400.5, 300, -0.5, 1000-0.5);
      this->mHistRawAdc3 = new TH2F(HistRawAdc3Name, "BTOW ADC, 2401 <= SoftId <= 3540;SoftId;ADC", 1140, 2400.5, 3540.5, 300, -0.5, 1000-0.5);
      this->mHistRawAdc4 = new TH2F(HistRawAdc4Name, "BTOW ADC, 3541 <= SoftId <= 4800;SoftId;ADC", 1260, 3540.5, 4800.5, 300, -0.5, 1000-0.5);

      this->mHistRawAdcPsd1 = new TH2F(HistRawAdcPsd1Name, "BPRS ADC, 1 <= SoftId <= 1220;SoftId;ADC",    1220, 0000.5, 1220.5, 300, -0.5, 1000-0.5);
      this->mHistRawAdcPsd2 = new TH2F(HistRawAdcPsd2Name, "BPRS ADC, 1221 <= SoftId <= 2400;SoftId;ADC", 1180, 1220.5, 2400.5, 300, -0.5, 1000-0.5);
      this->mHistRawAdcPsd3 = new TH2F(HistRawAdcPsd3Name, "BPRS ADC, 2401 <= SoftId <= 3540;SoftId;ADC", 1140, 2400.5, 3540.5, 300, -0.5, 1000-0.5);
      this->mHistRawAdcPsd4 = new TH2F(HistRawAdcPsd4Name, "BPRS ADC, 3541 <= SoftId <= 4800;SoftId;ADC", 1260, 3540.5, 4800.5, 300, -0.5, 1000-0.5);

      this->mHistSmdFeeSum = new TH2F(HistSmdFeeSumName, "SMD FEE Sum;Module;Sum", 120, 0.5, 120+0.5, 100, -0.5, 100000-0.5);
      this->mHistPsdFeeSum = new TH2F(HistPsdFeeSumName, "PSD FEE Sum;PMT Box;Sum", 60, 0.5, 60+0.5, 100, -0.5, 40000-0.5);

      ADDHIST(mHistRawAdc1)
      ADDHIST(mHistRawAdc2)
      ADDHIST(mHistRawAdc3)
      ADDHIST(mHistRawAdc4)
      ADDHIST(mHistRawAdcPsd1)
      ADDHIST(mHistRawAdcPsd2)
      ADDHIST(mHistRawAdcPsd3)
      ADDHIST(mHistRawAdcPsd4)
      ADDHIST(mHistSmdFeeSum)
      ADDHIST(mHistPsdFeeSum)
    
    for (int i = 0;i < 12;i++) {
	TString name;
	TString title;
	name = Form("%s_%u", HistHighTowerSpectrumName, i);
	title = Form("JetPatch %u - HighTower spectrum;HighTower", i);	
	this->mHistHighTowerSpectrum[i] = new TH1F(name.Data(), title.Data(), 64, -0.5, 64-0.5);
	ADDHIST(mHistHighTowerSpectrum[i])
	name = Form("%s_%u", HistPatchSumSpectrumName, i);
	title = Form("JetPatch %u - PatchSum spectrum;PatchSum", i);	
	this->mHistPatchSumSpectrum[i] = new TH1F(name.Data(), title.Data(), 200, -0.5, 200-0.5);    	
	ADDHIST(mHistPatchSumSpectrum[i])
    }
    
    this->mHistTriggerCorruptionHighTower = new TH1F(HistTriggerCorruptionHighTowerName, "HighTower trigger corruption;triggerPatch;events", 300, -0.5, 300-0.5);
    this->mHistTriggerCorruptionPatchSum = new TH1F(HistTriggerCorruptionPatchSumName, "PatchSum trigger corruption;triggerPatch;events", 300, -0.5, 300-0.5);
    this->mHistTriggerCorruptionHighTowerCorr = new TH2F(HistTriggerCorruptionHighTowerCorrName, "HighTower trigger corruption;DSM HighTower;Simulated HighTower", 64, 0, 64, 64, 0, 64);
    this->mHistTriggerCorruptionPatchSumCorr = new TH2F(HistTriggerCorruptionPatchSumCorrName, "PatchSum trigger corruption;DSM PatchSum;Simulated PatchSum", 64, 0, 64, 64, 0, 64);

      ADDHIST(mHistTriggerCorruptionHighTower)
      ADDHIST(mHistTriggerCorruptionPatchSum)
      ADDHIST(mHistTriggerCorruptionHighTowerCorr)
      ADDHIST(mHistTriggerCorruptionPatchSumCorr)

    for (int i = 0;i < 4800;i++) {
	this->mTowerData[i][0] = 1; // unmask
	this->mTowerData[i][1] = 0; // ped
	this->mTowerData[i][2] = 0; // FEE ped
    }
    this->mTriggerPedestalShift = 24 * 100;
    for (int i = 0;i < 300;i++) {
	this->mPatchData[i][0] = 1; // unmask HT
	this->mPatchData[i][1] = 1; // unmask PA
	this->mPatchData[i][2] = 3; // bit conversion mode
	this->mPatchData[i][3] = 2; // formula
	this->mPatchData[i][4] = 1; // formula parameter 0
	this->mPatchData[i][5] = 0; // formula parameter 1
	this->mPatchData[i][6] = 0; // formula parameter 2
	this->mPatchData[i][7] = 1; // formula parameter 3
	this->mPatchData[i][8] = 0; // formula parameter 4
	this->mPatchData[i][9] = 0; // formula parameter 5
	this->mPatchData[i][10] = 0; // number of masked towers
    }
}
//-------------------------------------------------------------------
BEMCPlots::~BEMCPlots() {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
    if (this->mHistTot) delete this->mHistTot; this->mHistTot = 0;

    if (this->mHistDsmL0InputHighTower) delete this->mHistDsmL0InputHighTower; this->mHistDsmL0InputHighTower = 0;
    if (this->mHistDsmL0InputPatchSum) delete this->mHistDsmL0InputPatchSum; this->mHistDsmL0InputPatchSum = 0;

    if (this->mHistDsmL1InputHighTowerBits) delete this->mHistDsmL1InputHighTowerBits; this->mHistDsmL1InputHighTowerBits = 0;
    if (this->mHistDsmL1InputPatchSum) delete this->mHistDsmL1InputPatchSum; this->mHistDsmL1InputPatchSum = 0;

    if (this->mHistDsmL2InputHighTowerBits) delete this->mHistDsmL2InputHighTowerBits; this->mHistDsmL2InputHighTowerBits = 0;
    if (this->mHistDsmL2InputPatchSumBits) delete this->mHistDsmL2InputPatchSumBits; this->mHistDsmL2InputPatchSumBits = 0;
    if (this->mHistDsmL2InputPatchSum) delete this->mHistDsmL2InputPatchSum; this->mHistDsmL2InputPatchSum = 0;

    if (this->mHistDsmL3InputHighTowerBits) delete this->mHistDsmL3InputHighTowerBits; this->mHistDsmL3InputHighTowerBits = 0;
    if (this->mHistDsmL3InputPatchSumBits) delete this->mHistDsmL3InputPatchSumBits; this->mHistDsmL3InputPatchSumBits = 0;
    if (this->mHistDsmL3InputBackToBackBit) delete this->mHistDsmL3InputBackToBackBit; this->mHistDsmL3InputBackToBackBit = 0;
    if (this->mHistDsmL3InputJPsiTopoBit) delete this->mHistDsmL3InputJPsiTopoBit; this->mHistDsmL3InputJPsiTopoBit = 0;
    if (this->mHistDsmL3InputJetPatchTopoBit) delete this->mHistDsmL3InputJetPatchTopoBit; this->mHistDsmL3InputJetPatchTopoBit = 0;

    if (this->mHistRawAdc1) delete this->mHistRawAdc1; this->mHistRawAdc1 = 0;
    if (this->mHistRawAdc2) delete this->mHistRawAdc2; this->mHistRawAdc2 = 0;
    if (this->mHistRawAdc3) delete this->mHistRawAdc3; this->mHistRawAdc3 = 0;
    if (this->mHistRawAdc4) delete this->mHistRawAdc4; this->mHistRawAdc4 = 0;

    if (this->mHistRawAdcPsd1) delete this->mHistRawAdcPsd1; this->mHistRawAdcPsd1 = 0;
    if (this->mHistRawAdcPsd2) delete this->mHistRawAdcPsd2; this->mHistRawAdcPsd2 = 0;
    if (this->mHistRawAdcPsd3) delete this->mHistRawAdcPsd3; this->mHistRawAdcPsd3 = 0;
    if (this->mHistRawAdcPsd4) delete this->mHistRawAdcPsd4; this->mHistRawAdcPsd4 = 0;

    if (this->mHistSmdFeeSum) delete this->mHistSmdFeeSum; this->mHistSmdFeeSum = 0;
    if (this->mHistPsdFeeSum) delete this->mHistPsdFeeSum; this->mHistPsdFeeSum = 0;

    for (int i = 0;i < 12;i++) {
    	if (this->mHistHighTowerSpectrum[i]) delete this->mHistHighTowerSpectrum[i]; this->mHistHighTowerSpectrum[i] = 0;
    	if (this->mHistPatchSumSpectrum[i]) delete this->mHistPatchSumSpectrum[i]; this->mHistPatchSumSpectrum[i] = 0;
    }

    if (this->mHistTriggerCorruptionHighTower) delete this->mHistTriggerCorruptionHighTower; this->mHistTriggerCorruptionHighTower = 0;
    if (this->mHistTriggerCorruptionPatchSum) delete this->mHistTriggerCorruptionPatchSum; this->mHistTriggerCorruptionPatchSum = 0;
    if (this->mHistTriggerCorruptionHighTowerCorr) delete this->mHistTriggerCorruptionHighTowerCorr; this->mHistTriggerCorruptionHighTowerCorr = 0;
    if (this->mHistTriggerCorruptionPatchSumCorr) delete this->mHistTriggerCorruptionPatchSumCorr; this->mHistTriggerCorruptionPatchSumCorr = 0;
    
    this->clear(0);
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}
//-------------------------------------------------------------------
void BEMCPlots::init(unsigned int date, unsigned int time, const char *bemcStatus) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
    this->clear(bemcStatus);
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}
//-------------------------------------------------------------------
void BEMCPlots::clear(const char *bemcStatus) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
    if (mDebug >= 2) cout << "bemcStatus = " << bemcStatus << endl;
    if (this->mHistTot) this->mHistTot->Reset();

    if (this->mHistDsmL0InputHighTower) this->mHistDsmL0InputHighTower->Reset();
    if (this->mHistDsmL0InputPatchSum) this->mHistDsmL0InputPatchSum->Reset();

    if (this->mHistDsmL1InputHighTowerBits) this->mHistDsmL1InputHighTowerBits->Reset();
    if (this->mHistDsmL1InputPatchSum) this->mHistDsmL1InputPatchSum->Reset();

    if (this->mHistDsmL2InputHighTowerBits) this->mHistDsmL2InputHighTowerBits->Reset();
    if (this->mHistDsmL2InputPatchSumBits) this->mHistDsmL2InputPatchSumBits->Reset();
    if (this->mHistDsmL2InputPatchSum) this->mHistDsmL2InputPatchSum->Reset();

    if (this->mHistDsmL3InputHighTowerBits) this->mHistDsmL3InputHighTowerBits->Reset();
    if (this->mHistDsmL3InputPatchSumBits) this->mHistDsmL3InputPatchSumBits->Reset();
    if (this->mHistDsmL3InputBackToBackBit) this->mHistDsmL3InputBackToBackBit->Reset();
    if (this->mHistDsmL3InputJPsiTopoBit) this->mHistDsmL3InputJPsiTopoBit->Reset();
    if (this->mHistDsmL3InputJetPatchTopoBit) this->mHistDsmL3InputJetPatchTopoBit->Reset();

    if (this->mHistRawAdc1) this->mHistRawAdc1->Reset();
    if (this->mHistRawAdc2) this->mHistRawAdc2->Reset();
    if (this->mHistRawAdc3) this->mHistRawAdc3->Reset();
    if (this->mHistRawAdc4) this->mHistRawAdc4->Reset();

    if (this->mHistRawAdcPsd1) this->mHistRawAdcPsd1->Reset();
    if (this->mHistRawAdcPsd2) this->mHistRawAdcPsd2->Reset();
    if (this->mHistRawAdcPsd3) this->mHistRawAdcPsd3->Reset();
    if (this->mHistRawAdcPsd4) this->mHistRawAdcPsd4->Reset();

    if (this->mHistSmdFeeSum) this->mHistSmdFeeSum->Reset();
    if (this->mHistPsdFeeSum) this->mHistPsdFeeSum->Reset();

    for (int i = 0;i < 12;i++) {
    	if (this->mHistHighTowerSpectrum[i]) this->mHistHighTowerSpectrum[i]->Reset();
    	if (this->mHistPatchSumSpectrum[i]) this->mHistPatchSumSpectrum[i]->Reset();
    }

    if (this->mHistTriggerCorruptionHighTower) this->mHistTriggerCorruptionHighTower->Reset();
    if (this->mHistTriggerCorruptionPatchSum) this->mHistTriggerCorruptionPatchSum->Reset();
    if (this->mHistTriggerCorruptionHighTowerCorr) this->mHistTriggerCorruptionHighTowerCorr->Reset();
    if (this->mHistTriggerCorruptionPatchSumCorr) this->mHistTriggerCorruptionPatchSumCorr->Reset();

    for (int i = 0;i < 4800;i++) {
	this->mTowerData[i][0] = 1; // unmask
	this->mTowerData[i][1] = 0; // ped
	this->mTowerData[i][2] = 0; // FEE ped
    }
    this->mTriggerPedestalShift = 24 * 100;
    for (int i = 0;i < 300;i++) {
	this->mPatchData[i][0] = 1; // unmask HT
	this->mPatchData[i][1] = 1; // unmask PA
	this->mPatchData[i][2] = 3; // bit conversion mode
	this->mPatchData[i][3] = 2; // formula
	this->mPatchData[i][4] = 1; // formula parameter 0
	this->mPatchData[i][5] = 0; // formula parameter 1
	this->mPatchData[i][6] = 0; // formula parameter 2
	this->mPatchData[i][7] = 1; // formula parameter 3
	this->mPatchData[i][8] = 0; // formula parameter 4
	this->mPatchData[i][9] = 0; // formula parameter 5
	this->mPatchData[i][10] = 0; // number of masked towers
    }
    if (bemcStatus) {
	ifstream ifstr(bemcStatus);
	if (ifstr.good()) {
	    cout << "Reading BEMC trigger status file " << bemcStatus << endl;
	} else {
	    cout << "Cannot open BEMC trigger status file! " << bemcStatus << endl;
	}
	while (ifstr.good()) {
	    string token;
	    do {
		if (token == "#") {
		    char dummy[4096];
		    ifstr.getline(dummy, sizeof(dummy));
		}
		ifstr >> token;
	    } while (ifstr.good() && (token != "SoftId") && (token != "triggerPatch") && (token != "TriggerPedestalShift"));
	    if (ifstr.good()) {
		if (token == "SoftId") {
		    int softId, crate, crateSeq, unmaskTower, unmaskHT, unmaskPA;
		    float ped;
		    ifstr >> softId >> crate >> crateSeq >> unmaskTower >> unmaskHT >> unmaskPA >> ped;
		    if (mDebug >= 2) cout << "Read: " << token << " " << softId << "\t" << crate << "\t" << crateSeq << "\t" << unmaskTower << "\t" << unmaskHT << "\t" << unmaskPA << "\t" << ped << endl;
		    if ((softId >= 1) && (softId <= 4800)) {
			this->mTowerData[softId - 1][0] = unmaskTower;
			this->mTowerData[softId - 1][1] = int(ped * 100.0);
			int triggerPatch;
			if ((unmaskTower == 0) && BEMCDecoder && BEMCDecoder->GetTriggerPatchFromCrate(crate, crateSeq, triggerPatch)) {
			    if ((triggerPatch >= 0) && (triggerPatch < 300)) {
				this->mPatchData[triggerPatch][10] += 1;
			    }
			}
		    }
		} else if (token == "triggerPatch") {
		    int triggerPatch, crate, crateSeq, unmaskHT, unmaskPA, bitConv, formula, formulaParam0, formulaParam1, formulaParam2, formulaParam3, formulaParam4, formulaParam5;
		    ifstr >> triggerPatch >> crate >> crateSeq >> unmaskHT >> unmaskPA >> bitConv >> formula >> formulaParam0 >> formulaParam1 >> formulaParam2 >> formulaParam3 >> formulaParam4 >> formulaParam5;
		    if (mDebug >= 2) cout << "Read: " << token << " " << triggerPatch << "\t" << crate << "\t" << crateSeq << "\t" << unmaskHT << "\t" << unmaskPA << "\t" << bitConv << "\t" << formula << "\t" << formulaParam0 << "\t" << formulaParam1 << "\t" << formulaParam2 << "\t" << formulaParam3 << "\t" << formulaParam4 << "\t" << formulaParam5 << endl;
		    if ((triggerPatch >= 0) && (triggerPatch < 300)) {
			this->mPatchData[triggerPatch][0] = unmaskHT;
			this->mPatchData[triggerPatch][1] = unmaskPA;
			this->mPatchData[triggerPatch][2] = bitConv;
			this->mPatchData[triggerPatch][3] = formula;
			this->mPatchData[triggerPatch][4] = formulaParam0;
			this->mPatchData[triggerPatch][5] = formulaParam1;
			this->mPatchData[triggerPatch][6] = formulaParam2;
			this->mPatchData[triggerPatch][7] = formulaParam3;
			this->mPatchData[triggerPatch][8] = formulaParam4;
			this->mPatchData[triggerPatch][9] = formulaParam5;
		    }
		} else if (token == "TriggerPedestalShift") {
		    float pedShift;
		    ifstr >> pedShift;
		    this->mTriggerPedestalShift = int(pedShift * 100.0);
		    if (mDebug >= 2) cout << "Read: " << token << " " << pedShift << endl;
		}
	    }
	}
	ifstr.close();
	for (int i = 0;i < 4800;i++) {
	    this->mTowerData[i][2] = getFEEpedestal(float(this->mTowerData[i][1]) / 100.0, this->mTriggerPedestalShift / 100.0, false/*(i >= (3285 - 1)) && (i <= (3288 - 1))*/);
	}
    }
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}
//-------------------------------------------------------------------
void BEMCPlots::saveHistograms(TFile *hfile) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
    if (!hfile || (mDebug >= 2)) cout << "hfile = " << hfile << endl;
    if (hfile) {
	hfile->cd();
        if (this->mHistTot) this->mHistTot->Write();

        if (this->mHistDsmL0InputHighTower) this->mHistDsmL0InputHighTower->Write();
        if (this->mHistDsmL0InputPatchSum) this->mHistDsmL0InputPatchSum->Write();

        if (this->mHistDsmL1InputHighTowerBits) this->mHistDsmL1InputHighTowerBits->Write();
        if (this->mHistDsmL1InputPatchSum) this->mHistDsmL1InputPatchSum->Write();

        if (this->mHistDsmL2InputHighTowerBits) this->mHistDsmL2InputHighTowerBits->Write();
        if (this->mHistDsmL2InputPatchSumBits) this->mHistDsmL2InputPatchSumBits->Write();
        if (this->mHistDsmL2InputPatchSum) this->mHistDsmL2InputPatchSum->Write();

        if (this->mHistDsmL3InputHighTowerBits) this->mHistDsmL3InputHighTowerBits->Write();
        if (this->mHistDsmL3InputPatchSumBits) this->mHistDsmL3InputPatchSumBits->Write();
        if (this->mHistDsmL3InputBackToBackBit) this->mHistDsmL3InputBackToBackBit->Write();
        if (this->mHistDsmL3InputJPsiTopoBit) this->mHistDsmL3InputJPsiTopoBit->Write();
        if (this->mHistDsmL3InputJetPatchTopoBit) this->mHistDsmL3InputJetPatchTopoBit->Write();

        if (this->mHistRawAdc1) this->mHistRawAdc1->Write();
        if (this->mHistRawAdc2) this->mHistRawAdc2->Write();
        if (this->mHistRawAdc3) this->mHistRawAdc3->Write();
        if (this->mHistRawAdc4) this->mHistRawAdc4->Write();

        if (this->mHistRawAdcPsd1) this->mHistRawAdcPsd1->Write();
        if (this->mHistRawAdcPsd2) this->mHistRawAdcPsd2->Write();
        if (this->mHistRawAdcPsd3) this->mHistRawAdcPsd3->Write();
        if (this->mHistRawAdcPsd4) this->mHistRawAdcPsd4->Write();

        if (this->mHistSmdFeeSum) this->mHistSmdFeeSum->Write();
        if (this->mHistPsdFeeSum) this->mHistPsdFeeSum->Write();

        for (int i = 0;i < 12;i++) {
    	    if (this->mHistHighTowerSpectrum[i]) this->mHistHighTowerSpectrum[i]->Write();
    	    if (this->mHistPatchSumSpectrum[i]) this->mHistPatchSumSpectrum[i]->Write();
	}

        if (this->mHistTriggerCorruptionHighTower) this->mHistTriggerCorruptionHighTower->Write();
        if (this->mHistTriggerCorruptionPatchSum) this->mHistTriggerCorruptionPatchSum->Write();
        if (this->mHistTriggerCorruptionHighTowerCorr) this->mHistTriggerCorruptionHighTowerCorr->Write();
        if (this->mHistTriggerCorruptionPatchSumCorr) this->mHistTriggerCorruptionPatchSumCorr->Write();

    }
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}
//-------------------------------------------------------------------
void BEMCPlots::processEvent( char *datap
			    , const unsigned char *dsmL0WestInput
                	    , const unsigned char *dsmL0EastInput
                	    , const unsigned short *dsmL1Input
                	    , const unsigned short *dsmL2Input
                	    , const unsigned short *dsmL3Input
                	    ) {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
    if (!BEMCDecoder) BEMCDecoder = new StEmcDecoder();
    //if (!datap || (mDebug >= 2)) cout << "datap = " << (int*)datap << endl;
    if (!dsmL0WestInput || (mDebug >= 2)) cout << "dsmL0WestInput = " << (int*)dsmL0WestInput << endl;
    if (!dsmL0EastInput || (mDebug >= 2)) cout << "dsmL0EastInput = " << (int*)dsmL0EastInput << endl;
    if (!dsmL1Input || (mDebug >= 2)) cout << "dsmL1Input = " << (int*)dsmL1Input << endl;
    if (!dsmL2Input || (mDebug >= 2)) cout << "dsmL2Input = " << (int*)dsmL2Input << endl;
    if (!dsmL3Input || (mDebug >= 2)) cout << "dsmL3Input = " << (int*)dsmL3Input << endl;
    if (!BEMCDecoder || (mDebug >= 2)) cout << "BEMCDecoder = " << BEMCDecoder << endl;

    bool DSM_L0_present = BEMC_DSM_L0_decoder(dsmL0WestInput, dsmL0EastInput, &(this->mDsmL0InputHighTower[0]), &(this->mDsmL0InputPatchSum[0]));

    bool DSM_L1_present = BEMC_DSM_L1_decoder(dsmL1Input, &(this->mDsmL1InputHighTowerBits[0][0]), &(this->mDsmL1InputPatchSum[0][0]));

    bool DSM_L2_present = BEMC_DSM_L2_decoder(dsmL2Input, &(this->mDsmL2InputHighTowerBits[0]), &(this->mDsmL2InputPatchSumBits[0]), &(this->mDsmL2InputPatchSum[0]));

    bool DSM_L3_present = BEMC_DSM_L3_decoder(dsmL3Input, &(this->mDsmL3InputHighTowerBits[0]), &(this->mDsmL3InputPatchSumBits[0]), &(this->mDsmL3InputBackToBackBit[0]), &(this->mDsmL3InputJPsiTopoBit[0]), &(this->mDsmL3InputJetPatchTopoBit[0]));

    if (this->mHistTot) this->mHistTot->Fill(0.5);

    if (DSM_L0_present) {
	int jetPatchSum[12];
	int jetPatchHT[12];
	for (int i = 0;i < 12;i++) {
	    jetPatchSum[i] = 0;
	    jetPatchHT[i] = 0;
	}
	for (int i = 0;i < 300;i++) {
    	    if (this->mHistDsmL0InputHighTower) this->mHistDsmL0InputHighTower->Fill(i, this->mDsmL0InputHighTower[i]);
    	    if (this->mHistDsmL0InputPatchSum) this->mHistDsmL0InputPatchSum->Fill(i, this->mDsmL0InputPatchSum[i]);

	    if (BEMCDecoder) {
		int jetPatch, jetPatchSeq;
		if (BEMCDecoder->GetJetPatchAndSequenceFromTriggerPatch(i, jetPatch, jetPatchSeq)) {
    		    if (jetPatchHT[jetPatch] < this->mDsmL0InputHighTower[i]) jetPatchHT[jetPatch] = this->mDsmL0InputHighTower[i];
    		    jetPatchSum[jetPatch] += this->mDsmL0InputPatchSum[i];
		}
	    }

	    if (mDebug >= 3) cout << "TriggerPatch " << i << ": HighTower = " << this->mDsmL0InputHighTower[i] << ", PatchSum = " << this->mDsmL0InputPatchSum[i] << endl;
	}
	for (int i = 0;i < 12;i++) {
    	    if (this->mHistHighTowerSpectrum[i]) this->mHistHighTowerSpectrum[i]->Fill(jetPatchHT[i]);
    	    if (this->mHistPatchSumSpectrum[i]) this->mHistPatchSumSpectrum[i]->Fill(jetPatchSum[i]);
	}
    }

    if (DSM_L1_present) {
	int ch = 0;
	for (int idsmL1 = 0;idsmL1 < 6;idsmL1++) {
	    for (int idsmL1ch = 0;idsmL1ch < 6;idsmL1ch++) {
		int idsmL1WestFirst = (idsmL1 < 3) ? (idsmL1 + 3) : (idsmL1 - 3);
    		if (this->mHistDsmL1InputHighTowerBits) this->mHistDsmL1InputHighTowerBits->Fill(ch, this->mDsmL1InputHighTowerBits[idsmL1WestFirst][idsmL1ch]);
    		if (this->mHistDsmL1InputPatchSum) this->mHistDsmL1InputPatchSum->Fill(ch, this->mDsmL1InputPatchSum[idsmL1WestFirst][idsmL1ch]);
		ch++;
	    }
	}
    }

    if (DSM_L2_present) {
	for (int ijp = 0;ijp < 12;ijp++) {
    	    if (this->mHistDsmL2InputHighTowerBits) this->mHistDsmL2InputHighTowerBits->Fill(ijp, this->mDsmL2InputHighTowerBits[ijp]);
    	    if (this->mHistDsmL2InputPatchSumBits) this->mHistDsmL2InputPatchSumBits->Fill(ijp, this->mDsmL2InputPatchSumBits[ijp]);
	}
	for (int ijpp = 0;ijpp < 6;ijpp++) {
	    int ijppWestFirst = (ijpp < 3) ? (ijpp + 3) : (ijpp - 3);
    	    if (this->mHistDsmL2InputPatchSum) this->mHistDsmL2InputPatchSum->Fill(ijppWestFirst, this->mDsmL2InputPatchSum[ijpp]);
	}
    }

    if (DSM_L3_present) {
    	    if (this->mHistDsmL3InputHighTowerBits) this->mHistDsmL3InputHighTowerBits->Fill(this->mDsmL3InputHighTowerBits[0]);
    	    if (this->mHistDsmL3InputPatchSumBits) this->mHistDsmL3InputPatchSumBits->Fill(this->mDsmL3InputPatchSumBits[0]);
    	    if (this->mHistDsmL3InputBackToBackBit) this->mHistDsmL3InputBackToBackBit->Fill(this->mDsmL3InputBackToBackBit[0]);
    	    if (this->mHistDsmL3InputJPsiTopoBit) this->mHistDsmL3InputJPsiTopoBit->Fill(this->mDsmL3InputJPsiTopoBit[0]);
    	    if (this->mHistDsmL3InputJetPatchTopoBit) this->mHistDsmL3InputJetPatchTopoBit->Fill(this->mDsmL3InputJetPatchTopoBit[0]);    
    }

    daqReader *rdr = (daqReader*)(datap);
    daq_dta *dd_btow = rdr ? (rdr->det("btow")->get("adc")) : 0;
    if (dd_btow) while (dd_btow->iterate()) {
	btow_t *d = (btow_t *) dd_btow->Void;
	if (d) {
		if (DSM_L0_present) {
		    for (int i = 0;i < 300;i++) {
			this->mDsmSimuHighTower[i] = 0;
			this->mDsmSimuPatchSum[i] = 0;
		    }
		}
		for(int i = 0;i < (BTOW_MAXFEE * BTOW_DATSIZE);i++) {
		    int tdc = i % BTOW_MAXFEE;
		    int tdc_channel = i / BTOW_MAXFEE;
	    	    int count = d->preamble[tdc][0];
		    int error = 0;//(*(header + tdc + 30));
		    if((error==0 && count==164) || true) {
			// OK
			int softId;
			if (BEMCDecoder && BEMCDecoder->GetTowerIdFromDaqId(i, softId)) {
			    if ((softId >= 1) && (softId <= 4800)) {
				int adc = d->adc[tdc][tdc_channel];
				if ((softId >= 1) && (softId <= 1220)) {
				    if (this->mHistRawAdc1) this->mHistRawAdc1->Fill(softId, adc);
				} else if ((softId >= 1221) && (softId <= 2400)) {
				    if (this->mHistRawAdc2) this->mHistRawAdc2->Fill(softId, adc);
				} else if ((softId >= 2401) && (softId <= 3540)) {
				    if (this->mHistRawAdc3) this->mHistRawAdc3->Fill(softId, adc);
				} else if ((softId >= 3541) && (softId <= 4800)) {
				    if (this->mHistRawAdc4) this->mHistRawAdc4->Fill(softId, adc);
				}
				if (DSM_L0_present && (this->mTowerData[softId - 1][0] != 0)) {
				    int crate, crateSeq;
				    if (BEMCDecoder->GetTowerCrateFromDaqId(i, crate, crateSeq)) {
					int triggerPatch;
					if (BEMCDecoder->GetTriggerPatchFromCrate(crate, crateSeq, triggerPatch)) {
					    if ((triggerPatch >= 0) && (triggerPatch < 300)) {
						int ht, pa;
						if (triggerPatch == 27300) cout << "SoftId " << softId << " ";
						simulateFEEaction(adc, this->mTowerData[softId - 1][2], this->mPatchData[triggerPatch][2], ht, pa, (triggerPatch == 27300));
						if (ht > this->mDsmSimuHighTower[triggerPatch]) this->mDsmSimuHighTower[triggerPatch] = ht;
						this->mDsmSimuPatchSum[triggerPatch] += pa;
					    }
					}
				    }
				}
			    }
			}
		    }
		}
		for (int i = 0;i < 300;i++) {
		    if (this->mPatchData[i][0] == 0) this->mDsmSimuHighTower[i] = 0;
		    if (this->mPatchData[i][1] == 0) {
			this->mDsmSimuPatchSum[i] = 0;
		    } else {
			int lut;
			simulateFEELUT(this->mDsmSimuPatchSum[i], this->mPatchData[i][3], this->mPatchData[i][4], this->mPatchData[i][5], this->mPatchData[i][6], this->mPatchData[i][7], this->mPatchData[i][8], this->mPatchData[i][9], this->mPatchData[i][10], (int)(this->mTriggerPedestalShift / 100.0), lut, (i == 27300));
			this->mDsmSimuPatchSum[i] = lut;
		    }
		    //cout << "Trigger patch " << i;
		    if (this->mDsmL0InputHighTower[i] != this->mDsmSimuHighTower[i]) {
			if (mDebug >= 2) cout << i << ": HT " << this->mDsmSimuHighTower[i] << " != " << this->mDsmL0InputHighTower[i] << endl;
			if (this->mHistTriggerCorruptionHighTower) this->mHistTriggerCorruptionHighTower->Fill(i);
			if (this->mHistTriggerCorruptionHighTowerCorr) this->mHistTriggerCorruptionHighTowerCorr->Fill(this->mDsmL0InputHighTower[i], this->mDsmSimuHighTower[i]);
		    } else {
			//cout << "- " << i << "HT OK" << endl;
		    }
		    if (this->mDsmL0InputPatchSum[i] != this->mDsmSimuPatchSum[i]) {
			if (mDebug >= 2) cout << i << ": PA " << this->mDsmSimuPatchSum[i] << " != " << this->mDsmL0InputPatchSum[i] << endl;
			if (this->mHistTriggerCorruptionPatchSum) this->mHistTriggerCorruptionPatchSum->Fill(i);
			if (this->mHistTriggerCorruptionPatchSumCorr) this->mHistTriggerCorruptionPatchSumCorr->Fill(this->mDsmL0InputPatchSum[i], this->mDsmSimuPatchSum[i]);
		    } else {
			//cout << "- " << i << "PA OK" << endl;
		    }
		    //cout << endl;
		}
	}
    }
    for (int bsmd_fiber = 0;bsmd_fiber < BSMD_FIBERS;bsmd_fiber++) {
	int bprs_fiber = bsmd_fiber - 8;
	daq_dta *dd_bsmd = rdr ? (rdr->det("bsmd")->get("adc", 0, bsmd_fiber)) : 0;
	if (dd_bsmd) while (dd_bsmd->iterate()) {
	    bsmd_t *d = (bsmd_t *) dd_bsmd->Void;
	    if (d && BEMCDecoder) {

	    int det, m, e, s;
	    int feeSum[120];
	    int softId, box, wire, Avalue;
	    int pmtSum[60];
	    for (int i = 0;i < 120;i++) feeSum[i] = 0;
	    for (int i = 0;i < 60;i++) pmtSum[i] = 0;
	    for (int fiber_channel = 0;fiber_channel < BSMD_DATSIZE;fiber_channel++) {
		    if (BEMCDecoder->GetSmdCoord(bsmd_fiber, fiber_channel, det, m, e, s)) {
			if ((m >= 1) && (m <= 120)) {
			    int adc = d->adc[fiber_channel];
			    feeSum[m - 1] += adc;
			}
		    }
		    if (BEMCDecoder->GetPsdId(bprs_fiber, fiber_channel, softId, box, wire, Avalue)) {
			if ((box >= 1) && (box <= 60)) {
    			    int adc = d->adc[fiber_channel];
			    pmtSum[box - 1] += adc;
			    if ((softId >= 1) && (softId <= 1220)) {
			        if (this->mHistRawAdcPsd1) this->mHistRawAdcPsd1->Fill(softId, adc);
			    } else if ((softId >= 1221) && (softId <= 2400)) {
			        if (this->mHistRawAdcPsd2) this->mHistRawAdcPsd2->Fill(softId, adc);
			    } else if ((softId >= 2401) && (softId <= 3540)) {
			        if (this->mHistRawAdcPsd3) this->mHistRawAdcPsd3->Fill(softId, adc);
			    } else if ((softId >= 3541) && (softId <= 4800)) {
			        if (this->mHistRawAdcPsd4) this->mHistRawAdcPsd4->Fill(softId, adc);
			    }
			}
		    }
	    }
	    for (int i = 0;i < 120;i++) {
		if (this->mHistSmdFeeSum) this->mHistSmdFeeSum->Fill(i + 1, feeSum[i]);
	    }
	    for (int i = 0;i < 60;i++) {
		if (this->mHistPsdFeeSum) this->mHistPsdFeeSum->Fill(i + 1, pmtSum[i]);
	    }

	    }
	}
    }

    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}
//-------------------------------------------------------------------
