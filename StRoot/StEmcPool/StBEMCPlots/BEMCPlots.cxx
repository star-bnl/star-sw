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
#include <TDatime.h>

#ifndef NEW_DAQ_READER
#	include "evpReader.hh"
#	include "emcReader.h"
#	include "trgReader.h"
#define BSMD_FIBERS     12
#define BSMD_DATSIZE    4800
#else
#	include "DAQ_READER/daqReader.h"
#	include "DAQ_READER/daq_dta.h"
#	include "DAQ_BSMD/daq_bsmd.h"
#	include "DAQ_BTOW/daq_btow.h"
#	include "DAQ_EMC/daq_emc.h"
#	include "DAQ_TRG/daq_trg.h"
#       include <RTS/include/daqFormats.h>
#endif

#include <StEmcUtil/database/StEmcDecoder.h>
#include <StEmcUtil/geometry/StEmcGeom.h>

#include "BEMCPlots.h"
#include "BEMC_DSM_decoder.h"
#include "BEMCPlotsNames.h"


BEMCPlots *BEMCPlotsInstance = 0;
StEmcDecoder *BEMCDecoder = 0;


#define BEMCOK 1
#define BEMCNOTINSTALLED 2
#define BEMCCORRUPTED 3


//--------------------------------------



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

    this->mHistTot = new TH1F(HistTotName, "BEMC, Total number of events processed;;Number of events", 1, 0, 1);
    ADDHIST(this->mHistTot)

    this->mHistDsmL0InputHighTower = new TH2F(HistDsmL0InputHighTowerName, "BEMC DSM L0 Input - HighTower;triggerPatch;HighTower", 300, -0.5, 300 - 0.5, 64, 0, 64);
    ADDHIST(this->mHistDsmL0InputHighTower)
    this->mHistDsmL0InputPatchSum = new TH2F(HistDsmL0InputPatchSumName, "BEMC DSM L0 Input - PatchSum;triggerPatch;PatchSum", 300, -0.5, 300 - 0.5, 64, 0, 64);
    ADDHIST(this->mHistDsmL0InputPatchSum)

    this->mHistDsmL1InputHighTowerBits = new TH2F(HistDsmL1InputHighTowerBitsName, "BEMC DSM L1 Input - HighTower bits;DSM Level-1 channels;HighTower bits", 36, -0.5, 36-0.5, 5, 0, 5);
    ADDHIST(this->mHistDsmL1InputHighTowerBits)
    this->mHistDsmL1InputPatchSum = new TH2F(HistDsmL1InputPatchSumName, "BEMC DSM L1 Input - PatchSum;Channel;PatchSum", 36, -0.5, 36-0.5, 128, 0, 256/*1024*/);
    ADDHIST(this->mHistDsmL1InputPatchSum)

    this->mHistDsmL2InputHighTowerBits = new TH2F(HistDsmL2InputHighTowerBitsName, "BEMC DSM L2 Input - HighTower bits;JetPatch;HighTower bits", BEMCNJET, -0.5, BEMCNJET-0.5, 5, 0, 5);
    ADDHIST(this->mHistDsmL2InputHighTowerBits)
    this->mHistDsmL2InputPatchSumBits = new TH2F(HistDsmL2InputPatchSumBitsName, "BEMC DSM L2 Input - PatchSum bits;JetPatch;PatchSum bits", BEMCNJET, -0.5, BEMCNJET-0.5, 4, 0, 4);
    ADDHIST(this->mHistDsmL2InputPatchSumBits)
    this->mHistDsmL2InputPatchSum = new TH2F(HistDsmL2InputPatchSumName, "BEMC DSM L2 Input - PatchSum;JetPatch pair;PatchSum", 6, -0.5, 6-0.5, 256, 0, 256);
    ADDHIST(this->mHistDsmL2InputPatchSum)

    this->mHistDsmL3InputHighTowerBits = new TH1F(HistDsmL3InputHighTowerBitsName, "BEMC DSM L3 Input - HighTower bits;HighTower bits", 4, -0.5, 4-0.5);
    ADDHIST(this->mHistDsmL3InputHighTowerBits)
    this->mHistDsmL3InputPatchSumBits = new TH1F(HistDsmL3InputPatchSumBitsName, "BEMC DSM L3 Input - PatchSum bits;PatchSum bits", 4, -0.5, 4-0.5);
    ADDHIST(this->mHistDsmL3InputPatchSumBits)
    this->mHistDsmL3InputBackToBackBit = new TH1F(HistDsmL3InputBackToBackBitName, "BEMC DSM L3 Input - Back-to-Back bit;Back-to-Back bit", 2, -0.5, 2-0.5);
    ADDHIST(this->mHistDsmL3InputBackToBackBit)
    this->mHistDsmL3InputJPsiTopoBit = new TH1F(HistDsmL3InputJPsiTopoBitName, "BEMC DSM L3 Input - J/Psi topology bit;J/Psi bit", 2, -0.5, 2-0.5);
    ADDHIST(this->mHistDsmL3InputJPsiTopoBit)
    this->mHistDsmL3InputJetPatchTopoBit = new TH1F(HistDsmL3InputJetPatchTopoBitName, "BEMC DSM L3 Input - JetPatch topology bit;JetPatch bit", 2, -0.5, 2-0.5);
    ADDHIST(this->mHistDsmL3InputJetPatchTopoBit)

      this->mHistRawAdc1 = new TH2F(HistRawAdc1Name, "BTOW ADC, 1 <= SoftId <= 1220;SoftId;ADC",    1220, 0000.5, 1220.5, 300, -0.5, 4096-0.5);
      this->mHistRawAdc2 = new TH2F(HistRawAdc2Name, "BTOW ADC, 1221 <= SoftId <= 2400;SoftId;ADC", 1180, 1220.5, 2400.5, 300, -0.5, 4096-0.5);
      this->mHistRawAdc3 = new TH2F(HistRawAdc3Name, "BTOW ADC, 2401 <= SoftId <= 3540;SoftId;ADC", 1140, 2400.5, 3540.5, 300, -0.5, 4096-0.5);
      this->mHistRawAdc4 = new TH2F(HistRawAdc4Name, "BTOW ADC, 3541 <= SoftId <= 4800;SoftId;ADC", 1260, 3540.5, 4800.5, 300, -0.5, 4096-0.5);

      this->mHistRawAdc1zoom = new TH2F(HistRawAdc1NameZoom, "BTOW ADC, 1 <= SoftId <= 1220;SoftId;ADC",    1220, 0000.5, 1220.5, 300, -0.5, 500-0.5);
      this->mHistRawAdc2zoom = new TH2F(HistRawAdc2NameZoom, "BTOW ADC, 1221 <= SoftId <= 2400;SoftId;ADC", 1180, 1220.5, 2400.5, 300, -0.5, 500-0.5);
      this->mHistRawAdc3zoom = new TH2F(HistRawAdc3NameZoom, "BTOW ADC, 2401 <= SoftId <= 3540;SoftId;ADC", 1140, 2400.5, 3540.5, 300, -0.5, 500-0.5);
      this->mHistRawAdc4zoom = new TH2F(HistRawAdc4NameZoom, "BTOW ADC, 3541 <= SoftId <= 4800;SoftId;ADC", 1260, 3540.5, 4800.5, 300, -0.5, 500-0.5);

      this->mHistRawAdcPsd1 = new TH2F(HistRawAdcPsd1Name, "BPRS ADC, 1 <= SoftId <= 1220;SoftId;ADC",    1220, 0000.5, 1220.5, 300, -0.5, 1000-0.5);
      this->mHistRawAdcPsd2 = new TH2F(HistRawAdcPsd2Name, "BPRS ADC, 1221 <= SoftId <= 2400;SoftId;ADC", 1180, 1220.5, 2400.5, 300, -0.5, 1000-0.5);
      this->mHistRawAdcPsd3 = new TH2F(HistRawAdcPsd3Name, "BPRS ADC, 2401 <= SoftId <= 3540;SoftId;ADC", 1140, 2400.5, 3540.5, 300, -0.5, 1000-0.5);
      this->mHistRawAdcPsd4 = new TH2F(HistRawAdcPsd4Name, "BPRS ADC, 3541 <= SoftId <= 4800;SoftId;ADC", 1260, 3540.5, 4800.5, 300, -0.5, 1000-0.5);

      this->mHistSmdFeeSum = new TH2F(HistSmdFeeSumName, "BSMD FEE Sum;Module;Sum", 120, 0.5, 120+0.5, 100, -0.5, 5000-0.5);
      this->mHistPsdFeeSum = new TH2F(HistPsdFeeSumName, "BPRS FEE Sum;PMT Box;Sum", 60, 0.5, 60+0.5, 100, -0.5, 5000-0.5);

      this->mHistSmdFeeSumNonZS = new TH2F(HistSmdFeeSumNonZSName, "BSMD FEE Sum, Non-ZS;Module;Sum", 120, 0.5, 120+0.5, 100, -0.5, 100000-0.5);
      this->mHistPsdFeeSumNonZS = new TH2F(HistPsdFeeSumNonZSName, "BPRS FEE Sum, Non-ZS;PMT Box;Sum", 60, 0.5, 60+0.5, 100, -0.5, 40000-0.5);

      ADDHIST(this->mHistRawAdc1)
      ADDHIST(this->mHistRawAdc2)
      ADDHIST(this->mHistRawAdc3)
      ADDHIST(this->mHistRawAdc4)

      ADDHIST(this->mHistRawAdc1zoom)
      ADDHIST(this->mHistRawAdc2zoom)
      ADDHIST(this->mHistRawAdc3zoom)
      ADDHIST(this->mHistRawAdc4zoom)

      ADDHIST(this->mHistRawAdcPsd1)
      ADDHIST(this->mHistRawAdcPsd2)
      ADDHIST(this->mHistRawAdcPsd3)
      ADDHIST(this->mHistRawAdcPsd4)

      ADDHIST(this->mHistSmdFeeSum)
      ADDHIST(this->mHistPsdFeeSum)

      ADDHIST(this->mHistSmdFeeSumNonZS)
      ADDHIST(this->mHistPsdFeeSumNonZS)
    
    for (int i = 0;i < BEMCNJET;i++) {
	TString name;
	TString title;
	name = Form("%s_%u", HistHighTowerSpectrumName, i);
	title = Form("JetPatch %u - HighTower spectrum;HighTower", i);	
	this->mHistHighTowerSpectrum[i] = new TH1F(name.Data(), title.Data(), 64, -0.5, 64-0.5);
	ADDHIST(this->mHistHighTowerSpectrum[i])
	name = Form("%s_%u", HistPatchSumSpectrumName, i);
	title = Form("JetPatch %u - PatchSum spectrum;PatchSum", i);	
	this->mHistPatchSumSpectrum[i] = new TH1F(name.Data(), title.Data(), 200, -0.5, 300-0.5);
	ADDHIST(this->mHistPatchSumSpectrum[i])
    }
      
      this->mHistTriggerCorruptionHighTower = new TH1F(HistTriggerCorruptionHighTowerName, "HighTower DSM L0 Input; triggerPatch;events", 300, -0.5, 300-0.5);
      this->mHistTriggerCorruptionPatchSum = new TH1F(HistTriggerCorruptionPatchSumName, "PatchSum DSM L0 Input; triggerPatch;events", 300, -0.5, 300-0.5);
      this->mHistDSM0HTCorr = new TH2F(HistDSM0HTCorrName, "HighTower DSM L0 Input; DSM HighTower;Simulated HighTower", 64, 0, 64, 64, 0, 64);
      this->mHistDSM0TPCorr = new TH2F(HistDSM0TPCorrName, "PatchSum DSM L0 Input; DSM PatchSum;Simulated PatchSum", 64, 0, 64, 64, 0, 64);
      
      ADDHIST(this->mHistTriggerCorruptionHighTower)
      ADDHIST(this->mHistTriggerCorruptionPatchSum)
      ADDHIST(this->mHistDSM0HTCorr)
      ADDHIST(this->mHistDSM0TPCorr)
	
      this->mHist_TDC_status      = new TH2F(Hist_TDC_statusName, "BEMC TDC Status;0=total 1=OK 2=Not Installed3=Corrupted",5,-0.5,4.5,30,-0.5,29.5);
      this->mHist_SMD_status      = new TH2F(Hist_SMD_statusName, "BEMC SMD Status;0=total 1=OK 2=Not Installed3=Corrupted",5,-0.5,4.5,8,-0.5,7.5);
      this->mHist_PSD_status      = new TH2F(Hist_PSD_statusName, "BEMC PSD Status;0=total 1=OK 2=Not Installed 3=Corrupted",5,-0.5,4.5,4,-0.5,3.5);
      this->mHist_BTOW_Corruption = new TH1F(Hist_BTOW_CorruptionName, "BEMC TDC corruption frequency;0=total 1=OK 2=Not Installed 3=Corrupted",5,-0.5,4.5);
      
      this->mHist_btow_spectra_1  = new TH2F(Hist_btow_spectra_1Name, "BEMC tower spectrum  0 < TDC < 10;X = 160*TDC + index", 1600,  -0.5,1599.5,100,0,500);
      this->mHist_btow_spectra_2  = new TH2F(Hist_btow_spectra_2Name, "BEMC tower spectrum 10 < TDC < 20;X = 160*TDC + index", 1600,1599.5,3199.5,100,0,500);
      this->mHist_btow_spectra_3  = new TH2F(Hist_btow_spectra_3Name, "BEMC tower spectrum 20 < TDC < 30;X = 160*TDC + index", 1600,3199.5,4799.5,100,0,500);
      
    this->mHist_smd_spectra     = new TH1F(Hist_smd_spectraName, "BEMC SMD total ADC",250,100000.,6000000.);
    this->mHist_smd_spectraNonZS= new TH1F(Hist_smd_spectraNonZSName, "BEMC SMD total ADC, Non-ZS",250,100000.,6000000.);
    this->mHist_smd_capacitor   = new TH2F(Hist_smd_capacitorName, "BEMC SMD capacitor distribution",128,-0.5,127.5,8,-0.5,7.5);
    this->mHist_smd_sum         = new TH2F(Hist_smd_sumName, "BEMC SMD total ADC per fiber",250,100000.,1000000.,8,-0.5,7.5);
    this->mHist_psd_spectra     = new TH1F(Hist_psd_spectraName, "BEMC PSD total ADC",250,100000.,4000000.);
    this->mHist_psd_spectraNonZS= new TH1F(Hist_psd_spectraNonZSName, "BEMC PSD total ADC, Non-ZS",250,100000.,4000000.);
    this->mHist_psd_capacitor   = new TH2F(Hist_psd_capacitorName, "BEMC PSD capacitor distribution",128,-0.5,127.5,4,-0.5,3.5);
    this->mHist_psd_sum         = new TH2F(Hist_psd_sumName, "BEMC PSD total ADC per fiber",250,100000.,1000000.,4,-0.5,3.5);

    this->mHist_HTMAX_spectra   = new TH2F(Hist_HTMAX_spectraName, "BEMC Maximum High Tower spectrum", 300,-0.5,299.5,64,-0.5,63.5);
    this->mHist_PAMAX_spectra   = new TH2F(Hist_PAMAX_spectraName, "BEMC Maximum Patch Sum spectrum", 300,-0.5,299.5,64,-0.5,63.5);
    this->mHist_HTMAX_dist      = new TH1F(Hist_HTMAX_distName, "BEMC Maximum High Tower distribution", 300,-0.5,299.5);
    this->mHist_PAMAX_dist      = new TH1F(Hist_PAMAX_distName, "BEMC Maximum Patch Sum distribution", 300,-0.5,299.5);

    this->mHist_JET_spectra     = new TH2F(Hist_JET_spectraName, "BEMC Jet sum spectrum", BEMCNJET,-0.5,BEMCNJET-0.5,80,-0.5,79.5);
    this->mHist_JETMAX_spectra  = new TH2F(Hist_JETMAX_spectraName, "BEMC Maximum Jet sum spectrum", BEMCNJET,-0.5,BEMCNJET-0.5,80,-0.5,79.5);
    this->mHist_JET_ped         = new TH2F(Hist_JET_pedName, "BEMC Jet sum pedestal", BEMCNJET,-0.5,BEMCNJET-0.5,30,15,45);
    this->mHist_JETMAX_dist     = new TH1F(Hist_JETMAX_distName, "BEMC Maximum Jet sum distribution", BEMCNJET,-0.5,BEMCNJET-0.5);

    ADDHIST(this->mHist_TDC_status)
    ADDHIST(this->mHist_SMD_status)
    ADDHIST(this->mHist_PSD_status)
    ADDHIST(this->mHist_BTOW_Corruption)

    ADDHIST(this->mHist_btow_spectra_1)
    ADDHIST(this->mHist_btow_spectra_2)
    ADDHIST(this->mHist_btow_spectra_3)

    ADDHIST(this->mHist_smd_spectra)
    ADDHIST(this->mHist_smd_spectraNonZS)
    ADDHIST(this->mHist_smd_capacitor)
    ADDHIST(this->mHist_smd_sum)
    ADDHIST(this->mHist_psd_spectra)
    ADDHIST(this->mHist_psd_spectraNonZS)
    ADDHIST(this->mHist_psd_capacitor)
    ADDHIST(this->mHist_psd_sum)

    ADDHIST(this->mHist_HTMAX_spectra)
    ADDHIST(this->mHist_PAMAX_spectra)
    ADDHIST(this->mHist_HTMAX_dist)
    ADDHIST(this->mHist_PAMAX_dist)

    ADDHIST(this->mHist_JET_spectra)
    ADDHIST(this->mHist_JETMAX_spectra)
    ADDHIST(this->mHist_JET_ped)
    ADDHIST(this->mHist_JETMAX_dist)

    this->mHist_ADCEtaPhi_TowHits = new TH2F(Hist_ADCEtaPhi_TowHitsName, "Tower hits>ped+20; Phi Bin; Eta Bin",120 ,-3.15 ,3.15, 40, -1, 1); 
    this->mHist_ADCEtaPhi_Pre1Hits = new TH2F(Hist_ADCEtaPhi_Pre1HitsName, "BPSD hits>ped+20;Phi Bin; Eta Bin",120, -3.15, 3.15, 40, -1, 1);     
     
    ADDHIST(this->mHist_ADCEtaPhi_TowHits)
    ADDHIST(this->mHist_ADCEtaPhi_Pre1Hits)
      

#undef ADDHIST

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
    memset(this->BEMCJPPED, 0, sizeof(this->BEMCJPPED));
    memset(this->BEMCNJPPED, 0, sizeof(this->BEMCNJPPED));

}
//-------------------------------------------------------------------
BEMCPlots::~BEMCPlots() {
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;

#define DELETEHIST(HIST) if (HIST) delete (HIST); (HIST) = 0;

    DELETEHIST(this->mHistTot)

    DELETEHIST(this->mHistDsmL0InputHighTower)
    DELETEHIST(this->mHistDsmL0InputPatchSum)

    DELETEHIST(this->mHistDsmL1InputHighTowerBits)
    DELETEHIST(this->mHistDsmL1InputPatchSum)

    DELETEHIST(this->mHistDsmL2InputHighTowerBits)
    DELETEHIST(this->mHistDsmL2InputPatchSumBits)
    DELETEHIST(this->mHistDsmL2InputPatchSum)

    DELETEHIST(this->mHistDsmL3InputHighTowerBits)
    DELETEHIST(this->mHistDsmL3InputPatchSumBits)
    DELETEHIST(this->mHistDsmL3InputBackToBackBit)
    DELETEHIST(this->mHistDsmL3InputJPsiTopoBit)
    DELETEHIST(this->mHistDsmL3InputJetPatchTopoBit)

    DELETEHIST(this->mHistRawAdc1)
    DELETEHIST(this->mHistRawAdc2)
    DELETEHIST(this->mHistRawAdc3)
    DELETEHIST(this->mHistRawAdc4)

    DELETEHIST(this->mHistRawAdc1zoom)
    DELETEHIST(this->mHistRawAdc2zoom)
    DELETEHIST(this->mHistRawAdc3zoom)
    DELETEHIST(this->mHistRawAdc4zoom)

    DELETEHIST(this->mHistRawAdcPsd1)
    DELETEHIST(this->mHistRawAdcPsd2)
    DELETEHIST(this->mHistRawAdcPsd3)
    DELETEHIST(this->mHistRawAdcPsd4)

    DELETEHIST(this->mHistSmdFeeSum)
    DELETEHIST(this->mHistPsdFeeSum)

    DELETEHIST(this->mHistSmdFeeSumNonZS)
    DELETEHIST(this->mHistPsdFeeSumNonZS)

    for (int i = 0;i < BEMCNJET;i++) {
	DELETEHIST(this->mHistHighTowerSpectrum[i])
    	DELETEHIST(this->mHistPatchSumSpectrum[i])
    }

    DELETEHIST(this->mHistTriggerCorruptionHighTower)
    DELETEHIST(this->mHistTriggerCorruptionPatchSum)
    DELETEHIST(this->mHistDSM0HTCorr)
    DELETEHIST(this->mHistDSM0TPCorr)

    DELETEHIST(this->mHist_TDC_status)
    DELETEHIST(this->mHist_SMD_status)
    DELETEHIST(this->mHist_PSD_status)
    DELETEHIST(this->mHist_BTOW_Corruption)

    DELETEHIST(this->mHist_btow_spectra_1)
    DELETEHIST(this->mHist_btow_spectra_2)
    DELETEHIST(this->mHist_btow_spectra_3)

    DELETEHIST(this->mHist_smd_spectra)
    DELETEHIST(this->mHist_smd_spectraNonZS)
    DELETEHIST(this->mHist_smd_capacitor)
    DELETEHIST(this->mHist_smd_sum)
    DELETEHIST(this->mHist_psd_spectra)
    DELETEHIST(this->mHist_psd_spectraNonZS)
    DELETEHIST(this->mHist_psd_capacitor)
    DELETEHIST(this->mHist_psd_sum)

    DELETEHIST(this->mHist_HTMAX_spectra)
    DELETEHIST(this->mHist_PAMAX_spectra)
    DELETEHIST(this->mHist_HTMAX_dist)
    DELETEHIST(this->mHist_PAMAX_dist)

    DELETEHIST(this->mHist_JET_spectra)
    DELETEHIST(this->mHist_JETMAX_spectra)
    DELETEHIST(this->mHist_JET_ped)

    DELETEHIST(this->mHist_JETMAX_dist)
      
    DELETEHIST(this->mHist_ADCEtaPhi_TowHits)
    DELETEHIST(this->mHist_ADCEtaPhi_Pre1Hits)
    
    
#undef DELETEHIST

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

#define RESETHIST(HIST) if (HIST) (HIST)->Reset();

    RESETHIST(this->mHistTot)

    RESETHIST(this->mHistDsmL0InputHighTower)
    RESETHIST(this->mHistDsmL0InputPatchSum)

    RESETHIST(this->mHistDsmL1InputHighTowerBits)
    RESETHIST(this->mHistDsmL1InputPatchSum)

    RESETHIST(this->mHistDsmL2InputHighTowerBits)
    RESETHIST(this->mHistDsmL2InputPatchSumBits)
    RESETHIST(this->mHistDsmL2InputPatchSum)

    RESETHIST(this->mHistDsmL3InputHighTowerBits)
    RESETHIST(this->mHistDsmL3InputPatchSumBits)
    RESETHIST(this->mHistDsmL3InputBackToBackBit)
    RESETHIST(this->mHistDsmL3InputJPsiTopoBit)
    RESETHIST(this->mHistDsmL3InputJetPatchTopoBit)

    RESETHIST(this->mHistRawAdc1)
    RESETHIST(this->mHistRawAdc2)
    RESETHIST(this->mHistRawAdc3)
    RESETHIST(this->mHistRawAdc4)

    RESETHIST(this->mHistRawAdc1zoom)
    RESETHIST(this->mHistRawAdc2zoom)
    RESETHIST(this->mHistRawAdc3zoom)
    RESETHIST(this->mHistRawAdc4zoom)

    RESETHIST(this->mHistRawAdcPsd1)
    RESETHIST(this->mHistRawAdcPsd2)
    RESETHIST(this->mHistRawAdcPsd3)
    RESETHIST(this->mHistRawAdcPsd4)

    RESETHIST(this->mHistSmdFeeSum)
    RESETHIST(this->mHistPsdFeeSum)

    RESETHIST(this->mHistSmdFeeSumNonZS)
    RESETHIST(this->mHistPsdFeeSumNonZS)

    for (int i = 0;i < BEMCNJET;i++) {
    	RESETHIST(this->mHistHighTowerSpectrum[i])
    	RESETHIST(this->mHistPatchSumSpectrum[i])
    }

    RESETHIST(this->mHistTriggerCorruptionHighTower)
    RESETHIST(this->mHistTriggerCorruptionPatchSum)
    RESETHIST(this->mHistDSM0HTCorr)
    RESETHIST(this->mHistDSM0TPCorr)

    RESETHIST(this->mHist_TDC_status)
    RESETHIST(this->mHist_SMD_status)
    RESETHIST(this->mHist_PSD_status)
    RESETHIST(this->mHist_BTOW_Corruption)

    RESETHIST(this->mHist_btow_spectra_1)
    RESETHIST(this->mHist_btow_spectra_2)
    RESETHIST(this->mHist_btow_spectra_3)

    RESETHIST(this->mHist_smd_spectra)
    RESETHIST(this->mHist_smd_spectraNonZS)
    RESETHIST(this->mHist_smd_capacitor)
    RESETHIST(this->mHist_smd_sum)
    RESETHIST(this->mHist_psd_spectra)
    RESETHIST(this->mHist_psd_spectraNonZS)
    RESETHIST(this->mHist_psd_capacitor)
    RESETHIST(this->mHist_psd_sum)

    RESETHIST(this->mHist_HTMAX_spectra)
    RESETHIST(this->mHist_PAMAX_spectra)
    RESETHIST(this->mHist_HTMAX_dist)
    RESETHIST(this->mHist_PAMAX_dist)

    RESETHIST(this->mHist_JET_spectra)
    RESETHIST(this->mHist_JETMAX_spectra)
    RESETHIST(this->mHist_JET_ped)
    RESETHIST(this->mHist_JETMAX_dist)

    RESETHIST(this->mHist_ADCEtaPhi_TowHits)
    RESETHIST(this->mHist_ADCEtaPhi_Pre1Hits)


#undef RESETHIST

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
    memset(this->BEMCJPPED, 0, sizeof(this->BEMCJPPED));
    memset(this->BEMCNJPPED, 0, sizeof(this->BEMCNJPPED));
 
    if (!BEMCDecoder) BEMCDecoder = new StEmcDecoder();

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
		    int softId, crate, crateSeq, unmaskTower, unmaskHT, unmaskPA, triggerPatch;
		    float ped;
		    ifstr >> softId >> crate >> crateSeq >> unmaskTower >> unmaskHT >> unmaskPA >> ped >> triggerPatch;
		    if (mDebug >= 2) cout << "Read: " << token << " " << softId << "\t" << crate << "\t" << crateSeq << "\t" << unmaskTower << "\t" << unmaskHT << "\t" << unmaskPA << "\t" << ped << "\t" << triggerPatch << endl;
		    if ((softId >= 1) && (softId <= 4800)) {
		      towerPed[softId-1]=ped;
		      this->mTowerData[softId - 1][0] = unmaskTower;
		      this->mTowerData[softId - 1][1] = int(ped * 100.0);
		      //int triggerPatch;
		      //if ((unmaskTower == 0) && BEMCDecoder && BEMCDecoder->GetTriggerPatchFromCrate(crate, crateSeq, triggerPatch)) {
		      if ((triggerPatch >= 0) && (triggerPatch < 300) && (unmaskTower == 0)) {
			this->mPatchData[triggerPatch][10] += 1;
		      }
		      //}
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

#define SAVEHIST(HIST) if (HIST) (HIST)->Write();

        SAVEHIST(this->mHistTot)

        SAVEHIST(this->mHistDsmL0InputHighTower)
        SAVEHIST(this->mHistDsmL0InputPatchSum)

        SAVEHIST(this->mHistDsmL1InputHighTowerBits)
        SAVEHIST(this->mHistDsmL1InputPatchSum)

        SAVEHIST(this->mHistDsmL2InputHighTowerBits)
        SAVEHIST(this->mHistDsmL2InputPatchSumBits)
        SAVEHIST(this->mHistDsmL2InputPatchSum)

	SAVEHIST(this->mHistDsmL3InputHighTowerBits)
        SAVEHIST(this->mHistDsmL3InputPatchSumBits)
        SAVEHIST(this->mHistDsmL3InputBackToBackBit)
        SAVEHIST(this->mHistDsmL3InputJPsiTopoBit)
        SAVEHIST(this->mHistDsmL3InputJetPatchTopoBit)

        SAVEHIST(this->mHistRawAdc1)
        SAVEHIST(this->mHistRawAdc2)
        SAVEHIST(this->mHistRawAdc3)
        SAVEHIST(this->mHistRawAdc4)

	SAVEHIST(this->mHistRawAdc1zoom)
        SAVEHIST(this->mHistRawAdc2zoom)
        SAVEHIST(this->mHistRawAdc3zoom)
        SAVEHIST(this->mHistRawAdc4zoom)

        SAVEHIST(this->mHistRawAdcPsd1)
        SAVEHIST(this->mHistRawAdcPsd2)
        SAVEHIST(this->mHistRawAdcPsd3)
        SAVEHIST(this->mHistRawAdcPsd4)

        SAVEHIST(this->mHistSmdFeeSum)
        SAVEHIST(this->mHistPsdFeeSum)

        SAVEHIST(this->mHistSmdFeeSumNonZS)
        SAVEHIST(this->mHistPsdFeeSumNonZS)

        for (int i = 0;i < BEMCNJET;i++) {
    	    SAVEHIST(this->mHistHighTowerSpectrum[i])
    	    SAVEHIST(this->mHistPatchSumSpectrum[i])
	}

        SAVEHIST(this->mHistTriggerCorruptionHighTower)
        SAVEHIST(this->mHistTriggerCorruptionPatchSum)
        SAVEHIST(this->mHistDSM0HTCorr)
        SAVEHIST(this->mHistDSM0TPCorr)

	SAVEHIST(this->mHist_TDC_status)
	SAVEHIST(this->mHist_SMD_status)
	SAVEHIST(this->mHist_PSD_status)
	SAVEHIST(this->mHist_BTOW_Corruption)

	SAVEHIST(this->mHist_btow_spectra_1)
	SAVEHIST(this->mHist_btow_spectra_2)
	SAVEHIST(this->mHist_btow_spectra_3)

	SAVEHIST(this->mHist_smd_spectra)
	SAVEHIST(this->mHist_smd_spectraNonZS)
	SAVEHIST(this->mHist_smd_capacitor)
	SAVEHIST(this->mHist_smd_sum)
	SAVEHIST(this->mHist_psd_spectra)
	SAVEHIST(this->mHist_psd_spectraNonZS)
	SAVEHIST(this->mHist_psd_capacitor)
	SAVEHIST(this->mHist_psd_sum)

	SAVEHIST(this->mHist_HTMAX_spectra)
	SAVEHIST(this->mHist_PAMAX_spectra)
	SAVEHIST(this->mHist_HTMAX_dist)
	SAVEHIST(this->mHist_PAMAX_dist)

	SAVEHIST(this->mHist_JET_spectra)
	SAVEHIST(this->mHist_JETMAX_spectra)
	SAVEHIST(this->mHist_JET_ped)

	SAVEHIST(this->mHist_JETMAX_dist)

        SAVEHIST(this->mHist_ADCEtaPhi_TowHits)
        SAVEHIST(this->mHist_ADCEtaPhi_Pre1Hits)
       

	  
#undef SAVEHIST

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
#ifdef NEW_DAQ_READER
  daqReader *rdr = (daqReader*)(datap);
#else
  evpReader *evp_reader = (evpReader*)(datap);
  int ret = emcReader(datap);
#endif
  
  {
#ifdef NEW_DAQ_READER
    TDatime evt_time(rdr->evt_time); // time in unix seconds
#else
    TDatime evt_time(evp_reader->evt_time);
#endif
    if (BEMCDecoder) BEMCDecoder->SetDateTime(evt_time.GetDate(),evt_time.GetTime());
  }


  /*
    if (!dsmL0WestInput || !dsmL0EastInput || !dsmL1Input || !dsmL2Input || !dsmL3Input) {
    #ifdef NEW_DAQ_READER
    daq_dta *dd_trg = rdr ? (rdr->det("trg")->get("legacy")) : 0;
    if (dd_trg) while (dd_trg->iterate()) {
    trg_t *d = (trg_t *) dd_trg->Void;
    if (d) {
    dsmL0WestInput = &(d->BEMC[0][0]);
    dsmL0EastInput = &(d->BEMC[1][0]);
    dsmL1Input = &(d->BEMC_l1[0]);
    dsmL2Input = ((unsigned short*)d->trg_sum ? (((TrgSumData*)d->trg_sum)->DSMdata.EMC) : 0);
    dsmL3Input = ((unsigned short*)d->trg_sum ? (((TrgSumData*)d->trg_sum)->DSMdata.lastDSM) : 0);
    }
    }
    #else
    trgReader(datap);
    dsmL0WestInput = &(trg.BEMC[0][0]);
    dsmL0EastInput = &(trg.BEMC[1][0]);
    dsmL1Input = trg.BEMC_l1;
    dsmL2Input = ((unsigned short*) trg.trg_sum ? (((TrgSumData*)trg.trg_sum)->DSMdata.EMC) : 0);
    dsmL3Input = ((unsigned short*) trg.trg_sum ? (((TrgSumData*)trg.trg_sum)->DSMdata.lastDSM) : 0);
    #endif
    }
  */
  if (!datap || (mDebug >= 2)) cout << "datap = " << (int*)datap << endl;
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
    int jetPatchSum[BEMCNJET];
    int jetPatchHT[BEMCNJET];
    memset(jetPatchSum, 0, sizeof(jetPatchSum));
    memset(jetPatchHT, 0, sizeof(jetPatchHT));
    int MAXHT = 0;
    int MAXPA = 0;
    int MAXHTID = 0;
    int MAXPAID = 0;
    for (int i = 0;i < 300;i++) {
      if (this->mHistDsmL0InputHighTower) this->mHistDsmL0InputHighTower->Fill(i, this->mDsmL0InputHighTower[i]);
      if (this->mHistDsmL0InputPatchSum) this->mHistDsmL0InputPatchSum->Fill(i, this->mDsmL0InputPatchSum[i]);
      
      if((this->mDsmL0InputHighTower[i] > MAXHT) && (this->mDsmL0InputHighTower[i] < 63)) {MAXHT = this->mDsmL0InputHighTower[i]; MAXHTID = i;}
      if((this->mDsmL0InputPatchSum[i] > MAXPA) && (this->mDsmL0InputPatchSum[i] < 63)) {MAXPA = this->mDsmL0InputPatchSum[i]; MAXPAID = i;}
      
      if (BEMCDecoder) {
	int jetPatch = -1, jetPatchSeq = -1;
	if (BEMCDecoder->GetJetPatchAndSequenceFromTriggerPatch(i, jetPatch, jetPatchSeq)) {
	  if ((jetPatch >= 0) && (jetPatch < BEMCNJET)) {
	    if (jetPatchHT[jetPatch] < this->mDsmL0InputHighTower[i]) jetPatchHT[jetPatch] = this->mDsmL0InputHighTower[i];
	    jetPatchSum[jetPatch] += this->mDsmL0InputPatchSum[i];
	  }
	}
      }
      
      if (mDebug >= 3) cout << "TriggerPatch " << i << ": HighTower = " << this->mDsmL0InputHighTower[i] << ", PatchSum = " << this->mDsmL0InputPatchSum[i] << endl;
    }
    if(this->mHist_HTMAX_spectra) this->mHist_HTMAX_spectra->Fill((float)MAXHTID,(float)MAXHT);
    if(this->mHist_PAMAX_spectra) this->mHist_PAMAX_spectra->Fill((float)MAXPAID,(float)MAXPA);
    
    int HTTH = 12;
    int PATH = 12;
    if((MAXHT > HTTH) && this->mHist_HTMAX_dist) this->mHist_HTMAX_dist->Fill((float)MAXHTID);
    if((MAXPA > PATH) && this->mHist_PAMAX_dist) this->mHist_PAMAX_dist->Fill((float)MAXPAID);
    
    int MAXJETID =0;
    int MAXJETVALUE =-9999;
    for (int i = 0;i < BEMCNJET;i++) {
      if (this->mHistHighTowerSpectrum[i]) this->mHistHighTowerSpectrum[i]->Fill(jetPatchHT[i]);
      if (this->mHistPatchSumSpectrum[i]) this->mHistPatchSumSpectrum[i]->Fill(jetPatchSum[i]);
      if (jetPatchSum[i] > MAXJETVALUE) { MAXJETVALUE = jetPatchSum[i]; MAXJETID = i;}  
      if (this->mHist_JET_spectra) this->mHist_JET_spectra->Fill(i, jetPatchSum[i]);  
    }
    if (this->mHist_JETMAX_spectra) this->mHist_JETMAX_spectra->Fill(MAXJETID,MAXJETVALUE);
    int JETPATH = 35;
    if ((MAXJETVALUE > JETPATH) && this->mHist_JETMAX_dist) this->mHist_JETMAX_dist->Fill(MAXJETID);
    
    for (int i=0;i<BEMCNJET;i++) {
      if (i != MAXJETID) {
	BEMCJPPED[i] += jetPatchSum[i];
	BEMCNJPPED[i]++;
	if(BEMCNJPPED[i]==10) {
	  if (this->mHist_JET_ped) this->mHist_JET_ped->Fill(i,(float)BEMCJPPED[i]/(float)BEMCNJPPED[i]);
	  BEMCJPPED[i] = 0;
	  BEMCNJPPED[i] = 0;
	} 
      }
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
    for (int ijp = 0;ijp < BEMCNJET;ijp++) {
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
  
  int STATUS = BEMCNOTINSTALLED; //NOT PRESENT
#ifdef NEW_DAQ_READER
  daq_dta *dd_btow = rdr ? (rdr->det("btow")->get("adc")) : 0;
  if (dd_btow) while (dd_btow->iterate()) {
    btow_t *d = (btow_t *) dd_btow->Void;
    if (d) {
#else
      if ((ret >= 0) && emc.btow_in) {
	unsigned short *header = emc.btow_raw; // BTOW event header
	if (header) {
#endif
	  if (DSM_L0_present) {
	    memset(this->mDsmSimuHighTower, 0, sizeof(this->mDsmSimuHighTower));
	    memset(this->mDsmSimuPatchSum, 0, sizeof(this->mDsmSimuPatchSum));
	  }
	  int TDCStatus[BTOW_MAXFEE];
	  memset(TDCStatus, BEMCNOTINSTALLED, sizeof(TDCStatus));
	  int TDCTotal = 0;
	  STATUS = BEMCOK; //OK
	  for (int tdc = 0; tdc < BTOW_MAXFEE;tdc++) {
#ifdef NEW_DAQ_READER
	    int count = d->preamble[tdc][0];
	    int error = d->preamble[tdc][1];
#else
	    int count = (*(header + tdc));
	    int error = (*(header + tdc + 30));
#endif
	    if ((error == 0) && (count == (BTOW_PRESIZE + BTOW_DATSIZE))) TDCStatus[tdc] = BEMCOK; // OK
	    else if ((error == 4095) && (count == 4095)) TDCStatus[tdc] = BEMCNOTINSTALLED; // NOT INSTALLED
	    else TDCStatus[tdc] = BEMCCORRUPTED; //CORRUPTED    
	    if (TDCStatus[tdc] == BEMCCORRUPTED) STATUS = BEMCCORRUPTED; // if any crate is corrupted, mark event as corrupted
	    if (this->mHist_TDC_status) this->mHist_TDC_status->Fill(0.0, tdc);
	    if (this->mHist_TDC_status) this->mHist_TDC_status->Fill((float)TDCStatus[tdc], tdc);
	  }
	  for(int i = 0;i < (BTOW_MAXFEE * BTOW_DATSIZE);i++) {
	    int tdc = i % BTOW_MAXFEE;
#ifdef NEW_DAQ_READER
	    int tdc_channel = i / BTOW_MAXFEE;
	    int count = d->preamble[tdc][0];
	    int error = d->preamble[tdc][1];
#else
	    int count = (*(header + tdc));
	    int error = (*(header + tdc + 30));
#endif
	    if((error==0) && (count == (BTOW_PRESIZE + BTOW_DATSIZE))) {
	      // OK
#ifdef NEW_DAQ_READER
	      int adc = d->adc[tdc][tdc_channel];
#else
	      int adc = emc.btow[i];
#endif
	      TDCTotal += adc;
	      int daqid = ((tdc * BTOW_DATSIZE) + tdc_channel);
	      if ((tdc >= 0)  && (tdc < 10) && (TDCStatus[tdc]!=BEMCNOTINSTALLED) && this->mHist_btow_spectra_1) this->mHist_btow_spectra_1->Fill(daqid, adc);
	      if ((tdc >= 10) && (tdc < 20) && (TDCStatus[tdc]!=BEMCNOTINSTALLED) && this->mHist_btow_spectra_2) this->mHist_btow_spectra_2->Fill(daqid, adc);
	      if ((tdc >= 20) && (tdc < 30) && (TDCStatus[tdc]!=BEMCNOTINSTALLED) && this->mHist_btow_spectra_3) this->mHist_btow_spectra_3->Fill(daqid, adc);
	      int softId = -1;
	      float iphi, eta, adcped;
	      
	      if (BEMCDecoder && BEMCDecoder->GetTowerIdFromDaqId(i, softId)) {
		
		adcped=adc-towerPed[softId-1];
		
		StEmcGeom *BEMCGeom = StEmcGeom::instance("bemc");
		
		if(adcped>20)
		  {
		    BEMCGeom->getEta(softId, eta);
		    BEMCGeom->getPhi(softId, iphi);
		    if ((this->mHist_ADCEtaPhi_TowHits) && (this->mTowerData[softId - 1][0] != 0) ) this->mHist_ADCEtaPhi_TowHits->Fill(iphi, eta);
		  }
		
		if ((softId >= 1) && (softId <= 4800)) {
		  if ((softId >= 1) && (softId <= 1220)) {
		    if (this->mHistRawAdc1) this->mHistRawAdc1->Fill(softId, adc);
		    if (this->mHistRawAdc1zoom) this->mHistRawAdc1zoom->Fill(softId, adc);
		  } else if ((softId >= 1221) && (softId <= 2400)) {
		    if (this->mHistRawAdc2) this->mHistRawAdc2->Fill(softId, adc);
		    if (this->mHistRawAdc2zoom) this->mHistRawAdc2zoom->Fill(softId, adc);
		  } else if ((softId >= 2401) && (softId <= 3540)) {
		    if (this->mHistRawAdc3) this->mHistRawAdc3->Fill(softId, adc);
		    if (this->mHistRawAdc3zoom) this->mHistRawAdc3zoom->Fill(softId, adc);
		  } else if ((softId >= 3541) && (softId <= 4800)) {
		    if (this->mHistRawAdc4) this->mHistRawAdc4->Fill(softId, adc);
		    if (this->mHistRawAdc4zoom) this->mHistRawAdc4zoom->Fill(softId, adc);
		  }
		  
		  if (DSM_L0_present && (this->mTowerData[softId - 1][0] != 0)) {
		    int crate = -1, crateSeq = -1;
		    if (BEMCDecoder->GetTowerCrateFromDaqId(i, crate, crateSeq)) {
		      int triggerPatch = -1;
		      if (BEMCDecoder->GetTriggerPatchFromCrate(crate, crateSeq, triggerPatch)) {
			if ((triggerPatch >= 0) && (triggerPatch < 300)) {
			  int ht = -1, pa = -1;
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
	      int lut = -1;
	      simulateFEELUT(this->mDsmSimuPatchSum[i], this->mPatchData[i][3], this->mPatchData[i][4], this->mPatchData[i][5], this->mPatchData[i][6], this->mPatchData[i][7], this->mPatchData[i][8], this->mPatchData[i][9], this->mPatchData[i][10], (int)(this->mTriggerPedestalShift / 100.0), lut, (i == 27300));
	      this->mDsmSimuPatchSum[i] = lut;
	    }
	    //cout << "Trigger patch " << i;
	    if (this->mDsmL0InputHighTower[i] != this->mDsmSimuHighTower[i]) {
	      if (mDebug >= 2) cout << i << ": HT " << this->mDsmSimuHighTower[i] << " != " << this->mDsmL0InputHighTower[i] << endl;
	      if (this->mHistTriggerCorruptionHighTower) this->mHistTriggerCorruptionHighTower->Fill(i);
	      if (this->mHistDSM0HTCorr) this->mHistDSM0HTCorr->Fill(this->mDsmL0InputHighTower[i], this->mDsmSimuHighTower[i]);
	    } else {
	      //cout << "- " << i << "HT OK" << endl;
	    }
	    if (this->mDsmL0InputPatchSum[i] != this->mDsmSimuPatchSum[i]) {
	      if (mDebug >= 2) cout << i << ": PA " << this->mDsmSimuPatchSum[i] << " != " << this->mDsmL0InputPatchSum[i] << endl;
	      if (this->mHistTriggerCorruptionPatchSum) this->mHistTriggerCorruptionPatchSum->Fill(i);
	      if (this->mHistDSM0TPCorr) this->mHistDSM0TPCorr->Fill(this->mDsmL0InputPatchSum[i], this->mDsmSimuPatchSum[i]);
	    } else {
	      //cout << "- " << i << "PA OK" << endl;
	    }
	    //cout << endl;
	  }
	}
      }
      if (this->mHist_BTOW_Corruption) this->mHist_BTOW_Corruption->Fill(0.0);
      if (this->mHist_BTOW_Corruption) this->mHist_BTOW_Corruption->Fill(STATUS);
      
      {
    int totalSumSMD = 0;
    int totalSumPSD = 0;
    int feeSum[120];
    int pmtSum[60];
    memset(feeSum, 0, sizeof(feeSum));
    memset(pmtSum, 0, sizeof(pmtSum));
    bool smdPresent = false;
    bool psdPresent = false;
    for (int bsmd_fiber = 0;bsmd_fiber < BSMD_FIBERS;bsmd_fiber++) {
	int bprs_fiber = bsmd_fiber - 8;
#ifdef NEW_DAQ_READER
	daq_dta *dd_bsmd = rdr ? (rdr->det("bsmd")->get("adc", 0, bsmd_fiber + 1)) : 0; // RTS_READER counts everything from 1, not from 0
	if (dd_bsmd) while (dd_bsmd->iterate()) {
	    bsmd_t *d = (bsmd_t *) dd_bsmd->Void;
	    if (d && BEMCDecoder) {
#else
	if (emc.bsmd_in && BEMCDecoder) {
		{
#endif
		int fiberSum = 0;
		int det = -1, m = -1, e = -1, s = -1;
		int softId = -1, box = -1, wire = -1, Avalue = -1;
#ifdef NEW_DAQ_READER
    		int cap = d->cap;
#else
    		int cap = emc.bsmd_cap[bsmd_fiber];
#endif
		for (int fiber_channel = 0;fiber_channel < BSMD_DATSIZE;fiber_channel++) {
#ifdef NEW_DAQ_READER
		    int adc = d->adc[fiber_channel];
#else
		    int adc = emc.bsmd[bsmd_fiber][fiber_channel];
#endif
		    fiberSum += adc;
		    if ((bsmd_fiber >= 0) && (bsmd_fiber < 8)) {
			if (BEMCDecoder->GetSmdCoord(bsmd_fiber, fiber_channel, det, m, e, s)) {
			    smdPresent = true;
			    totalSumSMD += adc;
			    if ((m >= 1) && (m <= 120)) {
				feeSum[m - 1] += adc;
			    }
			}
		    } else {
			if (BEMCDecoder->GetPsdId(bprs_fiber, fiber_channel, softId, box, wire, Avalue)) {
			    psdPresent = true;
			    totalSumPSD += adc;
			    if ((box >= 1) && (box <= 60)) {
				pmtSum[box - 1] += adc;
				float iphi, eta;
			        StEmcGeom *BEMCGeom = StEmcGeom::instance("bemc");
				if(adc>20){
				  BEMCGeom->getEta(softId, eta);
				  BEMCGeom->getPhi(softId, iphi);
				  if (this->mHist_ADCEtaPhi_Pre1Hits) this->mHist_ADCEtaPhi_Pre1Hits->Fill(iphi, eta);
				}
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
		}
		int BSMD_STATUS = BEMCOK;
		if (fiberSum == 0) BSMD_STATUS = BEMCNOTINSTALLED;
		//fprintf(stderr,"Sum for fiber %d = %f\n",bsmd_fiber,fiberSum);
		if((bsmd_fiber >= 0) && (bsmd_fiber < 8)) {
		    if (this->mHist_SMD_status) this->mHist_SMD_status->Fill(0.0, bsmd_fiber);
    		    if (this->mHist_SMD_status) this->mHist_SMD_status->Fill(BSMD_STATUS, bsmd_fiber);
		    if(BSMD_STATUS==BEMCOK) {
    			if (this->mHist_smd_capacitor) this->mHist_smd_capacitor->Fill(cap, bsmd_fiber);
    			if (this->mHist_smd_sum) this->mHist_smd_sum->Fill(fiberSum, bsmd_fiber);
		    }
		} else {
    		    if (this->mHist_PSD_status) this->mHist_PSD_status->Fill(0.0, bprs_fiber);
    		    if (this->mHist_PSD_status) this->mHist_PSD_status->Fill(BSMD_STATUS, bprs_fiber);
		    if(BSMD_STATUS==BEMCOK) {
			if (this->mHist_psd_capacitor) this->mHist_psd_capacitor->Fill(cap, bprs_fiber);
    			if (this->mHist_psd_sum) this->mHist_psd_sum->Fill(fiberSum, bprs_fiber);
    		    }
		}
	    }
	}
    }
    if (smdPresent) for (int i = 0;i < 120;i++) {
	if (this->mHistSmdFeeSum) this->mHistSmdFeeSum->Fill(i + 1, feeSum[i]);
    }
    if (psdPresent) for (int i = 0;i < 60;i++) {
	if (this->mHistPsdFeeSum) this->mHistPsdFeeSum->Fill(i + 1, pmtSum[i]);
    }
    if (smdPresent && this->mHist_smd_spectra) this->mHist_smd_spectra->Fill(totalSumSMD);
    if (psdPresent && this->mHist_psd_spectra) this->mHist_psd_spectra->Fill(totalSumPSD);
    }

    {
    int totalSumSMDNonZS = 0;
    int totalSumPSDNonZS = 0;
    int feeSumNonZS[120];
    int pmtSumNonZS[60];
    memset(feeSumNonZS, 0, sizeof(feeSumNonZS));
    memset(pmtSumNonZS, 0, sizeof(pmtSumNonZS));
    bool smdPresentNonZS = false;
    bool psdPresentNonZS = false;
    for (int bsmd_fiber = 0;bsmd_fiber < BSMD_FIBERS;bsmd_fiber++) {
	int bprs_fiber = bsmd_fiber - 8;
#ifdef NEW_DAQ_READER
	daq_dta *dd_bsmd = rdr ? (rdr->det("bsmd")->get("adc_non_zs", 0, bsmd_fiber + 1)) : 0;
	if (dd_bsmd) while (dd_bsmd->iterate()) {
	    bsmd_t *d = (bsmd_t *) dd_bsmd->Void;
	    if (d && BEMCDecoder) {
#else
	if (emc.bsmd_in && BEMCDecoder) {
		{
#endif
		int fiberSum = 0;
		int det = -1, m = -1, e = -1, s = -1;
		int softId = -1, box = -1, wire = -1, Avalue = -1;
#ifdef NEW_DAQ_READER
    		int cap = d->cap;
#else
    		int cap = emc.bsmd_cap[bsmd_fiber];
#endif
		for (int fiber_channel = 0;fiber_channel < BSMD_DATSIZE;fiber_channel++) {
#ifdef NEW_DAQ_READER
		    int adc = d->adc[fiber_channel];
#else
		    int adc = emc.bsmd[bsmd_fiber][fiber_channel];
#endif
		    fiberSum += adc;
		    if ((bsmd_fiber >= 0) && (bsmd_fiber < 8)) {
			if (BEMCDecoder->GetSmdCoord(bsmd_fiber, fiber_channel, det, m, e, s)) {
			    smdPresentNonZS = true;
			    totalSumSMDNonZS += adc;
			    if ((m >= 1) && (m <= 120)) {
				feeSumNonZS[m - 1] += adc;
			    }
			}
		    } else {
			if (BEMCDecoder->GetPsdId(bprs_fiber, fiber_channel, softId, box, wire, Avalue)) {
			    psdPresentNonZS = true;
			    totalSumPSDNonZS += adc;
			    if ((box >= 1) && (box <= 60)) {
				pmtSumNonZS[box - 1] += adc;
			    }
		        }
		    }
		}
	    }
	}
    }
    if (smdPresentNonZS) for (int i = 0;i < 120;i++) {
	if (this->mHistSmdFeeSumNonZS) this->mHistSmdFeeSumNonZS->Fill(i + 1, feeSumNonZS[i]);
    }
    if (psdPresentNonZS) for (int i = 0;i < 60;i++) {
	if (this->mHistPsdFeeSumNonZS) this->mHistPsdFeeSumNonZS->Fill(i + 1, pmtSumNonZS[i]);
    }
    if (smdPresentNonZS && this->mHist_smd_spectraNonZS) this->mHist_smd_spectraNonZS->Fill(totalSumSMDNonZS);
    if (psdPresentNonZS && this->mHist_psd_spectraNonZS) this->mHist_psd_spectraNonZS->Fill(totalSumPSDNonZS);
    } 
	
    if (mDebug >= 10) cout << __FILE__ << ":" << __LINE__ << endl;
}



