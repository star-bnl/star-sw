#ifndef BEMCPlots_H
#define BEMCPlots_H

class TH1F;
class TH2F;
class TFile;
class TObjArray;

#define BEMCNJET 12
#define BEMCTOW 4800

class BEMCPlots {
  
 public:
  BEMCPlots(TObjArray *list = 0);
  ~BEMCPlots();
  
  void init(unsigned int date, unsigned int time, const char *bemcStatus);
  void clear(const char *bemcStatus);
  void saveHistograms(TFile *hfile);    
  void processEvent(	  char *rdr
			  , const unsigned char *dsmL0WestInput = 0
			  , const unsigned char *dsmL0EastInput = 0
			  , const unsigned short *dsmL1Input = 0
			  , const unsigned short *dsmL2Input = 0
			  , const unsigned short *dsmL3Input = 0
			  );
  
  int getDebug() {return mDebug;}
  void setDebug(int d) {mDebug = d;}

  // These are called from Pplots
  static void initHisto(TObjArray *list = 0, const char *bemcStatus = 0);
  static void resetHisto(const char *bemcStatus = 0);
  static void saveHisto(TFile *hfile);    
  static void fillHisto(char *rdr
			, const unsigned char *dsmL0WestInput = 0
			, const unsigned char *dsmL0EastInput = 0
			, const unsigned short *dsmL1Input = 0
			, const unsigned short *dsmL2Input = 0
			, const unsigned short *dsmL3Input = 0
			);
  
  
 private:
  int mDebug;
  
  int mDsmL0InputHighTower[300];
  int mDsmL0InputPatchSum[300];
  
  int mDsmL1InputHighTowerBits[6][6];
  int mDsmL1InputPatchSum[6][6];
  
  int mDsmL2InputHighTowerBits[12];
  int mDsmL2InputPatchSumBits[12];
  int mDsmL2InputPatchSum[6];
  
  int mDsmL3InputHighTowerBits[1];
  int mDsmL3InputPatchSumBits[1];
  int mDsmL3InputBackToBackBit[1];
  int mDsmL3InputJPsiTopoBit[1];
  int mDsmL3InputJetPatchTopoBit[1];
  
  TH1F *mHistTot;
  TH2F *mHistDsmL0InputHighTower;
  TH2F *mHistDsmL0InputPatchSum;
  
  TH2F *mHistDsmL1InputHighTowerBits;
  TH2F *mHistDsmL1InputPatchSum;
  
  TH2F *mHistDsmL2InputHighTowerBits;
  TH2F *mHistDsmL2InputPatchSumBits;
  TH2F *mHistDsmL2InputPatchSum;
  
  TH1F *mHistDsmL3InputHighTowerBits;
  TH1F *mHistDsmL3InputPatchSumBits;
  TH1F *mHistDsmL3InputBackToBackBit;
  TH1F *mHistDsmL3InputJPsiTopoBit;
  TH1F *mHistDsmL3InputJetPatchTopoBit;
  
  TH2F *mHistRawAdc1;
  TH2F *mHistRawAdc2;
  TH2F *mHistRawAdc3;
  TH2F *mHistRawAdc4;
  
  TH2F *mHistRawAdc1zoom;
  TH2F *mHistRawAdc2zoom;
  TH2F *mHistRawAdc3zoom;
  TH2F *mHistRawAdc4zoom;
  
  TH2F *mHistRawAdcPsd1;
  TH2F *mHistRawAdcPsd2;
  TH2F *mHistRawAdcPsd3;
  TH2F *mHistRawAdcPsd4;
  
  TH2F *mHistSmdFeeSum;
  TH2F *mHistPsdFeeSum;
  
  TH2F *mHistSmdFeeSumNonZS;
  TH2F *mHistPsdFeeSumNonZS;
  
  TH1F *mHistHighTowerSpectrum[BEMCNJET];
  TH1F *mHistPatchSumSpectrum[BEMCNJET];
  
  int mTowerData[4800][3];
  int mPatchData[300][11];
  int mTriggerPedestalShift;
  TH1F *mHistTriggerCorruptionHighTower;
  TH1F *mHistTriggerCorruptionPatchSum;
  int mDsmSimuHighTower[300];
  int mDsmSimuPatchSum[300];
  TH2F *mHistDSM0HTCorr;
  TH2F *mHistDSM0TPCorr;
  
  TH2F *mHist_TDC_status;
  TH2F *mHist_SMD_status;
  TH2F *mHist_PSD_status;
  TH1F *mHist_BTOW_Corruption;
  
  TH2F *mHist_btow_spectra_1;
  TH2F *mHist_btow_spectra_2;
  TH2F *mHist_btow_spectra_3;
  
  TH1F *mHist_smd_spectra;
  TH1F *mHist_smd_spectraNonZS;
  TH2F *mHist_smd_capacitor;
  TH2F *mHist_smd_sum;
  TH1F *mHist_psd_spectra;
  TH1F *mHist_psd_spectraNonZS;
  TH2F *mHist_psd_capacitor;
  TH2F *mHist_psd_sum;
  
  TH2F *mHist_HTMAX_spectra;
  TH2F *mHist_PAMAX_spectra;
  TH1F *mHist_HTMAX_dist;
  TH1F *mHist_PAMAX_dist;
  
  TH2F *mHist_JET_ped;
  TH2F *mHist_JET_spectra;
  TH2F *mHist_JETMAX_spectra;
  TH1F *mHist_JETMAX_dist;
  
  TH2F *mHist_ADCEtaPhi_TowHits;
  TH2F *mHist_ADCEtaPhi_Pre1Hits;
  
  int BEMCNJPPED[BEMCNJET];
  int BEMCJPPED[BEMCNJET];
  float towerPed[BEMCTOW];

};

#endif

