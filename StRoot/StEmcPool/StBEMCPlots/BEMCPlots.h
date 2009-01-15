#ifndef BEMCPlots_H
#define BEMCPlots_H

class TH1F;
class TH2F;
class TFile;
class TPad;
class TObjArray;

class BEMCPlots {

public:
    BEMCPlots(TObjArray *list = 0);
    ~BEMCPlots();

    void init(unsigned int date, unsigned int time, const char *bemcStatus);
    void clear(const char *bemcStatus);
    void saveHistograms(TFile *hfile);    
    void processEvent(	  char *datap
			, const unsigned char *dsmL0WestInput
			, const unsigned char *dsmL0EastInput
			, const unsigned short *dsmL1Input
			, const unsigned short *dsmL2Input
			, const unsigned short *dsmL3Input
			);

    int getDebug() {return mDebug;}
    void setDebug(int d) {mDebug = d;}

    // These are called from Pplots
    static void initHisto(TObjArray *list = 0, const char *bemcStatus = 0);
    static void resetHisto(const char *bemcStatus = 0);
    static void saveHisto(TFile *hfile);    
    static void fillHisto(char *datap
			, const unsigned char *dsmL0WestInput
			, const unsigned char *dsmL0EastInput
			, const unsigned short *dsmL1Input
			, const unsigned short *dsmL2Input
			, const unsigned short *dsmL3Input
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
    
    TH2F *mHistRawAdcPsd1;
    TH2F *mHistRawAdcPsd2;
    TH2F *mHistRawAdcPsd3;
    TH2F *mHistRawAdcPsd4;

    TH2F *mHistSmdFeeSum;
    TH2F *mHistPsdFeeSum;
    
    TH1F *mHistHighTowerSpectrum[12];
    TH1F *mHistPatchSumSpectrum[12];
    
    int mTowerData[4800][3];
    int mPatchData[300][11];
    int mTriggerPedestalShift;
    TH1F *mHistTriggerCorruptionHighTower;
    TH1F *mHistTriggerCorruptionPatchSum;
    int mDsmSimuHighTower[300];
    int mDsmSimuPatchSum[300];
    TH2F *mHistTriggerCorruptionHighTowerCorr;
    TH2F *mHistTriggerCorruptionPatchSumCorr;
};

#endif

