#ifndef StFmsCalibMaker_h
#define StFmsCalibMaker_h

#include "StMaker.h"

class StFmsCalibMakerQa;
class StFmsCollection;
class StFmsDbMaker;
class StFmsPointPair;
class StTriggerData;
class StTriggerId;

class TFile;
class TTree;
class TH1F;
class TH2F;

class StFmsCalibMaker : public StMaker
{
    public:

        StFmsCalibMaker(const char* name = "fmsCalibMaker") : StMaker(name) {}
        ~StFmsCalibMaker() {}

		void GetMap(void) { mGetMap = true; }
		void GetQaHist(void) { mGetQaHist = true; }
		void GetQaHistAdc(int trigId) { mGetQaHistAdc = true; mTrigMB = trigId; }
		void GetQaTree(void) { mGetQaTree = true; }
		void ReadCellStat(const char* list); //Get cell status from external file
		void VpdTimingCut(int vpdCut) { mApplyVpdTime = true; mVpdCut = vpdCut; } 
        void SetOutName(const char* name) { mOutName = name; }

		Int_t CheckFmsTrigger(const StTriggerId& trigId, const int mFmsTrigIdBase = 480800); //RUN15pp200
		Int_t ReadBbcSlewing(const char* filename_bbc); //Oleg's BBC slewing correction function
		Float_t GetBbcZCorr(const StTriggerData* triggerData);
		Float_t GetBbcZCorrMass(StFmsPointPair* pair, Float_t bbcZ, bool returnOpenA = false);

        Int_t Init();
        Int_t InitRun(int runNo);
        Int_t Finish();
        Int_t Make();

    private:

		StFmsCalibMakerQa* mQa;
        StFmsCollection*   mFmsColl;
        StFmsDbMaker*      mFmsDbMk;
        TFile*             mFile;

        const char* mOutName = "fmsCalib.root";
		bool mApplyBbcZvtx = false; //Turns to true if ReadBbcSlewing is used
		bool mApplyVpdTime = false; //Turns to true if VpdTimingCut is used
		bool mGetMap       = false; //Get maps FmsMapBase, FmsBitShift for calibFms.C
		bool mGetQaHist    = false; //Get QA histograms: related to mass
		bool mGetQaHistAdc = false; //Get QA histograms: ADC distributions by single physics trigger
		bool mGetQaTree    = false; //Get QA tree
		bool mReadCellStat = false; 
		std::map<int, int> mCellStat[4]; //0 for not bad/dead, 1 for bad, 2 for dead, and 9 for converged
		enum mCellStatIndex {GOOD, BAD, DEAD, NA3, NA4, NA5, NA6, NA7, NA8, CONVERGED};

        int mEvent  = 0;
		int mRunNo  = 0;
		int mTrig   = 0;
		int mTrigMB = 0;
		int mVpdCut = 0;
		int mXing   = 0;
		Float_t mBbcZ = -999.;
		Float_t mBbcSlew[2][16][3]; //Oleg, [east/west][pmt][parameter]

		TH2F* mH2_mass[4];     //Mass vs. Channel, default
		TH2F* mH2_massFine[4]; //Mass vs. Channel, x5 bins

        ClassDef(StFmsCalibMaker, 1);
};

#endif
