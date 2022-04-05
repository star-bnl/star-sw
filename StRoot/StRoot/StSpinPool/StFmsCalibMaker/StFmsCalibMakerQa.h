#ifndef StFmsCalibMakerQa_h
#define StFmsCalibMakerQa_h

#include "StFmsCalibMaker.h"

class StFmsCalibMakerQa : public StMaker
{
	public:

		virtual void CreateQaHist(int detId, int maxCh);
		virtual void CreateQaHistAdc(int detId, int maxCh, int trigId);
		virtual void CreateQaHistZVtx(void);
		virtual void CreateQaTree(void);
		virtual void ResetQaTree(void);

		//QA histograms, alphabetical order
		TH1F* mH1_bbcZ;
		TH1F* mH1_diffMass;
		TH1F* mH1_diffOpenA;
		TH1F* mH1_nEvents;
		TH1F* mH1_trig;

		TH2F* mH2_adc[4];
		TH2F* mH2_adcWide[4];
		TH2F* mH2_massWide[4];     //QA, Mass, 0 < mass < 1 (GeV)
		TH2F* mH2_massPairE[4][7]; //QA, Mass vs. pairE, Zgg < 0.7, in stepwidth of 0.1
		TH2F* mH2_massOpenA[4][7]; //QA, Mass vs. openA, Zgg < 0.7, in stepwidth of 0.1
		TH2F* mH2_massZgg[4][7];   //QA, Mass vs. Zgg, pair E > 20, in stepwidth of 10
		TH2F* mH2_pointsEP[4];     //QA, Points' eta vs. phi, by detId
		TH2F* mH2_pointsXY[2][3];  //QA, Points' XY, by point 0/1, by all/large/small

		//Tree
		TTree* T;
        static const short mNhitMax = 500;
        short mNhit;
		short mDetId [mNhitMax];
		short mCh    [mNhitMax];
		short mPointB[mNhitMax]; //The point this hit belong to: either 0 or 1
		float mHitE  [mNhitMax];
		short mCluTowers[2]; //# of participating towers
		float mCluMax[2];    //SigmaMax
        float mCluMin[2];    //SigmaMin
        float mCluX[2];
        float mCluY[2];
        float mPointE[2];
        float mPointX[2];
        float mPointY[2];
        float mMass;
        float mOpenA;
		float mZgg;
		int   mTrigBit;

		ClassDef(StFmsCalibMakerQa, 1);
};

#endif
