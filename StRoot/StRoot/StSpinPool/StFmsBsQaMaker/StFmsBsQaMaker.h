#ifndef StFmsBsQaMaker_h
#define StFmsBsQaMaker_h

#include "StMaker.h"

class StFmsCollection;
class StFmsDbMaker;

class TFile;
class TH1F;
class TH2F;

class StFmsBsQaMaker : public StMaker
{
	public:

		StFmsBsQaMaker(const char* name = "fmsBsQaMaker");
		~StFmsBsQaMaker() {}

		virtual Int_t Init();
		virtual Int_t InitRun(int runNo);
		virtual Int_t Make();
		virtual Int_t Finish();

		void SetOutputName(const char* name) { mOutputName = name; }

	private:

		StFmsCollection* mFmsColl;
		StFmsDbMaker* mFmsDbMk;
		TFile* mFile;

		//nSep: separate each detId by given number of channels
		enum {nBit = 12, nChLg = 578, nChSm = 288, nDet = 4};
		const char* mOutputName = "fmsBsQa.root";
		int mEvent = 0;

		std::map<int, int> chToN[nDet]; //Convert FMS channel # to temporary index
		std::map<int, int> nToCh[nDet];

		TH2F* mH2_adc[nDet];
		TH2F* mH2_bs_DB[nDet];
		TH2F* mH2_bs_data[nDet];
		TH2F* mH2_chMap[nDet];

		ClassDef(StFmsBsQaMaker, 1.0);
};

#endif
