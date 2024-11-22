#ifndef ST_FST_FAST_SIM_MAKER_H
#define ST_FST_FAST_SIM_MAKER_H

class StFtsHit;
class StEvent;
class StRnDHitCollection;
class StRnDHit;

#include "StChain/StMaker.h"
#include <vector>

#include "TH1F.h"
#include "TH2F.h"
#include "TH3F.h"


class StFstFastSimMaker : public StMaker {
	public:
		explicit StFstFastSimMaker(const Char_t *name = "fstFastSim");
		virtual ~StFstFastSimMaker() {}
		int Make();
		int Init();
		int Finish();

		/// Set min/max active radii for each disk
		void SetDisk(const int i, const float rmn, const float rmx);
		void SetInEfficiency(float ineff = 0.1) { mInEff = ineff; }
		void SetQAFileName(TString filename = 0.1) { mQAFileName = filename; }
		void SetFillHist(const bool hist = false) { mHist = hist; }
		void setGEANTPassthrough(bool passthrough = false) { mGEANTPassthrough = passthrough; }

	private:
		void FillSilicon(StEvent *event);
		StRnDHitCollection *hitCollection = nullptr;

		int mNumR;
		int mNumPHI;
		int mNumSEC;
		float mInEff;
		bool mHist;
		bool mGEANTPassthrough;
		TString mQAFileName;

		TH3F *hTrutHitYXDisk;
		TH2F *hTrutHitRDisk;
		TH2F *hTrutHitRShower[3];
		TH2F *hTrutHitPhiDisk;
		TH2F *hTrutHitPhiZ;
		TH3F *hRecoHitYXDisk;
		TH2F *hRecoHitRDisk;
		TH2F *hRecoHitPhiDisk;
		TH2F *hRecoHitPhiZ;
		TH2F *hGlobalDRDisk;
		TH1F *hGlobalZ;

		TH2F *h2GlobalXY;
		TH2F *h2GlobalSmearedXY;
		TH2F *h2GlobalDeltaXY;
		TH3F *h3GlobalDeltaXYDisk;
		TH3F *h3GlobalDeltaXYR;
		TFile *fOut;

		ClassDef(StFstFastSimMaker, 0)
};

#endif
