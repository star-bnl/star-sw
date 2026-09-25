#ifndef StRHICfSimBBC_HH
#define StRHICfSimBBC_HH

#include <algorithm>
#include <vector>

#include "TObject.h"
#include "StRHICfSimPar.h"

using namespace std;

class StRHICfSimBBC : public TObject
{
    public: 
        StRHICfSimBBC();
        ~StRHICfSimBBC();

        void Clear(Option_t *option = "");

        void SetLargeADC(int dir, int pmt, short adc);
        void SetSmallADC(int dir, int pmt, short adc);
        void SetLargeSimTrkId(int dir, int pmt, int idx);
        void SetSmallSimTrkId(int dir, int pmt, int idx);

        Int_t GetLargeADC(int dir, int pmt);
        Int_t GetSmallADC(int dir, int pmt);
        Int_t GetLargeSum(int dir);
        Int_t GetSmallSum(int dir);
        Int_t GetEastSum();
        Int_t GetWestSum();
        Int_t GetSum();

        Int_t GetLargeSimTrkNum(int dir, int pmt);
        Int_t GetSmallSimTrkNum(int dir, int pmt);

        Int_t GetLargeSimTrkId(int dir, int pmt, int idx);
        Int_t GetSmallSimTrkId(int dir, int pmt, int idx);

    private:
        Short_t mLargeADC[rSideNum][rBBCLargePmtNum];
        Short_t mSmallADC[rSideNum][rBBCSmallPmtNum];

        vector<int> mEastLargeSimTrkId[rBBCLargePmtNum];
        vector<int> mWestLargeSimTrkId[rBBCLargePmtNum];
        vector<int> mEastSmallSimTrkId[rBBCSmallPmtNum];
        vector<int> mWestSmallSimTrkId[rBBCSmallPmtNum];

    ClassDef(StRHICfSimBBC,1)
};

#endif
