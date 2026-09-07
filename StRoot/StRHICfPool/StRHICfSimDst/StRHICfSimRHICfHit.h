#ifndef StRHICfSimRHICfHit_HH
#define StRHICfSimRHICfHit_HH

#include <algorithm>
#include <vector>
#include "TObject.h"
#include "StRHICfSimPar.h"

using namespace std;

class StRHICfSimRHICfHit : public TObject
{
    public: 
        StRHICfSimRHICfHit();
        ~StRHICfSimRHICfHit();

        void Clear(Option_t *option = "");

        void SetFCdE(int tower, float val);
        void SetPlatedE(int tower, int plate, float val);
        void SetGSOBardE(int tower, int layer, int xy, int bar, float val);

        void SetSimTrkId(int tower, int id, double incidentPosX, double incidentPosY, double incidentEnergy);

        void SetL20(int tower, float val);
        void SetL90(int tower, float val);

        void SetGSOMaxLayer(int tower, int order, int val);
        void SetMaxPeakBin(int tower, int layer, int xy, int val);

        void SetSingleHitNum(int tower, int layer, int xy, int val);
        void SetSingleHitPos(int tower, int layer, int xy, float val);
        void SetSinglePeakHeight(int tower, int layer, int xy, float val);
        void SetSingleFitChi2(int tower, int layer, int xy, float val);

        void SetMultiHitNum(int tower, int val);
        void SetMultiHitPos(int tower, int layer, int xy, int order, float val);
        void SetMultiPeakHeight(int tower, int layer, int xy, int order, float val);
        void SetMultiPeakRaw(int tower, int layer, int xy, int order, float val);
        void SetMultiEnergySum(int tower, int layer, int xy, int order, float val);
        void SetMultiFitChi2(int tower, int layer, int xy, float val);

        Float_t GetFCdE(int tower); 
        Float_t GetPlatedE(int tower, int plate);
        Float_t GetGSOBardE(int tower, int layer, int xy, int bar);

        Int_t GetSimTrkNum(int tower);
        Int_t GetSimTrkId(int tower, int idx);
        Double_t GetSimTrkIncidentPos(int tower, int idx, int xy);
        Double_t GetSimTrkIncidentEnergy(int tower, int idx);

        Float_t GetL20(int tower);
        Float_t GetL90(int tower);

        Int_t GetGSOMaxLayer(int tower, int order);
        Int_t GetMaxPeakBin(int tower, int layer, int xy);

        Int_t GetSingleHitNum(int tower, int layer, int xy);
        Float_t GetSingleHitPos(int tower, int layer, int xy);
        Float_t GetSinglePeakHeight(int tower, int layer, int xy);
        Float_t GetSingleFitChi2(int tower, int layer, int xy);

        Int_t GetMultiHitNum(int tower);
        Float_t GetMultiHitPos(int tower, int layer, int xy, int order);
        Float_t GetMultiPeakHeight(int tower, int layer, int xy, int order);
        Float_t GetMultiPeakRaw(int tower, int layer, int xy, int order);
        Float_t GetMultiEnergySum(int tower, int layer, int xy, int order);
        Float_t GetMultiFitChi2(int tower, int layer, int xy);

    private:
        // RHICf Raw Hits (geant truth data)
        Float_t mFCdE[rTowerNum];
        Float_t mPlatedE[rTowerNum][rPlateNum];
        Float_t mSmallGSOBardE[rLayerNum][rXYNum][rSmallBarNum]; // TS
        Float_t mLargeGSOBardE[rLayerNum][rXYNum][rLargeBarNum]; // TL

        vector<int> mSimTrkId[rTowerNum];
        vector<double> mSimTrkIncidentPosX[rTowerNum];
        vector<double> mSimTrkIncidentPosY[rTowerNum];
        vector<double> mSimTrkIncidentE[rTowerNum];

        // RHICf Reco Hits (reconstruction)
        Float_t mL20[rTowerNum];
        Float_t mL90[rTowerNum];

        Int_t mGSOMaxLayer[rTowerNum][2];
        Int_t mMaxPeakBarIdx[rTowerNum][rLayerNum][rXYNum];

        Int_t mSingleHitNum[rTowerNum][rLayerNum][rXYNum];
        Float_t mSingleHitPos[rTowerNum][rLayerNum][rXYNum];
        Float_t mSingleHitHeight[rTowerNum][rLayerNum][rXYNum];
        Float_t mSingleFitChi2[rTowerNum][rLayerNum][rXYNum];

        Int_t mMultiHitNum[rTowerNum];
        Float_t mMultiHitPos[rTowerNum][rLayerNum][rXYNum][2];
        Float_t mMultiHitHeight[rTowerNum][rLayerNum][rXYNum][2];
        Float_t mMultiHitRaw[rTowerNum][rLayerNum][rXYNum][2];
        Float_t mMultiEnergySum[rTowerNum][rLayerNum][rXYNum][2];
        Float_t mMultiFitChi2[rTowerNum][rLayerNum][rXYNum];

    ClassDef(StRHICfSimRHICfHit,1)
};

#endif
