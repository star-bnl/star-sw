#include "StRHICfSimRHICfHit.h"

ClassImp(StRHICfSimRHICfHit)

StRHICfSimRHICfHit::StRHICfSimRHICfHit()
{
    Clear();
}

StRHICfSimRHICfHit::~StRHICfSimRHICfHit()
{
}

void StRHICfSimRHICfHit::Clear(Option_t *option)
{
    fill_n(&mFCdE[0], rTowerNum, -999.);
    fill_n(&mPlatedE[0][0], rTowerNum * rPlateNum, -999.);
    fill_n(&mSmallGSOBardE[0][0][0], rLayerNum*rXYNum*rSmallBarNum, -999.);
    fill_n(&mLargeGSOBardE[0][0][0], rLayerNum*rXYNum*rLargeBarNum, -999.);

    for(int i=0; i<rTowerNum; i++){
        mSimTrkId[i].clear();
        mSimTrkIncidentPosX[i].clear();
        mSimTrkIncidentPosY[i].clear();
        mSimTrkIncidentE[i].clear();
    }

    fill_n(&mL20[0], rTowerNum, -999.);
    fill_n(&mL90[0], rTowerNum, -999.);
    fill_n(&mGSOMaxLayer[0][0], rTowerNum * 2, -999.);
    fill_n(&mMaxPeakBarIdx[0][0][0], rTowerNum*rLayerNum*rXYNum, -999.);
    fill_n(&mSingleHitNum[0][0][0], rTowerNum*rLayerNum*rXYNum, -999.);
    fill_n(&mSingleHitPos[0][0][0], rTowerNum*rLayerNum*rXYNum, -999.);
    fill_n(&mSingleHitHeight[0][0][0], rTowerNum*rLayerNum*rXYNum, -999.);
    fill_n(&mSingleFitChi2[0][0][0], rTowerNum*rLayerNum*rXYNum, -999.);
    fill_n(&mMultiHitNum[0], rTowerNum, -999.);
    fill_n(&mMultiHitPos[0][0][0][0], rTowerNum*rLayerNum*rXYNum*2, -999.);
    fill_n(&mMultiHitHeight[0][0][0][0], rTowerNum*rLayerNum*rXYNum*2, -999.);
    fill_n(&mMultiHitRaw[0][0][0][0], rTowerNum*rLayerNum*rXYNum*2, -999.);
    fill_n(&mMultiEnergySum[0][0][0][0], rTowerNum*rLayerNum*rXYNum*2, -999.);
    fill_n(&mMultiFitChi2[0][0][0], rTowerNum*rLayerNum*rXYNum, -999.);
}

void StRHICfSimRHICfHit::SetFCdE(int tower, float val){mFCdE[tower] = val;}
void StRHICfSimRHICfHit::SetPlatedE(int tower, int plate, float val){mPlatedE[tower][plate] = val;}

void StRHICfSimRHICfHit::SetGSOBardE(int tower, int layer, int xy, int bar, float val)
{
    if(tower == rTS){mSmallGSOBardE[layer][xy][bar] = val;}
    if(tower == rTL){mLargeGSOBardE[layer][xy][bar] = val;}
}

void StRHICfSimRHICfHit::SetSimTrkId(int tower, int id, double incidentPosX, double incidentPosY, double incidentEnergy)
{
    mSimTrkId[tower].push_back(id);
    mSimTrkIncidentPosX[tower].push_back(incidentPosX);
    mSimTrkIncidentPosY[tower].push_back(incidentPosY);
    mSimTrkIncidentE[tower].push_back(incidentEnergy);
}

void StRHICfSimRHICfHit::SetL20(int tower, float val){mL20[tower] = val;}
void StRHICfSimRHICfHit::SetL90(int tower, float val){mL90[tower] = val;}

void StRHICfSimRHICfHit::SetGSOMaxLayer(int tower, int order, int val){mGSOMaxLayer[tower][order] = val;}
void StRHICfSimRHICfHit::SetMaxPeakBin(int tower, int layer, int xy, int val){mMaxPeakBarIdx[tower][layer][xy] = val;}

void StRHICfSimRHICfHit::SetSingleHitNum(int tower, int layer, int xy, int val){mSingleHitNum[tower][layer][xy] = val;}
void StRHICfSimRHICfHit::SetSingleHitPos(int tower, int layer, int xy, float val){mSingleHitPos[tower][layer][xy] = val;}
void StRHICfSimRHICfHit::SetSinglePeakHeight(int tower, int layer, int xy, float val){mSingleHitHeight[tower][layer][xy] = val;}
void StRHICfSimRHICfHit::SetSingleFitChi2(int tower, int layer, int xy, float val){mSingleFitChi2[tower][layer][xy] = val;}

void StRHICfSimRHICfHit::SetMultiHitNum(int tower, int val){mMultiHitNum[tower] = val;}
void StRHICfSimRHICfHit::SetMultiHitPos(int tower, int layer, int xy, int order, float val){mMultiHitPos[tower][layer][xy][order] = val;}
void StRHICfSimRHICfHit::SetMultiPeakHeight(int tower, int layer, int xy, int order, float val){mMultiHitHeight[tower][layer][xy][order] = val;}
void StRHICfSimRHICfHit::SetMultiPeakRaw(int tower, int layer, int xy, int order, float val){mMultiHitRaw[tower][layer][xy][order] = val;}
void StRHICfSimRHICfHit::SetMultiEnergySum(int tower, int layer, int xy, int order, float val){mMultiEnergySum[tower][layer][xy][order] = val;}
void StRHICfSimRHICfHit::SetMultiFitChi2(int tower, int layer, int xy, float val){mMultiFitChi2[tower][layer][xy] = val;}

Float_t StRHICfSimRHICfHit::GetFCdE(int tower){return mFCdE[tower];}
Float_t StRHICfSimRHICfHit::GetPlatedE(int tower, int plate){return mPlatedE[tower][plate];}

Float_t StRHICfSimRHICfHit::GetGSOBardE(int tower, int layer, int xy, int bar)
{
    if(tower == rTS){return mSmallGSOBardE[layer][xy][bar];}
    if(tower == rTL){return mLargeGSOBardE[layer][xy][bar];}
    return -999.;
}

Int_t StRHICfSimRHICfHit::GetSimTrkNum(int tower){return mSimTrkId[tower].size();}
Int_t StRHICfSimRHICfHit::GetSimTrkId(int tower, int idx){return mSimTrkId[tower][idx];}
Double_t StRHICfSimRHICfHit::GetSimTrkIncidentPos(int tower, int idx, int xy)
{
    if(xy == 0){return mSimTrkIncidentPosX[tower][idx];}
    if(xy == 1){return mSimTrkIncidentPosY[tower][idx];}
    return -999.;
}
Double_t StRHICfSimRHICfHit::GetSimTrkIncidentEnergy(int tower, int idx){return mSimTrkIncidentE[tower][idx];}

Float_t StRHICfSimRHICfHit::GetL20(int tower){return mL20[tower];}
Float_t StRHICfSimRHICfHit::GetL90(int tower){return mL90[tower];}

Int_t StRHICfSimRHICfHit::GetGSOMaxLayer(int tower, int order){return mGSOMaxLayer[tower][order];}
Int_t StRHICfSimRHICfHit::GetMaxPeakBin(int tower, int layer, int xy){return mMaxPeakBarIdx[tower][layer][xy];}

Int_t StRHICfSimRHICfHit::GetSingleHitNum(int tower, int layer, int xy){return mSingleHitNum[tower][layer][xy];}
Float_t StRHICfSimRHICfHit::GetSingleHitPos(int tower, int layer, int xy){return mSingleHitPos[tower][layer][xy];}
Float_t StRHICfSimRHICfHit::GetSinglePeakHeight(int tower, int layer, int xy){return mSingleHitHeight[tower][layer][xy];}
Float_t StRHICfSimRHICfHit::GetSingleFitChi2(int tower, int layer, int xy){return mSingleFitChi2[tower][layer][xy];}

Int_t StRHICfSimRHICfHit::GetMultiHitNum(int tower){return mMultiHitNum[tower];}
Float_t StRHICfSimRHICfHit::GetMultiHitPos(int tower, int layer, int xy, int order){return mMultiHitPos[tower][layer][xy][order];}
Float_t StRHICfSimRHICfHit::GetMultiPeakHeight(int tower, int layer, int xy, int order){return mMultiHitHeight[tower][layer][xy][order];}
Float_t StRHICfSimRHICfHit::GetMultiPeakRaw(int tower, int layer, int xy, int order){return mMultiHitRaw[tower][layer][xy][order];}
Float_t StRHICfSimRHICfHit::GetMultiEnergySum(int tower, int layer, int xy, int order){return mMultiEnergySum[tower][layer][xy][order];}
Float_t StRHICfSimRHICfHit::GetMultiFitChi2(int tower, int layer, int xy){return mMultiFitChi2[tower][layer][xy];}