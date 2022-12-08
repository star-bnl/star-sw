#include "StRHICfHit.h"

ClassImp(StRHICfHit)

StRHICfHit::StRHICfHit()
{
	clear();
}

StRHICfHit::~StRHICfHit()
{
}

void StRHICfHit::clear()
{
    memset(mPlateE, 0., sizeof(mPlateE));
    memset(mGSOBarSmallE, 0., sizeof(mGSOBarSmallE));
    memset(mGSOBarLargeE, 0., sizeof(mGSOBarLargeE));

    memset(mL20, 0.,sizeof(mL20));
    memset(mL90, 0.,sizeof(mL90));

    memset(mPointNum, 0, sizeof(mPointNum));
    memset(mGSOMaxLayer, 0, sizeof(mGSOMaxLayer));
    memset(mMaxPeakBin, 0, sizeof(mMaxPeakBin));

    memset(mSingleHitNum, 0, sizeof(mSingleHitNum));
    memset(mSingleHitPos, 0.,sizeof(mSingleHitPos));
    memset(mSinglePeakHeight, 0.,sizeof(mSinglePeakHeight));
    memset(mSingleChiSquare, 0.,sizeof(mSingleChiSquare));

    memset(mMultiHitNum, 0, sizeof(mMultiHitNum));
    memset(mMultiHitPos, 0.,sizeof(mMultiHitPos));
    memset(mMultiPeakHeight, 0.,sizeof(mMultiPeakHeight));
    memset(mMultiPeakRaw, 0.,sizeof(mMultiPeakRaw));
    memset(mMultiEnergySum, 0.,sizeof(mMultiEnergySum));
    memset(mMultiChiSquare, 0.,sizeof(mMultiChiSquare));
}

void StRHICfHit::setPlateEnergy(Int_t tower, Int_t plate, Float_t val) {mPlateE[tower][plate] = val;}
void StRHICfHit::setGSOBarEnergy(Int_t tower, Int_t layer, Int_t xy, Int_t bar, Float_t val) 
{
    if(tower==0){mGSOBarSmallE[layer][xy][bar] = val;}
    if(tower==1){mGSOBarLargeE[layer][xy][bar] = val;}
}

void StRHICfHit::setL20(Int_t tower, Float_t val){mL20[tower] = val;}
void StRHICfHit::setL90(Int_t tower, Float_t val){mL90[tower] = val;}

void StRHICfHit::setPointNum(Int_t tower, Int_t val){mPointNum[tower] = val;}
void StRHICfHit::setGSOMaxLayer(Int_t tower, Int_t order, Int_t val){mGSOMaxLayer[tower][order] = val;}
void StRHICfHit::setMaxPeakBin(Int_t tower, Int_t layer, Int_t xy, Int_t val){mMaxPeakBin[tower][layer][xy] = val;}

void StRHICfHit::setSingleHitNum(Int_t tower, Int_t layer, Int_t xy, Int_t val){mSingleHitNum[tower][layer][xy] = val;}
void StRHICfHit::setSingleHitPos(Int_t tower, Int_t layer, Int_t xy, Float_t val){mSingleHitPos[tower][layer][xy] = val;}
void StRHICfHit::setSinglePeakHeight(Int_t tower, Int_t layer, Int_t xy, Float_t val){mSinglePeakHeight[tower][layer][xy] = val;}
void StRHICfHit::setSingleFitChi2(Int_t tower, Int_t layer, Int_t xy, Float_t val){mSingleChiSquare[tower][layer][xy] = val;}

void StRHICfHit::setMultiHitNum(Int_t tower, Int_t val){mMultiHitNum[tower] = val;}
void StRHICfHit::setMultiHitPos(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){mMultiHitPos[tower][layer][xy][order] = val;}
void StRHICfHit::setMultiPeakHeight(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){mMultiPeakHeight[tower][layer][xy][order] = val;}
void StRHICfHit::setMultiPeakRaw(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){mMultiPeakRaw[tower][layer][xy][order] = val;}
void StRHICfHit::setMultiEnergySum(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){mMultiEnergySum[tower][layer][xy][order] = val;}
void StRHICfHit::setMultiFitChi2(Int_t tower, Int_t layer, Int_t xy, Float_t val){mMultiChiSquare[tower][layer][xy] = val;}

Float_t StRHICfHit::getPlateEnergy(Int_t tower, Int_t plate){return mPlateE[tower][plate];}
Float_t StRHICfHit::getGSOBarEnergy(Int_t tower, Int_t layer, Int_t xy, Int_t bar) 
{
    if(tower==0){return mGSOBarSmallE[layer][xy][bar];}
    if(tower==1){return mGSOBarLargeE[layer][xy][bar];}
    return 0;
}

Float_t StRHICfHit::getL20(Int_t tower){return mL20[tower];}
Float_t StRHICfHit::getL90(Int_t tower){return mL90[tower];}

Int_t StRHICfHit::getPointNum(Int_t tower){return mPointNum[tower];}
Int_t StRHICfHit::getGSOMaxLayer(Int_t tower, Int_t order){return mGSOMaxLayer[tower][order];}
Int_t StRHICfHit::getMaxPeakBin(Int_t tower, Int_t layer, Int_t xy){return mMaxPeakBin[tower][layer][xy];}

Int_t StRHICfHit::getSingleHitNum(Int_t tower, Int_t layer, Int_t xy){return mSingleHitNum[tower][layer][xy];}
Float_t StRHICfHit::getSingleHitPos(Int_t tower, Int_t layer, Int_t xy){return mSingleHitPos[tower][layer][xy];}
Float_t StRHICfHit::getSinglePeakHeight(Int_t tower, Int_t layer, Int_t xy){return mSinglePeakHeight[tower][layer][xy];}
Float_t StRHICfHit::getSingleFitChi2(Int_t tower, Int_t layer, Int_t xy){return mSingleChiSquare[tower][layer][xy];}

Int_t StRHICfHit::getMultiHitNum(Int_t tower){return mMultiHitNum[tower];}
Float_t StRHICfHit::getMultiHitPos(Int_t tower, Int_t layer, Int_t xy, Int_t order){return mMultiHitPos[tower][layer][xy][order];}
Float_t StRHICfHit::getMultiPeakHeight(Int_t tower, Int_t layer, Int_t xy, Int_t order){return mMultiPeakHeight[tower][layer][xy][order];}
Float_t StRHICfHit::getMultiPeakRaw(Int_t tower, Int_t layer, Int_t xy, Int_t order){return mMultiPeakRaw[tower][layer][xy][order];}
Float_t StRHICfHit::getMultiEnergySum(Int_t tower, Int_t layer, Int_t xy, Int_t order){return mMultiEnergySum[tower][layer][xy][order];}
Float_t StRHICfHit::getMultiFitChi2(Int_t tower, Int_t layer, Int_t xy){return mMultiChiSquare[tower][layer][xy];}
