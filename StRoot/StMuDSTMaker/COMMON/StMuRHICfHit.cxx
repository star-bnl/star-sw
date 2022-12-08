#include "StMuRHICfHit.h"

ClassImp(StMuRHICfHit)

StMuRHICfHit::StMuRHICfHit()
{
	clear();
}

StMuRHICfHit::~StMuRHICfHit()
{
}

void StMuRHICfHit::clear()
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

void StMuRHICfHit::setPlateEnergy(Int_t tower, Int_t plate, Float_t val) {mPlateE[tower][plate] = val;}
void StMuRHICfHit::setGSOBarEnergy(Int_t tower, Int_t layer, Int_t xy, Int_t bar, Float_t val) 
{
    if(tower==0){mGSOBarSmallE[layer][xy][bar] = val;}
    if(tower==1){mGSOBarLargeE[layer][xy][bar] = val;}
}

void StMuRHICfHit::setL20(Int_t tower, Float_t val){mL20[tower] = val;}
void StMuRHICfHit::setL90(Int_t tower, Float_t val){mL90[tower] = val;}

void StMuRHICfHit::setPointNum(Int_t tower, Int_t val){mPointNum[tower] = val;}
void StMuRHICfHit::setGSOMaxLayer(Int_t tower, Int_t order, Int_t val){mGSOMaxLayer[tower][order] = val;}
void StMuRHICfHit::setMaxPeakBin(Int_t tower, Int_t layer, Int_t xy, Int_t val){mMaxPeakBin[tower][layer][xy] = val;}

void StMuRHICfHit::setSingleHitNum(Int_t tower, Int_t layer, Int_t xy, Int_t val){mSingleHitNum[tower][layer][xy] = val;}
void StMuRHICfHit::setSingleHitPos(Int_t tower, Int_t layer, Int_t xy, Float_t val){mSingleHitPos[tower][layer][xy] = val;}
void StMuRHICfHit::setSinglePeakHeight(Int_t tower, Int_t layer, Int_t xy, Float_t val){mSinglePeakHeight[tower][layer][xy] = val;}
void StMuRHICfHit::setSingleFitChi2(Int_t tower, Int_t layer, Int_t xy, Float_t val){mSingleChiSquare[tower][layer][xy] = val;}

void StMuRHICfHit::setMultiHitNum(Int_t tower, Int_t val){mMultiHitNum[tower] = val;}
void StMuRHICfHit::setMultiHitPos(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){mMultiHitPos[tower][layer][xy][order] = val;}
void StMuRHICfHit::setMultiPeakHeight(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){mMultiPeakHeight[tower][layer][xy][order] = val;}
void StMuRHICfHit::setMultiPeakRaw(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){mMultiPeakRaw[tower][layer][xy][order] = val;}
void StMuRHICfHit::setMultiEnergySum(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){mMultiEnergySum[tower][layer][xy][order] = val;}
void StMuRHICfHit::setMultiFitChi2(Int_t tower, Int_t layer, Int_t xy, Float_t val){mMultiChiSquare[tower][layer][xy] = val;}

Float_t StMuRHICfHit::getPlateEnergy(Int_t tower, Int_t plate){return mPlateE[tower][plate];}
Float_t StMuRHICfHit::getGSOBarEnergy(Int_t tower, Int_t layer, Int_t xy, Int_t bar) 
{
    if(tower==0){return mGSOBarSmallE[layer][xy][bar];}
    if(tower==1){return mGSOBarLargeE[layer][xy][bar];}
    return 0;
}

Float_t StMuRHICfHit::getL20(Int_t tower){return mL20[tower];}
Float_t StMuRHICfHit::getL90(Int_t tower){return mL90[tower];}

Int_t StMuRHICfHit::getPointNum(Int_t tower){return mPointNum[tower];}
Int_t StMuRHICfHit::getGSOMaxLayer(Int_t tower, Int_t order){return mGSOMaxLayer[tower][order];}
Int_t StMuRHICfHit::getMaxPeakBin(Int_t tower, Int_t layer, Int_t xy){return mMaxPeakBin[tower][layer][xy];}

Int_t StMuRHICfHit::getSingleHitNum(Int_t tower, Int_t layer, Int_t xy){return mSingleHitNum[tower][layer][xy];}
Float_t StMuRHICfHit::getSingleHitPos(Int_t tower, Int_t layer, Int_t xy){return mSingleHitPos[tower][layer][xy];}
Float_t StMuRHICfHit::getSinglePeakHeight(Int_t tower, Int_t layer, Int_t xy){return mSinglePeakHeight[tower][layer][xy];}
Float_t StMuRHICfHit::getSingleFitChi2(Int_t tower, Int_t layer, Int_t xy){return mSingleChiSquare[tower][layer][xy];}

Int_t StMuRHICfHit::getMultiHitNum(Int_t tower){return mMultiHitNum[tower];}
Float_t StMuRHICfHit::getMultiHitPos(Int_t tower, Int_t layer, Int_t xy, Int_t order){return mMultiHitPos[tower][layer][xy][order];}
Float_t StMuRHICfHit::getMultiPeakHeight(Int_t tower, Int_t layer, Int_t xy, Int_t order){return mMultiPeakHeight[tower][layer][xy][order];}
Float_t StMuRHICfHit::getMultiPeakRaw(Int_t tower, Int_t layer, Int_t xy, Int_t order){return mMultiPeakRaw[tower][layer][xy][order];}
Float_t StMuRHICfHit::getMultiEnergySum(Int_t tower, Int_t layer, Int_t xy, Int_t order){return mMultiEnergySum[tower][layer][xy][order];}
Float_t StMuRHICfHit::getMultiFitChi2(Int_t tower, Int_t layer, Int_t xy){return mMultiChiSquare[tower][layer][xy];}
