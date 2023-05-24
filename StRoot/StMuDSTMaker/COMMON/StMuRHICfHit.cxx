#include "StMuRHICfHit.h"

ClassImp(StMuRHICfHit)

#define RHICF_MAXLAYER_TOWERNUM 2
#define RHICF_SINGLE_TOWERNUM 8
#define RHICF_MULTI_TOWERNUM 16
#define RHICF_MAXLAYER_INDEX(a, b) (2*a+b)
#define RHICF_SINGLE_INDEX(a, b, c) (8*a+2*b+c)
#define RHICF_MULTI_INDEX(a, b, c, d) (16*a+4*b+2*c+d)

StMuRHICfHit::StMuRHICfHit()
{
  mIsSaveDataArray = false; 
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

  if(mIsSaveDataArray){
    mL20 -> Reset();
    mL90 -> Reset();
    mGSOMaxLayer -> Reset();
    mMaxPeakBin -> Reset();
    mSingleHitNum -> Reset();
    mSingleHitPos -> Reset();
    mSinglePeakHeight -> Reset();
    mSingleChiSquare -> Reset();
    mMultiHitNum -> Reset();
    mMultiHitPos -> Reset();
    mMultiPeakHeight -> Reset();
    mMultiPeakRaw-> Reset();
    mMultiEnergySum -> Reset();
    mMultiChiSquare -> Reset();
  }
}

void StMuRHICfHit::initDataArray() 
{
  mIsSaveDataArray = true;
  mL20 = new TArrayF(RHICF_MAXLAYER_TOWERNUM);
  mL90 = new TArrayF(RHICF_MAXLAYER_TOWERNUM);

  mGSOMaxLayer = new TArrayI(2*RHICF_MAXLAYER_TOWERNUM);
  mMaxPeakBin = new TArrayI(2*RHICF_SINGLE_TOWERNUM);

  mSingleHitNum = new TArrayI(2*RHICF_SINGLE_TOWERNUM);
  mSingleHitPos = new TArrayF(2*RHICF_SINGLE_TOWERNUM);
  mSinglePeakHeight = new TArrayF(2*RHICF_SINGLE_TOWERNUM);
  mSingleChiSquare = new TArrayF(2*RHICF_SINGLE_TOWERNUM);

  mMultiHitNum = new TArrayI(RHICF_MAXLAYER_TOWERNUM);
  mMultiHitPos = new TArrayF(2*RHICF_MULTI_TOWERNUM);
  mMultiPeakHeight = new TArrayF(2*RHICF_MULTI_TOWERNUM);
  mMultiPeakRaw = new TArrayF(2*RHICF_MULTI_TOWERNUM);
  mMultiEnergySum = new TArrayF(2*RHICF_MULTI_TOWERNUM);
  mMultiChiSquare = new TArrayF(2*RHICF_SINGLE_TOWERNUM);
}

Bool_t StMuRHICfHit::isSaveDataArray(){return mIsSaveDataArray;}

void StMuRHICfHit::setPlateEnergy(Int_t tower, Int_t plate, Float_t val) {mPlateE[tower][plate] = val;}
void StMuRHICfHit::setGSOBarEnergy(Int_t tower, Int_t layer, Int_t xy, Int_t bar, Float_t val) 
{
  if(tower==0){mGSOBarSmallE[layer][xy][bar] = val;}
  if(tower==1){mGSOBarLargeE[layer][xy][bar] = val;}
}

void StMuRHICfHit::setL20(Int_t tower, Float_t val){if(mL20){mL20->AddAt(val, tower);}}
void StMuRHICfHit::setL90(Int_t tower, Float_t val){if(mL90){mL90->AddAt(val, tower);}}

void StMuRHICfHit::setGSOMaxLayer(Int_t tower, Int_t order, Int_t val){if(mGSOMaxLayer){mGSOMaxLayer->AddAt(val, RHICF_MAXLAYER_INDEX(tower, order));}}
void StMuRHICfHit::setMaxPeakBin(Int_t tower, Int_t layer, Int_t xy, Int_t val){if(mMaxPeakBin){mMaxPeakBin->AddAt(val, RHICF_SINGLE_INDEX(tower,layer,xy));}}

void StMuRHICfHit::setSingleHitNum(Int_t tower, Int_t layer, Int_t xy, Int_t val){if(mSingleHitNum){mSingleHitNum->AddAt(val, RHICF_SINGLE_INDEX(tower,layer,xy));}}
void StMuRHICfHit::setSingleHitPos(Int_t tower, Int_t layer, Int_t xy, Float_t val){if(mSingleHitPos){mSingleHitPos->AddAt(val, RHICF_SINGLE_INDEX(tower,layer,xy));}}
void StMuRHICfHit::setSinglePeakHeight(Int_t tower, Int_t layer, Int_t xy, Float_t val){if(mSinglePeakHeight){mSinglePeakHeight->AddAt(val, RHICF_SINGLE_INDEX(tower,layer,xy));}}
void StMuRHICfHit::setSingleFitChi2(Int_t tower, Int_t layer, Int_t xy, Float_t val){if(mSingleChiSquare){mSingleChiSquare->AddAt(val, RHICF_SINGLE_INDEX(tower,layer,xy));}}

void StMuRHICfHit::setMultiHitNum(Int_t tower, Int_t val){if(mMultiHitNum){mMultiHitNum->AddAt(val, tower);}}
void StMuRHICfHit::setMultiHitPos(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){if(mMultiHitPos){mMultiHitPos->AddAt(val, RHICF_MULTI_INDEX(tower,layer,xy,order));}}
void StMuRHICfHit::setMultiPeakHeight(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){if(mMultiPeakHeight){mMultiPeakHeight->AddAt(val, RHICF_MULTI_INDEX(tower,layer,xy,order));}}
void StMuRHICfHit::setMultiPeakRaw(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){if(mMultiPeakRaw){mMultiPeakRaw->AddAt(val, RHICF_MULTI_INDEX(tower,layer,xy,order));}}
void StMuRHICfHit::setMultiEnergySum(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){if(mMultiEnergySum){mMultiEnergySum->AddAt(val, RHICF_MULTI_INDEX(tower,layer,xy,order));}}
void StMuRHICfHit::setMultiFitChi2(Int_t tower, Int_t layer, Int_t xy, Float_t val){if(mMultiChiSquare){mMultiChiSquare->AddAt(val, RHICF_SINGLE_INDEX(tower,layer,xy));}}

Float_t StMuRHICfHit::getPlateEnergy(Int_t tower, Int_t plate){return mPlateE[tower][plate];}
Float_t StMuRHICfHit::getGSOBarEnergy(Int_t tower, Int_t layer, Int_t xy, Int_t bar) 
{
  if(tower==0){return mGSOBarSmallE[layer][xy][bar];}
  if(tower==1){return mGSOBarLargeE[layer][xy][bar];}
  return 0;
}

Float_t StMuRHICfHit::getL20(Int_t tower)
{
  if(mL20){return mL20->At(tower);}
  return 0.;
}

Float_t StMuRHICfHit::getL90(Int_t tower)
{
  if(mL90){return mL90->At(tower);}
  return 0.;
}

Int_t StMuRHICfHit::getGSOMaxLayer(Int_t tower, Int_t order)
{
  if(mGSOMaxLayer){return mGSOMaxLayer->At(RHICF_MAXLAYER_INDEX(tower,order));}
  return -999;
}

Int_t StMuRHICfHit::getMaxPeakBin(Int_t tower, Int_t layer, Int_t xy)
{
  if(mMaxPeakBin){return mMaxPeakBin->At(RHICF_SINGLE_INDEX(tower,layer,xy));}
  return -999;
}

Int_t StMuRHICfHit::getSingleHitNum(Int_t tower, Int_t layer, Int_t xy)
{
  if(mSingleHitNum){return mSingleHitNum->At(RHICF_SINGLE_INDEX(tower,layer,xy));}
  return -999;
}

Float_t StMuRHICfHit::getSingleHitPos(Int_t tower, Int_t layer, Int_t xy)
{
  if(mSingleHitPos){return mSingleHitPos->At(RHICF_SINGLE_INDEX(tower,layer,xy));}
  return 0.;
}

Float_t StMuRHICfHit::getSinglePeakHeight(Int_t tower, Int_t layer, Int_t xy)
{
  if(mSinglePeakHeight){return mSinglePeakHeight->At(RHICF_SINGLE_INDEX(tower,layer,xy));}
  return 0.;
}

Float_t StMuRHICfHit::getSingleFitChi2(Int_t tower, Int_t layer, Int_t xy)
{
  if(mSingleChiSquare){return mSingleChiSquare->At(RHICF_SINGLE_INDEX(tower,layer,xy));}
  return 0.;
}

Int_t StMuRHICfHit::getMultiHitNum(Int_t tower)
{
  if(mMultiHitNum){return mMultiHitNum->At(tower);}
  return -999;
}

Float_t StMuRHICfHit::getMultiHitPos(Int_t tower, Int_t layer, Int_t xy, Int_t order)
{
  if(mMultiHitPos){return mMultiHitPos->At(RHICF_MULTI_INDEX(tower,layer,xy,order));}
  return 0.;
}

Float_t StMuRHICfHit::getMultiPeakHeight(Int_t tower, Int_t layer, Int_t xy, Int_t order)
{
  if(mMultiPeakHeight){return mMultiPeakHeight->At(RHICF_MULTI_INDEX(tower,layer,xy,order));}
  return 0.;
}

Float_t StMuRHICfHit::getMultiPeakRaw(Int_t tower, Int_t layer, Int_t xy, Int_t order)
{
  if(mMultiPeakRaw){return mMultiPeakRaw->At(RHICF_MULTI_INDEX(tower,layer,xy,order));}
  return 0.;
}

Float_t StMuRHICfHit::getMultiEnergySum(Int_t tower, Int_t layer, Int_t xy, Int_t order)
{
  if(mMultiEnergySum){return mMultiEnergySum->At(RHICF_MULTI_INDEX(tower,layer,xy,order));}
  return 0.;
}

Float_t StMuRHICfHit::getMultiFitChi2(Int_t tower, Int_t layer, Int_t xy)
{
  if(mMultiChiSquare){return mMultiChiSquare->At(RHICF_SINGLE_INDEX(tower,layer,xy));}
  return 0.;
}
