#include "StRHICfHit.h"

ClassImp(StRHICfHit)

#define RHICF_MAXLAYER_TOWERNUM 2
#define RHICF_SINGLE_TOWERNUM 8
#define RHICF_MULTI_TOWERNUM 16
#define RHICF_MAXLAYER_INDEX(a, b) (2*a+b)
#define RHICF_SINGLE_INDEX(a, b, c) (8*a+2*b+c)
#define RHICF_MULTI_INDEX(a, b, c, d) (16*a+4*b+2*c+d)

StRHICfHit::StRHICfHit()
{
  mIsSaveDataArray = false; 
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

void StRHICfHit::initDataArray() 
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

void StRHICfHit::deleteDataArray()
{
  mIsSaveDataArray = false;

  delete mL20;
  delete mL90;
  delete mGSOMaxLayer;
  delete mMaxPeakBin;
  delete mSingleHitNum;
  delete mSingleHitPos;
  delete mSinglePeakHeight;
  delete mSingleChiSquare;
  delete mMultiHitNum;
  delete mMultiHitPos;
  delete mMultiPeakHeight;
  delete mMultiPeakRaw;
  delete mMultiEnergySum;
  delete mMultiChiSquare;

  mL20 = nullptr;
  mL90 = nullptr;
  mGSOMaxLayer = nullptr;
  mMaxPeakBin = nullptr;
  mSingleHitNum = nullptr;
  mSingleHitPos = nullptr;
  mSinglePeakHeight = nullptr;
  mSingleChiSquare = nullptr;
  mMultiHitNum = nullptr;
  mMultiHitPos = nullptr;
  mMultiPeakHeight = nullptr;
  mMultiPeakRaw = nullptr;
  mMultiEnergySum = nullptr;
  mMultiChiSquare = nullptr;
}

Bool_t StRHICfHit::isSaveDataArray(){return mIsSaveDataArray;}

void StRHICfHit::setPlateEnergy(Int_t tower, Int_t plate, Float_t val) {mPlateE[tower][plate] = val;}
void StRHICfHit::setGSOBarEnergy(Int_t tower, Int_t layer, Int_t xy, Int_t bar, Float_t val) 
{
  if(tower==0){mGSOBarSmallE[layer][xy][bar] = val;}
  if(tower==1){mGSOBarLargeE[layer][xy][bar] = val;}
}

void StRHICfHit::setL20(Int_t tower, Float_t val){if(mL20){mL20->AddAt(val, tower);}}
void StRHICfHit::setL90(Int_t tower, Float_t val){if(mL90){mL90->AddAt(val, tower);}}

void StRHICfHit::setGSOMaxLayer(Int_t tower, Int_t order, Int_t val){if(mGSOMaxLayer){mGSOMaxLayer->AddAt(val, RHICF_MAXLAYER_INDEX(tower, order));}}
void StRHICfHit::setMaxPeakBin(Int_t tower, Int_t layer, Int_t xy, Int_t val){if(mMaxPeakBin){mMaxPeakBin->AddAt(val, RHICF_SINGLE_INDEX(tower,layer,xy));}}

void StRHICfHit::setSingleHitNum(Int_t tower, Int_t layer, Int_t xy, Int_t val){if(mSingleHitNum){mSingleHitNum->AddAt(val, RHICF_SINGLE_INDEX(tower,layer,xy));}}
void StRHICfHit::setSingleHitPos(Int_t tower, Int_t layer, Int_t xy, Float_t val){if(mSingleHitPos){mSingleHitPos->AddAt(val, RHICF_SINGLE_INDEX(tower,layer,xy));}}
void StRHICfHit::setSinglePeakHeight(Int_t tower, Int_t layer, Int_t xy, Float_t val){if(mSinglePeakHeight){mSinglePeakHeight->AddAt(val, RHICF_SINGLE_INDEX(tower,layer,xy));}}
void StRHICfHit::setSingleFitChi2(Int_t tower, Int_t layer, Int_t xy, Float_t val){if(mSingleChiSquare){mSingleChiSquare->AddAt(val, RHICF_SINGLE_INDEX(tower,layer,xy));}}

void StRHICfHit::setMultiHitNum(Int_t tower, Int_t val){if(mMultiHitNum){mMultiHitNum->AddAt(val, tower);}}
void StRHICfHit::setMultiHitPos(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){if(mMultiHitPos){mMultiHitPos->AddAt(val, RHICF_MULTI_INDEX(tower,layer,xy,order));}}
void StRHICfHit::setMultiPeakHeight(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){if(mMultiPeakHeight){mMultiPeakHeight->AddAt(val, RHICF_MULTI_INDEX(tower,layer,xy,order));}}
void StRHICfHit::setMultiPeakRaw(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){if(mMultiPeakRaw){mMultiPeakRaw->AddAt(val, RHICF_MULTI_INDEX(tower,layer,xy,order));}}
void StRHICfHit::setMultiEnergySum(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val){if(mMultiEnergySum){mMultiEnergySum->AddAt(val, RHICF_MULTI_INDEX(tower,layer,xy,order));}}
void StRHICfHit::setMultiFitChi2(Int_t tower, Int_t layer, Int_t xy, Float_t val){if(mMultiChiSquare){mMultiChiSquare->AddAt(val, RHICF_SINGLE_INDEX(tower,layer,xy));}}

Float_t StRHICfHit::getPlateEnergy(Int_t tower, Int_t plate){return mPlateE[tower][plate];}
Float_t StRHICfHit::getGSOBarEnergy(Int_t tower, Int_t layer, Int_t xy, Int_t bar) 
{
  if(tower==0){return mGSOBarSmallE[layer][xy][bar];}
  if(tower==1){return mGSOBarLargeE[layer][xy][bar];}
  return 0;
}

Float_t StRHICfHit::getL20(Int_t tower)
{
  if(mL20){return mL20->At(tower);}
  return 0.;
}

Float_t StRHICfHit::getL90(Int_t tower)
{
  if(mL90){return mL90->At(tower);}
  return 0.;
}

Int_t StRHICfHit::getGSOMaxLayer(Int_t tower, Int_t order)
{
  if(mGSOMaxLayer){return mGSOMaxLayer->At(RHICF_MAXLAYER_INDEX(tower,order));}
  return -999;
}

Int_t StRHICfHit::getMaxPeakBin(Int_t tower, Int_t layer, Int_t xy)
{
  if(mMaxPeakBin){return mMaxPeakBin->At(RHICF_SINGLE_INDEX(tower,layer,xy));}
  return -999;
}

Int_t StRHICfHit::getSingleHitNum(Int_t tower, Int_t layer, Int_t xy)
{
  if(mSingleHitNum){return mSingleHitNum->At(RHICF_SINGLE_INDEX(tower,layer,xy));}
  return -999;
}

Float_t StRHICfHit::getSingleHitPos(Int_t tower, Int_t layer, Int_t xy)
{
  if(mSingleHitPos){return mSingleHitPos->At(RHICF_SINGLE_INDEX(tower,layer,xy));}
  return 0.;
}

Float_t StRHICfHit::getSinglePeakHeight(Int_t tower, Int_t layer, Int_t xy)
{
  if(mSinglePeakHeight){return mSinglePeakHeight->At(RHICF_SINGLE_INDEX(tower,layer,xy));}
  return 0.;
}

Float_t StRHICfHit::getSingleFitChi2(Int_t tower, Int_t layer, Int_t xy)
{
  if(mSingleChiSquare){return mSingleChiSquare->At(RHICF_SINGLE_INDEX(tower,layer,xy));}
  return 0.;
}

Int_t StRHICfHit::getMultiHitNum(Int_t tower)
{
  if(mMultiHitNum){return mMultiHitNum->At(tower);}
  return -999;
}

Float_t StRHICfHit::getMultiHitPos(Int_t tower, Int_t layer, Int_t xy, Int_t order)
{
  if(mMultiHitPos){return mMultiHitPos->At(RHICF_MULTI_INDEX(tower,layer,xy,order));}
  return 0.;
}

Float_t StRHICfHit::getMultiPeakHeight(Int_t tower, Int_t layer, Int_t xy, Int_t order)
{
  if(mMultiPeakHeight){return mMultiPeakHeight->At(RHICF_MULTI_INDEX(tower,layer,xy,order));}
  return 0.;
}

Float_t StRHICfHit::getMultiPeakRaw(Int_t tower, Int_t layer, Int_t xy, Int_t order)
{
  if(mMultiPeakRaw){return mMultiPeakRaw->At(RHICF_MULTI_INDEX(tower,layer,xy,order));}
  return 0.;
}

Float_t StRHICfHit::getMultiEnergySum(Int_t tower, Int_t layer, Int_t xy, Int_t order)
{
  if(mMultiEnergySum){return mMultiEnergySum->At(RHICF_MULTI_INDEX(tower,layer,xy,order));}
  return 0.;
}

Float_t StRHICfHit::getMultiFitChi2(Int_t tower, Int_t layer, Int_t xy)
{
  if(mMultiChiSquare){return mMultiChiSquare->At(RHICF_SINGLE_INDEX(tower,layer,xy));}
  return 0.;
}
