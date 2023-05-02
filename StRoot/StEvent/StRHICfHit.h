#ifndef StRHICfHit_hh
#define StRHICfHit_hh

#include <TObject.h>
#include <TArrayI.h>
#include <TArrayF.h>
#include "StEnumerations.h"

class StRHICfHit : public TObject 
{
  public:
    StRHICfHit();
    ~StRHICfHit();

    void clear();

    void initDataArray(); 
    void deleteDataArray();

    Bool_t isSaveDataArray(); 

    void setPlateEnergy(Int_t tower, Int_t plate, Float_t val);
    void setGSOBarEnergy(Int_t tower, Int_t layer, Int_t xy, Int_t bar, Float_t val);

    void setL20(Int_t tower, Float_t val);
    void setL90(Int_t tower, Float_t val);

    void setGSOMaxLayer(Int_t tower, Int_t order, Int_t val);
    void setMaxPeakBin(Int_t tower, Int_t layer, Int_t xy, Int_t val);

    void setSingleHitNum(Int_t tower, Int_t layer, Int_t xy, Int_t val);
    void setSingleHitPos(Int_t tower, Int_t layer, Int_t xy, Float_t val);
    void setSinglePeakHeight(Int_t tower, Int_t layer, Int_t xy, Float_t val);
    void setSingleFitChi2(Int_t tower, Int_t layer, Int_t xy, Float_t val);

    void setMultiHitNum(Int_t tower, Int_t val);
    void setMultiHitPos(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val);
    void setMultiPeakHeight(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val);
    void setMultiPeakRaw(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val);
    void setMultiEnergySum(Int_t tower, Int_t layer, Int_t xy, Int_t order, Float_t val);
    void setMultiFitChi2(Int_t tower, Int_t layer, Int_t xy, Float_t val);

    Float_t getPlateEnergy(Int_t tower, Int_t plate);
    Float_t getGSOBarEnergy(Int_t tower, Int_t layer, Int_t xy, Int_t bar);

    Float_t getL20(Int_t tower);
    Float_t getL90(Int_t tower);

    Int_t getGSOMaxLayer(Int_t tower, Int_t order);
    Int_t getMaxPeakBin(Int_t tower, Int_t layer, Int_t xy);

    Int_t getSingleHitNum(Int_t tower, Int_t layer, Int_t xy);
    Float_t getSingleHitPos(Int_t tower, Int_t layer, Int_t xy);
    Float_t getSinglePeakHeight(Int_t tower, Int_t layer, Int_t xy);
    Float_t getSingleFitChi2(Int_t tower, Int_t layer, Int_t xy);

    Int_t getMultiHitNum(Int_t tower);
    Float_t getMultiHitPos(Int_t tower, Int_t layer, Int_t xy, Int_t order);
    Float_t getMultiPeakHeight(Int_t tower, Int_t layer, Int_t xy, Int_t order);
    Float_t getMultiPeakRaw(Int_t tower, Int_t layer, Int_t xy, Int_t order);
    Float_t getMultiEnergySum(Int_t tower, Int_t layer, Int_t xy, Int_t order);
    Float_t getMultiFitChi2(Int_t tower, Int_t layer, Int_t xy);

  private:
    Float_t mPlateE[kRHICfNtower][kRHICfNplate];
    Float_t mGSOBarSmallE[kRHICfNlayer][kRHICfNxy][kRHICfNbarSmall];
    Float_t mGSOBarLargeE[kRHICfNlayer][kRHICfNxy][kRHICfNbarLarge];

    TArrayF* mL20 = nullptr;
    TArrayF* mL90 = nullptr;

    TArrayI* mGSOMaxLayer = nullptr;
    TArrayI* mMaxPeakBin = nullptr;

    TArrayI* mSingleHitNum = nullptr;
    TArrayF* mSingleHitPos = nullptr;
    TArrayF* mSinglePeakHeight = nullptr;
    TArrayF* mSingleChiSquare = nullptr;

    TArrayI* mMultiHitNum = nullptr;
    TArrayF* mMultiHitPos = nullptr;
    TArrayF* mMultiPeakHeight = nullptr;
    TArrayF* mMultiPeakRaw = nullptr;
    TArrayF* mMultiEnergySum = nullptr;
    TArrayF* mMultiChiSquare = nullptr;

    Bool_t mIsSaveDataArray;

  ClassDef(StRHICfHit,1)
};

#endif
