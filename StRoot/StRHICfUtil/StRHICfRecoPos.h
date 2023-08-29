#ifndef StRHICfRecoPos_hh
#define StRHICfRecoPos_hh

#include <TROOT.h>
#include <TMath.h>
#include <TF1.h>
#include <TH1D.h>
#include <TSpectrum.h>
#include <TGraphErrors.h>

#include "StRoot/StRHICfUtil/StRHICfFunction.h"

class StRHICfRecoPos : public StRHICfFunction
{
  public:
    StRHICfRecoPos();
    ~StRHICfRecoPos();

    void init();

    void setGSOBarEnergy(int tower, int layer, int xy, int bar, Double_t val);
    void setGSOBarTable(int tower, int layer, int xy, int bar, Double_t val);

    bool fillData();
    bool calculate();
    bool separateMultiFit(int tower);

    int getGSOMaxLayer(int tower, int order);
    int getMaximumBin(int tower, int layer, int xy);
    int getEvalHitNum(int tower);
    int getEvalHitNum(int tower, int layer, int xy);

    double getDepositEnergy(int tower, int layer);
    double getMultiEnergySum(int tower, int layer, int xy, int order);
    Double_t getSinglePeakPos(int tower, int layer, int xy);
    Double_t getMultiPeakPos(int tower, int layer, int xy, int order);
    double getSinglePeakHeight(int tower, int layer, int xy);
    double getMultiPeakHeight(int tower, int layer, int xy, int order);

    double getMultiPeakRaw(int tower, int layer, int xy, int order);
    double getEvalPeakHeight(int tower, int layer, int xy, int order);
    
    double getSingleChi2(int tower, int layer, int xy);
    double getMultiChi2(int tower, int layer, int xy);

    bool getWorthy(int tower);
    bool getOverlap(int tower, int xy);

  private:
    void findMaxLayer();
    void initSetParamaters();
    void fitting();

    Double_t getLorenzianSingle(Double_t* x, Double_t *par);
    Double_t getLorenzianMulti(Double_t* x, Double_t *par);

    // hit position finder base
    TH1D* mGSOBarHist[kRHICfNtower][kRHICfNlayer][kRHICfNxy];
    TH1D* mGSOBarHistExpend[kRHICfNtower][kRHICfNlayer][kRHICfNxy];
    TGraphErrors* mGSOBarGraph[kRHICfNtower][kRHICfNlayer][kRHICfNxy];

    //hit position fitting
    TF1* mSingleFit[kRHICfNtower][kRHICfNlayer][kRHICfNxy];
    TF1* mMultiFit[kRHICfNtower][kRHICfNlayer][kRHICfNxy];
    TF1* mEachFit[kRHICfNtower][kRHICfNlayer][kRHICfNxy][kRHICfNorder];

    TSpectrum* mSpectrum[kRHICfNtower][kRHICfNlayer][kRHICfNxy];

    int mGSOMaxLayer[kRHICfNtower][kRHICfNorder];
    Double_t mGSOBarSE[kRHICfNlayer][kRHICfNxy][kRHICfNbarSmall];
    Double_t mGSOBarTE[kRHICfNlayer][kRHICfNxy][kRHICfNbarLarge];
    Double_t mGSOBarSTable[kRHICfNlayer][kRHICfNxy][kRHICfNbarSmall];
    Double_t mGSOBarTTable[kRHICfNlayer][kRHICfNxy][kRHICfNbarLarge];

    bool mWorthy[kRHICfNtower];
    bool mOverlap[kRHICfNtower][kRHICfNxy];

    // Define the constant
    const Double_t mNoiseThreshold = 0.02;
    const Double_t mPedADCRMS = 3.0;
    const Double_t mAvgConvFactor = 3.5e5;
    const Double_t m1000Vto600VFactor = 0.05;
    const Double_t mSpecialFactor = 0.477;
    const Double_t mGSOBarMapError = 0.15;
    const Double_t mTSpecSigma = 1.0;
    const Double_t mTSpecRatioThreshold = 0.05;
    const Double_t mPeakDistThreshold = 3.0;
    
    // Define the fitting parameters
    const Double_t mParWidth1 = 1.0;
    const Double_t mParWidth2 = 10.0;
    const Double_t mParRatio = 0.6;
    const Double_t mParBaseLine = 0.00001;

    //Define the fitting parameter Limits
    const Double_t mParWidth1Min = 0.5;
    const Double_t mParWidth1Max = 5.0;
    const Double_t mParHeightMin = 0.0;
    const Double_t mParHeightMax = 100.0;
    const Double_t mParWidth2Min = 5.0;
    const Double_t mParWidth2Max = 40.0;
    const Double_t mParRatioMin = 0.0;
    const Double_t mParRatioMax = 1.0;
};

#endif