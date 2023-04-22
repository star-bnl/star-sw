#ifndef StRHICfRecoEnergy_hh
#define StRHICfRecoEnergy_hh

#include <TROOT.h>
#include <TMath.h>
#include <TH2D.h>

#include "StRoot/StRHICfUtil/StRHICfFunction.h"

class StRHICfRecoEnergy : public StRHICfFunction
{
  public:
    StRHICfRecoEnergy();
    ~StRHICfRecoEnergy();

    void init();
    bool calculate();

    void setRunType(int runType);
    void setPlateEnergy(int tower, int layer, double val);
    void setResultHitPos(int tower, int xy, double val);
    void setResultHitNum(int tower, int val);
    void setMultiHitPos(int tower, int layer, int xy, int order, double val);
    void setMultiPeakHeight(int tower, int layer, int xy, int order, double val);
    void setOverlap(int tower, int xy, bool val);
    void setLeakageInTable(int tower, int layer, TH2D* table); 
    void setLeakageOutTablePhoton(int tower, int layer, TH2D* table); 
    void setLeakageOutTableNeutron(int tower, TH2D* table); 

    int getResultHitNum(int tower);
    double getPlateSumEnergy(int tower, bool all=true);
    double getResultEnergy(int tower, int particle);
    double getEnergyRatio(int tower, int order);

  private:
    void recoPhotonEnergySimple(int tower, double& energy); 
    void recoPhotonEnergySingle(int tower);
    void recoPhotonEnergyDouble();
    void recoHadronEnergy(int tower);
    void correctLightYield();
    void getCalibrationEnergy(int tower, double posX1, double posY1, double posX2=0., double posY2=0., double firstRatio=-999., double secondRatio=-999.);

    double getSumEnergy(int tower, int startIdx, int endIdx, double* energy);
    double getPhotonEnergyConvert(int tower, double sumEnergy);
    double getHadronEnergyConvert(int tower, double sumEnergy);
    double getLeakageInPhoton(int tower, int layer, double x, double y); 
    double getLeakageOutPhoton(int tower, int layer, double x, double y); 
    double getLeakageOutNeutron(int tower, double x, double y);

    TH2D* mLeakInTablePhoton[kRHICfNtower][kRHICfNplate];
    TH2D* mLeakOutTablePhoton[kRHICfNtower][kRHICfNplate];
    TH2D* mLeakOutTableNeutron[kRHICfNtower];

    int mRunType;
    int mRecoHitNum[kRHICfNtower];
    bool mOverlap[kRHICfNtower][kRHICfNxy];

    double mRecoHitPos[kRHICfNtower][kRHICfNxy];
    double mMultiHitPos[kRHICfNtower][kRHICfNorder][kRHICfNxy][kRHICfNorder]; 
    double mMultiPeakHeight[kRHICfNtower][kRHICfNorder][kRHICfNxy][kRHICfNorder]; 
    double mPlateE[kRHICfNtower][kRHICfNplate];
    double mPlateECorr[kRHICfNtower][kRHICfNplate];
    double mSumEnergy[kRHICfNtower][kRHICfNorder]; 
    double mResultEnergy[kRHICfNtower][kRHICfNorder]; 
    double mEnergyRatio[kRHICfNtower][kRHICfNorder];

    const double mEnergyThreshold = 10.0;
};

#endif