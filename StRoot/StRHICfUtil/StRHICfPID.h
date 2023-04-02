#ifndef StRHICfPID_hh
#define StRHICfPID_hh

#include "StRoot/StRHICfUtil/StRHICfFunction.h"

class StRHICfPID : public StRHICfFunction
{
  public:
    StRHICfPID();
    ~StRHICfPID();

    void init();
    bool calculate();

    void setPlateEnergy(int tower, int layer, float val);

    int getPID(int tower);
    float getL20(int tower);
    float getL90(int tower);

  private:
    float checkStep(int layer);
    float layerSumEnergy(int tower, int layer);
    float findRadiationLength(int tower, float ratio);
    float calculateEquation(int tower, int layer, float sumE);

    int mPID[kRHICfNtower];
    float mPlateE[kRHICfNtower][kRHICfNplate];
    float mPlateSumE[kRHICfNtower];
    float mL20[kRHICfNtower];
    float mL90[kRHICfNtower];
    bool mPlateEIs;

    int mPlateIdxNum = kRHICfNplate - 1;
    float mL20Const = 0.2;
    float mL90Const = 0.9;
};

#endif