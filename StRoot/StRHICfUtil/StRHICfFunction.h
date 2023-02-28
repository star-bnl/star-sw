/***************************************************************************
 * Author: Seunghwan Lee
 ***************************************************************************
 *
 * Description: RHICf Useful class for calibration and reconstruction
 *
 ***************************************************************************
 * $Id: StRHICfFunction.h,v 1.0 2022/08/25 2:51:00 SeunghwanLee Exp $
 * $Log: StRHICfFunction.h,v $
 ***************************************************************************/

#ifndef StRHICfFunction_H
#define StRHICfFunction_H

#include <algorithm>
#include <iostream>
#include <string>
#include "StRoot/StEvent/StEnumerations.h"

using namespace std;

enum RHICfRunType
{
  kRHICfTL = 0, 
  kRHICfTS = 1, 
  kRHICfTOP = 2 
};

enum RHICfIsValue
{
  kRHICfOk = true,
  kRHICfFatal = false
};

enum RHICfParticle
{
  kRHICfPhoton = 0,
  kRHICfHadron = 1
};

class StRHICfFunction 
{
  public: 
    StRHICfFunction();
    virtual ~StRHICfFunction();

  void initChecker();

    void setRunType(int type){mRunType = type;}
    int getRunType(){return mRunType;}
    int checkRunTypeForRHICf2017(int runNum);
    int checkGSOBarSize(int tower); // 0 = small, 1 = large

    float rescaleEnergyFactor(int tower, int layer);
    
    bool checkGSOBarEnergy(float val);
    bool checkGSOBarTable(float val);
    bool checkPlateEnergy(float val);
    bool checkRecoValue(string opt, float val);
    bool checkLeakageTable(string opt, bool val);

  private:
    int mRunType;
    int mGSOBarNum;
    int mGSOBarTableNum;
    int mPlateNum;
    int mValueForRecoNum[5]; // in StRHICfRecoEnergy, check setting parameter[ResultHitPos, ResultHitNum, MultiHitPos, MultiPeakHeight, Overlap]
    int mLeakageNum[3];
};

#endif
