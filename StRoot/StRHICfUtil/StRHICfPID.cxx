#include "StRHICfPID.h"

StRHICfPID::StRHICfPID()
{
  init();
}

void StRHICfPID::init()
{
  std::fill(mPID, mPID+kRHICfNtower, -1);
  std::fill(mPlateSumE, mPlateSumE+kRHICfNtower, 0.);
  std::fill(mL20, mL20+kRHICfNtower, 0.);
  std::fill(mL90, mL90+kRHICfNtower, 0.);
  std::fill(&mPlateE[0][0], &mPlateE[kRHICfNtower-1][kRHICfNplate], 0.);
  mPlateEIs = false;
}

void StRHICfPID::setPlateEnergy(int tower, int layer, float val){
  mPlateE[tower][layer] = val;
  mPlateEIs = checkPlateEnergy(mPlateE[tower][layer]);
}

int StRHICfPID::getPID(int tower){return mPID[tower];}

float StRHICfPID::getL20(int tower){return mL20[tower];}

float StRHICfPID::getL90(int tower){return mL90[tower];}

bool StRHICfPID::calculate()
{
  if(!mPlateEIs){return kRHICfFatal;}

  for(int it=0; it<kRHICfNtower; it++){
    for(int ip=0; ip<mPlateIdxNum; ip++){mPlateSumE[it] += layerSumEnergy(it, ip);}
    mPlateSumE[it] += mPlateE[it][0];

    mL20[it] = findRadiationLength(it, mL20Const);
    mL90[it] = findRadiationLength(it, mL90Const);

    if(mL90[it] <= 0.15 * mL20[it] + 20){mPID[it] = kRHICfPhoton;}
    else{mPID[it] = kRHICfHadron;}
  }

  std::cout << "StRHICfPID::calculate() -- Done " << std::endl;
  return kRHICfOk;
}

float StRHICfPID::checkStep(int layer)
{
  if(layer <= 10){return 2.;}
  return 4.;
}

float StRHICfPID::layerSumEnergy(int tower, int layer)
{
  float tmpSumE = (mPlateE[tower][layer] + mPlateE[tower][layer+1]) *checkStep(layer) /2.;
  return tmpSumE;
}

float StRHICfPID::findRadiationLength(int tower, float ratio)
{
  float tmpSumE = mPlateSumE[tower]*ratio;
  float length = 2.;
  tmpSumE -= mPlateE[tower][0];

  for(int ip=0; ip<mPlateIdxNum; ip++){
    float tmpLength = calculateEquation(tower, ip, tmpSumE);

    if(tmpLength >= 0.){
      length += tmpLength;
      return length;
    }
    else{
      length += checkStep(ip);
      tmpSumE -= layerSumEnergy(tower, ip);
    }
  }

  return 0.1;
}

float StRHICfPID::calculateEquation(int tower, int layer, float sumE)
{
  // Define the equation : constA*x^2 + constB*x + constC = 0
  float constA = (mPlateE[tower][layer+1] - mPlateE[tower][layer])/checkStep(layer);
  float constB = 2. * mPlateE[tower][layer];
  float constC = -2. * sumE;

  // Case: No solution to the equation.
  if(constB*constB - 4.*constA*constC < 0.){return -1;}

  // Case: 2 solution.
  float solA = (-1. *constB - TMath::Sqrt(constB *constB -4. *constA *constC))/(2. *constA);
  float solB = (-1. *constB + TMath::Sqrt(constB *constB -4. *constA *constC))/(2. *constA);

  if(solA >= 0. && solA <= checkStep(layer)){return solA;}
  if(solB >= 0. && solB <= checkStep(layer)){return solB;}

  return -1.;
}