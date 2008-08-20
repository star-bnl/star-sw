#include <iostream>
#include "StLuminosityHolder.h"

using std::cout;
using std::endl;

ClassImp(StLuminosityHolder)

StLuminosityHolder::StLuminosityHolder(int run) : mRunNumber(run),mXsec(0),mVertexCut(0)
{
  ClearVectors();
}
//_____________________________________________________________________________
void StLuminosityHolder::ClearVectors()
{
  mTriggers.clear();
  mLumTotal.clear();
  mLumCuts.clear();
  mLumVertex.clear();
  mLumSoftTrig.clear();
  mPrescales.clear();
  mNTotal.clear();
  mNCuts.clear();
  mNVertex.clear();
  mNSoftTrig.clear();
}
