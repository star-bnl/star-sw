#include "StFwdDataEvent.h"

ClassImp(StFwdDataEvent);

StFwdDataEvent::StFwdDataEvent()
{}

StFwdDataEvent::~StFwdDataEvent()
{}

Short_t StFwdDataEvent::BlueSpin(Int_t spinbit)
{
  if( spinbit==10 || spinbit==9 ){ return 1; }
  else if( spinbit==6 || spinbit==5 ){ return -1; }
  else{ return 0; }
}

Short_t StFwdDataEvent::YellowSpin(Int_t spinbit)
{
  if( spinbit==10 || spinbit==6 ){ return 1; }
  else if( spinbit==9 || spinbit==5 ){ return -1; }
  else{ return 0; }
}

void StFwdDataEvent::Clear(Option_t* option)
{
  mRunTime = -1;
  mRunNum = -1;
  mFill = 0;
  mEvent = -1;
  mBx48Id = -1;
  mBx7Id = -1;
  mSpin = 0;

  mTofMultiplicity = -1;

  mPrimVertRanking = -1;
  mPrimVx = -999;
  mPrimVy = -999;
  mPrimVz = -999;
  mVpdVz = -999;
  mBbcVz = -999;
  mBbcTacDiff = 0;
  mEpdTacEarlyW = 0;
  mEpdTacEarlyE = 0;
  mEpdAvgW = 0;
  mEpdAvgE = 0;
  mEpdVz = -999;
  mZdcVz = -999;
  mFoundVertex = 0;

  mClusterSize = 0;
}

void StFwdDataEvent::Print(Option_t* option) const
{
  std::cout <<"## StFwdDataEvent|ClusterSize:"<<mClusterSize << std::endl;
  std::cout << " + |RunTime:"<<mRunTime << "|RunNum:"<<mRunNum << "|mFill:"<<mFill << "|mEvent:"<<mEvent << "|Bx48Id:"<<mBx48Id << "|Bx7Id:"<<mBx7Id << "|mSpin:"<<mSpin << "|mTofMult:"<<mTofMultiplicity << std::endl;

  std::cout << " + |VertexBit:"<<mFoundVertex << "|VpdVz:"<<mVpdVz << "|ZdcVz:"<<mZdcVz << "|BbcTDiff:"<<mBbcTacDiff << "|BbcVz:"<<mBbcVz << std::endl;
  std::cout << " + |EpdTacEarlyW:"<<mEpdTacEarlyW << "|EpdTacEarlyE:"<<mEpdTacEarlyE << "|EpdTacAvgW:"<<mEpdAvgW << "|EpdTacAvgE:"<<mEpdAvgE << "|EpdVz:"<<mEpdVz << std::endl;
}


