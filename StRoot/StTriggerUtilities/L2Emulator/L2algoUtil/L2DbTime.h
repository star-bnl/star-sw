#ifndef __L2DbTime_h__
#define __L2DbTime_h__
/*
 * \class StL2DbTime
 *
 */


#include "TString.h"
#include <iostream>
#include <fstream>
#include <vector>
#include <algorithm>

class L2DbTime 
{
 public:
  L2DbTime();
  ~L2DbTime(){ /* nop */};
  Bool_t valid( Int_t d, Int_t t );
  Bool_t eof();
  Bool_t comment();
  void print();

  std::ifstream &read( std::ifstream &in );

  double getFullStartTime() { return  mDateStart+mTimeStart/1.e6; }
  double getFullFinishTime() { return  mDateFinish+mTimeFinish/1.e6; }
  TString getPedFile(){ return mPedFile; }
  TString getMaskFile(){ return mMaskFile; }
  TString getBuf1(){ return mPedFile; } // tmp, will clean it up later, JanB
  TString getBuf2(){ return mMaskFile; } // tmp, will clean it up later, JanB
  TString getTag(){ return mTag; } 

  // overload << operator for read in
 private:
 protected:
  TString mTag;
  Int_t mDateStart;
  Int_t mDateFinish;
  Int_t mTimeStart;
  Int_t mTimeFinish;
  TString mPedFile;
  TString mMaskFile;

};
#endif
