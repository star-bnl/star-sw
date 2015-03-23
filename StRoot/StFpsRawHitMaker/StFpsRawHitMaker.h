// \class StFpsRawHitMaker
// \author Akio Ogawa
//
//   This is the hit maker for the FPS data. 
//   It makes use of its base class functions to read daq files into the StEvent Data structure.
// 
//  $Id: StFpsRawHitMaker.h,v 1.1 2015/03/23 11:33:20 jeromel Exp $
//  $Log: StFpsRawHitMaker.h,v $
//  Revision 1.1  2015/03/23 11:33:20  jeromel
//  Peer review closed 2015/03/20 - content added
//

#ifndef STAR_StFpsRawHitMaker_HH
#define STAR_StFpsRawHitMaker_HH

#include "StRoot/StChain/StRTSBaseMaker.h"

class StFmsCollection;

class StFpsRawHitMaker : public StRTSBaseMaker{
public: 
  StFpsRawHitMaker(const Char_t* name="FpsHit");
  ~StFpsRawHitMaker();
  Int_t InitRun(Int_t runNumber);
  Int_t Make();
  void setPrePost(int v) {mPrePost=v;} //if 0 (default) takes only triggered crossing. none-0 accept prepost as well,
  
private:
  enum {kFpsDetId=15, kFpsQtCrate=8};

  int mPrePost;
  Int_t prepareEnvironment();
  StFmsCollection *mFmsCollection; //!
  
  ClassDef(StFpsRawHitMaker,0);
};

#endif
