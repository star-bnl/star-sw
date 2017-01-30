// \class StFpsRawHitMaker
// \author Akio Ogawa
//
//   This is the hit maker for the FPS data. 
//   It makes use of its base class functions to read daq files into the StEvent Data structure.
// 
//  $Id: StFpsRawHitMaker.h,v 1.3 2017/01/30 17:49:28 akio Exp $
//  $Log: StFpsRawHitMaker.h,v $
//  Revision 1.3  2017/01/30 17:49:28  akio
//  adding FPost
//
//  Revision 1.2  2015/09/02 14:55:18  akio
//  Modified to work with StFmsDbMaker
//

#ifndef STAR_StFpsRawHitMaker_HH
#define STAR_StFpsRawHitMaker_HH

#include "StRoot/StChain/StRTSBaseMaker.h"
class StFmsDbMaker;
class StFmsCollection;

class StFpsRawHitMaker : public StRTSBaseMaker{
public: 
  StFpsRawHitMaker(const Char_t* name="FpsHit");
  ~StFpsRawHitMaker();
  Int_t InitRun(Int_t runNumber);
  Int_t Make();
  void setPrePost(int v) {mPrePost=v;} //if 0 (default) takes only triggered crossing. none-0 accept prepost as well,
  
private:
  enum {kFpsQtCrate=8, kFpostQtCrate=9};

  int mPrePost;
  Int_t prepareEnvironment();
  StFmsCollection *mFmsCollection; //!
  StFmsDbMaker *mFmsDbMaker; //!

  ClassDef(StFpsRawHitMaker,0);
};

#endif
