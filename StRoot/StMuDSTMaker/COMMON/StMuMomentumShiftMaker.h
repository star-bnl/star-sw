/***************************************************************************
 *
 * $Id: StMuMomentumShiftMaker.h,v 1.1 2005/03/17 21:55:00 mvl Exp $
 * Author: Marco van Leeuwen, LBNL
 ***************************************************************************/
#ifndef __StMuMomentumShiftmaker_hh__
#define __StMuMomentumShiftmaker_hh__

//#include "StMuDstMaker.h"
#include "StMaker.h"
#include "TString.h"

class TFile;
class TTree;
class StMuTrack;
class StKinkMuDst;
class StV0MuDst;
class StXiMuDst;

class StMuMomentumShiftMaker: public StMaker {
  
public:
  StMuMomentumShiftMaker();
  StMuMomentumShiftMaker(const char *outDir);
  ~StMuMomentumShiftMaker() {;}

  void setScaleFactor(Float_t scale) { mMomentumScale=scale; }
  void setWriteMuDst(Bool_t writeFlag) { mWriteMuDst=writeFlag; } 
  void ScaleMomentum(StMuTrack *track);
  void ScaleMomentum(StKinkMuDst *kink);
  void ScaleMomentum(StV0MuDst *kink);
  void ScaleMomentum(StXiMuDst *kink);
  int Make();
  int Finish();
 private:
  Float_t mMomentumScale;
  Bool_t  mWriteMuDst;
  TString mOutDir;
  TFile  *mOutFile;
  TTree  *mOutTree;

  ClassDef(StMuMomentumShiftMaker,0)
};

#endif
