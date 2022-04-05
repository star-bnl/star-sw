/***************************************************************************
 *
 * $Id: StMuMomentumShiftMaker.h,v 1.2 2011/04/08 01:25:51 fisyak Exp $
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
#ifndef __NO_STRANGE_MUDST__
class StKinkMuDst;
class StV0MuDst;
class StXiMuDst;
#endif
class StMuMomentumShiftMaker: public StMaker {
  
public:
  StMuMomentumShiftMaker();
  StMuMomentumShiftMaker(const char *outDir);
  ~StMuMomentumShiftMaker() {;}

  void setScaleFactor(Float_t scale) { mMomentumScale=scale; }
  void setWriteMuDst(Bool_t writeFlag) { mWriteMuDst=writeFlag; } 
  void ScaleMomentum(StMuTrack *track);
#ifndef __NO_STRANGE_MUDST__
  void ScaleMomentum(StKinkMuDst *kink);
  void ScaleMomentum(StV0MuDst *kink);
  void ScaleMomentum(StXiMuDst *kink);
#endif
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
