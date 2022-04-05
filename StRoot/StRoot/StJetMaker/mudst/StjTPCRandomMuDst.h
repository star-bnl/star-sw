// -*- mode: c++;-*-
// $Id: StjTPCRandomMuDst.h,v 1.1 2010/05/24 17:42:39 pibero Exp $
#ifndef STJTPCRANDOMMUDST_H
#define STJTPCRANDOMMUDST_H

#include "StjTPCMuDst.h"
#include "StSpinPool/StRandomSelector/StRandomSelector.h"

#include <vector>

class StMuDstMaker;
class StMuTrack;

class StjTPCRandomMuDst : public StjTPCMuDst {

public:
  StjTPCRandomMuDst(StMuDstMaker* uDstMaker, Double_t randomSelectorProb, bool randomSelectorAt, UInt_t randomSelectorSeed);
  virtual ~StjTPCRandomMuDst() { }

  StjTrackList getTrackList();

protected:

  StRandomSelector _randomSelector;

};

#endif // STJTPCRANDOMMUDST_H
