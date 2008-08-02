// -*- mode: c++;-*-
// $Id: StjTPCTrackMaker.h,v 1.2 2008/08/02 19:22:27 tai Exp $
#ifndef STJETTPCTRACKMAKER_HH
#define STJETTPCTRACKMAKER_HH

#include "StMaker.h"
#include <Rtypes.h>

class StjTrackListWriter;

class TDirectory;
class TTree;

class StMuDstMaker;

namespace StSpinJet {
  class StjTPC;
}

class StjTPCTrackMaker : public StMaker {

public:

  StjTPCTrackMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker);
  virtual ~StjTPCTrackMaker() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjTPCTrackMaker.h,v 1.2 2008/08/02 19:22:27 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  TDirectory* _file;

  StMuDstMaker* _uDstMaker;

  StSpinJet::StjTPC*  _tpc;

  StjTrackListWriter* _writer;

  ClassDef(StjTPCTrackMaker, 0)

};

#endif // STJETTPCTRACKMAKER_HH
