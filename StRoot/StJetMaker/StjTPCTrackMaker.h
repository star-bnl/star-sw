// -*- mode: c++;-*-
// $Id: StjTPCTrackMaker.h,v 1.1 2008/08/02 04:05:30 tai Exp $
#ifndef STJETTPCTRACKMAKER_HH
#define STJETTPCTRACKMAKER_HH

#include "StMaker.h"
#include <Rtypes.h>

class StJetTrackListWriter;

class TDirectory;
class TTree;

class StMuDstMaker;

namespace StSpinJet {
  class StJetTPC;
}

class StJetTPCTrackMaker : public StMaker {

public:

  StJetTPCTrackMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker);
  virtual ~StJetTPCTrackMaker() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjTPCTrackMaker.h,v 1.1 2008/08/02 04:05:30 tai Exp $ built "__DATE__" "__TIME__; return cvs;}

private:

  TDirectory* _file;

  StMuDstMaker* _uDstMaker;

  StSpinJet::StJetTPC*  _tpc;

  StJetTrackListWriter* _writer;

  ClassDef(StJetTPCTrackMaker, 0)

};

#endif // STJETTPCTRACKMAKER_HH
