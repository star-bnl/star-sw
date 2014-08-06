// -*- mode: c++;-*-
// $Id: StjTPCTrackMaker.h,v 1.5 2014/08/06 11:43:22 jeromel Exp $
#ifndef STJTPCTRACKMAKER_H
#define STJTPCTRACKMAKER_H

#include "StMaker.h"
#include <Rtypes.h>

class StjTrackListWriter;

class TDirectory;
class TTree;

class StMuDstMaker;

class StjTPC;

class StjTPCTrackMaker : public StMaker {

public:

  StjTPCTrackMaker(const Char_t *name, TDirectory* file, StMuDstMaker* uDstMaker);
  virtual ~StjTPCTrackMaker() { }

  Int_t Init();
  Int_t Make();
  Int_t Finish();
    
  const char* GetCVS() const
  {static const char cvs[]="Tag $Name:  $ $Id: StjTPCTrackMaker.h,v 1.5 2014/08/06 11:43:22 jeromel Exp $ built " __DATE__ " " __TIME__; return cvs;}

private:

  TDirectory* _file;

  StMuDstMaker* _uDstMaker;

  StjTPC*  _tpc;

  StjTrackListWriter* _writer;

  ClassDef(StjTPCTrackMaker, 0)

};

#endif // STJTPCTRACKMAKER_H
