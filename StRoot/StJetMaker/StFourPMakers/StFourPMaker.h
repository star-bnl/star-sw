// -*- mode: c++;-*-
// $Id: StFourPMaker.h,v 1.11 2008/07/14 19:59:54 tai Exp $
#ifndef StFourPMaker_h
#define StFourPMaker_h

#include <StMaker.h>

#include "../StMuTrackFourVec.h"
#include "StJetFinder/AbstractFourVec.h"


typedef std::vector<AbstractFourVec*> FourList;

class StFourPMaker : public StMaker {
public:

  StFourPMaker(const char *name)
    : StMaker(name) { }

  virtual void Clear(const Option_t*);

  virtual FourList &getTracks() { return tracks; };
  Int_t numTracks(void) { return tracks.size(); };

  ///Access to BEMC event corruption flag
  virtual bool bemcCorrupt() const { return false; }

protected:

  FourList tracks;

public:

    ClassDef(StFourPMaker,0)
};
#endif



