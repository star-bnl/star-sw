// -*- mode: c++;-*-
// $Id: StFourPMaker.h,v 1.9 2008/06/02 01:28:34 tai Exp $
#ifndef StFourPMaker_h
#define StFourPMaker_h
#include <map>

#include <StMaker.h>

#include "../StMuTrackFourVec.h"
#include "StJetFinder/AbstractFourVec.h"

class StEvent;
class StEmcClusterCollection;    

class StEmcPoint;
class StMuDst;
class StMuEmcCollection;
class StMuDstMaker;

typedef std::vector<AbstractFourVec*> FourList;

class StFourPMaker : public StMaker {
public:

  StFourPMaker(const char *name, StMuDstMaker *pevent);
  virtual Int_t Init();
  virtual Int_t Make();
  virtual Int_t Finish();
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



