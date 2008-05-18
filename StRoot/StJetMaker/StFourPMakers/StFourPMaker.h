// -*- mode: c++;-*-
// $Id: StFourPMaker.h,v 1.8 2008/05/18 23:23:06 tai Exp $
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

    void SetEtaLow(Float_t eta_low) { eta_low_lim = eta_low; };
    void SetEtaHigh(Float_t eta_high) { eta_high_lim = eta_high; };
    Float_t GetEtaLow(void) const { return eta_low_lim; };
    Float_t GetEtaHigh(void) const { return eta_high_lim; };

    ///Access to BEMC event corruption flag
  virtual bool bemcCorrupt() const { return false; }

protected:

  FourList tracks;
  //  bool mCorrupt; 


public:

    Float_t          eta_high_lim;
    Float_t          eta_low_lim;

    ClassDef(StFourPMaker,0)
};
#endif



