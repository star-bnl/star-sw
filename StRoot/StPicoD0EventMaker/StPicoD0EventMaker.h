#ifndef StPicoD0EventMaker_h
#define StPicoD0EventMaker_h

/* **************************************************
 *  A Maker that reads StPicoEvents' and creates 
 *  StPicoD0Events and stores them.
 *
 *  Authors:  Xin Dong        (xdong@lbl.gov)
 *            **Mustafa Mustafa (mmustafa@lbl.gov)
 *
 *  **Code Maintainer
 *
 * **************************************************
 */

#include <bitset>
#include <climits>
#include <vector>

#include "StMaker.h"
#include "StThreeVectorF.hh"
#include "../StPicoKFVertexFitter/StPicoKFVertexFitter.h"
#include "StPicoKfVertexEvent.h"

class TTree;
class TFile;
class StPicoDstMaker;
class StPicoEvent;
class StPicoTrack;
class StPicoD0Event;
class StKaonPion;
class StPicoD0Hists;

class StPicoD0EventMaker : public StMaker 
{
  public:
    StPicoD0EventMaker(char const* makerName, StPicoDstMaker* picoMaker, char const* fileBaseName);
    virtual ~StPicoD0EventMaker();
    
    virtual Int_t Init();
    virtual Int_t Make();
    virtual void  Clear(Option_t *opt="");
    virtual Int_t Finish();
    
  private:
    bool  isGoodEvent() const;
    bool  isGoodTrigger() const;
    bool  isGoodForVertexFit(StPicoTrack const*,StThreeVectorF const& vtx) const;
    bool  isGoodTrack(StPicoTrack const*) const;
    bool  isPion(StPicoTrack const*) const;
    bool  isKaon(StPicoTrack const*) const;
    bool  isGoodPair(StKaonPion const &) const;
    bool  isGoodMass(StKaonPion const &) const;
    int   getD0PtIndex(StKaonPion const& kp) const;
    bool  isGoodQaPair(StKaonPion const&, StPicoTrack const&,StPicoTrack const&);
    size_t popcount(size_t) const;

    StPicoDstMaker* mPicoDstMaker;
    StPicoEvent*    mPicoEvent;
    StPicoD0Hists*  mPicoD0Hists;
    StPicoKFVertexFitter mKfVertexFitter;
    StPicoKfVertexEvent mKfVertexEvent;

    TFile* mOutputFile;
    TTree* mTree;
    StPicoD0Event* mPicoD0Event;

    ClassDef(StPicoD0EventMaker, 0)
};
inline size_t StPicoD0EventMaker::popcount(size_t n) const
{
    std::bitset<sizeof(size_t) * CHAR_BIT> b(n);
    return b.count();
}
#endif
