//St4pMaker.h
//M.L. Miller (MIT)
//adapted from Thomas' Henry's work
//9/04

#ifndef St4pMaker_HH
#define St4pMaker_HH

#include "StMaker.h"
#include "StJetFinder/StProtoJet.h"

class StMuDstMaker;
class StEmcTrackMatcher;

class St4pMaker : public StMaker
{
public:
    
    St4pMaker(const char *name, StMuDstMaker*, StEmcTrackMatcher*);
    virtual ~St4pMaker();

    virtual Int_t Make();
    ///The 4p objects in the mTracks container are owned by this class!  They are deleted in each call to clear!
    ///This may change, in the future we will use Sti/base/Factory class
    virtual void Clear(const Option_t*);
    
    ///Access to the container of 4 momenta.
    typedef StProtoJet::FourVecList FourList;
    FourList& fourMomenta();
    
protected:
    StMuDstMaker* mMuDstMaker; //!
    StEmcTrackMatcher* mEmcTrackMatcher;
    FourList mFourList;       //!

    ClassDef(St4pMaker,1)
};

//inlines:

inline St4pMaker::FourList& St4pMaker::fourMomenta()
{
    return mFourList;
}

#endif
