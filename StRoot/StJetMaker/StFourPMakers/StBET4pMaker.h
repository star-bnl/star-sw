//StBET4pMaker.h
//M.L. Miller (MTI Software)
//9/04

//BET: Barrel-Endcap-Tpc

#ifndef StBET4pMaker_HH
#define StBET4pmaker_HH

#include <map>
#include <vector>
using namespace std;

class StMuTrack;
class EmcHit;

#include "StJetMaker/StFourPMakers/Functors.h"
#include "StJetMaker/StFourPMakers/St4pMaker.h"

class StBET4pMaker : public St4pMaker
{
public:
    StBET4pMaker(const char* name, StMuDstMaker*, StEmcTrackMatcher*);
    virtual ~StBET4pMaker();

    virtual Int_t Make();
        
protected:

    typedef vector<StMuTrack*> TrackVec;
    typedef map<EmcHit*, EmcHit*, EmcHitLT> EmcHitMap;
    TrackVec mTracks;
    EmcHitMap mHits;
    
    ClassDef(StBET4pMaker,1)
};

#endif
