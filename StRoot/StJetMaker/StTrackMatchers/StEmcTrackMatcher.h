//StEmcTrackMatcher.h
//M.L. Miller (MIT Software)
//9/04

#ifndef StEmcTrackMatcher_HH
#define StEmcTrackMatcher_HH

#include "StMaker.h"
#include "StJetMaker/StEmcHitMakers/StBarrelHitMaker.h"
#include "StJetMaker/StEmcHitMakers/StEndcapHitMaker.h"
#include "StJetMaker/StTrackMatchers/Projection.h"

#include <vector>
#include <string>
#include <map>
using namespace std;

class StMuDstMaker;
class StMuTrack;
class TFile;
class TNtuple;

class StEmcTrackMatcher : public StMaker
{
public:
    StEmcTrackMatcher(const char* name, StMuDstMaker*, StBarrelHitMaker*, StEndcapHitMaker*);
    virtual ~StEmcTrackMatcher() {};
    
    virtual Int_t Init();
    virtual void Clear(Option_t*);
    virtual Int_t Make();
    virtual Int_t Finish();
    
    ///Container of emc hits, ordered by eta, then phi.  The pointers in this container are unique
    typedef vector<EmcHit*> EmcHitVec;
    EmcHitVec& hits() {return mHits;}
    
    ///Container of track-tower match candidates, keyed by track
    typedef multimap<StMuTrack*, Projection> TrackToTowerMap;
    TrackToTowerMap& trackToTowerMap() {return mTrackToTowerMap;}
    
    ///Container of track-tower match candidates, keyed by tower
    typedef multimap<EmcHit*, Projection> TowerToTrackMap;
    TowerToTrackMap& towerToTrackMap() {return mTowerToTrackMap;}
    
protected:
    //for a given projection, fill the maps
    void fillMap(const StThreeVectorD&, StMuTrack*, EmcHit*);

    typedef StBarrelHitMaker::BemcHitVec BemcHitVec;
    typedef StEndcapHitMaker::EemcHitVec EemcHitVec;
    
    StMuDstMaker* mMuDstMaker; //!
    StBarrelHitMaker* mBarrel; //!
    StEndcapHitMaker* mEndcap; //!

    ///Container of track <--> tower relationships
    TrackToTowerMap mTrackToTowerMap; //!
    TowerToTrackMap mTowerToTrackMap; //!
    EmcHitVec mHits; //!
    
    double mDeltaEta; //cut to search by...
    bool mPrint;
    

protected:
    //temporary dev. methods
    void match();
    void matchToBarrel(StMuTrack* track);
    void matchToEndcap(StMuTrack* track);

    ClassDef(StEmcTrackMatcher,1)
	};

//non-members-----------------------------
struct EmcHitSorter
{
    bool operator()(const EmcHit* lhs, const EmcHit* rhs) {
	const StThreeVectorD& lp = lhs->correctedPosition();
	const StThreeVectorD& rp = rhs->correctedPosition();
	if (lp.pseudoRapidity() < rp.pseudoRapidity()) {
	    return true;
	}
	else if (lp.pseudoRapidity() > rp.pseudoRapidity()) {
	    return false;
	}
	else {
	    return lp.phi() < rp.phi();
	}
    }
};
#endif
