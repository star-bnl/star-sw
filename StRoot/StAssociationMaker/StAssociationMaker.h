/**********************************************
 *
 * StAssociationMaker.h
 *
 **********************************************/

#ifndef StAssociationMaker_HH
#define StAssociationMaker_HH

#ifndef StMaker_H
#include "StMaker.h"
#endif

class StTpcHit;
class StGlobalTrack;

class StMcTpcHit;
class StMcTrack;
class StTrackPairInfo;

// Need to define a struct that relates a track with it's # of associated hits.
    
struct trackPing {
    StMcTrack* mcTrack;
    unsigned int nPings;
};

// Define the comparison to be used in the multimaps
struct compHit{
    bool operator()(const StTpcHit*,const StTpcHit*);
};


struct compTrack {
    bool operator()(const StGlobalTrack*, const StGlobalTrack*);
};


#ifndef __CINT__
#include <map>
#include <utility>
typedef  multimap<StTpcHit*, StMcTpcHit*, compHit> tpcHitMapType;//!
typedef  tpcHitMapType::iterator tpcHitMapIter;//!


typedef  multimap<StGlobalTrack*, StTrackPairInfo*, compTrack> trackMapType;//!
typedef  trackMapType::iterator trackMapIter;//!
typedef  trackMapType::const_iterator trackMapConstIter;//!

#else
class tpcHitMapType; //!
class tpcHitMapIter; //!

class trackMapType; //!
class trackMapIter; //!
class trackMapConstIter; //!

#endif


// Need to define the maps & Iterators, typedef them so we don't write the whole thing out every time



class StAssociationMaker : public StMaker {

 public:

    StMaker* currentChain;
    StAssociationMaker(const char* name = "Associations",
		       const char* title = "event/Associations");
    virtual ~StAssociationMaker();
    virtual void  Clear(const char* opt="");
    virtual Int_t Init();
    virtual Int_t Make();
    virtual Int_t Finish();

    TH1F*     mNumberOfPings;    //! Number of Hits Associated.
    
private:

    // Have to tell Root not to parse the Multimap stuff, or else it pukes.
    tpcHitMapType* tpcHitMap() { return mTpcHitMap; } //!
    trackMapType* trackMap() { return mTrackMap;} //!

//     multimap<StTpcHit*, StMcTpcHit*, compHit>*  tpcHitMap() { return mTpcHitMap; } //!
//     multimap<StGlobalTrack*, StTrackPairInfo*, compTrack>* trackMap() { return mTrackMap;} //!

    // Define the maps.  Note they are pointers to the maps.
    
    tpcHitMapType* mTpcHitMap; //!
    trackMapType* mTrackMap;  //!

//     multimap<StTpcHit*, StMcTpcHit*, compHit>* mTpcHitMap; //!
//     multimap<StGlobalTrack*, StTrackPairInfo*, compTrack>* mTrackMap; //!
    
    Bool_t drawinit;


    // the following is a ROOT macro  that is needed in all ROOT accessible code
    ClassDef(StAssociationMaker, 1)

};
#ifndef __CINT__
ostream& operator<<(ostream& out,
		    const pair<StGlobalTrack* const ,StTrackPairInfo*>& );
ostream& operator<<(ostream& out, const trackMapType& );
#endif
#endif

