/**********************************************
 *
 * $Id: StAssociationMaker.h,v 1.6 1999/07/30 16:19:14 calderon Exp $
 * $Log: StAssociationMaker.h,v $
 * Revision 1.6  1999/07/30 16:19:14  calderon
 * Use value_type typedef for inserting pairs in multimaps, Victor corrected iterators on HP in SL99h, Improved use of const for HP compilation
 *
 * Revision 1.5  1999/07/28 20:27:26  calderon
 * Version with SL99f libraries
 *
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
    bool operator()(const StTpcHit*,const StTpcHit*) const;
};


struct compTrack {
    bool operator()(const StGlobalTrack*, const StGlobalTrack*) const;
};

#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif


#ifndef __CINT__
#include <map>
#include <utility>
// Need to define the maps & Iterators, typedef them so we don't write the whole thing out every time
#ifndef ST_NO_TEMPLATE_DEF_ARGS
typedef  multimap<const StTpcHit*, const StMcTpcHit*, compHit> tpcHitMapType;//!

typedef  multimap<StGlobalTrack*, StTrackPairInfo*, compTrack> trackMapType;//!
#else
typedef  multimap<const StTpcHit*, const StMcTpcHit*, compHit, allocator< pair<const StTpcHit*, const StMcTpcHit*> > > tpcHitMapType;//!

typedef  multimap<StGlobalTrack*, StTrackPairInfo*, compTrack, allocator< pair<const StGlobalTrack*, StTrackPairInfo*> > > trackMapType;//!
#endif
typedef  tpcHitMapType::iterator tpcHitMapIter;//!
typedef  tpcHitMapType::value_type tpcHitMapValType; //!
typedef  trackMapType::iterator trackMapIter;//!
typedef  trackMapType::const_iterator trackMapConstIter;//!
typedef  trackMapType::value_type trackMapValType; //!
#else
class tpcHitMapType; //!
class tpcHitMapIter; //!
class tpcHitMapValType; //!
class trackMapType; //!
class trackMapValType; //!
class trackMapIter; //!
class trackMapConstIter; //!

#endif




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
    

    // Have to tell Root not to parse the Multimap stuff, or else it pukes.
    tpcHitMapType* tpcHitMap() { return mTpcHitMap; } //!
    trackMapType* trackMap() { return mTrackMap;} //!

//     multimap<StTpcHit*, StMcTpcHit*, compHit>*  tpcHitMap() { return mTpcHitMap; } //!
//     multimap<StGlobalTrack*, StTrackPairInfo*, compTrack>* trackMap() { return mTrackMap;} //!

private:

    // Define the maps.  Note they are pointers to the maps.
    
    tpcHitMapType* mTpcHitMap; //!
    trackMapType* mTrackMap;  //!

//     multimap<StTpcHit*, StMcTpcHit*, compHit>* mTpcHitMap; //!
//     multimap<StGlobalTrack*, StTrackPairInfo*, compTrack>* mTrackMap; //!
    
    Bool_t drawinit;

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StAssociationMaker.h,v 1.6 1999/07/30 16:19:14 calderon Exp $ built "__DATE__" "__TIME__; return cvs;}	
    // the following is a ROOT macro  that is needed in all ROOT accessible code
    ClassDef(StAssociationMaker, 1)

};
#ifndef __CINT__
ostream& operator<<(ostream& out,
		    const pair<StGlobalTrack* const ,StTrackPairInfo*>& );
ostream& operator<<(ostream& out, const trackMapType& );
#endif
#endif

