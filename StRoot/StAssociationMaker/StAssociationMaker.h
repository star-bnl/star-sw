/**********************************************
 *
 * $Id: StAssociationMaker.h,v 1.9 1999/12/08 00:00:24 calderon Exp $
 * $Log: StAssociationMaker.h,v $
 * Revision 1.9  1999/12/08 00:00:24  calderon
 * New version of StAssociationMaker.
 * -Uses new StEvent / StMcEvent
 * -Includes maps using reconstructed and monte carlo objects as keys for:
 *   TPC Hits
 *   SVT Hits
 *   FTPC Hits
 *   Tracks (using all 3 hit multimaps)
 *
 * Revision 1.8  1999/10/01 14:08:55  calderon
 * Added Local Hit resolution Histogram. It is made by default
 * without any requirement of association, to serve
 * as a diagnostic.
 * Before building track multimap, check the size of the
 * tpc hit map.  If it is too small, print out a warning
 * and exit.
 *
 * Revision 1.7  1999/09/09 23:51:22  calderon
 * Made the following changes:
 * StAssociationMaker
 *
 * -correct definition of multimap for Solaris/ObjectSpace
 * -clear candidate vector at the end of reconstructed track loop
 * -remove # of pings histogram
 *
 * StLocalHit
 *
 * -use math.h instead of cmath because of abs()
 * -change abs() to fabs() everywhere
 * -change bool's to int's so Solaris doesn't complain
 *
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
class StSvtHit;
class StFtpcHit;
class StGlobalTrack;

class StMcTpcHit;
class StMcSvtHit;
class StMcFtpcHit;
class StMcTrack;

class StTrackPairInfo;

class TH2F;
    
struct trackPing {
    StMcTrack* mcTrack;
    unsigned int nPingsTpc;
    unsigned int nPingsSvt;
    unsigned int nPingsFtpc;
};


#if !defined(ST_NO_NAMESPACES)
using namespace std;
#endif


#ifndef __CINT__
#include <map>
#include <utility>

// Define the comparisons to be used in the multimaps
struct compTpcHit{
    bool operator()(const StTpcHit*,const StTpcHit*) const;
};

struct compMcTpcHit{
    bool operator()(const StMcTpcHit*,const StMcTpcHit*) const;
};

struct compSvtHit{
    bool operator()(const StSvtHit*,const StSvtHit*) const;
};

struct compMcSvtHit{
    bool operator()(const StMcSvtHit*,const StMcSvtHit*) const;
};

struct compFtpcHit{
    bool operator()(const StFtpcHit*,const StFtpcHit*) const;
};

struct compMcFtpcHit{
    bool operator()(const StMcFtpcHit*,const StMcFtpcHit*) const;
};


struct compTrack {
    bool operator()(const StGlobalTrack*, const StGlobalTrack*) const;
};

struct compMcTrack {
    bool operator()(const StMcTrack*, const StMcTrack*) const;
};

// Need to define the maps & Iterators,
// typedef them so we don't write the whole thing out every time

#ifndef ST_NO_TEMPLATE_DEF_ARGS
//
// TPC
//
typedef  multimap<const StTpcHit*, const StMcTpcHit*, compTpcHit>   rcTpcHitMapType;//!
typedef  multimap<const StMcTpcHit*, const StTpcHit*, compMcTpcHit> mcTpcHitMapType;//!
//
// SVT
//
typedef  multimap<const StSvtHit*, const StMcSvtHit*, compSvtHit>   rcSvtHitMapType;//!
typedef  multimap<const StMcSvtHit*, const StSvtHit*, compMcSvtHit> mcSvtHitMapType;//!
//
// FTPC
//
typedef  multimap<const StFtpcHit*, const StMcFtpcHit*, compFtpcHit>   rcFtpcHitMapType;//!
typedef  multimap<const StMcFtpcHit*, const StFtpcHit*, compMcFtpcHit> mcFtpcHitMapType;//!
//
// Tracks
//
typedef  multimap<StGlobalTrack*, StTrackPairInfo*, compTrack>   rcTrackMapType;//!
typedef  multimap<StMcTrack*,     StTrackPairInfo*, compMcTrack> mcTrackMapType;//!

#else
// This type of definition is really criptic, but this is what ObjectSpace wants...

//
// TPC
//
typedef  const StTpcHit*    rcTpcHitMapKey;
typedef  const StTpcHit*    mcTpcHitMapValue;
typedef  const StMcTpcHit*  rcTpcHitMapValue;
typedef  const StMcTpcHit*  mcTpcHitMapKey;

typedef  multimap<rcTpcHitMapKey, rcTpcHitMapValue, compTpcHit,
    allocator< OS_PAIR(rcTpcHitMapKey, rcTpcHitMapValue) > > rcTpcHitMapType;//!

typedef  multimap<mcTpcHitMapKey, mcTpcHitMapValue, compMcTpcHit,
    allocator< OS_PAIR(mcTpcHitMapKey, mcTpcHitMapValue) > > mcTpcHitMapType;//!
//
// SVT
//
typedef  const StSvtHit*    rcSvtHitMapKey;
typedef  const StSvtHit*    mcSvtHitMapValue;
typedef  const StMcSvtHit*  rcSvtHitMapValue;
typedef  const StMcSvtHit*  mcSvtHitMapKey;

typedef  multimap<rcSvtHitMapKey, rcSvtHitMapValue, compSvtHit,
    allocator< OS_PAIR(rcSvtHitMapKey, rcSvtHitMapValue) > > rcSvtHitMapType;//!

typedef  multimap<mcSvtHitMapKey, mcSvtHitMapValue, compMcSvtHit,
    allocator< OS_PAIR(mcSvtHitMapKey, mcSvtHitMapValue) > > mcSvtHitMapType;//!
//
// FTPC
//
typedef  const StFtpcHit*    rcFtpcHitMapKey;
typedef  const StFtpcHit*    mcFtpcHitMapValue;
typedef  const StMcFtpcHit*  rcFtpcHitMapValue;
typedef  const StMcFtpcHit*  mcFtpcHitMapKey;

typedef  multimap<rcFtpcHitMapKey, rcFtpcHitMapValue, compFtpcHit,
    allocator< OS_PAIR(rcFtpcHitMapKey, rcFtpcHitMapValue) > > rcFtpcHitMapType;//!

typedef  multimap<mcFtpcHitMapKey, mcFtpcHitMapValue, compMcFtpcHit,
    allocator< OS_PAIR(mcFtpcHitMapKey, mcFtpcHitMapValue) > > mcFtpcHitMapType;//!

//
// Tracks
//
typedef  StGlobalTrack*    rcTrackMapKey;
typedef  StMcTrack*        mcTrackMapKey;
typedef  StTrackPairInfo*  trackMapValue;

typedef  multimap<rcTrackMapKey, trackMapValue, compTrack,
    allocator< OS_PAIR(rcTrackMapKey, trackMapValue) > > rcTrackMapType;//!
typedef  multimap<mcTrackMapKey, trackMapValue, compMcTrack,
    allocator< OS_PAIR(mcTrackMapKey, trackMapValue) > > mcTrackMapType;//!


#endif
typedef  rcTpcHitMapType::iterator          rcTpcHitMapIter;     //!
typedef  rcTpcHitMapType::value_type        rcTpcHitMapValType;  //!
typedef  rcSvtHitMapType::iterator          rcSvtHitMapIter;     //!
typedef  rcSvtHitMapType::value_type        rcSvtHitMapValType;  //!
typedef  rcFtpcHitMapType::iterator         rcFtpcHitMapIter;    //!
typedef  rcFtpcHitMapType::value_type       rcFtpcHitMapValType; //!
typedef  rcTrackMapType::iterator           rcTrackMapIter;      //!
typedef  rcTrackMapType::const_iterator     rcTrackMapConstIter; //!
typedef  rcTrackMapType::value_type         rcTrackMapValType;   //!

typedef  mcTpcHitMapType::iterator          mcTpcHitMapIter;     //!
typedef  mcTpcHitMapType::value_type        mcTpcHitMapValType;  //!
typedef  mcSvtHitMapType::iterator          mcSvtHitMapIter;     //!
typedef  mcSvtHitMapType::value_type        mcSvtHitMapValType;  //!
typedef  mcFtpcHitMapType::iterator         mcFtpcHitMapIter;    //!
typedef  mcFtpcHitMapType::value_type       mcFtpcHitMapValType; //!
typedef  mcTrackMapType::iterator           mcTrackMapIter;      //!
typedef  mcTrackMapType::const_iterator     mcTrackMapConstIter; //!
typedef  mcTrackMapType::value_type         mcTrackMapValType;   //!
#else
class rcTpcHitMapType;     //!
class rcTpcHitMapIter;     //!
class rcTpcHitMapValType;  //!
class rcSvtHitMapType;     //!
class rcSvtHitMapIter;     //!
class rcSvtHitMapValType;  //!
class rcFtpcHitMapType;    //!
class rcFtpcHitMapIter;    //!
class rcFtpcHitMapValType; //!
class rcTrackMapType;      //!
class rcTrackMapValType;   //!
class rcTrackMapIter;      //!
class rcTrackMapConstIter; //!

class mcTpcHitMapType;     //!
class mcTpcHitMapIter;     //!
class mcTpcHitMapValType;  //!
class mcSvtHitMapType;     //!
class mcSvtHitMapIter;     //!
class mcSvtHitMapValType;  //!
class mcFtpcHitMapType;    //!
class mcFtpcHitMapIter;    //!
class mcFtpcHitMapValType; //!
class mcTrackMapType;      //!
class mcTrackMapValType;   //!
class mcTrackMapIter;      //!
class mcTrackMapConstIter; //!

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

    TH2F*     mTpcLocalHitResolution;    //! Diff btw local  x and z coords of TPC hits.
    TH2F*     mSvtHitResolution;         //! Diff btw global x and z coords of SVT hits.
    TH2F*     mFtpcHitResolution;        //! Diff btw global r and phi coords of FTPC hits.
    
    // Have to tell CINT not to parse the Multimap stuff, or else it pukes.

    rcTpcHitMapType*  rcTpcHitMap()  { return mRcTpcHitMap; }  //!
    mcTpcHitMapType*  mcTpcHitMap()  { return mMcTpcHitMap; }  //!
    rcSvtHitMapType*  rcSvtHitMap()  { return mRcSvtHitMap; }  //!
    mcSvtHitMapType*  mcSvtHitMap()  { return mMcSvtHitMap; }  //!
    rcFtpcHitMapType* rcFtpcHitMap() { return mRcFtpcHitMap; } //!
    mcFtpcHitMapType* mcFtpcHitMap() { return mMcFtpcHitMap; } //!
    rcTrackMapType*   rcTrackMap()   { return mRcTrackMap; }   //!
    mcTrackMapType*   mcTrackMap()   { return mMcTrackMap; }   //!

private:

    // Define the maps.  Note they are pointers to the maps.
    
    rcTpcHitMapType*  mRcTpcHitMap;  //!
    mcTpcHitMapType*  mMcTpcHitMap;  //!
    rcSvtHitMapType*  mRcSvtHitMap;  //!
    mcSvtHitMapType*  mMcSvtHitMap;  //!
    rcFtpcHitMapType* mRcFtpcHitMap; //!
    mcFtpcHitMapType* mMcFtpcHitMap; //!
    rcTrackMapType*   mRcTrackMap;   //!
    mcTrackMapType*   mMcTrackMap;   //!

    Bool_t drawinit;

    virtual const char* GetCVS() const
    {static const char cvs[]="Tag $Name:  $ $Id: StAssociationMaker.h,v 1.9 1999/12/08 00:00:24 calderon Exp $ built "__DATE__" "__TIME__; return cvs;}	
    // the following is a ROOT macro  that is needed in all ROOT accessible code
    ClassDef(StAssociationMaker, 1)

};
#ifndef __CINT__
ostream& operator<<(ostream& out,
		    const pair<StGlobalTrack* const, StTrackPairInfo*>& );
ostream& operator<<(ostream& out,
		    const pair<StMcTrack* const, StTrackPairInfo*>& );

ostream& operator<<(ostream& out, const rcTrackMapType& );
ostream& operator<<(ostream& out, const mcTrackMapType& );
#endif
#endif

