/*************************************************
 *
 * $Id: StAssociationMaker.cxx,v 1.27 2000/05/11 15:34:29 calderon Exp $
 * $Log: StAssociationMaker.cxx,v $
 * Revision 1.27  2000/05/11 15:34:29  calderon
 * added option to print memory usage using StMemoryInfo, useful
 * for checking leaks.  If used, a lot of status information is printed
 * at several points in Make() and then in Clear().  Whatever is allocated
 * during Make() should be accounted for in Clear().  By default memory is
 * not checked, so there are a lot less output messages.
 *
 * Revision 1.26  2000/04/20 16:56:12  calderon
 * Speed up the tpc matching algorithm by using a seed to tell the iterator
 * where to start looping, instead of looping over every hit all the time.
 * Change the name from "Associations" to "StAssociationMaker"
 *
 * Revision 1.25  2000/04/19 16:30:18  calderon
 * return kStWarn when no StEvent or StMcEvent is found, instead of
 * exit.
 *
 * Revision 1.24  2000/04/12 21:33:14  calderon
 * return warnings instead of fatal when no maps are made
 *
 * Revision 1.23  2000/04/04 23:12:08  calderon
 * Speed up FTPC Hit association, taking advantage of hit sorting
 * and using the STL algorithm find_if.
 *
 * Revision 1.22  2000/03/29 16:14:08  calderon
 * Keep storing Vertices in V0 map that come from event generator, and hence don't
 * have a parent.
 * Check that these vertices are not used for Xi map.
 *
 * Revision 1.21  2000/03/28 02:57:32  calderon
 * Add additional check for V0 vertices: Make sure they also have a parent.
 *
 * Revision 1.20  2000/03/06 18:08:56  calderon
 * Hit comparisons are used for both sorting the hits in the
 * StMcEvent containers and for ordering the hits in the multimaps,
 * so they are kept now in StMcEvent.
 *
 * Revision 1.19  2000/02/22 16:18:45  ullrich
 * Applied temporary fix to cope with changed SVT hit
 * storage scheme in StEvent.
 *
 * Revision 1.18  2000/01/18 20:53:37  calderon
 * Changes to work with CC5
 *
 * Revision 1.17  2000/01/14 21:53:58  calderon
 * Make sure the common hits for each detector are initialized to zero.
 * (Thanks, Lee).
 *
 * Revision 1.16  2000/01/12 00:30:55  calderon
 * Modified Kink Vertex association as per Lee's request.
 *
 * Revision 1.15  1999/12/14 07:07:41  calderon
 * Added Ratio Number of Common Hits / Number of Reconstructed Hits for
 * each detector.
 * Numbering scheme from StEvent & StMcEvent as per SVT request
 * Added Kink, V0 and Xi vertex associations.
 *
 * Revision 1.14  1999/12/08 00:00:24  calderon
 * New version of StAssociationMaker.
 * -Uses new StEvent / StMcEvent
 * -Includes maps using reconstructed and monte carlo objects as keys for:
 *   TPC Hits
 *   SVT Hits
 *   FTPC Hits
 *   Tracks (using all 3 hit multimaps)
 *
 * Revision 1.13  1999/11/03 22:40:50  calderon
 * Fix bug in Clear() : check pointers before deleting maps.
 *
 * Revision 1.12  1999/10/18 16:11:50  calderon
 * Frank found 2 leaks that these changes will correct:
 * -Delete the TrackPairInfos in the Clear() method
 * -Correct the sub detector destructors to delete all
 *  instances to StLocalHit.
 *
 * Revision 1.11  1999/10/14 01:18:44  calderon
 * -Delete StTrackPairInfo objects owned by trackMap in
 *  StAssociationMaker destructor.
 * -Make sure there is a closestHit for filling the
 *  mLocalHitResolution histogram.
 *
 * Revision 1.10  1999/10/01 14:08:54  calderon
 * Added Local Hit resolution Histogram. It is made by default
 * without any requirement of association, to serve
 * as a diagnostic.
 * Before building track multimap, check the size of the
 * tpc hit map.  If it is too small, print out a warning
 * and exit.
 *
 * Revision 1.9  1999/09/28 14:35:00  fisyak
 * Remove cons dependence on non existing headers
 *
 * Revision 1.8  1999/09/23 21:25:18  calderon
 * Added Log & Id
 * Modified includes according to Yuri
 *
 * Revision 1.7  1999/09/15 18:40:56  calderon
 * -If there is no StEvent or StMcEvent, print message to cerr and exit.
 * -Update README for changes
 *
 * Revision 1.6  1999/09/09 23:51:21  calderon
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
 * Revision 1.5  1999/07/30 16:19:13  calderon
 * Use value_type typedef for inserting pairs in multimaps, Victor corrected iterators on HP in SL99h, Improved use of const for HP compilation
 *
 * Revision 1.4  1999/07/28 20:27:23  calderon
 * Version with SL99f libraries
 *
 *
 *************************************************/

#include <iostream.h>
#include <stdlib.h>
#include <string>
#include <vector>
#include <algorithm>
#include <math.h>
#if !defined(ST_NO_NAMESPACES)
using std::string;
using std::vector;
using std::find_if;
#endif

#include "StAssociationMaker.h"
#include "StMcParameterDB.h"
#include "StTrackPairInfo.hh"

#include "StGlobals.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVectorF.hh"


#include "StChain.h"
#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "TH2.h"


#include "StEventTypes.h"

#include "StMcEventTypes.hh"

#include "StEventMaker/StEventMaker.h"
#include "StMcEventMaker/StMcEventMaker.h"

#include "StMemoryInfo.hh"
// // Define the comparison to be used in the multimaps
    
// bool compTpcHit::operator()(const StTpcHit* h1, const StTpcHit* h2) const {
//     if        (h1->position().z() != h2->position().z()) {
// 	return h1->position().z() <  h2->position().z();
//     }
//     else if   (h1->position().y() != h2->position().y()) {
// 	return h1->position().y() <  h2->position().y();
//     }
//     else return h1->position().x() < h2->position().x();
    
// }
// bool compMcTpcHit::operator()(const StMcTpcHit* h1, const StMcTpcHit* h2) const {
//     if        (h1->position().z() != h2->position().z()) {
// 	return h1->position().z() <  h2->position().z();
//     }
//     else if   (h1->position().y() != h2->position().y()) {
// 	return h1->position().y() <  h2->position().y();
//     }
//     else return h1->position().x() < h2->position().x();
    
// }
// bool compSvtHit::operator()(const StSvtHit* h1, const StSvtHit* h2) const {
//     if        (h1->position().z() != h2->position().z()) {
// 	return h1->position().z() <  h2->position().z();
//     }
//     else if   (h1->position().y() != h2->position().y()) {
// 	return h1->position().y() <  h2->position().y();
//     }
//     else return h1->position().x() < h2->position().x();
    
// }
// bool compMcSvtHit::operator()(const StMcSvtHit* h1, const StMcSvtHit* h2) const {
//     if        (h1->position().z() != h2->position().z()) {
// 	return h1->position().z() <  h2->position().z();
//     }
//     else if   (h1->position().y() != h2->position().y()) {
// 	return h1->position().y() <  h2->position().y();
//     }
//     else return h1->position().x() < h2->position().x();
    
// }
// bool compFtpcHit::operator()(const StFtpcHit* h1, const StFtpcHit* h2) const {
//     if        (h1->position().z() != h2->position().z()) {
// 	return h1->position().z() <  h2->position().z();
//     }
//     else if   (h1->position().y() != h2->position().y()) {
// 	return h1->position().y() <  h2->position().y();
//     }
//     else return h1->position().x() < h2->position().x();
    
// }
// bool compMcFtpcHit::operator()(const StMcFtpcHit* h1, const StMcFtpcHit* h2) const {
//     if        (h1->position().z() != h2->position().z()) {
// 	return h1->position().z() <  h2->position().z();
//     }
//     else if   (h1->position().y() != h2->position().y()) {
// 	return h1->position().y() <  h2->position().y();
//     }
//     else return h1->position().x() < h2->position().x();
    
// }
    
class compFuncMcTpcHit{
public:
    bool operator()(const StMcTpcHit*) const;
    void setReferenceZ(float z) { mRefZ = z; }
    float mRefZ;
};
bool compFuncMcTpcHit::operator()(const StMcTpcHit* h) const {
    // comparison is btw hits in the same padrow
	return  (h->position().z()) > mRefZ;
}

class compFuncMcFtpcHit{
public:
    bool operator()(const StMcFtpcHit*) const;
    void setReferencePhi(float phi) { mRefPhi = phi; }
    float mRefPhi;
};
bool compFuncMcFtpcHit::operator()(const StMcFtpcHit* h) const {
    // comparison is btw hits in the same plane, so
    // z coordinate is irrelevant
	return  (h->position().phi()/degree) > mRefPhi;
}

bool compTrack::operator()(const StGlobalTrack* t1, const StGlobalTrack* t2) const {
    return t1 < t2;
}

bool compMcTrack::operator()(const StMcTrack* t1, const StMcTrack* t2) const {
    return t1 < t2;
}
bool compKinkVertex::operator()(const StKinkVertex* v1, const StKinkVertex* v2) const {
    return v1 < v2;
}
bool compV0Vertex::operator()(const StV0Vertex* v1, const StV0Vertex* v2) const {
    return v1 < v2;
}
bool compXiVertex::operator()(const StXiVertex* v1, const StXiVertex* v2) const {
    return v1 < v2;
}

bool compMcVertex::operator()(const StMcVertex* v1, const StMcVertex* v2) const {
    return v1 < v2;
}

// Print out Track pairs
ostream& operator<<(ostream& out,
		    const pair< StGlobalTrack* const, StTrackPairInfo*>& p)
{
    out << "StGlobalTrack at : " << (void*)p.first << endl;
    out << "StMcTrack     at : " << (void*)(p.second->partnerMcTrack()) << endl;
    out << "Common TPC  Hits : " << p.second->commonTpcHits()  << endl;
    out << "Common SVT  Hits : " << p.second->commonSvtHits()  << endl;
    out << "Common FTPC Hits : " << p.second->commonFtpcHits() << endl;
    
    return out;
}
ostream& operator<<(ostream& out,
		    const pair< StMcTrack* const, StTrackPairInfo*>& p)
{
    out << "StMcTrack at     : " << (void*)p.first << endl;
    out << "StGlobalTrack at : " << (void*)(p.second->partnerTrack()) << endl;
    out << "Common TPC  Hits : " << p.second->commonTpcHits()  << endl;
    out << "Common SVT  Hits : " << p.second->commonSvtHits()  << endl;
    out << "Common FTPC Hits : " << p.second->commonFtpcHits() << endl;
    
    return out;
}

// Print out the Track multimaps
ostream& operator<<(ostream& out, const rcTrackMapType& tm)
{
#if __SUNPRO_CC != 0x500
    copy(tm.begin(),tm.end(), ostream_iterator<rcTrackMapValType>(out,"\n"));
    
#else
    out << "Sorry, can't use ostream_iterator with this version of iostream.h !" << endl ;
#endif
    return out;
}

ostream& operator<<(ostream& out, const mcTrackMapType& tm)
{
#if __SUNPRO_CC != 0x500
    copy(tm.begin(),tm.end(), ostream_iterator<mcTrackMapValType>(out,"\n"));
#else
    out << "Sorry, can't use ostream_iterator with this version of iostream.h !" << endl ;
#endif
    return out;
}


ClassImp(StAssociationMaker)


//_________________________________________________
StAssociationMaker::StAssociationMaker(const char *name, const char *title):StMaker(name,title)
{
    //  StAssociationMaker Constructor
    // - zero all pointers defined in the header file

    mRcTpcHitMap  = 0;
    mMcTpcHitMap  = 0;
    mRcSvtHitMap  = 0;
    mMcSvtHitMap  = 0;
    mRcFtpcHitMap = 0;
    mMcFtpcHitMap = 0;
    mRcTrackMap   = 0;
    mMcTrackMap   = 0;
    mRcKinkMap   = 0;
    mMcKinkMap   = 0;
    mRcV0Map   = 0;
    mMcV0Map   = 0;
    mRcXiMap   = 0;
    mMcXiMap   = 0;
    
    mTpcLocalHitResolution = 0;   
    mSvtHitResolution      = 0;   
    mFtpcHitResolution     = 0;

    doPrintMemoryInfo = kFALSE;
    
}

//_________________________________________________
StAssociationMaker::~StAssociationMaker()
{
    //  StAssociationMaker Destructor
    cout << "Inside StAssociationMaker Destructor" << endl;
    
    if (mRcTpcHitMap) {
	mRcTpcHitMap->clear();
	SafeDelete(mRcTpcHitMap);
    }
    if (mMcTpcHitMap) {
	mMcTpcHitMap->clear();
	SafeDelete(mMcTpcHitMap);
    }
    if (mRcSvtHitMap) {
	mRcSvtHitMap->clear();
	SafeDelete(mRcSvtHitMap);
    }
    if (mMcSvtHitMap) {
	mMcSvtHitMap->clear();
	SafeDelete(mMcSvtHitMap);
    }
    if (mRcFtpcHitMap) {
	mRcFtpcHitMap->clear();
	SafeDelete(mRcFtpcHitMap);
    }
    if (mMcFtpcHitMap) {
	mMcFtpcHitMap->clear();
	SafeDelete(mMcFtpcHitMap);
    }
    
    if (mRcTrackMap) {
	// Delete the TrackPairInfos
	// Careful, only delete them once!
	for (rcTrackMapIter i=mRcTrackMap->begin(); i!=mRcTrackMap->end(); i++){
	    delete (*i).second;
	}
	// Delete the REC. TrackMap
	mRcTrackMap->clear();
	SafeDelete(mRcTrackMap);
    }
    if (mMcTrackMap) {
	mMcTrackMap->clear();
	SafeDelete(mMcTrackMap);
    }    
    if (mRcKinkMap) {
	mRcKinkMap->clear();
	SafeDelete(mRcKinkMap);
    }
    if (mMcKinkMap) {
	mMcKinkMap->clear();
	SafeDelete(mMcKinkMap);
    }
    if (mRcV0Map) {
	mRcV0Map->clear();
	SafeDelete(mRcV0Map);
    }
    if (mMcV0Map) {
	mMcV0Map->clear();
	SafeDelete(mMcV0Map);
    }
    if (mRcXiMap) {
	mRcXiMap->clear();
	SafeDelete(mRcXiMap);
    }
    if (mMcXiMap) {
	mMcXiMap->clear();
	SafeDelete(mMcXiMap);
    }
}

//_____________________________________________________________________________

void StAssociationMaker::Clear(const char* c)
{
    // StAssociationMaker - Clear,
    if (doPrintMemoryInfo) 
	StMemoryInfo::instance()->snapshot();
    
    // Delete TpcHitMap 
    if (mRcTpcHitMap) {
	mRcTpcHitMap->clear();
	SafeDelete(mRcTpcHitMap);
    if (doPrintMemoryInfo) 
	cout << "Deleted Rec. Tpc Hit Map" << endl;
    }
    if (mMcTpcHitMap) {
	mMcTpcHitMap->clear();
	SafeDelete(mMcTpcHitMap);
    if (doPrintMemoryInfo) 
	cout << "Deleted M.C. Tpc Hit Map" << endl;
    }
    if (mRcSvtHitMap) {
	mRcSvtHitMap->clear();
	SafeDelete(mRcSvtHitMap);
    if (doPrintMemoryInfo) 
	cout << "Deleted Rec. Svt Hit Map" << endl;
    }
    if (mMcSvtHitMap) {
	mMcSvtHitMap->clear();
	SafeDelete(mMcSvtHitMap);
    if (doPrintMemoryInfo) 
	cout << "Deleted M.C. Svt Hit Map" << endl;
    }
    if (mRcFtpcHitMap) {
	mRcFtpcHitMap->clear();
	SafeDelete(mRcFtpcHitMap);
    if (doPrintMemoryInfo) 
	cout << "Deleted Rec. Ftpc Hit Map" << endl;
    }
    if (mMcFtpcHitMap) {
	mMcFtpcHitMap->clear();
	SafeDelete(mMcFtpcHitMap);
    if (doPrintMemoryInfo) 
	cout << "Deleted M.C. Ftpc Hit Map" << endl;
    }
    
    if (mRcTrackMap) {
	// Delete the TrackPairInfos
	// Careful, only delete them once!
	for (rcTrackMapIter i=mRcTrackMap->begin(); i!=mRcTrackMap->end(); i++){
	    SafeDelete((*i).second);
	}
	// Delete the REC. TrackMap
	mRcTrackMap->clear();
	SafeDelete(mRcTrackMap);
    if (doPrintMemoryInfo) 
	cout << "Deleted Rec. Track Map" << endl;
    }
    if (mMcTrackMap) {
	mMcTrackMap->clear();
	SafeDelete(mMcTrackMap);
    if (doPrintMemoryInfo) 
	cout << "Deleted M.C. Track Map" << endl;

    }    
    if (mRcKinkMap) {
	mRcKinkMap->clear();
	SafeDelete(mRcKinkMap);
    if (doPrintMemoryInfo) 
	cout << "Deleted Rec. Kink Map" << endl;
    }
    if (mMcKinkMap) {
	mMcKinkMap->clear();
	SafeDelete(mMcKinkMap);
    if (doPrintMemoryInfo) 
	cout << "Deleted M.C. Kink Map" << endl;
    }
    if (mRcV0Map) {
	mRcV0Map->clear();
	SafeDelete(mRcV0Map);
    if (doPrintMemoryInfo) 
	cout << "Deleted Rec. V0 Map" << endl;
    }
    if (mMcV0Map) {
	mMcV0Map->clear();
	SafeDelete(mMcV0Map);
    if (doPrintMemoryInfo) 
	cout << "Deleted M.C. V0 Map" << endl;
    }
    if (mRcXiMap) {
	mRcXiMap->clear();
	SafeDelete(mRcXiMap);
    if (doPrintMemoryInfo) 
	cout << "Deleted Rec. Xi Map" << endl;
    }
    if (mMcXiMap) {
	mMcXiMap->clear();
	SafeDelete(mMcXiMap);
    if (doPrintMemoryInfo) 
	cout << "Deleted M.C. Xi Map" << endl;
    }

    if (doPrintMemoryInfo) {
	StMemoryInfo::instance()->snapshot();
	StMemoryInfo::instance()->print();
    }
    StMaker::Clear(c);
}

//_________________________________________________
Int_t StAssociationMaker::Finish()
{
  return StMaker::Finish();
}

//_________________________________________________
Int_t StAssociationMaker::Init()
{
    // StAssociationMaker - Init
    // Set up Histograms

    //
    // TPC
    //
    
    mTpcLocalHitResolution = new TH2F("TpcLocalHitResolution",
				      "Delta Z Vs Delta X for Nearby Hits",
				      50, -0.52, 0.52,
				      50, -0.24, 0.24);
    mTpcLocalHitResolution->SetXTitle("Local (Xmc - Xrec) (cm)");
    mTpcLocalHitResolution->SetYTitle("Zmc - Zrec (cm)");

    //
    // SVT
    //
    mSvtHitResolution = new TH2F("SvtHitResolution",
				 "Delta Z Vs Delta X for Nearby Hits",
				 50, -0.12, 0.12,
				 50, -0.12, 0.12);
    mSvtHitResolution->SetXTitle("Xmc - Xrec (cm)");
    mSvtHitResolution->SetYTitle("Zmc - Zrec (cm)");

    //
    // FTPC
    //
    mFtpcHitResolution = new TH2F("FtpcHitResolution",
				  "Delta Z Vs Delta X for Nearby Hits",
				  50, -0.32, 0.32,
				  50, -8, 0.8);
    mFtpcHitResolution->SetXTitle("Rmc - Rrec (cm)");
    mFtpcHitResolution->SetYTitle("PHImc - PHIrec (deg)");

    cout << "Cuts used in association for this run: " << endl;
    cout << *(StMcParameterDB::instance()) << endl;
    return StMaker::Init();
}

//_________________________________________________

Int_t StAssociationMaker::Make()
{
    cout << "AssociationMaker -- Make()" << endl;
    if (doPrintMemoryInfo) 
	StMemoryInfo::instance()->snapshot();
    //
    // Get StEvent
    //
    StEvent* rEvent = 0;
    rEvent = (StEvent*) GetInputDS("StEvent");

    if (!rEvent) {
	cerr << "No StEvent!!! " << endl;
	cerr << "Bailing out ..." << endl;
	return kStWarn;
    }
    
    //
    // Get StMcEvent
    //
    StMcEvent* mEvent = 0;
    mEvent = ((StMcEventMaker*) GetMaker("StMcEvent"))->currentMcEvent();
    if (!mEvent) {
	cerr << "No StMcEvent!!! " << endl;
	cerr << "Bailing out ..." << endl;
	return kStWarn;
    }
    
    //
    // Get the Pointers to the Collections.
    //

    // Tpc
    StTpcHitCollection*   rcTpcHitColl = rEvent->tpcHitCollection();
    StMcTpcHitCollection* mcTpcHitColl = mEvent->tpcHitCollection();
    // Svt
    StSvtHitCollection*   rcSvtHitColl = rEvent->svtHitCollection();
    StMcSvtHitCollection* mcSvtHitColl = mEvent->svtHitCollection();
    // Ftpc
    StFtpcHitCollection*   rcFtpcHitColl = rEvent->ftpcHitCollection();
    StMcFtpcHitCollection* mcFtpcHitColl = mEvent->ftpcHitCollection();

    
    // Get the pointer to the parameter DB,
    // the definitions of the cuts
    // should be done at the macro level.

    StMcParameterDB* parDB = StMcParameterDB::instance();
    

    //
    // Loop over TPC hits and make Associations
    //
    cout << "Making TPC Hit Associations..." << endl;
    
    StTpcHit*   rcTpcHit;
    StMcTpcHit* mcTpcHit;
    
    // Instantiate the Tpc Hit maps
    mRcTpcHitMap = new rcTpcHitMapType;
    mMcTpcHitMap = new mcTpcHitMapType;

    float tpcHitDistance;
    cout << "In Sector : ";
    for (unsigned int iSector=0;
	 iSector<rcTpcHitColl->numberOfSectors(); iSector++) {
	
	cout << iSector + 1 << " "; flush(cout);
	StTpcSectorHitCollection* tpcSectHitColl = rcTpcHitColl->sector(iSector);
	for (unsigned int iPadrow=0;
	     iPadrow<tpcSectHitColl->numberOfPadrows();
	     iPadrow++) {
	    StTpcPadrowHitCollection* tpcPadRowHitColl = tpcSectHitColl->padrow(iPadrow);
	    //PR(iPadrow);
	    
	    compFuncMcTpcHit tpcComp;
	    for (unsigned int iHit=0;
		 iHit<tpcPadRowHitColl->hits().size();
		 iHit++){
		//PR(iHit); 

		rcTpcHit = tpcPadRowHitColl->hits()[iHit];

		// Set the reference z for the comparison function
		// The comparison will be used to find the first Mc Hit
		// with a z greater than this reference, so that we don't loop
		// over the hits that we don't need to.
		
		tpcComp.setReferenceZ(rcTpcHit->position().z() - parDB->zCutTpc());
		StMcTpcHit* closestTpcHit = 0;

		// Find the first Mc Tpc Hit that might have a meaningful association.
		StMcTpcHitIterator tpcHitSeed = find_if (mcTpcHitColl->sector(iSector)->padrow(iPadrow)->hits().begin(),
							 mcTpcHitColl->sector(iSector)->padrow(iPadrow)->hits().end(),
							 tpcComp);
		
		bool isFirst = true;
		float xDiff, yDiff, zDiff;
		xDiff = yDiff = zDiff = -999;
		for (StMcTpcHitIterator jHit = tpcHitSeed;
		     jHit<mcTpcHitColl->sector(iSector)->padrow(iPadrow)->hits().end();
		     jHit++){
		    //PR(jHit); 
		    mcTpcHit = *jHit;
		    xDiff = mcTpcHit->position().x()-rcTpcHit->position().x();
		    yDiff = mcTpcHit->position().y()-rcTpcHit->position().y();
		    zDiff = mcTpcHit->position().z()-rcTpcHit->position().z();
		    
		    if ( zDiff > parDB->zCutTpc() ) break; //mc hits are sorted, save time!
		    
		    if (isFirst) {
			tpcHitDistance=xDiff*xDiff+zDiff*zDiff;
			closestTpcHit = mcTpcHit;
			isFirst = false;
		    }
		    if (xDiff*xDiff+zDiff*zDiff<tpcHitDistance) {
			tpcHitDistance = xDiff*xDiff+zDiff*zDiff;
			closestTpcHit = mcTpcHit;
		    }
		    
		    if ( fabs(xDiff)< parDB->xCutTpc() &&
			 fabs(yDiff)< parDB->yCutTpc() &&
			 fabs(zDiff)< parDB->zCutTpc()) {
			// Make Associations  Use maps,
			mRcTpcHitMap->insert(rcTpcHitMapValType (rcTpcHit, mcTpcHit) );
			mMcTpcHitMap->insert(mcTpcHitMapValType (mcTpcHit, rcTpcHit) );
		    }
		    
		} // End of Hits in Padrow loop for MC Hits
		if (closestTpcHit)
		    if(false)
		    mTpcLocalHitResolution->Fill(closestTpcHit->position().x()-
						 rcTpcHit->position().x(),
						 closestTpcHit->position().z()-
						 rcTpcHit->position().z() );
	    } // End of Hits in Padrow loop for Rec. Hits
	} // End of Padrow Loop for Rec. Hits
    } // End of Sector Loop for Rec. Hits
    
    cout << "\nFinished Making TPC Hit Associations *********" << endl;
    cout << "Number of Entries in TPC Hit Maps: " << mRcTpcHitMap->size() << endl;
    if (doPrintMemoryInfo) {
	cout << "End of TPC Hit Associations\n";
	StMemoryInfo::instance()->snapshot();
	StMemoryInfo::instance()->print();
    }
    
    //
    // Loop over SVT hits and make Associations
    //
    cout << "Making SVT Hit Associations..." << endl;
    
    StSvtHit*   rcSvtHit;
    StMcSvtHit* mcSvtHit;
    
    // Instantiate the Svt Hit maps
    mRcSvtHitMap = new rcSvtHitMapType;
    mMcSvtHitMap = new mcSvtHitMapType;

    float svtHitDistance;
    unsigned int nSvtHits = rcSvtHitColl->numberOfHits();
    cout << "In Barrel : ";
    for (unsigned int iBarrel=0;  nSvtHits &&
	     iBarrel<rcSvtHitColl->numberOfBarrels(); iBarrel++) {
	
	cout << iBarrel + 1 << " "; flush(cout);
	
	for (unsigned int iLadder=0;
	     iLadder<rcSvtHitColl->barrel(iBarrel)->numberOfLadders();
	     iLadder++) {
	    //PR(iLadder);
	    for (unsigned int iWafer=0;
		 iWafer<rcSvtHitColl->barrel(iBarrel)->ladder(iLadder)->numberOfWafers();
		 iWafer++) {
		//PR(iWafer);
			
		for (unsigned int iHit=0;
		     iHit<rcSvtHitColl->barrel(iBarrel)->ladder(iLadder)->wafer(iWafer)->hits().size();
		     iHit++){
		    //PR(iHit); 
		    rcSvtHit = rcSvtHitColl->barrel(iBarrel)->ladder(iLadder)->wafer(iWafer)->hits()[iHit];
				
		    StMcSvtHit* closestSvtHit = 0;
		    float newDist = 0;
		    for (unsigned int jHit=0;
			 jHit<mcSvtHitColl->barrel(iBarrel)->ladder(iLadder)->wafer(iWafer)->hits().size();
			 jHit++){
			//PR(jHit); 
			mcSvtHit = mcSvtHitColl->barrel(iBarrel)->ladder(iLadder)->wafer(iWafer)->hits()[jHit];
			float xDiff = mcSvtHit->position().x() - rcSvtHit->position().x();
			float yDiff = mcSvtHit->position().y() - rcSvtHit->position().y();
			float zDiff = mcSvtHit->position().z() - rcSvtHit->position().z();
			if ( zDiff > parDB->zCutSvt() ) break; //mc hits are sorted, save time!

			if (jHit==0) {
			    svtHitDistance=xDiff*xDiff+yDiff*yDiff+zDiff*zDiff;
			    closestSvtHit = mcSvtHit;
			}
			if ( (newDist = xDiff*xDiff+yDiff*yDiff+zDiff*zDiff) < svtHitDistance) {
			    svtHitDistance = newDist;
			    closestSvtHit = mcSvtHit;
			}
			
			if ( fabs(xDiff)< parDB->xCutSvt() &&
			     fabs(yDiff)< parDB->yCutSvt() &&
			     fabs(zDiff)< parDB->zCutSvt()) {
			    // Make Associations  Use maps,
			    mRcSvtHitMap->insert(rcSvtHitMapValType (rcSvtHit, mcSvtHit) );
			    mMcSvtHitMap->insert(mcSvtHitMapValType (mcSvtHit, rcSvtHit) );
			}
			
		    } // End of Hits in Wafer loop for MC Hits
		    if (closestSvtHit)
			mSvtHitResolution->Fill(closestSvtHit->position().x()-
						rcSvtHit->position().x(),
						closestSvtHit->position().z()-
					        rcSvtHit->position().z() );
		} // End of Hits in Wafer loop for Rec. Hits
	    } // End of Wafer Loop for Rec. Hits
	} // End of Ladder Loop for Rec. Hits
    } // End of Barrel Loop for Rec. Hits

    cout << "Finished Making SVT Hit Associations *********" << endl;
    cout << "Number of Entries in SVT Hit Maps: " << mRcSvtHitMap->size() << endl;
    if (doPrintMemoryInfo) {
	cout << "End of SVT Hit Associations\n";
	StMemoryInfo::instance()->snapshot();
	StMemoryInfo::instance()->print();
    }

    //
    // Loop over FTPC hits and make Associations
    //
    cout << "Making FTPC Hit Associations..." << endl;
    
    StFtpcHit*   rcFtpcHit = 0;
    StMcFtpcHit* mcFtpcHit = 0;
    
    // Instantiate the Ftpc Hit maps
    mRcFtpcHitMap = new rcFtpcHitMapType;
    mMcFtpcHitMap = new mcFtpcHitMapType;

    float ftpcHitDistance = 0;
    float minHitDistance = 0;
    cout << "In Plane : ";
    for (unsigned int iPlane=0;
	 iPlane<rcFtpcHitColl->numberOfPlanes(); iPlane++) {
	
	cout << iPlane + 1 << " "; flush(cout);
		
	for (unsigned int iSector=0;
	     iSector<rcFtpcHitColl->plane(iPlane)->numberOfSectors();
	     iSector++) {

	    compFuncMcFtpcHit ftpcComp;
	    //PR(iSector);

	    for (unsigned int iHit=0;
		 iHit<rcFtpcHitColl->plane(iPlane)->sector(iSector)->hits().size();
		 iHit++){
		//PR(iHit); 

		rcFtpcHit = rcFtpcHitColl->plane(iPlane)->sector(iSector)->hits()[iHit];

		ftpcComp.setReferencePhi((rcFtpcHit->position().phi()/degree) - parDB->phiCutFtpc());
		
		float rDiff, phiDiff, rDiffMin, phiDiffMin;
		rDiff = phiDiff = rDiffMin = phiDiffMin =0;

		//PR(mcFtpcHitColl->plane(iPlane)->hits().size());

		StMcFtpcHitIterator ftpcHitSeed = find_if (mcFtpcHitColl->plane(iPlane)->hits().begin(),
							   mcFtpcHitColl->plane(iPlane)->hits().end(),
							   ftpcComp);
		bool isFirst = true;
		for (StMcFtpcHitIterator jHit = ftpcHitSeed;
		     jHit<mcFtpcHitColl->plane(iPlane)->hits().end();
		     jHit++){
		    
		    mcFtpcHit = *jHit;
		    rDiff   = mcFtpcHit->position().perp() - rcFtpcHit->position().perp();
		    phiDiff = (mcFtpcHit->position().phi()  - rcFtpcHit->position().phi())/degree;

		    if ( phiDiff > parDB->phiCutFtpc() ) break; //mc hits are sorted, save time!
		    
		    ftpcHitDistance = (mcFtpcHit->position() - rcFtpcHit->position()).mag2();
		    
		    if (isFirst) {
			minHitDistance=ftpcHitDistance;
			rDiffMin=rDiff;
			phiDiffMin=phiDiff;
			isFirst = false;
		    }
		    if (ftpcHitDistance < minHitDistance) {
			minHitDistance = ftpcHitDistance;
			rDiffMin=rDiff;
			phiDiffMin=phiDiff;
		    }

		    if ( fabs(rDiff)< parDB->rCutFtpc() && fabs(phiDiff) < parDB->phiCutFtpc()) {
			// Make Associations  Use maps,
			mRcFtpcHitMap->insert(rcFtpcHitMapValType (rcFtpcHit, mcFtpcHit) );
			mMcFtpcHitMap->insert(mcFtpcHitMapValType (mcFtpcHit, rcFtpcHit) );
		    }
		    
		} // End of Hits in PLANE loop for MC Hits
		if (!isFirst)
		    mFtpcHitResolution->Fill(rDiffMin,phiDiffMin); //!
	    } // End of Hits in Sector loop for Rec. Hits
	} // End of Sector Loop for Rec. Hits
    } // End of Plane Loop for Rec. Hits
    
    cout << "\nFinished Making FTPC Hit Associations *********" << endl;
    cout << "Number of Entries in Ftpc Hit Maps: " << mRcFtpcHitMap->size() << endl;
    if (doPrintMemoryInfo) {
	cout << "End of FTPC Hit Associations\n";
	StMemoryInfo::instance()->snapshot();
	StMemoryInfo::instance()->print();
    }
    

    //
    // Check that Hit Maps are big enough to Make Track Maps
    //
    bool smallTpcHitMap, smallSvtHitMap, smallFtpcHitMap;
    smallTpcHitMap = smallSvtHitMap = smallFtpcHitMap = false;
    
    if (mRcTpcHitMap->size() < parDB->reqCommonHitsTpc()) {
	cout << " -----------  WARNING --------------- " << endl;
	cout << "   The Tpc Hit Map is too small for   " << endl;
	cout << "   any meaningful track association.  " << endl;
	cout << " ------------------------------------ " << endl;
	cout << "Entries in Hit Map  : " << mRcTpcHitMap->size() << endl;
	cout << "Required Common Hits: " << parDB->reqCommonHitsTpc() << endl;
	cout << "Suggest increase distance cuts." << endl;
	smallTpcHitMap = true;
    }
    
    if (mRcSvtHitMap->size() < parDB->reqCommonHitsSvt()) {
	cout << " -----------  WARNING --------------- " << endl;
	cout << "   The Svt Hit Map is too small for   " << endl;
	cout << "   any meaningful track association.  " << endl;
	cout << " ------------------------------------ " << endl;
	cout << "Entries in Hit Map  : " << mRcSvtHitMap->size() << endl;
	cout << "Required Common Hits: " << parDB->reqCommonHitsSvt() << endl;
	cout << "Suggest increase distance cuts." << endl;
	smallSvtHitMap = true;
    }
    if (mRcFtpcHitMap->size() < parDB->reqCommonHitsFtpc()) {
	cout << " -----------  WARNING --------------- " << endl;
	cout << "   The Ftpc Hit Map is too small for   " << endl;
	cout << "   any meaningful track association.  " << endl;
	cout << " ------------------------------------ " << endl;
	cout << "Entries in Hit Map  : " << mRcFtpcHitMap->size() << endl;
	cout << "Required Common Hits: " << parDB->reqCommonHitsFtpc() << endl;
	cout << "Suggest increase distance cuts." << endl;
	smallFtpcHitMap = true;
    }

    if (smallTpcHitMap && smallSvtHitMap && smallFtpcHitMap) {
	cout << "No Useful Hit Map to make Track Associations" << endl;
	return kStWarn;
    }
    
    //
    // Start doing Track Associations ----------------------
    //
    
    StSPtrVecTrackNode& rcTrackNodes = rEvent->trackNodes();
    StTrackNode*   trkNode;
    StGlobalTrack* rcTrack;
    
    StHit*     rcHit;
    StTpcHit*  rcKeyTpcHit;
    StSvtHit*  rcKeySvtHit;
    StFtpcHit* rcKeyFtpcHit;
    
    pair<rcTpcHitMapIter,rcTpcHitMapIter>   boundsTpc;
    pair<rcSvtHitMapIter,rcSvtHitMapIter>   boundsSvt;
    pair<rcFtpcHitMapIter,rcFtpcHitMapIter> boundsFtpc;

    rcTpcHitMapIter  tpcHMIter;
    rcSvtHitMapIter  svtHMIter;
    rcFtpcHitMapIter ftpcHMIter;
    
    const StMcTpcHit* mcValueTpcHit;
    const StMcSvtHit* mcValueSvtHit;
    const StMcFtpcHit* mcValueFtpcHit;

    StMcTrack* trackCand;
    StTrackPairInfo* trkPair;

    trackPing initializedTrackPing;
    initializedTrackPing.mcTrack = 0;
    initializedTrackPing.nPingsTpc = 0;
    initializedTrackPing.nPingsSvt = 0;
    initializedTrackPing.nPingsFtpc = 0;
    

    // Instantiate the Track map
    mRcTrackMap = new rcTrackMapType;
    mMcTrackMap = new mcTrackMapType;
    // Begin making associations
    cout << "Making Track Associations..." << endl;

    //
    // Loop over tracks nodes in StEvent
    // 
    unsigned int trkNodeI;
    for (trkNodeI = 0; trkNodeI < rcTrackNodes.size(); trkNodeI++){
	
#ifndef ST_NO_TEMPLATE_DEF_ARGS
	vector<trackPing> candidates(20, initializedTrackPing);
#else
	vector<trackPing, allocator<trackPing> > candidates(20, initializedTrackPing);
#endif
	trkNode = rcTrackNodes[trkNodeI]; // For a by-pointer collection we need to dereference once
	rcTrack = dynamic_cast<StGlobalTrack*>(trkNode->track(global));
	if (!rcTrack || !(rcTrack->detectorInfo()->hits().size()))
	    continue; // If there are no Tpc Hits, skip track.
	
	int nCandidates = 0;

	//
	// Get the hits of this track.
	//

	StPtrVecHit recTpcHits   = rcTrack->detectorInfo()->hits(kTpcId);
	StPtrVecHit recSvtHits   = rcTrack->detectorInfo()->hits(kSvtId);
	StPtrVecHit recFtpcHitsW = rcTrack->detectorInfo()->hits(kFtpcWestId);
	StPtrVecHit recFtpcHitsE = rcTrack->detectorInfo()->hits(kFtpcEastId);

		
	//
	// Loop over the TPC hits of the track
	//
	unsigned int recTpcHitI;
 	for (recTpcHitI = 0; recTpcHitI < recTpcHits.size(); recTpcHitI++) {
	    	    
	    rcHit = recTpcHits[recTpcHitI];
	    rcKeyTpcHit = dynamic_cast<StTpcHit*>(rcHit);
		
	    if (!rcKeyTpcHit) continue;
	    boundsTpc = mRcTpcHitMap->equal_range(rcKeyTpcHit);
	    
	    for (tpcHMIter=boundsTpc.first; tpcHMIter!=boundsTpc.second; ++tpcHMIter) {
		
		mcValueTpcHit = (*tpcHMIter).second;
		trackCand = mcValueTpcHit->parentTrack();
				
		// At this point we have a candidate Monte Carlo Track
		// If there are no candidates, create the first candidate.
		// If already there, increment its nPings.
		// If doesn't match any of the previous candidates, create new candidate.
		    
		if (nCandidates == 0) {
		    candidates[0].mcTrack    = trackCand;
		    candidates[0].nPingsTpc  = 1;
		    nCandidates++;
		    
		}
		
		else {
		    for (int iCandidate=0; iCandidate<nCandidates; iCandidate++){ 
			if (trackCand==candidates[iCandidate].mcTrack){
			    candidates[iCandidate].nPingsTpc++;
			    break;
			}
			if (iCandidate == (nCandidates-1)){
			    candidates[nCandidates].mcTrack = trackCand;
			    candidates[nCandidates].nPingsTpc  = 1;
			    nCandidates++;
			    break;
			}
		    } // candidate loop
		    
		}
	    } // mc hits in multimap
	    
	} // Tpc Hits from Track from StEvent loop

	//
	// Loop over the SVT hits of the track
	//

	unsigned int recSvtHitI;
 	for (recSvtHitI = 0; recSvtHitI < recSvtHits.size(); recSvtHitI++) {
	    // Loop over the SVT hits of the track
	    
	    rcHit = recSvtHits[recSvtHitI];
	    rcKeySvtHit = dynamic_cast<StSvtHit*>(rcHit);
		
	    if (!rcKeySvtHit) continue;
	    boundsSvt = mRcSvtHitMap->equal_range(rcKeySvtHit);
	    
	    for (svtHMIter=boundsSvt.first; svtHMIter!=boundsSvt.second; ++svtHMIter) {
		
		mcValueSvtHit = (*svtHMIter).second;
		trackCand = mcValueSvtHit->parentTrack();
				
		// At this point we have a candidate Monte Carlo Track
		// If there are no candidates, create the first candidate.
		// If already there, increment its nPings.
		// If doesn't match any of the previous candidates, create new candidate.
		    
		if (nCandidates == 0) {
		    candidates[0].mcTrack    = trackCand;
		    candidates[0].nPingsSvt  = 1;
		    nCandidates++;
		    
		}
		
		else {
		    for (int iCandidate=0; iCandidate<nCandidates; iCandidate++){ 
			if (trackCand==candidates[iCandidate].mcTrack){
			    candidates[iCandidate].nPingsSvt++;
			    break;
			}
			if (iCandidate == (nCandidates-1)){
			    candidates[nCandidates].mcTrack = trackCand;
			    candidates[nCandidates].nPingsSvt  = 1;
			    nCandidates++;
			    break;
			}
		    } // candidate loop
		    
		}
	    } // mc hits in multimap
	    
	} // Svt Hits from Track from StEvent loop

	//
	// Loop over the FTPC hits of the track.  Note that there are 2 loops,
	// one for the West and one for the East FTPC, but probably if there are hits in one
	// there won't be in the other one.
	//
       
	unsigned int recFtpcHitI;

	// Loop over the West FTPC hits of the track
 	for (recFtpcHitI = 0; recFtpcHitI < recFtpcHitsW.size(); recFtpcHitI++) {
	    
	    rcHit = recFtpcHitsW[recFtpcHitI];
	    rcKeyFtpcHit = dynamic_cast<StFtpcHit*>(rcHit);
	    
	    if (!rcKeyFtpcHit) continue;
	    boundsFtpc = mRcFtpcHitMap->equal_range(rcKeyFtpcHit);
	    
	    for (ftpcHMIter=boundsFtpc.first; ftpcHMIter!=boundsFtpc.second; ++ftpcHMIter) {
		
		mcValueFtpcHit = (*ftpcHMIter).second;
		trackCand = mcValueFtpcHit->parentTrack();
				
		// At this point we have a candidate Monte Carlo Track
		// If there are no candidates, create the first candidate.
		// If already there, increment its nPings.
		// If doesn't match any of the previous candidates, create new candidate.
		    
		if (nCandidates == 0) {
		    candidates[0].mcTrack    = trackCand;
		    candidates[0].nPingsFtpc  = 1;
		    nCandidates++;
		    
		}
		
		else {
		    for (int iCandidate=0; iCandidate<nCandidates; iCandidate++){ 
			if (trackCand==candidates[iCandidate].mcTrack){
			    candidates[iCandidate].nPingsFtpc++;
			    break;
			}
			if (iCandidate == (nCandidates-1)){
			    candidates[nCandidates].mcTrack = trackCand;
			    candidates[nCandidates].nPingsFtpc  = 1;
			    nCandidates++;
			    break;
			}
		    } // candidate loop
		    
		}
	    } // mc ftpc hits in multimap
	    
	} // West Ftpc Hits from Track from StEvent loop
	
	// Loop over the East FTPC hits of the track
	for (recFtpcHitI = 0; recFtpcHitI < recFtpcHitsE.size(); recFtpcHitI++) {
		    
	    
	    rcHit = recFtpcHitsE[recFtpcHitI];
	    rcKeyFtpcHit = dynamic_cast<StFtpcHit*>(rcHit);
		
	    if (!rcKeyFtpcHit) continue;
	    boundsFtpc = mRcFtpcHitMap->equal_range(rcKeyFtpcHit);
	    
	    for (ftpcHMIter=boundsFtpc.first; ftpcHMIter!=boundsFtpc.second; ++ftpcHMIter) {
		
		mcValueFtpcHit = (*ftpcHMIter).second;
		trackCand = mcValueFtpcHit->parentTrack();
				
		// At this point we have a candidate Monte Carlo Track
		// If there are no candidates, create the first candidate.
		// If already there, increment its nPings.
		// If doesn't match any of the previous candidates, create new candidate.
		    
		if (nCandidates == 0) {
		    candidates[0].mcTrack    = trackCand;
		    candidates[0].nPingsFtpc  = 1;
		    nCandidates++;
		    
		}
		
		else {
		    for (int iCandidate=0; iCandidate<nCandidates; iCandidate++){ 
			if (trackCand==candidates[iCandidate].mcTrack){
			    candidates[iCandidate].nPingsFtpc++;
			    break;
			}
			if (iCandidate == (nCandidates-1)){
			    candidates[nCandidates].mcTrack = trackCand;
			    candidates[nCandidates].nPingsFtpc  = 1;
			    nCandidates++;
			    break;
			}
		    } // candidate loop
		    
		}
	    } // mc hits in multimap
	    
	} // Ftpc Hits from Track from StEvent loop

	//
	// Now we need to associate the tracks that meet the commonHits criteria.
	//
	
	if (nCandidates>20) cout << "We Have More than 20 candidates!!! " << endl;
	if (candidates.size()>20) cout << "The candidate track vector has grown more than expected!! " << endl;
 	for (int iCandidate=0; iCandidate<nCandidates; iCandidate++){
	    //mNumberOfPings->Fill((float) candidates[iCandidate].nPings);
	    
	    
	    if (candidates[iCandidate].nPingsTpc  >= parDB->reqCommonHitsTpc() ||
		candidates[iCandidate].nPingsSvt  >= parDB->reqCommonHitsSvt() ||
		candidates[iCandidate].nPingsFtpc >= parDB->reqCommonHitsFtpc()){
		// We got a track pair !!
		// Add it to multimap
		
		trkPair = new StTrackPairInfo(rcTrack,
					      candidates[iCandidate].mcTrack,
					      candidates[iCandidate].nPingsTpc,
					      candidates[iCandidate].nPingsSvt,
					      candidates[iCandidate].nPingsFtpc);
		mRcTrackMap->insert(rcTrackMapValType (rcTrack, trkPair));
		mMcTrackMap->insert(mcTrackMapValType (candidates[iCandidate].mcTrack, trkPair));
		
		// print out the map
		//cout << "The map is now" << endl << *mRcTrackMap << endl;
	    }
	}
	
	// Clear the candidate vector
	candidates.clear();

    }// StEvent track loop
    
	 
    cout << "Finished Making Track Associations *********" << endl;
    cout << "Number of Entries in Track Maps: " << mRcTrackMap->size() << endl;
    if (doPrintMemoryInfo) {
	cout << "End of Track Associations\n";
	StMemoryInfo::instance()->snapshot();
	StMemoryInfo::instance()->print();
    }

    //
    // Start doing Vertex Associations ----------------------
    //

    // Instantiate the Vertex maps
    mRcKinkMap = new rcKinkMapType;
    mMcKinkMap = new mcKinkMapType;
    mRcV0Map   = new rcV0MapType;
    mMcV0Map   = new mcV0MapType;
    mRcXiMap   = new rcXiMapType;
    mMcXiMap   = new mcXiMapType;
    // Begin making associations
    cout << "Making Vertex Associations" << endl;

    StSPtrVecKinkVertex& kinks = rEvent->kinkVertices();

    
    cout << "Kinks..." << endl;

    // Loop over Kinks

    pair<rcTrackMapIter, rcTrackMapIter> kinkBoundsDaughter, kinkBoundsParent;
    
    StMcVertex* primary   = mEvent->primaryVertex();
    for (StKinkVertexIterator kvi = kinks.begin(); kvi!=kinks.end(); kvi++) {
	
	StKinkVertex* rcKink = *kvi; // Got Kink ...
	StTrack* kinkDaughter  = rcKink->daughter(0);
	StGlobalTrack* gKinkDaughter = dynamic_cast<StGlobalTrack*>(kinkDaughter);
	if (!gKinkDaughter) continue;
	// Got Daughter
	StTrack* kinkParent  = rcKink->parent();
	StGlobalTrack* gKinkParent = dynamic_cast<StGlobalTrack*>(kinkParent);
	if (!gKinkParent) continue;
	// Got Parent
	
	kinkBoundsDaughter = mRcTrackMap->equal_range(gKinkDaughter);
	// Loop over associated tracks of the daughter
	for (rcTrackMapIter trkIter = kinkBoundsDaughter.first; trkIter!=kinkBoundsDaughter.second; trkIter++) {
	    StMcTrack* mcDaughter = (*trkIter).second->partnerMcTrack(); // Get associated daughter
	    
	    StMcVertex* mcKink = mcDaughter->startVertex(); // Get Kink candidate 
	    if (mcKink == primary || mcKink == 0) continue;  // Check that it's not primary
	    const StMcTrack* mcParent = mcKink->parent();
	    
	    // Check that parents match
	    kinkBoundsParent = mRcTrackMap->equal_range(gKinkParent);
	    // loop over associated tracks of the parent
	    for (rcTrackMapIter trkIter2 = kinkBoundsParent.first;
		 trkIter2!=kinkBoundsParent.second; trkIter2++) {
		// Get associated parent
		if (mcParent == (*trkIter2).second->partnerMcTrack() ) {
		    
		    // Got a candidate!!
		    mRcKinkMap->insert(rcKinkMapValType (rcKink, mcKink));
		    mMcKinkMap->insert(mcKinkMapValType (mcKink, rcKink));
		}
	    }
	    
	}
    } // kink loop
    cout << "Finished Making kink Associations *********" << endl;
    cout << "Number of Entries in kink Maps: " << mRcKinkMap->size() << endl;
    if (doPrintMemoryInfo) {
	cout << "End of kink Associations\n";
	StMemoryInfo::instance()->snapshot();
	StMemoryInfo::instance()->print();
    }
	
    cout << "V0s..." << endl;

    StSPtrVecV0Vertex& v0s = rEvent->v0Vertices();    
   
    // Loop over V0s
    for (StV0VertexIterator v0vi = v0s.begin(); v0vi!=v0s.end(); v0vi++) {
	StV0Vertex* rcV0 = *v0vi; // Got V0 ...
	StTrack* v0Daughter1  = rcV0->daughter(0);
	StGlobalTrack* gV0Daughter1 = dynamic_cast<StGlobalTrack*>(v0Daughter1);
	if (!gV0Daughter1) continue;
	// Got Daughter1
	StTrack* v0Daughter2  = rcV0->daughter(1);
	StGlobalTrack* gV0Daughter2 = dynamic_cast<StGlobalTrack*>(v0Daughter2);
	if (!gV0Daughter2) continue;
	// Got Daughter2
	pair<rcTrackMapIter, rcTrackMapIter> v0Bounds1 = mRcTrackMap->equal_range(gV0Daughter1);
	pair<rcTrackMapIter, rcTrackMapIter> v0Bounds2 = mRcTrackMap->equal_range(gV0Daughter2);
	for (rcTrackMapIter trkIter1 = v0Bounds1.first; trkIter1!=v0Bounds1.second; trkIter1++) {
	    StMcTrack* mcDaughter1 = (*trkIter1).second->partnerMcTrack();
	    for (rcTrackMapIter trkIter2 = v0Bounds2.first; trkIter2!=v0Bounds2.second; trkIter2++) {
		StMcTrack* mcDaughter2 = (*trkIter2).second->partnerMcTrack();
		if (mcDaughter1->startVertex() == mcDaughter2->startVertex() &&
		    mcDaughter1->startVertex() != primary &&
		    mcDaughter1->startVertex() != 0) {
		    // Got a V0 candidate
		    mRcV0Map->insert(rcV0MapValType (rcV0, mcDaughter1->startVertex()));
		    mMcV0Map->insert(mcV0MapValType (mcDaughter1->startVertex(), rcV0));
		    
		}
	    }
	}
	
    } // V0 loop
    cout << "Finished Making V0 Associations *********" << endl;
    cout << "Number of Entries in V0 Maps: " << mRcV0Map->size() << endl;
    if (doPrintMemoryInfo) {
	cout << "End of V0 Associations\n";
	StMemoryInfo::instance()->snapshot();
	StMemoryInfo::instance()->print();
    }
    
    cout << "Xis..." << endl;

    StSPtrVecXiVertex& xis = rEvent->xiVertices();    
    
    // Loop over Xis
    for (StXiVertexIterator xvi = xis.begin(); xvi!=xis.end(); xvi++) {
	StXiVertex* rcXi = *xvi;
	StV0Vertex* rcV0ofXi = rcXi->v0Vertex();
	StTrack* rcBachelor = rcXi->bachelor();
	StGlobalTrack* gRcBachelor = dynamic_cast<StGlobalTrack*>(rcBachelor);
	if (!gRcBachelor) continue;
	pair<rcTrackMapIter, rcTrackMapIter> xiBounds = mRcTrackMap->equal_range(gRcBachelor);
	for (rcTrackMapIter trkIter3 = xiBounds.first; trkIter3!= xiBounds.second; trkIter3++){
	    StMcTrack*  mcBachelor = (*trkIter3).second->partnerMcTrack();
	    StMcVertex* mcXi = mcBachelor->startVertex();
	    if (mcXi == primary || mcXi == 0) continue;
	    pair<rcV0MapIter, rcV0MapIter> xiBoundsV0 = mRcV0Map->equal_range(rcV0ofXi);
	    for (rcV0MapIter v0Iter = xiBoundsV0.first; v0Iter!= xiBoundsV0.second; v0Iter++){
		StMcVertex* mcV0 = (*v0Iter).second;
		if (mcV0->parent() != 0 && mcXi == mcV0->parent()->startVertex()) {
		    // Got a Xi candidate
		    mRcXiMap->insert(rcXiMapValType (rcXi, mcXi));
		    mMcXiMap->insert(mcXiMapValType (mcXi, rcXi));
		    
		}
	    }
	}
    }
    cout << "Finished Making Xi Associations *********" << endl;
    cout << "Number of Entries in Xi Maps: " << mRcXiMap->size() << endl;

    if (doPrintMemoryInfo) {
	cout << "End of Make()\n";
	StMemoryInfo::instance()->snapshot();
	StMemoryInfo::instance()->print();
    }
    return kStOK;
}
