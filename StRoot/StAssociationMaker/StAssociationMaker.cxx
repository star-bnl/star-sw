/*************************************************
 *
 * $Id: StAssociationMaker.cxx,v 1.20 2000/03/06 18:08:56 calderon Exp $
 * $Log: StAssociationMaker.cxx,v $
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
#include <math.h>
#if !defined(ST_NO_NAMESPACES)
using std::string;
using std::vector;
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


}

//_________________________________________________
StAssociationMaker::~StAssociationMaker()
{
    //  StAssociationMaker Destructor
    cout << "Inside StAssociationMaker Destructor" << endl;
    
    if (mRcTpcHitMap) {
	mRcTpcHitMap->clear();
	SafeDelete(mRcTpcHitMap);
	cout << "Deleted Rec. Tpc Hit Map" << endl;
    }
    if (mMcTpcHitMap) {
	mMcTpcHitMap->clear();
	SafeDelete(mMcTpcHitMap);
	cout << "Deleted M.C. Tpc Hit Map" << endl;
    }
    if (mRcSvtHitMap) {
	mRcSvtHitMap->clear();
	SafeDelete(mRcSvtHitMap);
	cout << "Deleted Rec. Svt Hit Map" << endl;
    }
    if (mMcSvtHitMap) {
	mMcSvtHitMap->clear();
	SafeDelete(mMcSvtHitMap);
	cout << "Deleted M.C. Svt Hit Map" << endl;
    }
    if (mRcFtpcHitMap) {
	mRcFtpcHitMap->clear();
	SafeDelete(mRcFtpcHitMap);
	cout << "Deleted Rec. Ftpc Hit Map" << endl;
    }
    if (mMcFtpcHitMap) {
	mMcFtpcHitMap->clear();
	SafeDelete(mMcFtpcHitMap);
	cout << "Deleted M.C. Ftpc Hit Map" << endl;
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
	cout << "Deleted Rec. Track Map" << endl;
    }
    if (mMcTrackMap) {
	mMcTrackMap->clear();
	SafeDelete(mMcTrackMap);
	cout << "Deleted M.C. Track Map" << endl;

    }    
    if (mRcKinkMap) {
	mRcKinkMap->clear();
	SafeDelete(mRcKinkMap);
	cout << "Deleted Rec. Kink Map" << endl;
    }
    if (mMcKinkMap) {
	mMcKinkMap->clear();
	SafeDelete(mMcKinkMap);
	cout << "Deleted M.C. Kink Map" << endl;
    }
    if (mRcV0Map) {
	mRcV0Map->clear();
	SafeDelete(mRcV0Map);
	cout << "Deleted Rec. V0 Map" << endl;
    }
    if (mMcV0Map) {
	mMcV0Map->clear();
	SafeDelete(mMcV0Map);
	cout << "Deleted M.C. V0 Map" << endl;
    }
    if (mRcXiMap) {
	mRcXiMap->clear();
	SafeDelete(mRcXiMap);
	cout << "Deleted Rec. Xi Map" << endl;
    }
    if (mMcXiMap) {
	mMcXiMap->clear();
	SafeDelete(mMcXiMap);
	cout << "Deleted M.C. Xi Map" << endl;
    }
}

//_____________________________________________________________________________

void StAssociationMaker::Clear(const char*)
{
    // StAssociationMaker - Clear,
    
    // Delete TpcHitMap 
    if (mRcTpcHitMap) {
	mRcTpcHitMap->clear();
	SafeDelete(mRcTpcHitMap);
	cout << "Deleted Rec. Tpc Hit Map" << endl;
    }
    if (mMcTpcHitMap) {
	mMcTpcHitMap->clear();
	SafeDelete(mMcTpcHitMap);
	cout << "Deleted M.C. Tpc Hit Map" << endl;
    }
    if (mRcSvtHitMap) {
	mRcSvtHitMap->clear();
	SafeDelete(mRcSvtHitMap);
	cout << "Deleted Rec. Svt Hit Map" << endl;
    }
    if (mMcSvtHitMap) {
	mMcSvtHitMap->clear();
	SafeDelete(mMcSvtHitMap);
	cout << "Deleted M.C. Svt Hit Map" << endl;
    }
    if (mRcFtpcHitMap) {
	mRcFtpcHitMap->clear();
	SafeDelete(mRcFtpcHitMap);
	cout << "Deleted Rec. Ftpc Hit Map" << endl;
    }
    if (mMcFtpcHitMap) {
	mMcFtpcHitMap->clear();
	SafeDelete(mMcFtpcHitMap);
	cout << "Deleted M.C. Ftpc Hit Map" << endl;
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
	cout << "Deleted Rec. Track Map" << endl;
    }
    if (mMcTrackMap) {
	mMcTrackMap->clear();
	SafeDelete(mMcTrackMap);
	cout << "Deleted M.C. Track Map" << endl;

    }    
    if (mRcKinkMap) {
	mRcKinkMap->clear();
	SafeDelete(mRcKinkMap);
	cout << "Deleted Rec. Kink Map" << endl;
    }
    if (mMcKinkMap) {
	mMcKinkMap->clear();
	SafeDelete(mMcKinkMap);
	cout << "Deleted M.C. Kink Map" << endl;
    }
    if (mRcV0Map) {
	mRcV0Map->clear();
	SafeDelete(mRcV0Map);
	cout << "Deleted Rec. V0 Map" << endl;
    }
    if (mMcV0Map) {
	mMcV0Map->clear();
	SafeDelete(mMcV0Map);
	cout << "Deleted M.C. V0 Map" << endl;
    }
    if (mRcXiMap) {
	mRcXiMap->clear();
	SafeDelete(mRcXiMap);
	cout << "Deleted Rec. Xi Map" << endl;
    }
    if (mMcXiMap) {
	mMcXiMap->clear();
	SafeDelete(mMcXiMap);
	cout << "Deleted M.C. Xi Map" << endl;
    }
    StMaker::Clear();
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
    
    return StMaker::Init();
}

//_________________________________________________

Int_t StAssociationMaker::Make()
{
    cout << "AssociationMaker -- Make()" << endl;

    //
    // Get StEvent
    //
    StEvent* rEvent = 0;
    rEvent = (StEvent*) GetInputDS("StEvent");

    if (!rEvent) {
	cerr << "No StEvent!!! " << endl;
	cerr << "Bailing out ..." << endl;
	exit(1);
    }
    
    //
    // Get StMcEvent
    //
    StMcEvent* mEvent = 0;
    mEvent = ((StMcEventMaker*) gStChain->Maker("MCEvent"))->currentMcEvent();
    if (!mEvent) {
	cerr << "No StMcEvent!!! " << endl;
	cerr << "Bailing out ..." << endl;
	exit(1);
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
    cout << *parDB << endl;

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
    
    for (unsigned int iSector=0;
	 iSector<rcTpcHitColl->numberOfSectors(); iSector++) {
	
	cout << "In Sector : " << iSector + 1 << endl;
	StTpcSectorHitCollection* tpcSectHitColl = rcTpcHitColl->sector(iSector);
	for (unsigned int iPadrow=0;
	     iPadrow<tpcSectHitColl->numberOfPadrows();
	     iPadrow++) {
	    StTpcPadrowHitCollection* tpcPadRowHitColl = tpcSectHitColl->padrow(iPadrow);
	    //PR(iPadrow);
	    for (unsigned int iHit=0;
		 iHit<tpcPadRowHitColl->hits().size();
		 iHit++){
		//PR(iHit); 

		rcTpcHit = tpcPadRowHitColl->hits()[iHit];
		
		StMcTpcHit* closestTpcHit = 0;
		
		for (unsigned int jHit=0;
		     jHit<mcTpcHitColl->sector(iSector)->padrow(iPadrow)->hits().size();
		     jHit++){
		    //PR(jHit); 
		    mcTpcHit = mcTpcHitColl->sector(iSector)->padrow(iPadrow)->hits()[jHit];
		    float xDiff = mcTpcHit->position().x()-rcTpcHit->position().x();
		    float yDiff = mcTpcHit->position().y()-rcTpcHit->position().y();
		    float zDiff = mcTpcHit->position().z()-rcTpcHit->position().z();
		    
		    if ( zDiff > parDB->zCutTpc() ) break; //mc hits are sorted, save time!
		    
		    if (jHit==0) {
			tpcHitDistance=xDiff*xDiff+zDiff*zDiff;
			closestTpcHit = mcTpcHit;
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
		    mTpcLocalHitResolution->Fill(closestTpcHit->position().x()-
						 rcTpcHit->position().x(),
						 closestTpcHit->position().z()-
						 rcTpcHit->position().z() );
	    } // End of Hits in Padrow loop for Rec. Hits
	} // End of Padrow Loop for Rec. Hits
    } // End of Sector Loop for Rec. Hits
    
    cout << "Finished Making TPC Hit Associations *********" << endl;
    
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
    for (unsigned int iBarrel=0;  nSvtHits &&
	     iBarrel<rcSvtHitColl->numberOfBarrels(); iBarrel++) {
	
	cout << "In Barrel : " << iBarrel + 1 << endl;
	
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

    //
    // Loop over FTPC hits and make Associations
    //
    cout << "Making FTPC Hit Associations..." << endl;
    
    StFtpcHit*   rcFtpcHit;
    StMcFtpcHit* mcFtpcHit;
    
    // Instantiate the Ftpc Hit maps
    mRcFtpcHitMap = new rcFtpcHitMapType;
    mMcFtpcHitMap = new mcFtpcHitMapType;

    float ftpcHitDistance;
    float minHitDistance;
    
    for (unsigned int iPlane=0;
	 iPlane<rcFtpcHitColl->numberOfPlanes(); iPlane++) {
	
	cout << "In Plane : " << iPlane + 1 << endl;
		
	for (unsigned int iSector=0;
	     iSector<rcFtpcHitColl->plane(iPlane)->numberOfSectors();
	     iSector++) {
	   
	    //PR(iSector);
	    for (unsigned int iHit=0;
		 iHit<rcFtpcHitColl->plane(iPlane)->sector(iSector)->hits().size();
		 iHit++){
		//PR(iHit); 

		rcFtpcHit = rcFtpcHitColl->plane(iPlane)->sector(iSector)->hits()[iHit];
				
		StMcFtpcHit* closestFtpcHit = 0;
		float rDiff;
		float phiDiff;
		float rDiffMin;
		float phiDiffMin;
		for (unsigned int jHit=0;
		     jHit<mcFtpcHitColl->plane(iPlane)->hits().size();
		     jHit++){
		    //PR(jHit); 
		    mcFtpcHit = mcFtpcHitColl->plane(iPlane)->hits()[jHit];
		    rDiff   = mcFtpcHit->position().perp() - rcFtpcHit->position().perp();
		    phiDiff = (mcFtpcHit->position().phi()  - rcFtpcHit->position().phi())/degree;
		    if ( phiDiff > parDB->phiCutFtpc() ) break; //mc hits are sorted, save time!
		    
		    ftpcHitDistance = (mcFtpcHit->position() - rcFtpcHit->position()).mag2();
		    		    
		    if (jHit==0) {
			minHitDistance=ftpcHitDistance;
			closestFtpcHit = mcFtpcHit;
			rDiffMin=rDiff;
			phiDiffMin=phiDiff;
		    }
		    if (ftpcHitDistance<minHitDistance) {
			minHitDistance = ftpcHitDistance;
			closestFtpcHit = mcFtpcHit;
			rDiffMin=rDiff;
			phiDiffMin=phiDiff;
		    }

		    if ( fabs(rDiff)< parDB->rCutFtpc() && fabs(phiDiff) < parDB->phiCutFtpc()) {
			// Make Associations  Use maps,
			mRcFtpcHitMap->insert(rcFtpcHitMapValType (rcFtpcHit, mcFtpcHit) );
			mMcFtpcHitMap->insert(mcFtpcHitMapValType (mcFtpcHit, rcFtpcHit) );
		    }
		    
		} // End of Hits in PLANE loop for MC Hits
		if (closestFtpcHit)
		    mFtpcHitResolution->Fill(rDiffMin,phiDiffMin); //!
	    } // End of Hits in Sector loop for Rec. Hits
	} // End of Sector Loop for Rec. Hits
    } // End of Plane Loop for Rec. Hits
    
    cout << "Finished Making FTPC Hit Associations *********" << endl;
    

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
    
    if (mRcTpcHitMap->size() < parDB->reqCommonHitsTpc()) {
	cout << " -----------  WARNING --------------- " << endl;
	cout << "   The Tpc Hit Map is too small for   " << endl;
	cout << "   any meaningful track association.  " << endl;
	cout << " ------------------------------------ " << endl;
	cout << "Entries in Hit Map  : " << mRcTpcHitMap->size() << endl;
	cout << "Required Common Hits: " << parDB->reqCommonHitsTpc() << endl;
	cout << "Suggest increase distance cuts." << endl;
	smallSvtHitMap = true;
    }
    if (mRcTpcHitMap->size() < parDB->reqCommonHitsTpc()) {
	cout << " -----------  WARNING --------------- " << endl;
	cout << "   The Tpc Hit Map is too small for   " << endl;
	cout << "   any meaningful track association.  " << endl;
	cout << " ------------------------------------ " << endl;
	cout << "Entries in Hit Map  : " << mRcTpcHitMap->size() << endl;
	cout << "Required Common Hits: " << parDB->reqCommonHitsTpc() << endl;
	cout << "Suggest increase distance cuts." << endl;
	smallFtpcHitMap = true;
    }

    if (smallTpcHitMap && smallSvtHitMap && smallFtpcHitMap) {
	cout << "No Useful Hit Map to make Track Associations" << endl;
	return kStFatal;
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
    
#ifndef ST_NO_TEMPLATE_DEF_ARGS
    vector<trackPing> candidates(100, initializedTrackPing);
#else
    vector<trackPing, allocator<trackPing> > candidates(100, initializedTrackPing);
#endif

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
	
	if (nCandidates>100) cout << "We Have More than 100 candidates!!! " << endl;
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
	
	
    }// StEvent track loop

    // Clear the candidate vector
    candidates.clear();
	 
    cout << "Finished Making Track Associations *********" << endl;

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
    StSPtrVecV0Vertex& v0s = rEvent->v0Vertices();
    StSPtrVecXiVertex& xis = rEvent->xiVertices();

    
    cout << "Kinks..." << endl;

    // Loop over Kinks

    pair<rcTrackMapIter, rcTrackMapIter> kinkBoundsDaughter, kinkBoundsParent;
    StKinkVertex* rcKink  = 0;
    StTrack* kinkDaughter  = 0;
    StTrack* kinkParent  = 0;
    StGlobalTrack* gKinkDaughter = 0;
    StGlobalTrack* gKinkParent = 0;
    StMcTrack* mcDaughter = 0;
    StMcVertex* mcKink    = 0;
    StMcVertex* primary   = mEvent->primaryVertex();
    const StMcTrack*  mcParent  = 0;
    for (StKinkVertexIterator kvi = kinks.begin(); kvi!=kinks.end(); kvi++) {
	
	rcKink = *kvi; // Got Kink ...
	kinkDaughter  = rcKink->daughter(0);
	gKinkDaughter = dynamic_cast<StGlobalTrack*>(kinkDaughter);
	if (!gKinkDaughter) continue;
	// Got Daughter
	kinkParent  = rcKink->parent();
	gKinkParent = dynamic_cast<StGlobalTrack*>(kinkParent);
	if (!gKinkParent) continue;
	// Got Parent
	
	kinkBoundsDaughter = mRcTrackMap->equal_range(gKinkDaughter);
	// Loop over associated tracks of the daughter
	for (rcTrackMapIter trkIter = kinkBoundsDaughter.first; trkIter!=kinkBoundsDaughter.second; trkIter++) {
	    mcDaughter = (*trkIter).second->partnerMcTrack(); // Get associated daughter
	    
	    mcKink = mcDaughter->startVertex(); // Get Kink candidate 
	    if (mcKink == primary || mcKink == 0) continue;  // Check that it's not primary
	    mcParent = mcKink->parent();
	    
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
	
    cout << "V0s..." << endl;
    pair<rcTrackMapIter, rcTrackMapIter> v0Bounds1;
    pair<rcTrackMapIter, rcTrackMapIter> v0Bounds2;    
    StV0Vertex* rcV0  = 0;
    StTrack* v0Daughter1  = 0;
    StGlobalTrack* gV0Daughter1 = 0;
    StTrack* v0Daughter2  = 0;
    StGlobalTrack* gV0Daughter2 = 0;
    
    StMcTrack* mcDaughter1 = 0;
    StMcTrack* mcDaughter2 = 0;
    
    // Loop over V0s
    for (StV0VertexIterator v0vi = v0s.begin(); v0vi!=v0s.end(); v0vi++) {
	rcV0 = *v0vi; // Got V0 ...
	v0Daughter1  = rcV0->daughter(0);
	gV0Daughter1 = dynamic_cast<StGlobalTrack*>(v0Daughter1);
	if (!gV0Daughter1) continue;
	// Got Daughter1
	v0Daughter2  = rcV0->daughter(1);
	gV0Daughter2 = dynamic_cast<StGlobalTrack*>(v0Daughter2);
	if (!gV0Daughter2) continue;
	// Got Daughter2
	v0Bounds1 = mRcTrackMap->equal_range(gV0Daughter1);
	v0Bounds2 = mRcTrackMap->equal_range(gV0Daughter2);
	for (rcTrackMapIter trkIter1 = v0Bounds1.first; trkIter1!=v0Bounds1.second; trkIter1++) {
	    mcDaughter1 = (*trkIter1).second->partnerMcTrack();
	    for (rcTrackMapIter trkIter2 = v0Bounds2.first; trkIter2!=v0Bounds2.second; trkIter2++) {
		mcDaughter2 = (*trkIter2).second->partnerMcTrack();
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
    
    cout << "Xis..." << endl;
    pair<rcTrackMapIter, rcTrackMapIter> xiBounds;
    pair<rcV0MapIter, rcV0MapIter> xiBoundsV0;
    
    StXiVertex*    rcXi;
    StV0Vertex*    rcV0ofXi;
    StTrack*       rcBachelor;
    StGlobalTrack* gRcBachelor;
    StMcTrack*     mcBachelor;
    StMcVertex*    mcXi;
    StMcVertex*    mcV0;
    // Loop over Xis
    for (StXiVertexIterator xvi = xis.begin(); xvi!=xis.end(); xvi++) {
	rcXi = *xvi;
	rcV0ofXi = rcXi->v0Vertex();
	rcBachelor = rcXi->bachelor();
	gRcBachelor = dynamic_cast<StGlobalTrack*>(rcBachelor);
	if (!gRcBachelor) continue;
	xiBounds = mRcTrackMap->equal_range(gRcBachelor);
	for (rcTrackMapIter trkIter3 = xiBounds.first; trkIter3!= xiBounds.second; trkIter3++){
	    mcBachelor = (*trkIter3).second->partnerMcTrack();
	    mcXi = mcBachelor->startVertex();
	    if (mcXi == primary || mcXi == 0) continue;
	    xiBoundsV0 = mRcV0Map->equal_range(rcV0ofXi);
	    for (rcV0MapIter v0Iter = xiBoundsV0.first; v0Iter!= xiBoundsV0.second; v0Iter++){
		mcV0 = (*v0Iter).second;
		if (mcXi == mcV0->parent()->startVertex()) {
		    // Got a Xi candidate
		    mRcXiMap->insert(rcXiMapValType (rcXi, mcXi));
		    mMcXiMap->insert(mcXiMapValType (mcXi, rcXi));
		    
		}
	    }
	}
    }
      
    return kStOK;
}
