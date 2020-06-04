/*************************************************
 *
 * $Id: StAssociationMaker.cxx,v 1.62 2018/01/04 18:07:06 genevb Exp $
 * $Log: StAssociationMaker.cxx,v $
 * Revision 1.62  2018/01/04 18:07:06  genevb
 * Better counting of MC track pings (common hits)
 *
 * Revision 1.61  2015/03/13 18:44:44  perev
 * Roll back
 *
 * Revision 1.59  2014/06/24 20:57:35  jeromel
 * Remove flush()
 *
 * Revision 1.58  2012/07/31 22:08:49  perev
 * Track without detectorInfo ignored
 *
 * Revision 1.57  2011/07/19 19:11:15  perev
 * Cleanup
 *
 * Revision 1.56  2011/06/03 17:14:09  fisyak
 * Set FtpcHit IdTruth and  QA
 *
 * Revision 1.55  2011/04/01 19:40:07  perev
 * const++
 *
 * Revision 1.54  2010/10/11 19:15:25  fisyak
 * remove find_if due to bug in logic of its usage
 *
 * Revision 1.53  2010/06/22 22:06:33  fine
 * roll back the previous version to restore the nightly builds
 *
 * Revision 1.51  2009/11/10 20:19:36  fisyak
 * Change default to ITTF
 *
 * Revision 1.50  2007/04/28 17:55:43  perev
 * Redundant StChain.h removed
 *
 * Revision 1.49  2006/05/30 22:56:20  calderon
 * The check for null StEvent pointer had been moved down, move it back up
 * near the beginning of the Make() call, otherwise the pointer is used
 * without a check.
 *
 * Revision 1.48  2005/11/22 21:44:16  fisyak
 * Add Ssd to Associator, add IdTruth options for Svt and Ssd
 *
 * Revision 1.47  2005/09/28 21:31:46  fisyak
 * Persistent StMcEvent
 *
 * Revision 1.46  2005/07/19 21:28:56  perev
 * IdTruth
 *
 * Revision 1.45  2005/07/19 20:09:57  perev
 * IdTruth changes
 *
 * Revision 1.44  2004/06/01 18:06:02  calderon
 * Check for chiSquared values for the V0's when using/not using Est tracks.
 *
 * Revision 1.43  2004/03/26 23:26:34  calderon
 * -Adding switch to control Association based on IdTruth or on Distance.
 * -Adding debug2 output to print out all hits in a padrow, both reco and MC,
 * along with the IdTruth, quality (StHit) and the parentTrack()->key() (StMcHit)
 * as well as the hit coordiantes.  This is useful for debugging, but produces
 * lots of output, so it is only turned on in Debug()>=2.
 *
 * Revision 1.42  2004/02/13 22:39:39  calderon
 * At Helen's request, during the opton for associating est tracks,
 * if there is not an est track in the node, use the original global track.
 *
 * Revision 1.41  2004/02/08 00:08:14  calderon
 * Added method useEstTracks() requested by Helen, for association of estGlobals.
 *
 * Revision 1.40  2004/01/24 03:31:09  calderon
 * Changed the code to make it backward compatible.
 *
 * Revision 1.39  2004/01/13 21:04:54  fisyak
 * use IdTruth information for hit matching if any
 *
 * Revision 1.38  2003/10/08 20:15:03  calderon
 * using "Stiostream.h" and std::cout, std::ostream, as well as <cmath>.
 *
 * Revision 1.37  2003/09/02 17:55:28  perev
 * gcc 3.2 updates + WarnOff
 *
 * Revision 1.36  2003/06/27 03:01:18  calderon
 * The z cut now depends on z_mc.
 * The parameterization is done in the parameter DB
 * with a linearly increasing rms, symmetric in +/- z.
 *
 * Revision 1.35  2003/05/28 17:57:50  calderon
 * No longer use find_if for FTPC to solve a bug for y<0 found by Frank Simon.
 * Initialize tpcHitDistance and svtHitDistance to avoid a warning.
 *
 * Revision 1.34  2002/09/21 19:48:44  calderon
 * change encoded method for IT tracks to 263 (was 32770 before)
 *
 * Revision 1.33  2002/04/11 22:11:10  calderon
 * Changes to incorporate association of tracks from Sti
 * Basically all that was needed was to add a flag to switch between
 * Sti and regular EGR global tracks, and based on this flag, one looks for
 * the proper bits to be set in the StTrack::encodedMethod() member function.
 * Current default behaviour is still to use EGR global tracks, and the modification
 * is done in the macro with a call to StAssociationMaker::useInTracker()
 *
 * Revision 1.32  2001/07/16 17:18:42  calderon
 * Modification in the access of the L3 event at the beginning of loop.
 *
 * Revision 1.31  2001/05/09 21:25:59  calderon
 * Added debug messages to see where do we exceed the size of the candidate vector
 *
 * Revision 1.30  2001/05/08 21:29:56  calderon
 * Resize the candidates vector when we have more than 20 candidates.
 *
 * Revision 1.29  2001/04/27 18:41:47  calderon
 * Update with switches to use L3 Trigger.
 *
 * Revision 1.28  2000/06/09 19:54:02  calderon
 * use the new StMcHitComparisons
 * use the message manager more extensively
 * protection against the absence of hit collections
 *
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


#include "Stiostream.h"
#include <iterator>
#include <stdlib.h>
#include <string>
#include <vector>
#include <algorithm>
#include <cmath>
#if !defined(ST_NO_NAMESPACES)
using std::string;
using std::vector;
using std::cout;
using std::ostream;
#endif
#include "StAssociationMaker.h"
#include "StMcParameterDB.h"
#include "StTrackPairInfo.hh"

#include "StGlobals.hh"
#include "PhysicalConstants.h"
#include "SystemOfUnits.h"
#include "StThreeVectorF.hh"

#include "StMessMgr.h"

#include "St_DataSet.h"
#include "St_DataSetIter.h"
#include "TH2.h"

#include "StEventTypes.h"

#include "StMcEventTypes.hh"

#include "StMcEvent.hh"

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
		    const pair< const StGlobalTrack* const, StTrackPairInfo*>& p)
{
  out << "const StGlobalTrack at : " << (void*)p.first << endl;
  out << "const StMcTrack     at : " << (void*)(p.second->partnerMcTrack()) << endl;
  out << "Common TPC  Hits : " << p.second->commonTpcHits()  << endl;
  out << "Common SVT  Hits : " << p.second->commonSvtHits()  << endl;
  out << "Common SSD  Hits : " << p.second->commonSsdHits()  << endl;
  out << "Common FTPC Hits : " << p.second->commonFtpcHits() << endl;
  
  return out;
}
ostream& operator<<(ostream& out,
		    const pair< const StMcTrack* const, StTrackPairInfo*>& p)
{
  out << "const StMcTrack at     : " << (void*)p.first << endl;
  out << "const StGlobalTrack at : " << (void*)(p.second->partnerTrack()) << endl;
  out << "Common TPC  Hits : " << p.second->commonTpcHits()  << endl;
  out << "Common SVT  Hits : " << p.second->commonSvtHits()  << endl;
  out << "Common SSD  Hits : " << p.second->commonSsdHits()  << endl;
  out << "Common FTPC Hits : " << p.second->commonFtpcHits() << endl;
  
  return out;
}

// Print out the Track multimaps
ostream& operator<<(ostream& out, const rcTrackMapType& tm)
{
#if __SUNPRO_CC != 0x500
  copy(tm.begin(),tm.end(), ostream_iterator<rcTrackMapValType>(out,"\n"));
  
#else
  out << "Sorry, can't use ostream_iterator with this version of Stiostream.h !" << endl ;
#endif
  return out;
}

ostream& operator<<(ostream& out, const mcTrackMapType& tm)
{
#if __SUNPRO_CC != 0x500
  copy(tm.begin(),tm.end(), ostream_iterator<mcTrackMapValType>(out,"\n"));
#else
  out << "Sorry, can't use ostream_iterator with this version of Stiostream.h !" << endl ;
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
  mRcSsdHitMap  = 0;
  mMcSsdHitMap  = 0;
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
  mSsdHitResolution      = 0;   
  mFtpcHitResolution     = 0;
  
  doPrintMemoryInfo = kFALSE;
  mL3TriggerOn = false;
  mInTrackerOn = kTRUE;
  mEstTracksOn = false;
  mDistanceAssoc = kFALSE;
}

//_________________________________________________
StAssociationMaker::~StAssociationMaker()
{
  //  StAssociationMaker Destructor
  if(Debug()) gMessMgr->Info() << "Inside StAssociationMaker Destructor" << endm;
  
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
  if (mRcSsdHitMap) {
    mRcSsdHitMap->clear();
    SafeDelete(mRcSsdHitMap);
  }
  if (mMcSsdHitMap) {
    mMcSsdHitMap->clear();
    SafeDelete(mMcSsdHitMap);
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
  if (mRcSsdHitMap) {
    mRcSsdHitMap->clear();
    SafeDelete(mRcSsdHitMap);
    if (doPrintMemoryInfo) 
      cout << "Deleted Rec. Ssd Hit Map" << endl;
  }
  if (mMcSsdHitMap) {
    mMcSsdHitMap->clear();
    SafeDelete(mMcSsdHitMap);
    if (doPrintMemoryInfo) 
      cout << "Deleted M.C. Ssd Hit Map" << endl;
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
  // SSD
  //
  mSsdHitResolution = new TH2F("SsdHitResolution",
			       "Delta Z Vs Delta X for Nearby Hits",
			       50, -0.12, 0.12,
			       50, -0.12, 0.12);
  mSsdHitResolution->SetXTitle("Xmc - Xrec (cm)");
  mSsdHitResolution->SetYTitle("Zmc - Zrec (cm)");
  
  //
  // FTPC
  //
  mFtpcHitResolution = new TH2F("FtpcHitResolution",
				"Delta Z Vs Delta X for Nearby Hits",
				50, -0.32, 0.32,
				50, -8, 0.8);
  mFtpcHitResolution->SetXTitle("Rmc - Rrec (cm)");
  mFtpcHitResolution->SetYTitle("PHImc - PHIrec (deg)");
  
  gMessMgr->Info() << "Cuts used in association for this run: " << endm;
  gMessMgr->Info() << "\n" << *(StMcParameterDB::instance()) << "\n" << endm;
  if (mInTrackerOn) {
    gMessMgr->Info() << "Using IT Tracks in Association" << endm;
  }
  if (mEstTracksOn) {
    gMessMgr->Info() << "Using EST Tracks in Association" << endm;
  }
  return StMaker::Init();
}

//_________________________________________________

Int_t StAssociationMaker::Make()
{
  gMessMgr->Info() << "AssociationMaker::Make()" << endm;
  if (doPrintMemoryInfo) 
    StMemoryInfo::instance()->snapshot();
  
  if (mL3TriggerOn && mInTrackerOn) {
    gMessMgr->Warning() << "AssociationMaker::Make(): L3 and IT Tracks are both on!" << endm;
    return kStWarn;
  }
  if (mEstTracksOn && mInTrackerOn) {
    gMessMgr->Warning() << "AssociationMaker::Make(): EST and IT Tracks are both on!" << endm;
    return kStWarn;
  }
  if (Debug()) {
    if (mDistanceAssoc==true) {
      gMessMgr->Info() << "Using Distance Association" << endm;
    }
    else {
      gMessMgr->Info() << "Using Id Association" << endm;
    }
    
  }
  //
  // Get StEvent
  //
  StEvent* rEvent = 0;
  rEvent = (StEvent*) GetInputDS("StEvent");
  if (!rEvent) {
    gMessMgr->Warning() << "No StEvent!!! " << endm;
    gMessMgr->Warning() << "Bailing out ..." << endm;
    return kStWarn;
  }
  // Make the pointers to collections.
  // Tpc
  StTpcHitCollection*   rcTpcHitColl; 
  StMcTpcHitCollection* mcTpcHitColl; 
  // Svt				
  StSvtHitCollection*   rcSvtHitColl; 
  StMcSvtHitCollection* mcSvtHitColl; 
  // Ssd				
  StSsdHitCollection*   rcSsdHitColl; 
  StMcSsdHitCollection* mcSsdHitColl; 
  // Ftpc
  StFtpcHitCollection*   rcFtpcHitColl;
  StMcFtpcHitCollection* mcFtpcHitColl;
  

  // In order to make associations with the reconstructed L3 EVENT
  // we use the L3 hit collections, the switch below selects
  // the appropriate pointers.
  StL3Trigger* rL3Event = 0;
  if (!mL3TriggerOn) {
      // This case is for normal case, use hit collections from StEvent
      rcTpcHitColl = rEvent->tpcHitCollection();   
      rcSvtHitColl = rEvent->svtHitCollection();   
      rcSsdHitColl = rEvent->ssdHitCollection();   
      rcFtpcHitColl = rEvent->ftpcHitCollection();
  }
  else {
      // This case is for using the L3 Event
      if (rEvent){
	  rL3Event = rEvent->l3Trigger();
      }
    
      if (rL3Event) {
	  rcTpcHitColl = rL3Event->tpcHitCollection();
      }
      else {
	return kStWarn ;
      }
    
      rcSvtHitColl = 0;
      rcSsdHitColl = 0;
      rcFtpcHitColl = 0;
      
  }
  
  
  StSPtrVecTrackNode& rcTrackNodes = (!mL3TriggerOn) ? rEvent->trackNodes() : rL3Event->trackNodes();
  
  //
  // Get StMcEvent
  //
  StMcEvent* mEvent = 0;
  mEvent = (StMcEvent*) GetDataSet("StMcEvent");
  if (!mEvent) {
    gMessMgr->Error() << "No StMcEvent!!! " << endm;
    gMessMgr->Error() << "Bailing out ..." << endm;
    return kStWarn;
  }
  
  //
  // Get the Pointers to the Collections.
  //
  
  mcTpcHitColl = mEvent->tpcHitCollection();   
  mcSvtHitColl = mEvent->svtHitCollection();   
  mcSsdHitColl = mEvent->ssdHitCollection();   
  mcFtpcHitColl = mEvent->ftpcHitCollection();
  
  // Get the pointer to the parameter DB,
  // the definitions of the cuts
  // should be done at the macro level.
  
  StMcParameterDB* parDB = StMcParameterDB::instance();
  
  
  //
  // Loop over TPC hits and make Associations
  //
  if (rcTpcHitColl && mcTpcHitColl) {
    if (Debug()) gMessMgr->Info() << "Making TPC Hit Associations..." << endm;
    
    StTpcHit*   rcTpcHit;
    StMcTpcHit* mcTpcHit;
    
    // Instantiate the Tpc Hit maps
    mRcTpcHitMap = new rcTpcHitMapType;
    mMcTpcHitMap = new mcTpcHitMapType;
    
    int matchedR = 0;
    
    float tpcHitDistance = 9999;
    if (Debug()) cout << "In Sector : ";
    
    for (unsigned int iSector=0;
	 iSector<rcTpcHitColl->numberOfSectors(); iSector++) {
      
      if (Debug()) cout << iSector + 1 << " "; 
      StTpcSectorHitCollection* tpcSectHitColl = rcTpcHitColl->sector(iSector);
      for (unsigned int iPadrow=0;
	   iPadrow<tpcSectHitColl->numberOfPadrows();
	   iPadrow++) {
	StTpcPadrowHitCollection* tpcPadRowHitColl = tpcSectHitColl->padrow(iPadrow);
	//PR(iPadrow);
	int padrowMatchesId = 0;
	int padrowMatchesDi = 0;
	if (Debug()>=2 && iPadrow>=0) {
	  cout << endl;
	  cout << "Padrow " << iPadrow+1 << endl;
	  cout << "Reco hit index \tX pos\tY pos\tZ pos\tmIdTruth" << endl;
	  // In debug2 mode, print out all the reco hits and all the MC hits in this padrow
	  for (unsigned int iHit=0;
	       iHit<tpcPadRowHitColl->hits().size();
	       iHit++) {
	    rcTpcHit = tpcPadRowHitColl->hits()[iHit];
	    int qatru=rcTpcHit->qaTruth(); int idtru= rcTpcHit->idTruth();
	    cout << iHit << "\t" << rcTpcHit->position() << "\t" << idtru << "\t" << qatru << endl;
	  } // reco hits in debug2
	  cout << "MC Hit Key\tX pos\tY pos\tZ pos\tparent key" << endl;		    
	  for (StMcTpcHitIterator jHit = mcTpcHitColl->sector(iSector)->padrow(iPadrow)->hits().begin();
	       jHit != mcTpcHitColl->sector(iSector)->padrow(iPadrow)->hits().end();
	       jHit++){
	    mcTpcHit = *jHit;
	    cout << mcTpcHit->key()  << "\t" << mcTpcHit->position() << "\t" << mcTpcHit->parentTrack()->key() << endl;
	    
	  } // mc hits in debug2
	}// end of debug2 mode
	for (unsigned int iHit=0; iHit<tpcPadRowHitColl->hits().size(); iHit++){
	  //PR(iHit); 
	  
	  rcTpcHit = tpcPadRowHitColl->hits()[iHit];
	  
	  // switch the algorithm based on the option
	  // in shorthand:
	  // if (distance Association || !idTruth) {
	  //  do distance association
	  //  } else {
	  //  do id association
	  //  }
	  
	  //gMessMgr->Info() << "Backward compatibility mode:: distance cut association" << endm;
	  // Set the reference z for the comparison function
	  // The comparison will be used to find the first Mc Hit
	  // with a z greater than this reference, so that we don't loop
	  // over the hits that we don't need to.
	  StMcHit* closestTpcHit = 0;
	  
	  float xDiff, yDiff, zDiff;
	  xDiff = yDiff = zDiff = -999;
	  for (StMcTpcHitIterator jHit = mcTpcHitColl->sector(iSector)->padrow(iPadrow)->hits().begin();
	       jHit != mcTpcHitColl->sector(iSector)->padrow(iPadrow)->hits().end();
	       jHit++){
	    //PR(jHit); 
	    mcTpcHit = *jHit;
	    //int idMc = 0; // for case 2
	    //if (mcTpcHit->parentTrack()) idMc = mcTpcHit->parentTrack()->key(); // case 2
	    //if (idMc != rcTpcHit->idTruth()) continue; // case 2
	    //if (! mcTpcHit->parentTrack()->key()) { // no Id -> take one in the window 
	    if (mDistanceAssoc || !rcTpcHit->idTruth()) {
	      xDiff = mcTpcHit->position().x()-rcTpcHit->position().x();
	      yDiff = mcTpcHit->position().y()-rcTpcHit->position().y();
	      zDiff = mcTpcHit->position().z()-rcTpcHit->position().z();
	      if (!closestTpcHit) {
		tpcHitDistance=xDiff*xDiff+zDiff*zDiff;
		closestTpcHit = mcTpcHit;
	      }
	      if (xDiff*xDiff+zDiff*zDiff<tpcHitDistance) {
		tpcHitDistance = xDiff*xDiff+zDiff*zDiff;
		closestTpcHit = mcTpcHit;
	      }
	      
	      if ( fabs(xDiff)>= parDB->xCutTpc() ||
		   fabs(yDiff)>= parDB->yCutTpc() ||
		   fabs(zDiff)>= parDB->zCutTpc(mcTpcHit->position().z())) continue;
	    } else 
	      if (mcTpcHit->parentTrack()->key() != rcTpcHit->idTruth()) continue;
	    // Note: Association is within sector and pad row, so a Monte Carlo
	    // looper may have multiple Monte Carlo hits from one track associated
	    // to one and the same reconstructed hit!
	    // Make Associations  Use maps,
	    mRcTpcHitMap->insert(rcTpcHitMapValType (rcTpcHit, mcTpcHit) );
	    mMcTpcHitMap->insert(mcTpcHitMapValType (mcTpcHit, rcTpcHit) );
	    rcTpcHit->SetBit(StMcHit::kMatched,1);
	    mcTpcHit->SetBit(StMcHit::kMatched,1);
	    ++matchedR;
	    ++padrowMatchesDi;
	  }
	  if (Debug()==2)
	    if (closestTpcHit)
	      mTpcLocalHitResolution->Fill(closestTpcHit->position().x()-
					   rcTpcHit->position().x(),
					   closestTpcHit->position().z()-
					   rcTpcHit->position().z() );			
	} // End of Hits in Padrow loop for MC Hits
	if (Debug()>=2 && iPadrow>=0) {
	  cout << "Reco  Hits in Padrow   " << tpcPadRowHitColl->hits().size() << endl;
	  cout << "Id Matches in Padrow   " << padrowMatchesId << endl;
	  cout << "Distance Matches in pr " << padrowMatchesDi << endl;
	}
      } // End of Hits in Padrow loop for Rec. Hits
    } // End of Sector Loop for Rec. Hits
    // check non associated Mc hits 
    if (Debug()) {
      cout << "\n";
      gMessMgr->Info() << "Number of Entries in TPC Hit Maps: " << mRcTpcHitMap->size() 
		       << " counter " << matchedR << endm;
    }
    if (doPrintMemoryInfo) {
      if (Debug()) gMessMgr->Info() << "End of TPC Hit Associations\n" << endm;
      StMemoryInfo::instance()->snapshot();
      StMemoryInfo::instance()->print();
    }
  }
  //
  // Loop over SVT hits and make Associations
  //
  if (rcSvtHitColl && mcSvtHitColl) {
    
    if (Debug()) gMessMgr->Info() << "Making SVT Hit Associations..." << endm;
    
    StSvtHit*   rcSvtHit;
    StMcSvtHit* mcSvtHit;
    
    // Instantiate the Svt Hit maps
    mRcSvtHitMap = new rcSvtHitMapType;
    mMcSvtHitMap = new mcSvtHitMapType;
    
    float svtHitDistance = 9999;
    unsigned int nSvtHits = rcSvtHitColl->numberOfHits();
    if (Debug()) cout << "In Barrel : ";
    for (unsigned int iBarrel=0;  nSvtHits &&
	   iBarrel<rcSvtHitColl->numberOfBarrels(); iBarrel++) {
      
      if (Debug()) cout << iBarrel + 1 << " "; 
      
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
	      if (mDistanceAssoc || !rcSvtHit->idTruth()) {
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
		
		if ( fabs(xDiff)>= parDB->xCutSvt() ||
		     fabs(yDiff)>= parDB->yCutSvt() ||
		     fabs(zDiff)>= parDB->zCutSvt()) continue;
	      } else // new case, idTruth information exists
		if (mcSvtHit->parentTrack()->key() != rcSvtHit->idTruth()) continue;
	      // Make Associations  Use maps,
	      mRcSvtHitMap->insert(rcSvtHitMapValType (rcSvtHit, mcSvtHit) );
	      mMcSvtHitMap->insert(mcSvtHitMapValType (mcSvtHit, rcSvtHit) );
	      rcSvtHit->SetBit(StMcHit::kMatched,1);
	      mcSvtHit->SetBit(StMcHit::kMatched,1);
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
    
    if (Debug())  {
      cout << "\n";
      gMessMgr->Info() << "Number of Entries in SVT Hit Maps: " << mRcSvtHitMap->size() << endm;
    }
    if (doPrintMemoryInfo) {
      if (Debug()) gMessMgr->Info() << "End of SVT Hit Associations\n" << endm;
      StMemoryInfo::instance()->snapshot();
      StMemoryInfo::instance()->print();
    }
  }
  //
  // Loop over SSD hits and make Associations
  //
  if (rcSsdHitColl && mcSsdHitColl) {
    
    if (Debug()) gMessMgr->Info() << "Making SSD Hit Associations..." << endm;
    
    StSsdHit*   rcSsdHit;
    StMcSsdHit* mcSsdHit;
    
    // Instantiate the Ssd Hit maps
    mRcSsdHitMap = new rcSsdHitMapType;
    mMcSsdHitMap = new mcSsdHitMapType;
    
    float ssdHitDistance = 9999;
    unsigned int nSsdHits = rcSsdHitColl->numberOfHits();
    if (Debug()) cout << "In Ladder : ";
    for (unsigned int iLadder=0;  nSsdHits &&
	   iLadder<rcSsdHitColl->numberOfLadders(); iLadder++) {
      
      if (Debug()) cout << iLadder + 1 << " "; 
      
      for (unsigned int iWafer=0;
	   iWafer<rcSsdHitColl->ladder(iLadder)->numberOfWafers();
	   iWafer++) {
	//PR(iWafer);
	
	for (unsigned int iHit=0;
	     iHit<rcSsdHitColl->ladder(iLadder)->wafer(iWafer)->hits().size();
	     iHit++){
	  //PR(iHit); 
	  rcSsdHit = rcSsdHitColl->ladder(iLadder)->wafer(iWafer)->hits()[iHit];
	  
	  StMcSsdHit* closestSsdHit = 0;
	  float newDist = 0;
	  for (unsigned int jHit=0;
	       jHit<mcSsdHitColl->ladder(iLadder)->wafer(iWafer)->hits().size();
	       jHit++){
	    //PR(jHit); 
	    mcSsdHit = mcSsdHitColl->ladder(iLadder)->wafer(iWafer)->hits()[jHit];
	    float xDiff = mcSsdHit->position().x() - rcSsdHit->position().x();
	    float yDiff = mcSsdHit->position().y() - rcSsdHit->position().y();
	    float zDiff = mcSsdHit->position().z() - rcSsdHit->position().z();
	    if ( zDiff > parDB->zCutSsd() ) break; //mc hits are sorted, save time!
	    
	    if (jHit==0) {
	      ssdHitDistance=xDiff*xDiff+yDiff*yDiff+zDiff*zDiff;
	      closestSsdHit = mcSsdHit;
	    }
	    if ( (newDist = xDiff*xDiff+yDiff*yDiff+zDiff*zDiff) < ssdHitDistance) {
	      ssdHitDistance = newDist;
	      closestSsdHit = mcSsdHit;
	    }
	    if (mDistanceAssoc || !rcSsdHit->idTruth()) {
	      if ( fabs(xDiff)>= parDB->xCutSsd() ||
		   fabs(yDiff)>= parDB->yCutSsd() ||
		   fabs(zDiff)>= parDB->zCutSsd()) continue;
	    } else 
	      if (mcSsdHit->parentTrack()->key() != rcSsdHit->idTruth()) continue;
	    // Make Associations  Use maps,
	    mRcSsdHitMap->insert(rcSsdHitMapValType (rcSsdHit, mcSsdHit) );
	    mMcSsdHitMap->insert(mcSsdHitMapValType (mcSsdHit, rcSsdHit) );
	    rcSsdHit->SetBit(StMcHit::kMatched,1);
	    mcSsdHit->SetBit(StMcHit::kMatched,1);
	  } // End of Hits in Wafer loop for MC Hits
	  if (closestSsdHit)
	    mSsdHitResolution->Fill(closestSsdHit->position().x()-
				    rcSsdHit->position().x(),
				    closestSsdHit->position().z()-
				    rcSsdHit->position().z() );
	} // End of Hits in Wafer loop for Rec. Hits
      } // End of Wafer Loop for Rec. Hits
    } // End of Ladder Loop for Rec. Hits
    
    if (Debug())  {
      cout << "\n";
      gMessMgr->Info() << "Number of Entries in SSD Hit Maps: " << mRcSsdHitMap->size() << endm;
    }
    if (doPrintMemoryInfo) {
      if (Debug()) gMessMgr->Info() << "End of SSD Hit Associations\n" << endm;
      StMemoryInfo::instance()->snapshot();
      StMemoryInfo::instance()->print();
    }
  }
  //
  // Loop over FTPC hits and make Associations
  //
  if (rcFtpcHitColl && mcFtpcHitColl) {
    if (Debug()) gMessMgr->Info() << "Making FTPC Hit Associations..." << endm;
    
    StFtpcHit*   rcFtpcHit = 0;
    StMcFtpcHit* mcFtpcHit = 0;
    
    // Instantiate the Ftpc Hit maps
    mRcFtpcHitMap = new rcFtpcHitMapType;
    mMcFtpcHitMap = new mcFtpcHitMapType;
    
    float ftpcHitDistance = 0;
    float minHitDistance = 0;
    if (Debug()) cout << "In Plane : ";
    for (unsigned int iPlane=0;
	 iPlane<rcFtpcHitColl->numberOfPlanes(); iPlane++) {
      
      if (Debug()) cout << iPlane + 1 << " "; 
      
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
	  
	  StMcFtpcHitIterator ftpcHitSeed = mcFtpcHitColl->plane(iPlane)->hits().begin();
	  bool isFirst = true;
	  for (StMcFtpcHitIterator jHit = ftpcHitSeed;
	       jHit != mcFtpcHitColl->plane(iPlane)->hits().end();
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
	      rcFtpcHit->SetBit(StMcHit::kMatched,1);
	      rcFtpcHit->setIdTruth(mcFtpcHit->parentTrack()->key(),100);
	      mcFtpcHit->SetBit(StMcHit::kMatched,1);
	    }
	    
	  } // End of Hits in PLANE loop for MC Hits
	  if (!isFirst)
	    mFtpcHitResolution->Fill(rDiffMin,phiDiffMin); //!
	} // End of Hits in Sector loop for Rec. Hits
      } // End of Sector Loop for Rec. Hits
    } // End of Plane Loop for Rec. Hits
    
    if (Debug()) {
      cout << "\n";
      gMessMgr->Info() << "Number of Entries in Ftpc Hit Maps: " << mRcFtpcHitMap->size() << endm;
    }
    if (doPrintMemoryInfo) {
      if (Debug()) gMessMgr->Info() << "End of FTPC Hit Associations\n" << endm;
      StMemoryInfo::instance()->snapshot();
      StMemoryInfo::instance()->print();
    }
    
  }
  //
  // Check that Hit Maps are big enough to Make Track Maps
  //
  bool smallTpcHitMap, smallSvtHitMap, smallSsdHitMap, smallFtpcHitMap;
  smallTpcHitMap = smallSvtHitMap = smallSsdHitMap = smallFtpcHitMap = false;
  
  if (mRcTpcHitMap && mRcTpcHitMap->size()>0 && mRcTpcHitMap->size() < parDB->reqCommonHitsTpc()) {
    gMessMgr->Warning() << "\n-----------  WARNING ---------------\n"
			<< "   The Tpc Hit Map is too small for   \n"
			<< "   any meaningful track association.  \n"
			<< " ------------------------------------ \n"
			<< "Entries in Hit Map  : " << mRcTpcHitMap->size()      << "\n"
			<< "Required Common Hits: " << parDB->reqCommonHitsTpc() << "\n"
			<< "Suggest increase distance cuts." << endm;
    smallTpcHitMap = true;
  }
  
  if (mRcSvtHitMap && mRcSvtHitMap->size()>0 && mRcSvtHitMap->size() < parDB->reqCommonHitsSvt()) {
    gMessMgr->Warning() << "\n-----------  WARNING ---------------\n"
			<< "   The Svt Hit Map is too small for   \n"
			<< "   any meaningful track association.  \n"
			<< " ------------------------------------ \n"
			<< "Entries in Hit Map  : " << mRcSvtHitMap->size()       << "\n"
			<< "Required Common Hits: " << parDB->reqCommonHitsSvt()  << "\n"
			<< "Suggest increase distance cuts." << endm;
    smallSvtHitMap = true;
  }
  if (mRcSsdHitMap && mRcSsdHitMap->size()>0 && mRcSsdHitMap->size() < parDB->reqCommonHitsSsd()) {
    gMessMgr->Warning() << "\n-----------  WARNING ---------------\n"
			<< "   The Ssd Hit Map is too small for   \n"
			<< "   any meaningful track association.  \n"
			<< " ------------------------------------ \n"
			<< "Entries in Hit Map  : " << mRcSsdHitMap->size()       << "\n"
			<< "Required Common Hits: " << parDB->reqCommonHitsSsd()  << "\n"
			<< "Suggest increase distance cuts." << endm;
    smallSsdHitMap = true;
  }
  if (mRcFtpcHitMap && mRcFtpcHitMap->size()>0 && mRcFtpcHitMap->size() < parDB->reqCommonHitsFtpc()) {
    gMessMgr->Warning() << "\n-----------  WARNING ---------------\n"
			<< "   The Ftpc Hit Map is too small for  \n"
			<< "   any meaningful track association.  \n"
			<< " ------------------------------------ \n"
			<< "Entries in Hit Map  : " << mRcFtpcHitMap->size()       << "\n"
			<< "Required Common Hits: " << parDB->reqCommonHitsFtpc()  << "\n"
			<< "Suggest increase distance cuts." << endm;
    smallFtpcHitMap = true;
  }
  
  if ((smallTpcHitMap && smallSvtHitMap && smallSsdHitMap && smallFtpcHitMap) ||
      (!mRcTpcHitMap && !mRcSvtHitMap && mRcSsdHitMap && !mRcFtpcHitMap)) {
    gMessMgr->Error() << "No Useful Hit Map to make Track Associations" << endm;
    return kStWarn;
  }
  
  //
  // Start doing Track Associations ----------------------
  //
  
  StTrackNode*   trkNode;
  const StGlobalTrack* rcTrack;
  
  StHit*     rcHit;
  StTpcHit*  rcKeyTpcHit;
  StSvtHit*  rcKeySvtHit;
  StSsdHit*  rcKeySsdHit;
  StFtpcHit* rcKeyFtpcHit;
  
  pair<rcTpcHitMapIter,rcTpcHitMapIter>   boundsTpc;
  pair<rcSvtHitMapIter,rcSvtHitMapIter>   boundsSvt;
  pair<rcSsdHitMapIter,rcSsdHitMapIter>   boundsSsd;
  pair<rcFtpcHitMapIter,rcFtpcHitMapIter> boundsFtpc;
  
  rcTpcHitMapIter  tpcHMIter;
  rcSvtHitMapIter  svtHMIter;
  rcSsdHitMapIter  ssdHMIter;
  rcFtpcHitMapIter ftpcHMIter;
  
  const StMcTpcHit* mcValueTpcHit;
  const StMcSvtHit* mcValueSvtHit;
  const StMcSsdHit* mcValueSsdHit;
  const StMcFtpcHit* mcValueFtpcHit;
  
  const StMcTrack* trackCand;
  StTrackPairInfo* trkPair;
  
  trackPing initializedTrackPing;
  initializedTrackPing.mcTrack = 0;
  initializedTrackPing.nPingsTpc = 0;
  initializedTrackPing.nPingsSvt = 0;
  initializedTrackPing.nPingsSsd = 0;
  initializedTrackPing.nPingsFtpc = 0;
  
  
  // Instantiate the Track map
  mRcTrackMap = new rcTrackMapType;
  mMcTrackMap = new mcTrackMapType;
  // Begin making associations
  if(Debug()) gMessMgr->Info() << "Making Track Associations..." << endm;
  
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
    vector<const StMcTrack*> pingedMcTracks(16, 0);
    unsigned int nPingedMcTracks = 0;

    trkNode = rcTrackNodes[trkNodeI]; // For a by-pointer collection we need to dereference once
    if (!mEstTracksOn)
      rcTrack = dynamic_cast<const StGlobalTrack*>(trkNode->track(global));
    else { // Helen wants to keep the old global track from this node 
      rcTrack = dynamic_cast<const StGlobalTrack*>(trkNode->track(global));
    }
    if (!rcTrack) 	continue; 
// 		If there are no Tpc Hits, skip trackif (!rcTrack->detectorInfo()) continue;
    if (!(rcTrack->detectorInfo())) {// There is no detectorInfo How it could be 
static int mykount=0; mykount++;
      Warning("Make","*** %d No detectorInfo, track ignored ***", mykount);
      continue;
    }
    if (!(rcTrack->detectorInfo()->hits().size())) continue; 
    if (mInTrackerOn  && rcTrack->encodedMethod()!=263) continue; //for IT Tracks, skip the old globals
    if (!mInTrackerOn && rcTrack->encodedMethod()==263) continue; //for old globals, skip the IT tracks
    unsigned int nCandidates = 0;
    
    
    
    //
    // Loop over the TPC hits of the track
    //
    if (mRcTpcHitMap) {
      StPtrVecHit recTpcHits   = rcTrack->detectorInfo()->hits(kTpcId);
      unsigned int recTpcHitI;
      for (recTpcHitI = 0; recTpcHitI < recTpcHits.size(); recTpcHitI++) {
	
	rcHit = recTpcHits[recTpcHitI];
	rcKeyTpcHit = dynamic_cast<StTpcHit*>(rcHit);
	
	if (!rcKeyTpcHit) continue;
        nPingedMcTracks = 0;
	boundsTpc = mRcTpcHitMap->equal_range(rcKeyTpcHit);
	
	for (tpcHMIter=boundsTpc.first; tpcHMIter!=boundsTpc.second; ++tpcHMIter) {
	  
	  mcValueTpcHit = (*tpcHMIter).second;
	  if (! mcValueTpcHit) continue; // I added 0 for unmatched RcTpcHit
	  trackCand = mcValueTpcHit->parentTrack();

          // Skip any candidate Monte Carlo Tracks we already counted for this reconstructed hit
          // This can happen for Monte Carlo loopers that have multiple hits on the same pad row
          bool countedMcTrack = false;
          for (unsigned int iMcTrack = 0; iMcTrack<nPingedMcTracks; iMcTrack++) {
            if (trackCand == pingedMcTracks[iMcTrack]) {
              countedMcTrack = true;
              break;
            }
          }
          if (countedMcTrack) continue;
          if (nPingedMcTracks>=pingedMcTracks.size()) pingedMcTracks.resize(nPingedMcTracks+16,0);
          pingedMcTracks[nPingedMcTracks] = trackCand;
          nPingedMcTracks++;
	  
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
	    for (unsigned int iCandidate=0; iCandidate<nCandidates; iCandidate++){ 
	      if (trackCand==candidates[iCandidate].mcTrack){
		candidates[iCandidate].nPingsTpc++;
		break;
	      }
	      if (iCandidate == (nCandidates-1)){
		candidates[nCandidates].mcTrack = trackCand;
		candidates[nCandidates].nPingsTpc  = 1;
		nCandidates++;
				// check that we don't overstep the bounds,
				// if so increase the size of the vector in steps of 20 candidates
		if (nCandidates>=candidates.size()) {
		  candidates.resize(nCandidates+20, initializedTrackPing);
		  if (Debug()) cout << "Resizing in the TPC hits of the track " << endl;
		}
		break;
	      }
	    } // candidate loop
	    
	  }
	} // mc hits in multimap
	
      } // Tpc Hits from Track from StEvent loop
    }
    //
    // Loop over the SVT hits of the track
    //
    if (mRcSvtHitMap) {
      StPtrVecHit recSvtHits   = rcTrack->detectorInfo()->hits(kSvtId);
      unsigned int recSvtHitI;
      for (recSvtHitI = 0; recSvtHitI < recSvtHits.size(); recSvtHitI++) {
	// Loop over the SVT hits of the track
	
	rcHit = recSvtHits[recSvtHitI];
	rcKeySvtHit = dynamic_cast<StSvtHit*>(rcHit);
	
	if (!rcKeySvtHit) continue;
        nPingedMcTracks = 0;
	boundsSvt = mRcSvtHitMap->equal_range(rcKeySvtHit);
	
	for (svtHMIter=boundsSvt.first; svtHMIter!=boundsSvt.second; ++svtHMIter) {
	  
	  mcValueSvtHit = (*svtHMIter).second;
	  trackCand = mcValueSvtHit->parentTrack();

          // Skip any candidate Monte Carlo Tracks we already counted for this reconstructed hit
          // This can happen for Monte Carlo loopers that have multiple hits on the same pad row
          bool countedMcTrack = false;
          for (unsigned int iMcTrack = 0; iMcTrack<nPingedMcTracks; iMcTrack++) {
            if (trackCand == pingedMcTracks[iMcTrack]) {
              countedMcTrack = true;
              break;
            }
          }
          if (countedMcTrack) continue;
          if (nPingedMcTracks>=pingedMcTracks.size()) pingedMcTracks.resize(nPingedMcTracks+16,0);
          pingedMcTracks[nPingedMcTracks] = trackCand;
          nPingedMcTracks++;
	  
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
	    for (unsigned int iCandidate=0; iCandidate<nCandidates; iCandidate++){ 
	      if (trackCand==candidates[iCandidate].mcTrack){
		candidates[iCandidate].nPingsSvt++;
		break;
	      }
	      if (iCandidate == (nCandidates-1)){
		candidates[nCandidates].mcTrack = trackCand;
		candidates[nCandidates].nPingsSvt  = 1;
		nCandidates++;
				// check that we don't overstep the bounds,
				// if so increase the size of the vector in steps of 20 candidates
		if (nCandidates>=candidates.size()) {
		  candidates.resize(nCandidates+20, initializedTrackPing);
		  if (Debug()) cout << "Resizing in the SVT hits of the track " << endl;
		}
		break;
	      }
	    } // candidate loop
	    
	  }
	} // mc hits in multimap
	
      } // Svt Hits from Track from StEvent loop
    }
    //
    // Loop over the SSD hits of the track
    //
    if (mRcSsdHitMap) {
      StPtrVecHit recSsdHits   = rcTrack->detectorInfo()->hits(kSsdId);
      unsigned int recSsdHitI;
      for (recSsdHitI = 0; recSsdHitI < recSsdHits.size(); recSsdHitI++) {
	// Loop over the SSD hits of the track
	
	rcHit = recSsdHits[recSsdHitI];
	rcKeySsdHit = dynamic_cast<StSsdHit*>(rcHit);
	
	if (!rcKeySsdHit) continue;
        nPingedMcTracks = 0;
	boundsSsd = mRcSsdHitMap->equal_range(rcKeySsdHit);
	
	for (ssdHMIter=boundsSsd.first; ssdHMIter!=boundsSsd.second; ++ssdHMIter) {
	  
	  mcValueSsdHit = (*ssdHMIter).second;
	  trackCand = mcValueSsdHit->parentTrack();

          // Skip any candidate Monte Carlo Tracks we already counted for this reconstructed hit
          // This can happen for Monte Carlo loopers that have multiple hits on the same pad row
          bool countedMcTrack = false;
          for (unsigned int iMcTrack = 0; iMcTrack<nPingedMcTracks; iMcTrack++) {
            if (trackCand == pingedMcTracks[iMcTrack]) {
              countedMcTrack = true;
              break;
            }
          }
          if (countedMcTrack) continue;
          if (nPingedMcTracks>=pingedMcTracks.size()) pingedMcTracks.resize(nPingedMcTracks+16,0);
          pingedMcTracks[nPingedMcTracks] = trackCand;
          nPingedMcTracks++;
	  
	  // At this point we have a candidate Monte Carlo Track
	  // If there are no candidates, create the first candidate.
	  // If already there, increment its nPings.
	  // If doesn't match any of the previous candidates, create new candidate.
	  
	  if (nCandidates == 0) {
	    candidates[0].mcTrack    = trackCand;
	    candidates[0].nPingsSsd  = 1;
	    nCandidates++;
	    
	  }
	  
	  else {
	    for (unsigned int iCandidate=0; iCandidate<nCandidates; iCandidate++){ 
	      if (trackCand==candidates[iCandidate].mcTrack){
		candidates[iCandidate].nPingsSsd++;
		break;
	      }
	      if (iCandidate == (nCandidates-1)){
		candidates[nCandidates].mcTrack = trackCand;
		candidates[nCandidates].nPingsSsd  = 1;
		nCandidates++;
				// check that we don't overstep the bounds,
				// if so increase the size of the vector in steps of 20 candidates
		if (nCandidates>=candidates.size()) {
		  candidates.resize(nCandidates+20, initializedTrackPing);
		  if (Debug()) cout << "Resizing in the SSD hits of the track " << endl;
		}
		break;
	      }
	    } // candidate loop
	    
	  }
	} // mc hits in multimap
	
      } // Ssd Hits from Track from StEvent loop
    }
    //
    // Loop over the FTPC hits of the track.  Note that there are 2 loops,
    // one for the West and one for the East FTPC, but probably if there are hits in one
    // there won't be in the other one.
    //
    
    if (mRcFtpcHitMap) {
      StPtrVecHit recFtpcHitsW = rcTrack->detectorInfo()->hits(kFtpcWestId);
      StPtrVecHit recFtpcHitsE = rcTrack->detectorInfo()->hits(kFtpcEastId);
      unsigned int recFtpcHitI;
      
      // Loop over the West FTPC hits of the track
      for (recFtpcHitI = 0; recFtpcHitI < recFtpcHitsW.size(); recFtpcHitI++) {
	
	rcHit = recFtpcHitsW[recFtpcHitI];
	rcKeyFtpcHit = dynamic_cast<StFtpcHit*>(rcHit);
	
	if (!rcKeyFtpcHit) continue;
        nPingedMcTracks = 0;
	boundsFtpc = mRcFtpcHitMap->equal_range(rcKeyFtpcHit);
	
	for (ftpcHMIter=boundsFtpc.first; ftpcHMIter!=boundsFtpc.second; ++ftpcHMIter) {
	  
	  mcValueFtpcHit = (*ftpcHMIter).second;
	  trackCand = mcValueFtpcHit->parentTrack();

          // Skip any candidate Monte Carlo Tracks we already counted for this reconstructed hit
          // This can happen for Monte Carlo loopers that have multiple hits on the same pad row
          bool countedMcTrack = false;
          for (unsigned int iMcTrack = 0; iMcTrack<nPingedMcTracks; iMcTrack++) {
            if (trackCand == pingedMcTracks[iMcTrack]) {
              countedMcTrack = true;
              break;
            }
          }
          if (countedMcTrack) continue;
          if (nPingedMcTracks>=pingedMcTracks.size()) pingedMcTracks.resize(nPingedMcTracks+16,0);
          pingedMcTracks[nPingedMcTracks] = trackCand;
          nPingedMcTracks++;
	  
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
	    for (unsigned int iCandidate=0; iCandidate<nCandidates; iCandidate++){ 
	      if (trackCand==candidates[iCandidate].mcTrack){
		candidates[iCandidate].nPingsFtpc++;
		break;
	      }
	      if (iCandidate == (nCandidates-1)){
		candidates[nCandidates].mcTrack = trackCand;
		candidates[nCandidates].nPingsFtpc  = 1;
		nCandidates++;
				// check that we don't overstep the bounds,
				// if so increase the size of the vector in steps of 20 candidates
		if (nCandidates>=candidates.size()) {
		  candidates.resize(nCandidates+20, initializedTrackPing);
		  if (Debug()) cout << "Resizing in the East FTPC hits of the track " << endl;
		}
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
        nPingedMcTracks = 0;
	boundsFtpc = mRcFtpcHitMap->equal_range(rcKeyFtpcHit);
	
	for (ftpcHMIter=boundsFtpc.first; ftpcHMIter!=boundsFtpc.second; ++ftpcHMIter) {
	  
	  mcValueFtpcHit = (*ftpcHMIter).second;
	  trackCand = mcValueFtpcHit->parentTrack();

          // Skip any candidate Monte Carlo Tracks we already counted for this reconstructed hit
          // This can happen for Monte Carlo loopers that have multiple hits on the same pad row
          bool countedMcTrack = false;
          for (unsigned int iMcTrack = 0; iMcTrack<nPingedMcTracks; iMcTrack++) {
            if (trackCand == pingedMcTracks[iMcTrack]) {
              countedMcTrack = true;
              break;
            }
          }
          if (countedMcTrack) continue;
          if (nPingedMcTracks>=pingedMcTracks.size()) pingedMcTracks.resize(nPingedMcTracks+16,0);
          pingedMcTracks[nPingedMcTracks] = trackCand;
          nPingedMcTracks++;
	  
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
	    for (unsigned int iCandidate=0; iCandidate<nCandidates; iCandidate++){ 
	      if (trackCand==candidates[iCandidate].mcTrack){
		candidates[iCandidate].nPingsFtpc++;
		break;
	      }
	      if (iCandidate == (nCandidates-1)){
		candidates[nCandidates].mcTrack = trackCand;
		candidates[nCandidates].nPingsFtpc  = 1;
		nCandidates++;
				// check that we don't overstep the bounds,
				// if so increase the size of the vector in steps of 20 candidates
		if (nCandidates>=candidates.size()) {
		  candidates.resize(nCandidates+20, initializedTrackPing);
		  if (Debug()) cout << "Resizing in the West FTPC hits of the track " << endl;
		}
		break;
	      }
	    } // candidate loop
	    
	  }
	} // mc hits in multimap
	
      } // Ftpc Hits from Track from StEvent loop
    }
    //
    // Now we need to associate the tracks that meet the commonHits criteria.
    //
    
    if (nCandidates>20 || candidates.size()>20)
      cout << "We Have " << candidates.size() << " candidates!!! " << endl;
    if (candidates.size()<nCandidates) {
      cout << "The candidate track vector has grown more than expected!! " << endl;
      cout << "Something is wrong! We probably went out-of-bounds! " << endl;
    }
    for (unsigned int iCandidate=0; iCandidate<nCandidates; iCandidate++){
      //mNumberOfPings->Fill((float) candidates[iCandidate].nPings);
      
      
      if (candidates[iCandidate].nPingsTpc  >= parDB->reqCommonHitsTpc() ||
	  candidates[iCandidate].nPingsSvt  >= parDB->reqCommonHitsSvt() ||
	  candidates[iCandidate].nPingsSsd  >= parDB->reqCommonHitsSsd() ||
	  candidates[iCandidate].nPingsFtpc >= parDB->reqCommonHitsFtpc()){
	// We got a track pair !!
	// Add it to multimap
	
	trkPair = new StTrackPairInfo(rcTrack,
				      candidates[iCandidate].mcTrack,
				      candidates[iCandidate].nPingsTpc,
				      candidates[iCandidate].nPingsSvt,
				      candidates[iCandidate].nPingsSsd,
				      candidates[iCandidate].nPingsFtpc);
	mRcTrackMap->insert(rcTrackMapValType (rcTrack, trkPair));
	mMcTrackMap->insert(mcTrackMapValType (candidates[iCandidate].mcTrack, trkPair));
	
      }
    }
    
    // Clear the candidate vector
    candidates.clear();
    
  }// StEvent track loop
  // print out the map
  if (Debug() > 1) {
    cout << "The RcTrack map is now" << endl << *mRcTrackMap << endl;
    cout << "The McTrack map is now" << endl << *mMcTrackMap << endl;
  }
  
  if(Debug()){
    gMessMgr->Info() << "Number of Entries in Track Maps: " << mRcTrackMap->size() << endm;
  }
  if (doPrintMemoryInfo) {
    cout << "End of Track Associations\n";
    StMemoryInfo::instance()->snapshot();
    StMemoryInfo::instance()->print();
  }
  
  //
  // Start doing Vertex Associations ----------------------
  //
  if (!mL3TriggerOn) {
    // Instantiate the Vertex maps
    mRcKinkMap = new rcKinkMapType;
    mMcKinkMap = new mcKinkMapType;
    mRcV0Map   = new rcV0MapType;
    mMcV0Map   = new mcV0MapType;
    mRcXiMap   = new rcXiMapType;
    mMcXiMap   = new mcXiMapType;
    // Begin making associations
    if(Debug()) gMessMgr->Info() << "Making Vertex Associations" << endm;
    
    StSPtrVecKinkVertex& kinks = rEvent->kinkVertices();
    
    
    if(Debug()) gMessMgr->Info() << "Kinks..." << endm;
    
    // Loop over Kinks
    
    pair<rcTrackMapIter, rcTrackMapIter> kinkBoundsDaughter, kinkBoundsParent;
    
    const StMcVertex* primary   = mEvent->primaryVertex();
    for (StKinkVertexIterator kvi = kinks.begin(); kvi!=kinks.end(); kvi++) {
      
      StKinkVertex* rcKink = *kvi; // Got Kink ...
      StTrack* kinkDaughter  = rcKink->daughter(0);
      const StGlobalTrack* gKinkDaughter = dynamic_cast<const StGlobalTrack*>(kinkDaughter);
      if (!gKinkDaughter) continue;
      // Got Daughter
      StTrack* kinkParent  = rcKink->parent();
      const StGlobalTrack* gKinkParent = dynamic_cast<const StGlobalTrack*>(kinkParent);
      if (!gKinkParent) continue;
      // Got Parent
      
      kinkBoundsDaughter = mRcTrackMap->equal_range(gKinkDaughter);
      // Loop over associated tracks of the daughter
      for (rcTrackMapIter trkIter = kinkBoundsDaughter.first; trkIter!=kinkBoundsDaughter.second; trkIter++) {
	const StMcTrack* mcDaughter = (*trkIter).second->partnerMcTrack(); // Get associated daughter
	
	const StMcVertex* mcKink = mcDaughter->startVertex(); // Get Kink candidate 
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
    if(Debug()){
      gMessMgr->Info() << "Number of Entries in kink Maps: " << mRcKinkMap->size() << endm;
    }
    if (doPrintMemoryInfo) {
      cout << "End of kink Associations\n";
      StMemoryInfo::instance()->snapshot();
      StMemoryInfo::instance()->print();
    }
    
    if(Debug()) gMessMgr->Info() << "V0s..." << endm;
    
    StSPtrVecV0Vertex& v0s = rEvent->v0Vertices();    
    
    // Loop over V0s
    for (StV0VertexIterator v0vi = v0s.begin(); v0vi!=v0s.end(); v0vi++) {
      StV0Vertex* rcV0 = *v0vi; // Got V0 ...
      StTrack* v0Daughter1  = rcV0->daughter(0);
      if (mEstTracksOn && rcV0->chiSquared()==-16) continue; //when Est runs, the X^2 is always -24 or less
      if (!mEstTracksOn && rcV0->chiSquared()<-16) continue; //when Est didn't run it's ALWAYS -16.
      const StGlobalTrack* gV0Daughter1 = dynamic_cast<const StGlobalTrack*>(v0Daughter1);
      if (!gV0Daughter1) continue;
      // Got Daughter1
      StTrack* v0Daughter2  = rcV0->daughter(1);
      const StGlobalTrack* gV0Daughter2 = dynamic_cast<const StGlobalTrack*>(v0Daughter2);
      if (!gV0Daughter2) continue;
      // Got Daughter2
      pair<rcTrackMapIter, rcTrackMapIter> v0Bounds1 = mRcTrackMap->equal_range(gV0Daughter1);
      pair<rcTrackMapIter, rcTrackMapIter> v0Bounds2 = mRcTrackMap->equal_range(gV0Daughter2);
      for (rcTrackMapIter trkIter1 = v0Bounds1.first; trkIter1!=v0Bounds1.second; trkIter1++) {
	const StMcTrack* mcDaughter1 = (*trkIter1).second->partnerMcTrack();
	for (rcTrackMapIter trkIter2 = v0Bounds2.first; trkIter2!=v0Bounds2.second; trkIter2++) {
	  const StMcTrack* mcDaughter2 = (*trkIter2).second->partnerMcTrack();
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
    if(Debug()) {
      gMessMgr->Info() << "Number of Entries in V0 Maps: " << mRcV0Map->size() << endm;
    }
    if (doPrintMemoryInfo) {
      cout << "End of V0 Associations\n";
      StMemoryInfo::instance()->snapshot();
      StMemoryInfo::instance()->print();
    }
    
    if(Debug()) gMessMgr->Info() << "Xis..." << endm;
    
    StSPtrVecXiVertex& xis = rEvent->xiVertices();    
    
    // Loop over Xis
    for (StXiVertexIterator xvi = xis.begin(); xvi!=xis.end(); xvi++) {
      StXiVertex* rcXi = *xvi;
      if (mEstTracksOn && rcXi->chiSquared()==-16) continue; //when Est runs, the X^2 is always -24 or less
      if (!mEstTracksOn && rcXi->chiSquared()<-16) continue; //when Est didn't run it's ALWAYS -16.
      StV0Vertex* rcV0ofXi = rcXi->v0Vertex();
      StTrack* rcBachelor = rcXi->bachelor();
      const StGlobalTrack* gRcBachelor = dynamic_cast<const StGlobalTrack*>(rcBachelor);
      if (!gRcBachelor) continue;
      pair<rcTrackMapIter, rcTrackMapIter> xiBounds = mRcTrackMap->equal_range(gRcBachelor);
      for (rcTrackMapIter trkIter3 = xiBounds.first; trkIter3!= xiBounds.second; trkIter3++){
	const StMcTrack*  mcBachelor = (*trkIter3).second->partnerMcTrack();
	const StMcVertex* mcXi = mcBachelor->startVertex();
	if (mcXi == primary || mcXi == 0) continue;
	pair<rcV0MapIter, rcV0MapIter> xiBoundsV0 = mRcV0Map->equal_range(rcV0ofXi);
	for (rcV0MapIter v0Iter = xiBoundsV0.first; v0Iter!= xiBoundsV0.second; v0Iter++){
	  const StMcVertex* mcV0 = (*v0Iter).second;
	  if (mcV0->parent() != 0 && mcXi == mcV0->parent()->startVertex()) {
	    // Got a Xi candidate
	    mRcXiMap->insert(rcXiMapValType (rcXi, mcXi));
	    mMcXiMap->insert(mcXiMapValType (mcXi, rcXi));
	    
	  }
	}
      }
    }
    if(Debug()) {
      gMessMgr->Info() << "Number of Entries in Xi Maps: " << mRcXiMap->size() << endm;
    }
    
  }
  if (doPrintMemoryInfo) {
    cout << "End of Make()\n";
    StMemoryInfo::instance()->snapshot();
    StMemoryInfo::instance()->print();
  }
  return kStOK;
}
