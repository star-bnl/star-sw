/***************************************************************************
 *
 * $Id: StGlobalTrack.cxx,v 1.8 1999/07/16 12:34:49 fisyak Exp $
 *
 * Author: Thomas Ullrich, Jan 1999
 *
 * History:
 * 15/01/1999 T. Wenaus  Add table-based constructor
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StGlobalTrack.cxx,v $
 * Revision 1.8  1999/07/16 12:34:49  fisyak
 * Fix PidTraits
 *
 * Revision 1.8  1999/07/16 12:34:49  fisyak
 * Fix PidTraits
 *
 * Revision 1.7  1999/06/16 10:50:16  ullrich
 * Added members to hold the number of hits in case
 * the hits are not stored on the DST. Sync changes in
 * StEvent with StRootEvent.
 *
 * Revision 1.6  1999/06/11 17:28:28  fisyak
 * Update remove member functions
 *
 * Revision 1.5  1999/04/28 22:27:32  fisyak
 * New version with pointer instead referencies
 *
 * Revision 1.12  1999/04/08 14:58:32  ullrich
 * Moved PID traits from StTrack to StGlobalTrack.
 *
 * Revision 1.11  1999/03/23 22:00:09  ullrich
 * Minor mods.
 *
 * Revision 1.10  1999/02/24 12:49:04  ullrich
 * Added argument (h) to constructor needed to instatiate helix
 *
 * Revision 1.9  1999/02/23 21:24:32  ullrich
 * Removed obsolete EMC/SMD hit information (future cluster).
 *
 * Revision 1.8  1999/02/22 19:53:51  wenaus
 * cleaner deleting
 *
 * Revision 1.7  1999/02/22 19:25:18  genevb
 * StGlobalTrack constructor had a memory leak
 *
 * Revision 1.6  1999/02/22 03:49:43  wenaus
 * more careful deletion handling
 *
 * Revision 1.5  1999/02/15 16:17:02  wenaus
 * fix Double_t& -> Double_t referencing bug
 *
 * Revision 1.4  1999/02/12 02:01:16  wenaus
 * New track constructor to load helix params independently of table
 *
 * Revision 1.3  1999/02/10 21:50:30  wenaus
 * Plug memory leaks
 *
 * Revision 1.2  1999/01/15 22:53:44  wenaus
 * version with constructors for table-based loading
 *
static const char rcsid[] = "$Id: StGlobalTrack.cxx,v 1.8 1999/07/16 12:34:49 fisyak Exp $";
 * Completely Revised for New Version
#include "StGlobalTrack.h"
static const char rcsid[] = "$Id: StGlobalTrack.cxx,v 1.8 1999/07/16 12:34:49 fisyak Exp $";
 
#include "tables/dst_track.h"
#include "StVertex.h"
StCollectionImp(GlobalTrack)
StGlobalTrack::StGlobalTrack()
{
    mTpcHits = 0;
    mSvtHits = 0;
    mFtpcHits = 0;
    mTpcDedx = 0; 
    mFtpcDedx = 0;
    mSvtDedx = 0; 
    mPidTraits = new StTrackPidTraits(this);
    mNumberOfTpcHits = 0;
    mNumberOfSvtHits = 0;
    mNumberOfFtpcHits = 0; 
}
ClassImp(StGlobalTrack)
StGlobalTrack::StGlobalTrack(dst_track_st* trk,
                             double curvature,
                             double dip,
                             double phase,
                             StThreeVectorD& origin,
			     int h) : 
    StTrack(trk, curvature, dip, phase, origin, h)
{  
    mTpcHits = 0;
    mSvtHits = 0;
    mFtpcHits = 0;
    mTpcDedx = 0; 
    mFtpcDedx = 0;
    mSvtDedx = 0; 
    mPidTraits = new StTrackPidTraits(this);
    mNumberOfTpcHits = 0;
    mNumberOfSvtHits = 0;
    mNumberOfFtpcHits = 0; 
}
static const char rcsid[] = "$Id: StGlobalTrack.cxx,v 1.8 1999/07/16 12:34:49 fisyak Exp $";
StGlobalTrack::~StGlobalTrack() {
    //
    //   If a track gets deleted make sure
    //   that the reference counters of the
    //   related hits get decreased.
    //   Don't use the removeXxxHit() methods
    //   here since they are too slow.
    //
    SafeDelete(mTpcHits);
    SafeDelete(mSvtHits);
    SafeDelete(mFtpcHits);
    SafeDelete(mTpcDedx);
    SafeDelete(mPidTraits);
    SafeDelete(mFtpcDedx);
    SafeDelete(mSvtDedx);
}
StGlobalTrack::StGlobalTrack() {/* noop */}
void StGlobalTrack::setTpcDedx(StDedx* val) { mTpcDedx = val; }   
StGlobalTrack::StGlobalTrack(const dst_track_st& track) : StTrack(track) {/* noop */}
void StGlobalTrack::setFtpcDedx(StDedx* val) { mFtpcDedx = val; } 

void StGlobalTrack::setSvtDedx(StDedx* val) { mSvtDedx = val; }  

void StGlobalTrack::addTpcHit(StTpcHit* hit)

    if (! mTpcHits) mTpcHits = new StVecPtrTpcHit;
    mTpcHits->push_back(hit);
    hit->setTrackReferenceCount(hit->trackReferenceCount()+1);
    mNumberOfTpcHits++;
    if (this != &track)
        static_cast<StTrack&>(*this) = track;
void StGlobalTrack::addFtpcHit(StFtpcHit* hit)
{
    if (! mFtpcHits) mFtpcHits = new StVecPtrFtpcHit;
    mFtpcHits->push_back(hit);
    hit->setTrackReferenceCount(hit->trackReferenceCount()+1);
    mNumberOfFtpcHits++;
}
}
void StGlobalTrack::addSvtHit(StSvtHit* hit)
{
    if (! mSvtHits) mSvtHits = new StVecPtrSvtHit;
    mSvtHits->push_back(hit);
    hit->setTrackReferenceCount(hit->trackReferenceCount()+1);
    mNumberOfSvtHits++;
}

void StGlobalTrack::removeTpcHit(StTpcHit* hit)
{
    while (mTpcHits->Contains(hit)) {
	mTpcHits->Remove(hit);
	int i = hit->trackReferenceCount();
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
	mNumberOfTpcHits--;
    }
}

void StGlobalTrack::removeFtpcHit(StFtpcHit* hit)
{
    while (mFtpcHits->Contains(hit)) {
	mFtpcHits->Remove(hit);
	int i = hit->trackReferenceCount();
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
	mNumberOfFtpcHits--;
    }
}

void StGlobalTrack::removeSvtHit(StSvtHit* hit)
{
    while (mSvtHits->Contains(hit)) {
	mSvtHits->Remove(hit);
	int i = hit->trackReferenceCount();
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
	mNumberOfSvtHits--;
    }
}

int StGlobalTrack::numberOfTpcHits() const
{
    if (mTpcHits)
	return mTpcHits->size() ? mTpcHits->size() : mNumberOfTpcHits;
    else
	return mNumberOfTpcHits;
}

int StGlobalTrack::numberOfSvtHits() const
{
	return mNumberOfTpcHits;	
	return mSvtHits->size() ? mSvtHits->size() : mNumberOfSvtHits;
    else
	return mNumberOfSvtHits;
}

int StGlobalTrack::numberOfFtpcHits() const
{
	return mNumberOfTpcHits;	
	return mFtpcHits->size() ? mFtpcHits->size() : mNumberOfFtpcHits;
    else
	return mNumberOfFtpcHits;	
}

void StGlobalTrack::setNumberOfTpcHits(unsigned char n)
{
    mNumberOfTpcHits = n;
}

void StGlobalTrack::setNumberOfSvtHits(unsigned char n)
{
    mNumberOfSvtHits = n;
}

void StGlobalTrack::setNumberOfFtpcHits(unsigned char n)
{
    mNumberOfFtpcHits = n;
}

const StVertex*
StGlobalTrack::vertex() const { return 0; }
