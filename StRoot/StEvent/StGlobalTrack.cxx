/***************************************************************************
 *
 * $Id: StGlobalTrack.cxx,v 1.4 1999/04/27 01:24:20 fisyak Exp $
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
 * Revision 1.4  1999/04/27 01:24:20  fisyak
 * Fix intermidaiate version with pointer instead of referencies
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
static const Char_t rcsid[] = "$Id: StGlobalTrack.cxx,v 1.4 1999/04/27 01:24:20 fisyak Exp $";
 * version with constructors for table-based loading
 *
static const Char_t rcsid[] = "$Id: StGlobalTrack.cxx,v 1.4 1999/04/27 01:24:20 fisyak Exp $";
#include "StGlobalTrack.h"
static const char rcsid[] = "$Id: StGlobalTrack.cxx,v 1.4 1999/04/27 01:24:20 fisyak Exp $";
StCollectionImp(GlobalTrack)
StGlobalTrack::StGlobalTrack()
{
    mSvtHits = 0;
    mFtpcHits = 0;
    mPidTraits = 0;
    mNumberOfTpcHits = 0;
    mNumberOfSvtHits = 0;
    mNumberOfFtpcHits = 0; 
                             StThreeVectorF& origin,
                             Double_t dip,
  StTrack(trk, curvature, dip, phase, origin, h)
                             double curvature,
			     int h) : 
    StTrack(trk, curvature, dip, phase, origin, h)
{  
    mSvtHits = 0;
    mFtpcHits = 0;
    mPidTraits = 0;
    mNumberOfTpcHits = 0;
    mNumberOfSvtHits = 0;
    mNumberOfFtpcHits = 0; 
}
static const char rcsid[] = "$Id: StGlobalTrack.cxx,v 1.4 1999/04/27 01:24:20 fisyak Exp $";
StGlobalTrack::~StGlobalTrack() {
    //
#if 0
    StTpcHitIterator i;  
    StSvtHitIterator j; 
    StFtpcHitIterator k;
    Int_t counter;
    for(i = mTpcHits->begin(); i != mTpcHits->end(); i++) {
	counter = (*i)->trackReferenceCount();
	(*i)->setTrackReferenceCount(counter > 0 ? counter-1 : 0);
    }
    for(j = mSvtHits->begin(); j != mSvtHits->end(); j++) {
	counter = (*j)->trackReferenceCount();
	(*j)->setTrackReferenceCount(counter > 0 ? counter-1 : 0);
    }
    for(k = mFtpcHits->begin(); k != mFtpcHits->end(); k++) {
	counter = (*k)->trackReferenceCount();
	(*k)->setTrackReferenceCount(counter > 0 ? counter-1 : 0);
    }
    
    if (mTpcDedx) { delete mTpcDedx; mTpcDedx=0; }
    if (mFtpcDedx) { delete mFtpcDedx; mFtpcDedx=0; }
    if (mSvtDedx) { delete mSvtDedx; mSvtDedx=0; }
#endif
    SafeDelete(mTpcHits);
    SafeDelete(mFtpcHits);
    SafeDelete(mTpcDedx);
    SafeDelete(mPidTraits);
    SafeDelete(mFtpcDedx);
    SafeDelete(mSvtDedx);
}
StGlobalTrack::StGlobalTrack() {/* noop */}
void StGlobalTrack::setTpcDedx(StDedx* val) { mTpcDedx = val; }   
StGlobalTrack::StGlobalTrack(const dst_track_st& track) : StTrack(track) {/* noop */}

void StGlobalTrack::setSvtDedx(StDedx* val) { mSvtDedx = val; }  

void StGlobalTrack::addTpcHit(StTpcHit* hit)

    mTpcHits->push_back(hit);
    mNumberOfTpcHits++;
    if (this != &track)
        static_cast<StTrack&>(*this) = track;
void StGlobalTrack::addFtpcHit(StFtpcHit* hit)
{
    mFtpcHits->push_back(hit);
    mNumberOfFtpcHits++;
}
}
void StGlobalTrack::addSvtHit(StSvtHit* hit)
{
    mSvtHits->push_back(hit);
    StTpcHitIterator iter = mTpcHits->begin();
#if 0
    StTpcHitIterator iter = find(mTpcHits->begin(), mTpcHits->end(), hit);
    if (iter != mTpcHits->end()) mTpcHits->erase(iter);
}
  while (mTpcHits->Contains(hit)) {
#endif
    Int_t i = hit->trackReferenceCount();
    hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
  }
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
    StFtpcHitIterator iter = mFtpcHits->begin();
#if 0
    StFtpcHitIterator iter = find(mFtpcHits->begin(), mFtpcHits->end(), hit);
    if (iter != mFtpcHits->end()) {
	mFtpcHits->erase(iter);
	Int_t i = hit->trackReferenceCount();
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
    }
#endif
    Int_t i = hit->trackReferenceCount();
    hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
  }
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
    StSvtHitIterator iter = mSvtHits->begin();
#if 0
    StSvtHitIterator iter = find(mSvtHits->begin(), mSvtHits->end(), hit);
    if (iter != mSvtHits->end()) {
	mSvtHits->erase(iter);
	Int_t i = hit->trackReferenceCount();
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
    }
#endif
    Int_t i = hit->trackReferenceCount();
    hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
StCollectionImp(GlobalTrack)
  }
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
	mNumberOfSvtHits--;
{
    mNumberOfFtpcHits = n;
}

const StVertex*
StGlobalTrack::vertex() const { return 0; }
