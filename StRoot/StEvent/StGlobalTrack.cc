/***************************************************************************
 *
 * $Id: StGlobalTrack.cc,v 1.14 1999/06/07 14:53:02 ullrich Exp $
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
 * $Log: StGlobalTrack.cc,v $
 * Revision 1.14  1999/06/07 14:53:02  ullrich
 * Added separate method to set and access number of hits in case the hit
 * collections aren't filled, ie no hits are stored on the DST.
 *
 * Revision 1.13  1999/05/22 19:30:10  perev
 * length & methods added
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
 * fix double& -> double referencing bug
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
 **************************************************************************/
#include "StEvent/StGlobalTrack.hh"

static const char rcsid[] = "$Id: StGlobalTrack.cc,v 1.14 1999/06/07 14:53:02 ullrich Exp $";
 
StGlobalTrack::StGlobalTrack() : mPidTraits(*this)
{
    mTpcDedx = 0; 
    mFtpcDedx = 0;
    mSvtDedx = 0;
    mNumberOfTpcHits = 0;
    mNumberOfSvtHits = 0;
    mNumberOfFtpcHits = 0; 
}  
   
StGlobalTrack::StGlobalTrack(dst_track_st* trk,
                             double curvature,
                             double dip,
                             double phase,
                             StThreeVector<double>& origin,
			     int h) : 
    StTrack(trk, curvature, dip, phase, origin, h), mPidTraits(*this)
{  
    mTpcDedx = 0; 
    mFtpcDedx = 0;
    mSvtDedx = 0; 
    mNumberOfTpcHits = 0;
    mNumberOfSvtHits = 0;
    mNumberOfFtpcHits = 0; 
    mLength = trk->length;
}

StGlobalTrack::~StGlobalTrack() {
    //
    //   If a track gets deleted make sure
    //   that the reference counters of the
    //   related hits get decreased.
    //   Don't use the removeXxxHit() methods
    //   here since they are too slow.
    //
    StVecPtrTpcHitIterator i;  
    StVecPtrSvtHitIterator j; 
    StVecPtrFtpcHitIterator k;
    int counter;
    for(i = mTpcHits.begin(); i != mTpcHits.end(); i++) {
	counter = (*i)->trackReferenceCount();
	(*i)->setTrackReferenceCount(counter > 0 ? counter-1 : 0);
    }
    for(j = mSvtHits.begin(); j != mSvtHits.end(); j++) {
	counter = (*j)->trackReferenceCount();
	(*j)->setTrackReferenceCount(counter > 0 ? counter-1 : 0);
    }
    for(k = mFtpcHits.begin(); k != mFtpcHits.end(); k++) {
	counter = (*k)->trackReferenceCount();
	(*k)->setTrackReferenceCount(counter > 0 ? counter-1 : 0);
    }
    
    if (mTpcDedx) { delete mTpcDedx; mTpcDedx=0; }
    if (mFtpcDedx) { delete mFtpcDedx; mFtpcDedx=0; }
    if (mSvtDedx) { delete mSvtDedx; mSvtDedx=0; }
}

void StGlobalTrack::setTpcDedx(StDedx* val) { mTpcDedx = val; }   

void StGlobalTrack::setFtpcDedx(StDedx* val) { mFtpcDedx = val; } 

void StGlobalTrack::setSvtDedx(StDedx* val) { mSvtDedx = val; }  

void StGlobalTrack::addTpcHit(StTpcHit* hit)
{
    mTpcHits.push_back(hit);
    hit->setTrackReferenceCount(hit->trackReferenceCount()+1);
    mNumberOfTpcHits++;
}

void StGlobalTrack::addFtpcHit(StFtpcHit* hit)
{
    mFtpcHits.push_back(hit);
    hit->setTrackReferenceCount(hit->trackReferenceCount()+1);
    mNumberOfFtpcHits++;
}

void StGlobalTrack::addSvtHit(StSvtHit* hit)
{
    mSvtHits.push_back(hit);
    hit->setTrackReferenceCount(hit->trackReferenceCount()+1);
    mNumberOfSvtHits++;
}

void StGlobalTrack::removeTpcHit(StTpcHit* hit)
{
    StVecPtrTpcHitIterator iter = find(mTpcHits.begin(), mTpcHits.end(), hit);
    if (iter != mTpcHits.end()) {
	mTpcHits.erase(iter);
	int i = hit->trackReferenceCount();
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
	mNumberOfTpcHits--;
    }
}

void StGlobalTrack::removeFtpcHit(StFtpcHit* hit)
{
    StVecPtrFtpcHitIterator iter = find(mFtpcHits.begin(), mFtpcHits.end(), hit);
    if (iter != mFtpcHits.end()) {
	mFtpcHits.erase(iter);
	int i = hit->trackReferenceCount();
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
	mNumberOfFtpcHits--;
    }
}

void StGlobalTrack::removeSvtHit(StSvtHit* hit)
{
    StVecPtrSvtHitIterator iter = find(mSvtHits.begin(), mSvtHits.end(), hit);
    if (iter != mSvtHits.end()) {
	mSvtHits.erase(iter);
	int i = hit->trackReferenceCount();
	hit->setTrackReferenceCount(i > 0 ? i-1 : 0);
	mNumberOfSvtHits--;
    }
}

int StGlobalTrack::numberOfTpcHits() const
{
    if (mTpcHits.size() == 0)
	return (int)mNumberOfTpcHits;
    else
	return mTpcHits.size();
}

int StGlobalTrack::numberOfSvtHits() const
{
    if (mSvtHits.size() == 0)
	return (int)mNumberOfSvtHits;
    else
	return mSvtHits.size();
}

int StGlobalTrack::numberOfFtpcHits() const
{
    if (mFtpcHits.size() == 0)
	return (int)mNumberOfFtpcHits;
    else
	return mFtpcHits.size();
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

