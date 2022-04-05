/***************************************************************************
 *
 * $Id: StHltEvent.cxx,v 2.1 2011/02/01 19:45:48 ullrich Exp $
 *
 * Author: Liang Xue, Aihong Tang, Jan 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHltEvent.cxx,v $
 * Revision 2.1  2011/02/01 19:45:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StHltEvent.h"
#include "StHltTrack.h"
#include "StHltTrackNode.h"
#include "StHltBTofHit.h"
#include "StHltVpdHit.h"
#include "StHltBEmcTowerHit.h"
#include "StHltHighPt.h"
#include "StHltDiElectron.h"
#include "StHltTriggerReason.h"


#ifndef ST_NO_NAMESPACES
using std::swap;
#endif

ClassImp(StHltEvent)

StHltEvent::StHltEvent() {
    mVersion = 0;
    mTriggerReasonBitOred = 0;
    mVpdVertexZ = 0;
    mT0 = 0;
    mInnerSecGain = 0;
    mOuterSecGain = 0;
}

StHltEvent::~StHltEvent() { /* noop */ }

StThreeVectorF&
StHltEvent::vertex() {return mVertex;}

const StThreeVectorF&
StHltEvent::vertex() const {return mVertex;}

StThreeVectorF&
StHltEvent::lowMultVertex() {return mLowMultVertex;}

const StThreeVectorF&
StHltEvent::lowMultVertex() const {return mLowMultVertex;}

void 
StHltEvent::setVersion(unsigned int i) { mVersion = i;}

void 
StHltEvent::setTriggerReasonBitOred(unsigned int i)
{
	mTriggerReasonBitOred = i;
}

void 
StHltEvent::setVertex(const StThreeVectorF& val)
{
	mVertex = val;
}

void 
StHltEvent::setLowMultVertex(const StThreeVectorF& val)
{
	mLowMultVertex = val;
}


void 
StHltEvent::setVpdVertexZ(float val)
{
	mVpdVertexZ = val;
}

void 
StHltEvent::setT0(float val)
{
	mT0 = val;
}


void 
StHltEvent::setInnerSecGain(float val)
{
	mInnerSecGain = val;
}
void 
StHltEvent::setOuterSecGain(float val)
{
	mOuterSecGain = val;
}

void
StHltEvent::addGlobalTrack(const StHltTrack* val)
{
	if(val) mGlobalTrack.push_back(val);
}

void
StHltEvent::addPrimaryTrack(const StHltTrack* val)
{
	if(val) mPrimaryTrack.push_back(val);
}

void
StHltEvent::addTrackNode(const StHltTrackNode* val)
{
	if(val) mTrackNode.push_back(val);
}

void
StHltEvent::addBEmcTowerHit(const StHltBEmcTowerHit* val)
{
	if(val) mBEmcTowerHits.push_back(val);
}

void
StHltEvent::addBTofHit(const StHltBTofHit* val)
{
	if(val) mBTofHit.push_back(val);
}

void
StHltEvent::addVpdHit(const StHltVpdHit* val)
{
	if(val) mVpdHit.push_back(val);
}

void
StHltEvent::addHighPt(const StHltHighPt* val)
{
	if(val) mHighPt.push_back(val);
}

void
StHltEvent::addHeavyFragment(const StHltHeavyFragment* val)
{
	if(val) mHeavyFragment.push_back(val);
}

void
StHltEvent::addDiElectron(const StHltDiElectron* val)
{
	if(val) mDiElectron.push_back(val);
}

void
StHltEvent::addTriggerReason(const StHltTriggerReason* val)
{
	if(val) mTriggerReason.push_back(val);
}










