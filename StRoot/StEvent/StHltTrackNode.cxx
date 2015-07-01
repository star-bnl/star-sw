/***************************************************************************
 *
 * $Id: StHltTrackNode.cxx,v 2.1 2011/02/01 19:45:48 ullrich Exp $
 *
 * Author: Liang Xue, Aihong Tang, Jan 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHltTrackNode.cxx,v $
 * Revision 2.1  2011/02/01 19:45:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StHltTrackNode.h"
#include "StHltTrack.h"
#include "StHltBTofHit.h"
#include "StHltBEmcTowerHit.h"

ClassImp(StHltTrackNode)

	///< default constructor
	///< Initialization ...
StHltTrackNode::StHltTrackNode()
{
	mGlobalTrack = 0;
	mPrimaryTrack = 0;
	mBTofHit = 0;
	mBEmcTowerHit = 0;
    
	mGlobalTrackSN = -1;
	mPrimaryTrackSN = -1;
	mTofHitSN = -1;
	mEmcTowerSN = -1;
    
	mBEmcMatchPhiDiff = 0.;
	mBEmcMatchZEdge = 0.;
	mBTofProjChannel = 0.;
	mBTofCellLocalY = 0.;
	mBTofCellLocalZ = 0.;
	mBTofPathLength = 0.;
	mBeta = 0.;
	mTof = 0.;
}

StHltTrackNode::~StHltTrackNode() { /* noop */ }

StHltTrack*
StHltTrackNode::globalTrack()
{
	return mGlobalTrack;
}

const StHltTrack* 
StHltTrackNode::globalTrack() const
{
	return mGlobalTrack;
}

StHltTrack*
StHltTrackNode::primaryTrack()
{
	return mPrimaryTrack;
}

const StHltTrack*
StHltTrackNode::primaryTrack() const
{
	return mPrimaryTrack;
}

StHltBTofHit*
StHltTrackNode::bTofHit()
{
	return mBTofHit;
}

const StHltBTofHit*
StHltTrackNode::bTofHit() const
{
	return mBTofHit;
}

StHltBEmcTowerHit*
StHltTrackNode::bEmcTowerHit()
{
	return mBEmcTowerHit;
}

const StHltBEmcTowerHit*
StHltTrackNode::bEmcTowerHit() const
{
	return mBEmcTowerHit;
}

int
StHltTrackNode::globalTrackSN() const
{
	return mGlobalTrackSN;
}
int
StHltTrackNode::primaryTrackSN() const
{
	return mPrimaryTrackSN;
}
int
StHltTrackNode::tofHitSN() const
{
	return mTofHitSN;
}
int
StHltTrackNode::emcTowerSN() const
{
	return mEmcTowerSN;
}

double
StHltTrackNode::bEmcMatchPhiDiff() const
{
	return mBEmcMatchPhiDiff;
}

double
StHltTrackNode::bEmcMatchZEdge() const
{
	return mBEmcMatchZEdge;
}

float 
StHltTrackNode::bTofProjChannel() const
{
	return mBTofProjChannel;
}

float
StHltTrackNode::bTofCellLocalY() const
{
	return mBTofCellLocalY;
}

float 
StHltTrackNode::bTofCellLocalZ() const
{
	return mBTofCellLocalZ;
}

float
StHltTrackNode::bTofPathLength() const
{
	return mBTofPathLength;
}

float
StHltTrackNode::beta() const
{
	return mBeta;
}

float 
StHltTrackNode::tof() const
{
	return mTof;
}

void 
StHltTrackNode::setGlobalTrack(StHltTrack* val)
{
	mGlobalTrack = val;
}

void
StHltTrackNode::setPrimaryTrack(StHltTrack* val)
{
	mPrimaryTrack = val;
}

void
StHltTrackNode::setBTofHit(StHltBTofHit* val)
{
	mBTofHit = val;
}

void
StHltTrackNode::setBEmcTowerHit(StHltBEmcTowerHit* val)
{
	mBEmcTowerHit = val;
}

void
StHltTrackNode::setGlobalTrackSN(int val)
{
	mGlobalTrackSN = val;
}

void
StHltTrackNode::setPrimaryTrackSN(int val)
{
	mPrimaryTrackSN = val;
}

void
StHltTrackNode::setTofHitSN(int val)
{
	mTofHitSN = val;
}

void
StHltTrackNode::setEmcTowerSN(int val)
{
	mEmcTowerSN = val;
}

void
StHltTrackNode::setBEmcMatchPhiDiff(double val)
{
	mBEmcMatchPhiDiff = val;
}

void
StHltTrackNode::setBEmcMatchZEdge(double val)
{
	mBEmcMatchZEdge = val;
}

void
StHltTrackNode::setBTofProjChannel(float val)
{	
	mBTofProjChannel = val;
}

void
StHltTrackNode::setBTofCellLocalY(float val)
{
	mBTofCellLocalY = val;
}

void
StHltTrackNode::setBTofCellLocalZ(float val)
{
	mBTofCellLocalZ = val;
}

void
StHltTrackNode::setBTofPathLength(float val)
{
	mBTofPathLength = val;
}

void
StHltTrackNode::setBeta(float val)
{
	mBeta = val;
}

void
StHltTrackNode::setTof(float val)
{
	mTof = val;
}

ostream&
operator<<(ostream &os, const StHltTrackNode& node)
{
	os << " bemcMatchPhiDiff " << node.bEmcMatchPhiDiff()<<" bemcMatchZEdge "<<node.bEmcMatchZEdge()<<endl
    << " bTofProjChannel "<<node.bTofProjChannel() << " bTofCellLocalY "<< node.bTofCellLocalY() <<" bTofCellLocalZ "<<node.bTofCellLocalZ()<< " bTofPathLength "<<node.bTofPathLength()<<" beta "<<node.beta()<<" tof "<<node.tof()<<endl;
	return os;
    
}










