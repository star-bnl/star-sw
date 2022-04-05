/***************************************************************************
 *
 * $Id: StHltHighPt.cxx,v 2.1 2011/02/01 19:45:48 ullrich Exp $
 *
 * Author: Liang Xue, Aihong Tang, Jan 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHltHighPt.cxx,v $
 * Revision 2.1  2011/02/01 19:45:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StHltHighPt.h"

ClassImp(StHltHighPt)

	///< default constructor
StHltHighPt::StHltHighPt()
{
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

StHltHighPt::~StHltHighPt() { /* noop */ }

StHltTrack& StHltHighPt::globalTrack() { return mGlobalTrack; }
const StHltTrack& StHltHighPt::globalTrack() const { return mGlobalTrack; }

StHltTrack& StHltHighPt::primaryTrack() { return mPrimaryTrack; }
const StHltTrack& StHltHighPt::primaryTrack() const { return mPrimaryTrack; }

StHltBTofHit& StHltHighPt::bTofHit() { return mBTofHit; }
const StHltBTofHit& StHltHighPt::bTofHit() const { return mBTofHit; }

StHltBEmcTowerHit& StHltHighPt::bEmcTowerHit() { return mBEmcTowerHit; }
const StHltBEmcTowerHit& StHltHighPt::bEmcTowerHit() const { return mBEmcTowerHit; }

int StHltHighPt::globalTrackSN() const { return mGlobalTrackSN; }

int StHltHighPt::primaryTrackSN() const { return mPrimaryTrackSN; }

int StHltHighPt::tofHitSN() const { return mTofHitSN; }

int StHltHighPt::emcTowerSN() const { return mEmcTowerSN; }

double StHltHighPt::bEmcMatchPhiDiff() const { return mBEmcMatchPhiDiff; }

double StHltHighPt::bEmcMatchZEdge() const { return mBEmcMatchZEdge; }

float StHltHighPt::bTofProjChannel() const { return mBTofProjChannel; }

float StHltHighPt::bTofCellLocalY() const { return mBTofCellLocalY; }

float StHltHighPt::bTofCellLocalZ() const { return mBTofCellLocalZ; }

float StHltHighPt::bTofPathLength() const { return mBTofPathLength; }

float StHltHighPt::beta() const { return mBeta; }

float StHltHighPt::tof() const { return mTof; }

void StHltHighPt::setGlobalTrack(const StHltTrack& val) { mGlobalTrack = val; }

void StHltHighPt::setPrimaryTrack(const StHltTrack& val) { mPrimaryTrack = val; }

void StHltHighPt::setBTofHit(const StHltBTofHit& val) { mBTofHit = val; }

void StHltHighPt::setBEmcTowerHit(const StHltBEmcTowerHit& val) { mBEmcTowerHit = val; }

void StHltHighPt::setGlobalTrackSN(int val) { mGlobalTrackSN = val; }

void StHltHighPt::setPrimaryTrackSN(int val) { mPrimaryTrackSN = val; }

void StHltHighPt::setTofHitSN(int val) { mTofHitSN = val; }

void StHltHighPt::setEmcTowerSN(int val) { mEmcTowerSN = val; }

void StHltHighPt::setBEmcMatchPhiDiff(double val) { mBEmcMatchPhiDiff = val; }

void StHltHighPt::setBEmcMatchZEdge(double val) { mBEmcMatchZEdge = val; }

void StHltHighPt::setBTofProjChannel(float val) { mBTofProjChannel = val; }

void StHltHighPt::setBTofCellLocalY(float val) { mBTofCellLocalY = val; }

void StHltHighPt::setBTofCellLocalZ(float val) { mBTofCellLocalZ = val; }

void StHltHighPt::setBTofPathLength(float val) { mBTofPathLength = val; }

void StHltHighPt::setBeta(float val) { mBeta = val; }

void StHltHighPt::setTof(float val) { mTof = val; }


ostream&
operator<<(ostream &os, const StHltHighPt& trigger)
{
	os << " bemcMatchPhiDiff " << trigger.bEmcMatchPhiDiff()<<" bemcMatchZEdge "<<trigger.bEmcMatchZEdge()<< " bTofProjChannel "<<trigger.bTofProjChannel() << " bTofCellLocalY "<< trigger.bTofCellLocalY() <<" bTofCellLocalZ "<<trigger.bTofCellLocalZ()<< " bTofPathLength "<<trigger.bTofPathLength()<<" beta "<<trigger.beta()<<" tof "<<trigger.tof()<<endl;
	return os;
}




