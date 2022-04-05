/***************************************************************************
 *
 * $Id: StHltHeavyFragment.cxx,v 2.1 2011/02/01 19:45:48 ullrich Exp $
 *
 * Author: Liang Xue, Aihong Tang, Jan 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHltHeavyFragment.cxx,v $
 * Revision 2.1  2011/02/01 19:45:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StHltHeavyFragment.h"

ClassImp(StHltHeavyFragment)

StHltHeavyFragment::StHltHeavyFragment()
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

StHltHeavyFragment::~StHltHeavyFragment() { /* noop */ }

StHltTrack& StHltHeavyFragment::globalTrack() { return mGlobalTrack; }
const StHltTrack& StHltHeavyFragment::globalTrack() const { return mGlobalTrack; }

StHltTrack& StHltHeavyFragment::primaryTrack() { return mPrimaryTrack; }
const StHltTrack& StHltHeavyFragment::primaryTrack() const { return mPrimaryTrack; }

StHltBTofHit& StHltHeavyFragment::bTofHit() { return mBTofHit; }
const StHltBTofHit& StHltHeavyFragment::bTofHit() const { return mBTofHit; }

StHltBEmcTowerHit& StHltHeavyFragment::bEmcTowerHit() { return mBEmcTowerHit; }
const StHltBEmcTowerHit& StHltHeavyFragment::bEmcTowerHit() const { return mBEmcTowerHit; }

int StHltHeavyFragment::globalTrackSN() const { return mGlobalTrackSN; }

int StHltHeavyFragment::primaryTrackSN() const { return mPrimaryTrackSN; }

int StHltHeavyFragment::tofHitSN() const { return mTofHitSN; }

int StHltHeavyFragment::emcTowerSN() const { return mEmcTowerSN; }

double StHltHeavyFragment::bEmcMatchPhiDiff() const { return mBEmcMatchPhiDiff; }

double StHltHeavyFragment::bEmcMatchZEdge() const { return mBEmcMatchZEdge; }

float StHltHeavyFragment::bTofProjChannel() const { return mBTofProjChannel; }

float StHltHeavyFragment::bTofCellLocalY() const { return mBTofCellLocalY; }

float StHltHeavyFragment::bTofCellLocalZ() const { return mBTofCellLocalZ; }

float StHltHeavyFragment::bTofPathLength() const { return mBTofPathLength; }

float StHltHeavyFragment::beta() const { return mBeta; }

float StHltHeavyFragment::tof() const { return mTof; }

void StHltHeavyFragment::setGlobalTrack(const StHltTrack& val) { mGlobalTrack = val; }

void StHltHeavyFragment::setPrimaryTrack(const StHltTrack& val) { mPrimaryTrack = val; }

void StHltHeavyFragment::setBTofHit(const StHltBTofHit& val) { mBTofHit = val; }

void StHltHeavyFragment::setBEmcTowerHit(const StHltBEmcTowerHit& val) { mBEmcTowerHit = val; }

void StHltHeavyFragment::setGlobalTrackSN(int val) { mGlobalTrackSN = val; }

void StHltHeavyFragment::setPrimaryTrackSN(int val) { mPrimaryTrackSN = val; }

void StHltHeavyFragment::setTofHitSN(int val) { mTofHitSN = val; }

void StHltHeavyFragment::setEmcTowerSN(int val) { mEmcTowerSN = val; }

void StHltHeavyFragment::setBEmcMatchPhiDiff(double val) { mBEmcMatchPhiDiff = val; }

void StHltHeavyFragment::setBEmcMatchZEdge(double val) { mBEmcMatchZEdge = val; }

void StHltHeavyFragment::setBTofProjChannel(float val) { mBTofProjChannel = val; }

void StHltHeavyFragment::setBTofCellLocalY(float val) { mBTofCellLocalY = val; }

void StHltHeavyFragment::setBTofCellLocalZ(float val) { mBTofCellLocalZ = val; }

void StHltHeavyFragment::setBTofPathLength(float val) { mBTofPathLength = val; }

void StHltHeavyFragment::setBeta(float val) { mBeta = val; }

void StHltHeavyFragment::setTof(float val) { mTof = val; }


ostream&
operator<<(ostream &os, const StHltHeavyFragment& trigger)
{
	os << " bemcMatchPhiDiff " << trigger.bEmcMatchPhiDiff()<<" bemcMatchZEdge "<<trigger.bEmcMatchZEdge()<< " bTofProjChannel "<<trigger.bTofProjChannel() << " bTofCellLocalY "<< trigger.bTofCellLocalY() <<" bTofCellLocalZ "<<trigger.bTofCellLocalZ()<< " bTofPathLength "<<trigger.bTofPathLength()<<" beta "<<trigger.beta()<<" tof "<<trigger.tof()<<endl;
	return os;
}




