/***************************************************************************
 *
 * $Id: StHltDiElectron.cxx,v 2.1 2011/02/01 19:45:48 ullrich Exp $
 *
 * Author: Liang Xue, Aihong Tang, Jan 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHltDiElectron.cxx,v $
 * Revision 2.1  2011/02/01 19:45:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StHltDiElectron.h"

ClassImp(StHltDiElectron)

///< default constructor
StHltDiElectron::StHltDiElectron()
{
	mInvariantMass = 0;
	mPt = 0; 
	mPsi = 0;
	mTanl = 0; 
    
	mDaughter1SelectionBit = -1;
	mDaughter1GlobalTrackSN = -1;
	mDaughter1PrimaryTrackSN = -1;
	mDaughter1TofHitSN = -1;
	mDaughter1EmcTowerSN = -1;
	mDaughter1BEmcMatchPhiDiff = 0.;
	mDaughter1BEmcMatchZEdge = 0.;
	mDaughter1BTofProjChannel = 0.;
	mDaughter1BTofCellLocalY = 0.;
	mDaughter1BTofCellLocalZ = 0.;
	mDaughter1BTofPathLength = 0.;
	mDaughter1Beta = 0.;
	mDaughter1Tof = 0.;
    
	mDaughter2SelectionBit = -1;
	mDaughter2GlobalTrackSN = -1;
	mDaughter2PrimaryTrackSN = -1;
	mDaughter2TofHitSN = -1;
	mDaughter2EmcTowerSN = -1;
	mDaughter2BEmcMatchPhiDiff = 0.;
	mDaughter2BEmcMatchZEdge = 0.;
	mDaughter2BTofProjChannel = 0.;
	mDaughter2BTofCellLocalY = 0.;
	mDaughter2BTofCellLocalZ = 0.;
	mDaughter2BTofPathLength = 0.;
	mDaughter2Beta = 0.;
	mDaughter2Tof = 0.;
}

StHltDiElectron::~StHltDiElectron(){ /* noop */}

//-----------daughter1 information-------------------------------

StHltTrack& StHltDiElectron::daughter1globalTrack() { return mDaughter1GlobalTrack; }
const StHltTrack& StHltDiElectron::daughter1globalTrack() const { return mDaughter1GlobalTrack; }

StHltTrack& StHltDiElectron::daughter1primaryTrack() { return mDaughter1PrimaryTrack; }
const StHltTrack& StHltDiElectron::daughter1primaryTrack() const { return mDaughter1PrimaryTrack; }

StHltBTofHit& StHltDiElectron::daughter1bTofHit() { return mDaughter1BTofHit; }
const StHltBTofHit& StHltDiElectron::daughter1bTofHit() const { return mDaughter1BTofHit; }

StHltBEmcTowerHit& StHltDiElectron::daughter1bEmcTowerHit() { return mDaughter1BEmcTowerHit; }
const StHltBEmcTowerHit& StHltDiElectron::daughter1bEmcTowerHit() const { return mDaughter1BEmcTowerHit; }

int StHltDiElectron::daughter1SelectionBit() const {return mDaughter1SelectionBit;}

int StHltDiElectron::daughter1globalTrackSN() const { return mDaughter1GlobalTrackSN; }

int StHltDiElectron::daughter1primaryTrackSN() const { return mDaughter1PrimaryTrackSN; }

int StHltDiElectron::daughter1tofHitSN() const { return mDaughter1TofHitSN; }

int StHltDiElectron::daughter1emcTowerSN() const { return mDaughter1EmcTowerSN; }

double StHltDiElectron::daughter1bEmcMatchPhiDiff() const { return mDaughter1BEmcMatchPhiDiff; }

double StHltDiElectron::daughter1bEmcMatchZEdge() const { return mDaughter1BEmcMatchZEdge; }

float StHltDiElectron::daughter1bTofProjChannel() const { return mDaughter1BTofProjChannel; }

float StHltDiElectron::daughter1bTofCellLocalY() const { return mDaughter1BTofCellLocalY; }

float StHltDiElectron::daughter1bTofCellLocalZ() const { return mDaughter1BTofCellLocalZ; }

float StHltDiElectron::daughter1bTofPathLength() const { return mDaughter1BTofPathLength; }

float StHltDiElectron::daughter1beta() const { return mDaughter1Beta; }

float StHltDiElectron::daughter1tof() const { return mDaughter1Tof; }

void StHltDiElectron::setDaughter1GlobalTrack(const StHltTrack& val) { mDaughter1GlobalTrack = val; }

void StHltDiElectron::setDaughter1PrimaryTrack(const StHltTrack& val) { mDaughter1PrimaryTrack = val; }

void StHltDiElectron::setDaughter1BTofHit(const StHltBTofHit& val) { mDaughter1BTofHit = val; }

void StHltDiElectron::setDaughter1BEmcTowerHit(const StHltBEmcTowerHit& val) { mDaughter1BEmcTowerHit = val; }

void StHltDiElectron::setDaughter1SelectionBit(int val){mDaughter1SelectionBit=val;}

void StHltDiElectron::setDaughter1GlobalTrackSN(int val) { mDaughter1GlobalTrackSN = val; }

void StHltDiElectron::setDaughter1PrimaryTrackSN(int val) { mDaughter1PrimaryTrackSN = val; }

void StHltDiElectron::setDaughter1TofHitSN(int val) { mDaughter1TofHitSN = val; }

void StHltDiElectron::setDaughter1EmcTowerSN(int val) { mDaughter1EmcTowerSN = val; }

void StHltDiElectron::setDaughter1BEmcMatchPhiDiff(double val) { mDaughter1BEmcMatchPhiDiff = val; }

void StHltDiElectron::setDaughter1BEmcMatchZEdge(double val) { mDaughter1BEmcMatchZEdge = val; }

void StHltDiElectron::setDaughter1BTofProjChannel(float val) { mDaughter1BTofProjChannel = val; }

void StHltDiElectron::setDaughter1BTofCellLocalY(float val) { mDaughter1BTofCellLocalY = val; }

void StHltDiElectron::setDaughter1BTofCellLocalZ(float val) { mDaughter1BTofCellLocalZ = val; }

void StHltDiElectron::setDaughter1BTofPathLength(float val) { mDaughter1BTofPathLength = val; }

void StHltDiElectron::setDaughter1Beta(float val) { mDaughter1Beta = val; }

void StHltDiElectron::setDaughter1Tof(float val) { mDaughter1Tof = val; }

//-----------daughter2 information-------------------------------

StHltTrack& StHltDiElectron::daughter2globalTrack() { return mDaughter2GlobalTrack; }
const StHltTrack& StHltDiElectron::daughter2globalTrack() const { return mDaughter2GlobalTrack; }

StHltTrack& StHltDiElectron::daughter2primaryTrack() { return mDaughter2PrimaryTrack; }
const StHltTrack& StHltDiElectron::daughter2primaryTrack() const { return mDaughter2PrimaryTrack; }

StHltBTofHit& StHltDiElectron::daughter2bTofHit() { return mDaughter2BTofHit; }
const StHltBTofHit& StHltDiElectron::daughter2bTofHit() const { return mDaughter2BTofHit; }

StHltBEmcTowerHit& StHltDiElectron::daughter2bEmcTowerHit() { return mDaughter2BEmcTowerHit; }
const StHltBEmcTowerHit& StHltDiElectron::daughter2bEmcTowerHit() const { return mDaughter2BEmcTowerHit; }

int StHltDiElectron::daughter2SelectionBit() const {return mDaughter2SelectionBit;}

int StHltDiElectron::daughter2globalTrackSN() const { return mDaughter2GlobalTrackSN; }

int StHltDiElectron::daughter2primaryTrackSN() const { return mDaughter2PrimaryTrackSN; }

int StHltDiElectron::daughter2tofHitSN() const { return mDaughter2TofHitSN; }

int StHltDiElectron::daughter2emcTowerSN() const { return mDaughter2EmcTowerSN; }

double StHltDiElectron::daughter2bEmcMatchPhiDiff() const { return mDaughter2BEmcMatchPhiDiff; }

double StHltDiElectron::daughter2bEmcMatchZEdge() const { return mDaughter2BEmcMatchZEdge; }

float StHltDiElectron::daughter2bTofProjChannel() const { return mDaughter2BTofProjChannel; }

float StHltDiElectron::daughter2bTofCellLocalY() const { return mDaughter2BTofCellLocalY; }

float StHltDiElectron::daughter2bTofCellLocalZ() const { return mDaughter2BTofCellLocalZ; }

float StHltDiElectron::daughter2bTofPathLength() const { return mDaughter2BTofPathLength; }

float StHltDiElectron::daughter2beta() const { return mDaughter2Beta; }

float StHltDiElectron::daughter2tof() const { return mDaughter2Tof; }

void StHltDiElectron::setDaughter2GlobalTrack(const StHltTrack& val) { mDaughter2GlobalTrack = val; }

void StHltDiElectron::setDaughter2PrimaryTrack(const StHltTrack& val) { mDaughter2PrimaryTrack = val; }

void StHltDiElectron::setDaughter2BTofHit(const StHltBTofHit& val) { mDaughter2BTofHit = val; }

void StHltDiElectron::setDaughter2BEmcTowerHit(const StHltBEmcTowerHit& val) { mDaughter2BEmcTowerHit = val; }

void StHltDiElectron::setDaughter2SelectionBit(int val) {mDaughter2SelectionBit=val;}

void StHltDiElectron::setDaughter2GlobalTrackSN(int val) { mDaughter2GlobalTrackSN = val; }

void StHltDiElectron::setDaughter2PrimaryTrackSN(int val) { mDaughter2PrimaryTrackSN = val; }

void StHltDiElectron::setDaughter2TofHitSN(int val) { mDaughter2TofHitSN = val; }

void StHltDiElectron::setDaughter2EmcTowerSN(int val) { mDaughter2EmcTowerSN = val; }

void StHltDiElectron::setDaughter2BEmcMatchPhiDiff(double val) { mDaughter2BEmcMatchPhiDiff = val; }

void StHltDiElectron::setDaughter2BEmcMatchZEdge(double val) { mDaughter2BEmcMatchZEdge = val; }

void StHltDiElectron::setDaughter2BTofProjChannel(float val) { mDaughter2BTofProjChannel = val; }

void StHltDiElectron::setDaughter2BTofCellLocalY(float val) { mDaughter2BTofCellLocalY = val; }

void StHltDiElectron::setDaughter2BTofCellLocalZ(float val) { mDaughter2BTofCellLocalZ = val; }

void StHltDiElectron::setDaughter2BTofPathLength(float val) { mDaughter2BTofPathLength = val; }

void StHltDiElectron::setDaughter2Beta(float val) { mDaughter2Beta = val; }

void StHltDiElectron::setDaughter2Tof(float val) { mDaughter2Tof = val; }

//--------------pair information------------------------------------

void  StHltDiElectron::setInvariantMass(float val) { mInvariantMass = val; }

void  StHltDiElectron::setPt(float val) { mPt = val; }

void  StHltDiElectron::setPsi(float val) { mPsi = val; }

void  StHltDiElectron::setTanl(float val) { mTanl = val; }

ostream&
operator<<(ostream &os, const StHltDiElectron& de)
{
	os <<" invariantMass "<<de.invariantMass()<<" pt "<<de.pt()<<" psi "<<de.psi()<<" tanl "<<de.tanl()<<endl;
	return os;
}




