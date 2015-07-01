/***************************************************************************
 *
 * $Id: StHltTrack.cxx,v 2.1 2011/02/01 19:45:48 ullrich Exp $
 *
 * Author: Liang Xue, Aihong Tang, Jan 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StHltTrack.cxx,v $
 * Revision 2.1  2011/02/01 19:45:48  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StHltTrack.h"
#include "StHltTrackNode.h"

ClassImp(StHltTrack)

///< default constructor
///< Initialization ...
StHltTrack::StHltTrack(){
    mId = 0;
    mInnerMostRow = 0;
    mOuterMostRow = 0;
    mNHits = 0;
    mNDedx = 0; 
    mQ = 0; 
    mChi2[0]=0; mChi2[1]=0;
    mDedx = 0;
    mPt = 0; 
    mPhi0 = 0;
    mPsi = 0; 
    mR0 = 0; 
    mTanl = 0;
    mZ0 = 0; 
    mLength = 0;
    mDpt = 0; 
    mDpsi = 0;
    mDz0 = 0;
    mDtanl = 0;
    mTrackNode = 0;
}

StHltTrack::~StHltTrack(){/* noop */}

void 
StHltTrack::setType(StTrackType val)
{
	mType = val;
}

void 
StHltTrack::setId(int val)
{
	mId = val;
}

void 
StHltTrack::setFlag(unsigned short val)
{
	mFlag = val;
}

void
StHltTrack::setInnerMostRow(char val)
{
	mInnerMostRow = val;
}

void
StHltTrack::setOuterMostRow(char val)
{
	mOuterMostRow = val;
}

void
StHltTrack::setNHits(unsigned char val)
{
	mNHits = val;
}

void
StHltTrack::setNDedx(unsigned char val)
{
	mNDedx = val;
}

void
StHltTrack::setQ(char val)
{
	mQ = val;
}

void
StHltTrack::setChi2(int i, float val)
{
	if (i==0 || i==1) mChi2[i]=val; else return;
}

void
StHltTrack::setDedx(float val)
{
	mDedx = val;
}

void
StHltTrack::setPt(float val)
{
	mPt = val;
}

void
StHltTrack::setPhi0(float val)
{
	mPhi0 = val;
}

void
StHltTrack::setPsi(float val)
{
	mPsi = val;
}

void
StHltTrack::setR0(float val)
{
	mR0 = val;
}

void
StHltTrack::setTanl(float val)
{
	mTanl = val;
}

void
StHltTrack::setZ0(float val)
{
	mZ0 = val;
}

void
StHltTrack::setLength(float val)
{
	mLength = val;
}

void
StHltTrack::setDpt(float val)
{
	mDpt = val;
}

void
StHltTrack::setDpsi(float val)
{
	mDpsi = val;
}

void
StHltTrack::setDz0(float val)
{
	mDz0 = val;
}

void
StHltTrack::setDtanl(float val)
{
	mDtanl = val;
}

void
StHltTrack::setTrackNode(StHltTrackNode* val)
{
	mTrackNode = val;
}


ostream& 
operator<<(ostream &os, const StHltTrack& trk)
{
	os << " primary key "  << trk.id()<<" innerMostRow "<<trk.innerMostRow()
    << " outerMostRow " << trk.outerMostRow()<<endl
    << " nhits "<<trk.nHits() << " ndedx "<<trk.ndedx() << " q "<< trk.q()<<endl
    << " chi2[0] "<<trk.chi2(0)<< " chi2[1] "<<trk.chi2(1)
    << " dedx "<<trk.dedx()<<" pt "<<trk.pt()<<" phi0 "<<trk.phi0()
    << " psi "<<trk.psi()<<" r0 "<<trk.r0()<<" tanl "<<trk.tanl()
    << " z0 "<<trk.z0()<<" length "<<trk.length()<<" dpt "<<trk.dpt()
    << " dz0 "<<trk.dz0()<<" dtanl "<<trk.dtanl()<<endl;
	return os;
}









