/***************************************************************************
 *
 * $Id: StBTofHeader.cxx,v 2.5 2010/05/12 15:12:03 ullrich Exp $
 *
 * Author: Xin Dong, Nov 2008
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StBTofHeader.cxx,v $
 * Revision 2.5  2010/05/12 15:12:03  ullrich
 * Added member mNTzero and access methods.
 *
 * Revision 2.4  2009/11/23 22:45:51  ullrich
 * Fixed order of operator precedence in removeVpdHit().
 *
 * Revision 2.3  2009/11/23 22:24:05  ullrich
 * Cleaned up compiler warning in removeVpdHit().
 *
 * Revision 2.2  2009/01/15 00:45:19  ullrich
 * mTriggerTime becomes array, setVpdVz() gets default argument.
 *
 * Revision 2.1  2008/12/22 20:30:56  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/
#include "StBTofHeader.h"

ClassImp(StBTofHeader)

StBTofHeader::StBTofHeader()
{
    for(int i=0; i<MAXFIBER; i++) {
        mFiberHeader[i] = -1;
        mFiberTriggerWord[i] = 0;
        mTriggerTime[i] = 0;
    }
    for(int i=0; i<2; i++) mVpdHitPattern[i] = 0;
    for(int i=0; i<MAXVPDVZ; i++) mVpdVz[i] = -999.;
    mTStart = 0.;
    mTStartErr = 0.;
    mTDiff = -999.;
    for(int i=0; i<2; i++)
        for(int j=0; j<MAXVPD; j++)
            mVpdTime[i][j] = 0.;
    mNTzero = 0;
}

StBTofHeader::~StBTofHeader() {/* no op */}

short
StBTofHeader::fiberHeader(int fiberId) const { return mFiberHeader[fiberId]; }

unsigned int
StBTofHeader::fiberTriggerWord(int fiberId) const { return mFiberTriggerWord[fiberId]; }

unsigned int
StBTofHeader::vpdHitPattern(StBeamDirection eastwest) const
{
    return mVpdHitPattern[eastwest];
}

unsigned short
StBTofHeader::numberOfVpdHits(StBeamDirection eastwest) const
{
    unsigned short nHit = 0;
    for(int i=0; i<MAXVPD; i++) {
        int tubeId = i + 1;
        if (isVpdHit(eastwest, tubeId)) nHit++;
    }
    return nHit;
}

bool
StBTofHeader::isVpdHit(StBeamDirection eastwest, int tubeId) const
{
    return ( (mVpdHitPattern[eastwest])>>(tubeId-1) ) & 0x1;
}

float
StBTofHeader::vpdVz(int rank) const
{
    return mVpdVz[rank];
}

double
StBTofHeader::tStart() const { return mTStart; }

double
StBTofHeader::tStartError() const { return mTStartErr; }

double
StBTofHeader::tDiff() const { return mTDiff; }

double
StBTofHeader::vpdTime(StBeamDirection eastwest, int tubeId) const
{
    return mVpdTime[eastwest][tubeId-1];
}

unsigned int
StBTofHeader::triggerTime(int fiberId) const { return mTriggerTime[fiberId]; }

int
StBTofHeader::nTzero() const { return mNTzero; }

void
StBTofHeader::setFiberHeader(int fiberId, short val)
{
    mFiberHeader[fiberId] = val;
}

void
StBTofHeader::setFiberTriggerWord(int fiberId, unsigned int val)
{
    mFiberTriggerWord[fiberId] = val;
}

void
StBTofHeader::setVpdHit(StBeamDirection eastwest, int tubeId)
{
    mVpdHitPattern[eastwest] |= 0x1 << (tubeId-1);
}

void
StBTofHeader::removeVpdHit(StBeamDirection eastwest, int tubeId)
{
    mVpdHitPattern[eastwest] &= ( 0x7ffff - (0x1 << (tubeId-1)) );
}

void
StBTofHeader::setVpdHitPattern(StBeamDirection eastwest, unsigned int val)
{
    mVpdHitPattern[eastwest] = val;
}

void
StBTofHeader::setVpdVz(float vz, int rank)
{
    mVpdVz[rank] = vz;
}

void
StBTofHeader::setTStart(double t) { mTStart = t; }

void
StBTofHeader::setTStartError(double t_err) { mTStartErr = t_err; }

void
StBTofHeader::setTDiff(double tdiff) { mTDiff = tdiff; }

void
StBTofHeader::setVpdTime(StBeamDirection eastwest, int tubeId, double t)
{
    mVpdTime[eastwest][tubeId-1] = t;
}

void
StBTofHeader::setTriggerTime(unsigned int tdc, int fiberId) { mTriggerTime[fiberId] = tdc; }

void
StBTofHeader::setNTzero(short n) { mNTzero = n; }
