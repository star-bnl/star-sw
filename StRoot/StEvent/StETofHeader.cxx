/***************************************************************************
 *
 * $Id: StETofHeader.cxx,v 2.1 2018/07/09 14:53:48 ullrich Exp $
 *
 * Author: Pengfei Lyu, April 2018
 ***************************************************************************
 *
 * Description: This class stores ETofHeader information from the DAQ stream,
 * e.g. trigger time and STAR time (reset time stamp of the bTOF clock)
 * and other event-wise information
 *
 ***************************************************************************
 *
 * $Log: StETofHeader.cxx,v $
 * Revision 2.1  2018/07/09 14:53:48  ullrich
 * Initial Revision.
 *
 *
 ***************************************************************************/
#include "StETofHeader.h"
#include <map>

StETofHeader::StETofHeader()
: mTrgGdpbFullTime( 0 ),
  mTrgStarFullTime( 0 ),
  mStarToken( 0 ),
  mStarDaqCmdIn( 0 ),
  mStarTrgCmdIn( 0 ),
  mEventStatusFlag( 0 )
{
  mRocGdpbTs.clear();
  mRocStarTs.clear();
}


StETofHeader::StETofHeader( const double& trgGdpbTime, const double& trgStarTime,
                            const map< unsigned int, uint64_t >& gdpbTs, const map< unsigned int, uint64_t >& starTs,
                            const unsigned int& starToken, const unsigned int& starDaqCmdIn, const unsigned int& starTrgCmdIn,
                            const uint64_t& eventStatusFlag )
{
    mTrgGdpbFullTime = trgGdpbTime;
    mTrgStarFullTime = trgStarTime;

    mRocGdpbTs       = gdpbTs;
    mRocStarTs       = starTs;

    mStarToken       = starToken;
    mStarDaqCmdIn    = starDaqCmdIn;
    mStarTrgCmdIn    = starTrgCmdIn;
    mEventStatusFlag = eventStatusFlag;
}


StETofHeader::~StETofHeader()
{/* no op */}

double
StETofHeader::trgGdpbFullTime() const
{
    return mTrgGdpbFullTime;
}

double
StETofHeader::trgStarFullTime() const
{
    return mTrgStarFullTime;
}


map< unsigned int, uint64_t >
StETofHeader::rocGdpbTs() const
{
    return mRocGdpbTs;
}


map< unsigned int, uint64_t >
StETofHeader::rocStarTs() const
{
    return mRocStarTs;
}


unsigned int
StETofHeader::starToken() const
{
    return mStarToken;
}


unsigned int
StETofHeader::starDaqCmdIn() const
{
    return mStarDaqCmdIn;
}


unsigned int
StETofHeader::starTrgCmdIn() const
{
    return mStarTrgCmdIn;
}


uint64_t
StETofHeader::eventStatusFlag() const
{
    return mEventStatusFlag;
}


void
StETofHeader::setTrgGdpbFullTime( const double& gdpbFullTime )
{
    mTrgGdpbFullTime = gdpbFullTime;
}


void
StETofHeader::setTrgStarFullTime( const double& starFullTime )
{
    mTrgStarFullTime = starFullTime;
}


void
StETofHeader::setRocGdpbTs( const map< unsigned int, uint64_t >& gdpbTs )
{
    mRocGdpbTs = gdpbTs;
}


void
StETofHeader::setRocStarTs( const map< unsigned int, uint64_t >& starTs )
{
    mRocStarTs = starTs;
}


void
StETofHeader::setStarToken( const unsigned int& token )
{
    mStarToken = token;
}


void
StETofHeader::setStarDaqCmdIn( const unsigned int& daqCmdIn )
{
    mStarDaqCmdIn = daqCmdIn;
}


void
StETofHeader::setStarTrgCmdIn( const unsigned int& trgCmdIn )
{
    mStarTrgCmdIn = trgCmdIn;
}


void
StETofHeader::setEventStatusFlag( const uint64_t& statusFlag )
{
    mEventStatusFlag = statusFlag;
}
