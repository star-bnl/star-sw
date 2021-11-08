/***************************************************************************
 *
 * $Id: StETofHeader.cxx,v 2.3 2021/03/19 19:55:56 ullrich Exp $
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
 * Revision 2.3  2021/03/19 19:55:56  ullrich
 * include the front-end missmatch
 *
 * Revision 2.2  2018/07/27 13:44:55  jeromel
 * Changes by Florian
 *
 * Revision 2.1  2018/07/09 14:53:48  ullrich
 * Initial Revision.
 *
 *
 ***************************************************************************/
#include "StETofHeader.h"
#include "StETofUtil/StETofConstants.h"

#include <map>
#include <vector>
#include <iostream>

StETofHeader::StETofHeader()
: mTrgGdpbFullTime( 0 ),
  mTrgStarFullTime( 0 ),
  mStarToken( 0 ),
  mStarDaqCmdIn( 0 ),
  mStarTrgCmdIn( 0 ),
  mEventStatusFlag( 0 ),
  mMissMatchFlagVec( eTofConst::nGet4sInSystem, false ),
  mGoodEventFlagVec( eTofConst::nCountersInSystem, false )
{
    mRocGdpbTs.clear();
    mRocStarTs.clear();
}


StETofHeader::StETofHeader( const double& trgGdpbTime, const double& trgStarTime,
                            const map< unsigned int, uint64_t >& gdpbTs, const map< unsigned int, uint64_t >& starTs,
                            const unsigned int& starToken, const unsigned int& starDaqCmdIn, const unsigned int& starTrgCmdIn,
                            const uint64_t& eventStatusFlag )
: mTrgGdpbFullTime( trgGdpbTime ),
  mTrgStarFullTime( trgStarTime ),
  mStarToken( starToken ),
  mStarDaqCmdIn( starDaqCmdIn ),
  mStarTrgCmdIn( starTrgCmdIn ),
  mEventStatusFlag( eventStatusFlag ),
  mMissMatchFlagVec( eTofConst::nGet4sInSystem, false ),
  mGoodEventFlagVec( eTofConst::nCountersInSystem, false )
{
    setRocGdpbTs( gdpbTs );
    setRocStarTs( starTs );
}

StETofHeader::StETofHeader( const double& trgGdpbTime, const double& trgStarTime,
                            const map< unsigned int, uint64_t >& gdpbTs, const map< unsigned int, uint64_t >& starTs,
                            const unsigned int& starToken, const unsigned int& starDaqCmdIn, const unsigned int& starTrgCmdIn,
                            const uint64_t& eventStatusFlag, const std::vector<bool>& MissMatchFlagVec )
: mTrgGdpbFullTime( trgGdpbTime ),
  mTrgStarFullTime( trgStarTime ),
  mStarToken( starToken ),
  mStarDaqCmdIn( starDaqCmdIn ),
  mStarTrgCmdIn( starTrgCmdIn ),
  mEventStatusFlag( eventStatusFlag ),
  mMissMatchFlagVec( MissMatchFlagVec ),
  mGoodEventFlagVec( eTofConst::nCountersInSystem, false )
{
    setRocGdpbTs( gdpbTs );
    setRocStarTs( starTs );
}

StETofHeader::StETofHeader( const double& trgGdpbTime, const double& trgStarTime,
                            const map< unsigned int, uint64_t >& gdpbTs, const map< unsigned int, uint64_t >& starTs,
                            const unsigned int& starToken, const unsigned int& starDaqCmdIn, const unsigned int& starTrgCmdIn,
                            const uint64_t& eventStatusFlag, const std::vector<bool>& MissMatchFlagVec, const std::vector<bool>& GoodEventFlagVec  )
: mTrgGdpbFullTime( trgGdpbTime ),
  mTrgStarFullTime( trgStarTime ),
  mStarToken( starToken ),
  mStarDaqCmdIn( starDaqCmdIn ),
  mStarTrgCmdIn( starTrgCmdIn ),
  mEventStatusFlag( eventStatusFlag ),
  mMissMatchFlagVec( MissMatchFlagVec ),
  mGoodEventFlagVec( GoodEventFlagVec )
{
    setRocGdpbTs( gdpbTs );
    setRocStarTs( starTs ); 
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
    map< unsigned int, uint64_t > map_root_type( mRocGdpbTs.begin(), mRocGdpbTs.end() );

    return map_root_type;
}


map< unsigned int, uint64_t >
StETofHeader::rocStarTs() const
{
    map< unsigned int, uint64_t > map_root_type( mRocStarTs.begin(), mRocStarTs.end() );

    return map_root_type;
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

std::vector <bool>
StETofHeader::missMatchFlagVec() const
{
    return mMissMatchFlagVec;
}

std::vector <bool>
StETofHeader::goodEventFlagVec() const
{
    return mGoodEventFlagVec;
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
    mRocGdpbTs.insert( gdpbTs.begin(), gdpbTs.end() );
}


void
StETofHeader::setRocStarTs( const map< unsigned int, uint64_t >& starTs )
{
    mRocStarTs.insert( starTs.begin(), starTs.end() );
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

void    
StETofHeader::setGoodEventFlagVec( const std::vector<bool>& FlagVec )
{
    mGoodEventFlagVec = FlagVec;
}
