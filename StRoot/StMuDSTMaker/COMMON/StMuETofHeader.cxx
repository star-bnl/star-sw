/***************************************************************************
 *
 * $Id: StMuETofHeader.cxx,v 1.1 2019/02/21 13:32:54 jdb Exp $
 *
 * Author: Florian Seck, November 2018
 ***************************************************************************
 *
 * Description: This class stores ETofHeader information from the DAQ stream,
 * e.g. trigger time and STAR time (reset time stamp of the bTOF clock)
 * and other event-wise information in MuDsts
 *
 ***************************************************************************
 *
 * $Log: StMuETofHeader.cxx,v $
 * Revision 1.1  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 *
 ***************************************************************************/
#include "StMuETofHeader.h"
#include "StETofHeader.h"


StMuETofHeader::StMuETofHeader()
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


StMuETofHeader::StMuETofHeader( const double& trgGdpbTime, const double& trgStarTime,
                            const std::map< unsigned int, uint64_t >& gdpbTs, const std::map< unsigned int, uint64_t >& starTs,
                            const unsigned int& starToken, const unsigned int& starDaqCmdIn, const unsigned int& starTrgCmdIn,
                            const uint64_t& eventStatusFlag )
: mTrgGdpbFullTime( trgGdpbTime ),
  mTrgStarFullTime( trgStarTime ),
  mStarToken( starToken ),
  mStarDaqCmdIn( starDaqCmdIn ),
  mStarTrgCmdIn( starTrgCmdIn ),
  mEventStatusFlag( eventStatusFlag )

{
    setRocGdpbTs( gdpbTs );
    setRocStarTs( starTs );
}

StMuETofHeader::StMuETofHeader( const StETofHeader* header )
: mTrgGdpbFullTime( header->trgGdpbFullTime() ),
  mTrgStarFullTime( header->trgStarFullTime() ),
  mStarToken(       header->starToken()       ),
  mStarDaqCmdIn(    header->starDaqCmdIn()    ),
  mStarTrgCmdIn(    header->starTrgCmdIn()    ),
  mEventStatusFlag( header->eventStatusFlag() )

{
    setRocGdpbTs( header->rocGdpbTs() );
    setRocStarTs( header->rocStarTs() );
}

StMuETofHeader::~StMuETofHeader()
{/* no op */}

double
StMuETofHeader::trgGdpbFullTime() const
{
    return mTrgGdpbFullTime;
}

double
StMuETofHeader::trgStarFullTime() const
{
    return mTrgStarFullTime;
}


std::map< unsigned int, uint64_t >
StMuETofHeader::rocGdpbTs() const
{
    std::map< unsigned int, uint64_t > map_root_type( mRocGdpbTs.begin(), mRocGdpbTs.end() );

    return map_root_type;
}


std::map< unsigned int, uint64_t >
StMuETofHeader::rocStarTs() const
{
    std::map< unsigned int, uint64_t > map_root_type( mRocStarTs.begin(), mRocStarTs.end() );

    return map_root_type;
}


unsigned int
StMuETofHeader::starToken() const
{
    return mStarToken;
}


unsigned int
StMuETofHeader::starDaqCmdIn() const
{
    return mStarDaqCmdIn;
}


unsigned int
StMuETofHeader::starTrgCmdIn() const
{
    return mStarTrgCmdIn;
}


uint64_t
StMuETofHeader::eventStatusFlag() const
{
    return mEventStatusFlag;
}


void
StMuETofHeader::setTrgGdpbFullTime( const double& gdpbFullTime )
{
    mTrgGdpbFullTime = gdpbFullTime;
}


void
StMuETofHeader::setTrgStarFullTime( const double& starFullTime )
{
    mTrgStarFullTime = starFullTime;
}


void
StMuETofHeader::setRocGdpbTs( const std::map< unsigned int, uint64_t >& gdpbTs )
{
    mRocGdpbTs.insert( gdpbTs.begin(), gdpbTs.end() );
}


void
StMuETofHeader::setRocStarTs( const std::map< unsigned int, uint64_t >& starTs )
{
    mRocStarTs.insert( starTs.begin(), starTs.end() );
}


void
StMuETofHeader::setStarToken( const unsigned int& token )
{
    mStarToken = token;
}


void
StMuETofHeader::setStarDaqCmdIn( const unsigned int& daqCmdIn )
{
    mStarDaqCmdIn = daqCmdIn;
}


void
StMuETofHeader::setStarTrgCmdIn( const unsigned int& trgCmdIn )
{
    mStarTrgCmdIn = trgCmdIn;
}


void
StMuETofHeader::setEventStatusFlag( const uint64_t& statusFlag )
{
    mEventStatusFlag = statusFlag;
}
