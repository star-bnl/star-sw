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
#include <map>
#include <vector>
#include <iostream>

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


StETofHeader::StETofHeader( const Double_t& trgGdpbTime, const Double_t& trgStarTime,
                            const map< UInt_t, ULong64_t >& gdpbTs, const map< UInt_t, ULong64_t >& starTs,
                            const UInt_t& starToken, const UInt_t& starDaqCmdIn, const UInt_t& starTrgCmdIn,
                            const ULong64_t& eventStatusFlag )
: mTrgGdpbFullTime( trgGdpbTime ),
  mTrgStarFullTime( trgStarTime ),
  mStarToken( starToken ),
  mStarDaqCmdIn( starDaqCmdIn ),
  mStarTrgCmdIn( starTrgCmdIn ),
  mEventStatusFlag( eventStatusFlag )
{
    setRocGdpbTs( gdpbTs );
    setRocStarTs( starTs );
    const size_t kNbGet4sInSystem = 1728;
    mMissMatchFlagVec = vector<Bool_t>( kNbGet4sInSystem, false ); 
}

StETofHeader::StETofHeader( const Double_t& trgGdpbTime, const Double_t& trgStarTime,
                            const map< UInt_t, ULong64_t >& gdpbTs, const map< UInt_t, ULong64_t >& starTs,
                            const UInt_t& starToken, const UInt_t& starDaqCmdIn, const UInt_t& starTrgCmdIn,
                            const ULong64_t& eventStatusFlag, const vector<Bool_t>& MissMatchFlagVec )
: mTrgGdpbFullTime( trgGdpbTime ),
  mTrgStarFullTime( trgStarTime ),
  mStarToken( starToken ),
  mStarDaqCmdIn( starDaqCmdIn ),
  mStarTrgCmdIn( starTrgCmdIn ),
  mEventStatusFlag( eventStatusFlag ),
  mMissMatchFlagVec( MissMatchFlagVec )
{
    setRocGdpbTs( gdpbTs );
    setRocStarTs( starTs );
    //const size_t kNbGet4sInSystem = 1728;
    //mMissMatchFlagVec.resize( kNbGet4sInSystem );
    //for( auto mapcheck : mMissMatchFlagVec ){
    //	cout << mapcheck << endl;
    // }

}


StETofHeader::~StETofHeader()
{/* no op */}

Double_t
StETofHeader::trgGdpbFullTime() const
{
    return mTrgGdpbFullTime;
}

Double_t
StETofHeader::trgStarFullTime() const
{
    return mTrgStarFullTime;
}


map< UInt_t, ULong64_t >
StETofHeader::rocGdpbTs() const
{
    map< UInt_t, ULong64_t > map_root_type( mRocGdpbTs.begin(), mRocGdpbTs.end() );

    return map_root_type;
}


map< UInt_t, ULong64_t >
StETofHeader::rocStarTs() const
{
    map< UInt_t, ULong64_t > map_root_type( mRocStarTs.begin(), mRocStarTs.end() );

    return map_root_type;
}


UInt_t
StETofHeader::starToken() const
{
    return mStarToken;
}


UInt_t
StETofHeader::starDaqCmdIn() const
{
    return mStarDaqCmdIn;
}


UInt_t
StETofHeader::starTrgCmdIn() const
{
    return mStarTrgCmdIn;
}


ULong64_t
StETofHeader::eventStatusFlag() const
{
    return mEventStatusFlag;
}

vector <Bool_t>
StETofHeader::missMatchFlagVec() const
{
    return mMissMatchFlagVec;
}

void
StETofHeader::setTrgGdpbFullTime( const Double_t& gdpbFullTime )
{
    mTrgGdpbFullTime = gdpbFullTime;
}


void
StETofHeader::setTrgStarFullTime( const Double_t& starFullTime )
{
    mTrgStarFullTime = starFullTime;
}


void
StETofHeader::setRocGdpbTs( const map< UInt_t, ULong64_t >& gdpbTs )
{
    mRocGdpbTs.insert( gdpbTs.begin(), gdpbTs.end() );
}


void
StETofHeader::setRocStarTs( const map< UInt_t, ULong64_t >& starTs )
{
    mRocStarTs.insert( starTs.begin(), starTs.end() );
}


void
StETofHeader::setStarToken( const UInt_t& token )
{
    mStarToken = token;
}


void
StETofHeader::setStarDaqCmdIn( const UInt_t& daqCmdIn )
{
    mStarDaqCmdIn = daqCmdIn;
}


void
StETofHeader::setStarTrgCmdIn( const UInt_t& trgCmdIn )
{
    mStarTrgCmdIn = trgCmdIn;
}


void
StETofHeader::setEventStatusFlag( const ULong64_t& statusFlag )
{
    mEventStatusFlag = statusFlag;
}
