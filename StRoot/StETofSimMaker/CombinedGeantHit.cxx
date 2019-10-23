#include "StMessMgr.h" // for logging

#include "tables/St_g2t_ctf_hit_Table.h"

#include "SystemOfUnits.h"

#include "StETofUtil/StETofGeometry.h"
#include "StETofUtil/StETofConstants.h"
#include "CombinedGeantHit.h"
using namespace std;


CombinedGeantHit::CombinedGeantHit()
: mNRawHits( 0 ),
  mMomentum{ 0., 0., 0. },
  mLocalPos{ 0., 0., 0. },
  mSector( 0 ),
  mPlane( 0 ),
  mCounter( 0 ),
  mStrip( 0 ),
  mTrack_p( 0 ),
  mPathLength( 0 ),
  mTime( 0 ),
  mTot( 0 ),
  mClusterSize( 0 )
{

}


CombinedGeantHit::~CombinedGeantHit()
{
	/* no op */
}


void
CombinedGeantHit::addRawHit( const g2t_ctf_hit_st* hitIn, vector< int >& volumeIds )
{
    if( mNRawHits == 0 ) {
        mSector  = volumeIds[ 0 ];
        mPlane   = volumeIds[ 1 ];
        mCounter = volumeIds[ 2 ];
        mStrip   = volumeIds[ 3 ];

        mTrack_p = hitIn->track_p;
    }

    if( mTrack_p != hitIn->track_p || mSector != volumeIds[ 0 ] || mPlane != volumeIds[ 1 ] || 
        mCounter != volumeIds[ 2 ] || mStrip != volumeIds[ 3 ] ) {

        LOG_WARN << " CombinedGeantHit::addRawHit() -- cannot combine raw hits from different strips or different tracks" << endm;
        return;
    }    

    mNRawHits++;

    mMomentum[ 0 ] += hitIn->p[ 0 ];
    mMomentum[ 1 ] += hitIn->p[ 1 ];
    mMomentum[ 2 ] += hitIn->p[ 2 ];

    mLocalPos[ 0 ] += hitIn->x[ 0 ];
    mLocalPos[ 1 ] += hitIn->x[ 1 ];
    mLocalPos[ 2 ] += hitIn->x[ 2 ];

    mPathLength   += hitIn->s_track;
    mTime         += hitIn->tof / nanosecond;
}


void
CombinedGeantHit::averageRawHits()
{
    mMomentum[ 0 ] /= ( double ) mNRawHits;
    mMomentum[ 1 ] /= ( double ) mNRawHits;
    mMomentum[ 2 ] /= ( double ) mNRawHits;

    mLocalPos[ 0 ] /= ( double ) mNRawHits;
    mLocalPos[ 1 ] /= ( double ) mNRawHits;
    mLocalPos[ 2 ] /= ( double ) mNRawHits;

    // convert from gas gap local X to counter local X coordinate
    mLocalPos[ 0 ] += eTofConst::firstStripCenter + ( mStrip - 1 ) * eTofConst::stripPitch;
    // or just use the strip center
    //mLocalPos[ 0 ] = eTofConst::firstStripCenter + ( mStrip - 1 ) * eTofConst::stripPitch;

    mPathLength    /= ( double ) mNRawHits;
    mTime          /= ( double ) mNRawHits;
}


void
CombinedGeantHit::log() const
{
    LOG_INFO << "CombinedGeantHit: nRawHits = " << nRawHits() << ", volume: " << sector() << " " << plane() << " ";
    LOG_INFO << counter() << " " << strip() << ", local (x,y): (" << pos().x() << ", " << pos().y() << ")  ";
    LOG_INFO << "time = "  <<  time() << ", pathLength: " << pathLength() << ", p = " << mom().mag() << "  ";
    LOG_INFO << "track_p: " << track_p() << endm;
}


vector< int >
CombinedGeantHit::volumeVec() const
{
  vector<int> v( 4 );

  v.at( 0 ) = mSector;
  v.at( 1 ) = mPlane;
  v.at( 2 ) = mCounter;
  v.at( 3 ) = mStrip;

  return v;
}
