/***************************************************************************
 *
 * $Id: StMuETofHit.cxx,v 1.1 2019/02/21 13:32:54 jdb Exp $
 *
 * Author: Florian Seck, October 2018
 ***************************************************************************
 *
 * Description: Data class for expanded digital eTOF information in MuDsts:
 * eTOF hits created out of 2 or more eTOF digis from different sides of the
 * MRPC counters 
 *
 ***************************************************************************
 *
 * $Log: StMuETofHit.cxx,v $
 * Revision 1.1  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 *
 ***************************************************************************/
#include "StMuETofHit.h"
#include "StETofHit.h"
#include "StTrack.h"
#include <cmath>

StMuETofHit::StMuETofHit()
: mSector(0),
  mZPlane(0),
  mCounter(0),
  mTime(0), 
  mTotalTot(0),
  mClusterSize(0),
  mLocalX(0),
  mLocalY(0),
  mAssociatedTrackId( -1 ),
  mIndex2Primary(-1),
  mIndex2Global(-1),
  mIdTruth(0),
  mQuality(0)
{

}


StMuETofHit::StMuETofHit( const unsigned int sector, const unsigned int zPlane,  const unsigned int counter,
                          const double& time, const double& tot, const unsigned int clusterSize,
                          const double& localX, const double& localY )
: mSector(sector),
  mZPlane(zPlane),
  mCounter(counter),
  mTime(time),
  mTotalTot(tot),
  mClusterSize(clusterSize),
  mLocalX(localX),
  mLocalY(localY),
  mAssociatedTrackId( -1 ),
  mIndex2Primary(-1),
  mIndex2Global(-1),
  mIdTruth(0),
  mQuality(0)
{

}


StMuETofHit::StMuETofHit( const StMuETofHit& hitIn )
: mSector(hitIn.mSector),
  mZPlane(hitIn.mZPlane),
  mCounter(hitIn.mCounter),
  mTime(hitIn.mTime),
  mTotalTot(hitIn.mTotalTot),
  mClusterSize(hitIn.mClusterSize),
  mLocalX(hitIn.mLocalX),
  mLocalY(hitIn.mLocalY),
  mAssociatedTrackId(hitIn.mAssociatedTrackId),
  mIndex2Primary(hitIn.mIndex2Primary),
  mIndex2Global(hitIn.mIndex2Global),
  mIdTruth(hitIn.mIdTruth),
  mQuality(hitIn.mQuality)
{

}


StMuETofHit::StMuETofHit( const StETofHit* hitIn )
: mSector(          hitIn->sector()      ),
  mZPlane(          hitIn->zPlane()      ),
  mCounter(         hitIn->counter()     ),
  mTime(            hitIn->time()        ),
  mTotalTot(        hitIn->totalTot()    ),
  mClusterSize(     hitIn->clusterSize() ),
  mLocalX(          hitIn->localX()      ),
  mLocalY(          hitIn->localY()      ),
  mAssociatedTrackId( -1                 ),
  mIndex2Primary(     -1                 ),
  mIndex2Global(      -1                 ),
  mIdTruth(         hitIn->idTruth()     ),
  mQuality(         hitIn->qaTruth()     )
{
    if( hitIn->associatedTrack() != 0 ) {
        mAssociatedTrackId = hitIn->associatedTrack()->key(); 
    }

    //mAssociatedTrackId = ( hitIn->associatedTrack() ) ? hitIn->associatedTrack()->key() : -1;
}


StMuETofHit::~StMuETofHit()
{

}


/// ordering operators sorted by calibrated time.
bool
StMuETofHit::operator <( const StMuETofHit& rhs ) const
{
    return ( this->time() < rhs.time() ) ? kTRUE : kFALSE;
}


int
StMuETofHit::compare( const TObject* obj ) const
{
    return compare( ( StMuETofHit* ) obj );
}


int
StMuETofHit::compare( const StMuETofHit* hit ) const
{
    if( this->time() < hit->time() ) {
        // this ... other
        return -1;
    }
    else if( this->time() > hit->time() ) {
        // other ... hit
        return 1;
    }
    else {
        // this = other
        return 0;
    }
}


void
StMuETofHit::setHwAddress( const unsigned int sector, const unsigned int zPlane, const unsigned int counter )
{
    mSector  = sector;
    mZPlane  = zPlane;
    mCounter = counter;
}

void
StMuETofHit::setIdTruth( unsigned short idtruth, unsigned short qatruth )
{
    if( qatruth==0 ) qatruth = ( idtruth>>16 );
    idtruth  = idtruth&((1<<16)-1);
    mIdTruth = static_cast< UShort_t >( idtruth );
    mQuality = static_cast< UShort_t >( qatruth );
}
