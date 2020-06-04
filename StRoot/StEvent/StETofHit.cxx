/***************************************************************************
 *
 * $Id: StETofHit.cxx,v 2.2 2019/02/11 18:53:09 ullrich Exp $
 *
 * Author: Philipp Weidenkaff, April 2018
 ***************************************************************************
 *
 * Description: Data class for expanded digital eTOF information:
 * eTOF hits created out of 2 or more eTOF digis from different sides of the
 * MRPC counters 
 *
 ***************************************************************************
 *
 * $Log: StETofHit.cxx,v $
 * Revision 2.2  2019/02/11 18:53:09  ullrich
 * Added additional access functions to get the associated track & idTruth and qaTruth variables for simulated Hits.
 *
 * Revision 2.1  2018/07/09 14:53:48  ullrich
 * Initial Revision.
 *
 *
 ***************************************************************************/
#include "StETofHit.h"

StETofHit::StETofHit()
: mSector(0),
  mZPlane(0),
  mCounter(0),
  mTime(0), 
  mTotalTot(0),
  mClusterSize(0),
  mLocalX(0),
  mLocalY(0),
  mAssociatedTrack(0),
  mIdTruth(0),
  mQuality(0)
{
    /* no op */
}


StETofHit::StETofHit( const unsigned int sector, const unsigned int zPlane,  const unsigned int counter,
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
  mAssociatedTrack(0),
  mIdTruth(0),
  mQuality(0)
{
    /* no op */
}


StETofHit::StETofHit( const StETofHit& hitIn )
: mSector(hitIn.mSector),
  mZPlane(hitIn.mZPlane),
  mCounter(hitIn.mCounter),
  mTime(hitIn.mTime),
  mTotalTot(hitIn.mTotalTot),
  mClusterSize(hitIn.mClusterSize),
  mLocalX(hitIn.mLocalX),
  mLocalY(hitIn.mLocalY),
  mAssociatedTrack(hitIn.mAssociatedTrack),
  mIdTruth(hitIn.mIdTruth),
  mQuality(hitIn.mQuality)
{
    /* no op */
}


StETofHit::~StETofHit()
{
    /* no op */
}


/// ordering operators sorted by calibrated time.
bool
StETofHit::operator <( const StETofHit& rhs ) const
{
   return ( this->time() < rhs.time() ) ? kTRUE : kFALSE;
}


int
StETofHit::compare( const StObject* obj ) const
{
   return compare( ( StETofHit* ) obj );
}


int
StETofHit::compare( const StETofHit* hit ) const
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
StETofHit::setHwAddress( const unsigned int sector, const unsigned int zPlane, const unsigned int counter )
{
    mSector  = sector;
    mZPlane  = zPlane;
    mCounter = counter;
}

void
StETofHit::setIdTruth( unsigned short idtruth, unsigned short qatruth )
{
    if (qatruth==0) qatruth = (idtruth>>16);
    idtruth    = idtruth&((1<<16)-1);
    mIdTruth = static_cast<UShort_t>(idtruth);
    mQuality = static_cast<UShort_t>(qatruth);
}


ostream&
operator<<( ostream &os, const StETofHit& hit )
{
    os << " Time "          << hit.time()        << endl
       << " TotalTot "      << hit.totalTot()    << endl
       << " Clustersize "   << hit.clusterSize() << endl
       << " Sector "        << hit.sector()      << endl
       << " Plane "         << hit.zPlane()      << endl
       << " Counter "       << hit.counter()     << endl
       << " LocalX "        << hit.localX()      << endl
       << " LocalY "        << hit.localY()      << endl
       << " idTruth "       << hit.idTruth()     << endl
       << " qaTruth "       << hit.qaTruth()     << endl;
    return os;
}
