/***************************************************************************
 *
 * $Id: StETofDigi.cxx,v 2.1 2018/07/09 14:53:48 ullrich Exp $
 *
 * Author: Philipp Weidenkaff, April 2018
 ***************************************************************************
 *
 * Description: Data class for expanded digital eTOF information:
 * eTOF digis capture the electronic response of each side of the MRPC
 * counter read-out
 *
 ***************************************************************************
 *
 * $Log: StETofDigi.cxx,v $
 * Revision 2.1  2018/07/09 14:53:48  ullrich
 * Initial Revision.
 *
 *
 ***************************************************************************/ 
#include "StETofDigi.h"
#include <cmath>


StETofDigi::StETofDigi() 
: mSector(0),
  mZPlane(0),
  mCounter(0),
  mStrip(0),
  mSide(0),
  mRocId(0),
  mGet4Id(0),
  mElChan(0),
  mRawTime(0),
  mCalibTime(0),
  mRawTot(-1),
  mCalibTot(-1), 
  mAssociatedHit(0)
{

}


StETofDigi::StETofDigi( const unsigned int sector, const unsigned int zPlane, const unsigned int counter,
                        const unsigned int strip, const unsigned int side,
                        const double& time, const double& tot )
: mSector(sector),
  mZPlane(zPlane),
  mCounter(counter),
  mStrip(strip),
  mSide(side),
  mRocId(0),
  mGet4Id(0),
  mElChan(0),
  mRawTime(time),
  mCalibTime(0),
  mRawTot(tot),
  mCalibTot(-1), 
  mAssociatedHit(0)
{

}


StETofDigi::StETofDigi( const unsigned int rocId, const unsigned int get4Id, const unsigned int elChan,   
                        const double& time, const double& tot )
: mSector(0),
  mZPlane(0),
  mCounter(0),
  mStrip(0),
  mSide(0),
  mRocId(rocId),
  mGet4Id(get4Id),
  mElChan(elChan),
  mRawTime(time),
  mCalibTime(0),
  mRawTot(tot),
  mCalibTot(-1), 
  mAssociatedHit(0)
{

}


StETofDigi::StETofDigi( const StETofDigi& digiIn )
: mSector(digiIn.mSector),
  mZPlane(digiIn.mZPlane),
  mCounter(digiIn.mCounter),
  mStrip(digiIn.mStrip),
  mSide(digiIn.mSide),
  mRocId(digiIn.mRocId),
  mGet4Id(digiIn.mGet4Id),
  mElChan(digiIn.mElChan),
  mRawTime(digiIn.mRawTime),
  mCalibTime(digiIn.mCalibTime),
  mRawTot(digiIn.mRawTot),
  mCalibTot(digiIn.mCalibTot), 
  mAssociatedHit(digiIn.mAssociatedHit)
{

}


StETofDigi::~StETofDigi()
{

}


//Ordering operators sorted by calibrated time.
bool
StETofDigi::operator < ( const StETofDigi& rhs ) const
{
    return ( this->calibTime() < rhs.calibTime() ) ? kTRUE : kFALSE;
}


int
StETofDigi::compare( const StObject* obj ) const
{
    return compare( ( StETofDigi* )  obj );
}


int
StETofDigi::compare( const StETofDigi* digi ) const
{
    if( mCalibTime < digi->calibTime() ) {
        // this ... other
        return -1;
    }
    else if( mCalibTime > digi->calibTime() ) {
        // other ... this
        return 1;
    }
    else {
        // this = other
        return 0;
    }
}


void 
StETofDigi::setHwAddress( const unsigned int iRocId, const unsigned int iGet4Id, const unsigned int iElChan )
{
    mRocId  = iRocId;
    mGet4Id = iGet4Id;
    mElChan = iElChan;
}


void
StETofDigi::setGeoAddress( const unsigned int iSector, const unsigned int iZPlane,
                           const unsigned int iCounter, const unsigned int iChannel, const unsigned int iSide )
{
    mSector  = iSector;
    mZPlane  = iZPlane;
    mCounter = iCounter;
    mStrip   = iChannel;
    mSide    = iSide;
}


ostream&
operator<<( ostream &os, const StETofDigi& digi )
{
    os << " RawTime "   << digi.rawTime()   << endl
       << " RawTot "    << digi.rawTot()    << endl
       << " CalibTime " << digi.calibTime() << endl
       << " CalibTot "  << digi.calibTot()  << endl
       << " Sector "    << digi.sector()    << endl
       << " Plane "     << digi.zPlane()    << endl
       << " Counter "   << digi.counter()   << endl
       << " Strip "     << digi.strip()     << endl
       << " Side "      << digi.side()      << endl;

    return os;
}