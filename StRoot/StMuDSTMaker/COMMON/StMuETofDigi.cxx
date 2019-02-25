/***************************************************************************
 *
 * $Id: StMuETofDigi.cxx,v 1.1 2019/02/21 13:32:54 jdb Exp $
 *
 * Author: Florian Seck, October 2018
 ***************************************************************************
 *
 * Description: Data class for expanded digital eTOF information in MuDsts:
 * eTOF digis capture the electronic response of each side of the MRPC
 * counter read-out
 *
 ***************************************************************************
 *
 * $Log: StMuETofDigi.cxx,v $
 * Revision 1.1  2019/02/21 13:32:54  jdb
 * Inclusion of ETOF MuDst code. This code adds support for the full set of ETOF data which includes EtofDigi, EtofHit, EtofHeader. The code essentially copies similar structures from StEvent and additionally rebuilds the maps between Digis and Hits. Accessor methods are added based on the pattern from BTOF to provide access to data at various levels. The code for accessing the PID traits provided by ETOF is also provided
 *
 *
 ***************************************************************************/ 
#include "StMuETofDigi.h"
#include "StETofDigi.h"
#include <cmath>


StMuETofDigi::StMuETofDigi() 
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
  mAssociatedHitId( -1 )
{

}


StMuETofDigi::StMuETofDigi( const unsigned int sector, const unsigned int zPlane, const unsigned int counter,
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
  mAssociatedHitId( -1 )
{

}


StMuETofDigi::StMuETofDigi( const unsigned int rocId, const unsigned int get4Id, const unsigned int elChan,   
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
  mAssociatedHitId( -1 )
{

}


StMuETofDigi::StMuETofDigi( const StMuETofDigi& digiIn )
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
  mAssociatedHitId( digiIn.mAssociatedHitId )
{

}


StMuETofDigi::StMuETofDigi( const StETofDigi* digiIn )
: mSector(    digiIn->sector()    ),
  mZPlane(    digiIn->zPlane()    ),
  mCounter(   digiIn->counter()   ),
  mStrip(     digiIn->strip()     ),
  mSide(      digiIn->side()      ),
  mRocId(     digiIn->rocId()     ),
  mGet4Id(    digiIn->get4Id()    ),
  mElChan(    digiIn->elChan()    ),
  mRawTime(   digiIn->rawTime()   ),
  mCalibTime( digiIn->calibTime() ),
  mRawTot(    digiIn->rawTot()    ),
  mCalibTot(  digiIn->calibTot()  ), 
  mAssociatedHitId( -1 )
{

}


StMuETofDigi::StMuETofDigi( const StETofDigi* digiIn, const int assocHitId )
: mSector(    digiIn->sector()    ),
  mZPlane(    digiIn->zPlane()    ),
  mCounter(   digiIn->counter()   ),
  mStrip(     digiIn->strip()     ),
  mSide(      digiIn->side()      ),
  mRocId(     digiIn->rocId()     ),
  mGet4Id(    digiIn->get4Id()    ),
  mElChan(    digiIn->elChan()    ),
  mRawTime(   digiIn->rawTime()   ),
  mCalibTime( digiIn->calibTime() ),
  mRawTot(    digiIn->rawTot()    ),
  mCalibTot(  digiIn->calibTot()  ), 
  mAssociatedHitId( assocHitId )
{

}



StMuETofDigi::~StMuETofDigi()
{

}


//Ordering operators sorted by calibrated time.
bool
StMuETofDigi::operator < ( const StMuETofDigi& rhs ) const
{
    return ( this->calibTime() < rhs.calibTime() ) ? kTRUE : kFALSE;
}


int
StMuETofDigi::compare( const TObject* obj ) const
{
    return compare( ( StMuETofDigi* )  obj );
}


int
StMuETofDigi::compare( const StMuETofDigi* digi ) const
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
StMuETofDigi::setHwAddress( const unsigned int iRocId, const unsigned int iGet4Id, const unsigned int iElChan )
{
    mRocId  = iRocId;
    mGet4Id = iGet4Id;
    mElChan = iElChan;
}


void
StMuETofDigi::setGeoAddress( const unsigned int iSector, const unsigned int iZPlane,
                             const unsigned int iCounter, const unsigned int iChannel, const unsigned int iSide )
{
    mSector  = iSector;
    mZPlane  = iZPlane;
    mCounter = iCounter;
    mStrip   = iChannel;
    mSide    = iSide;
}