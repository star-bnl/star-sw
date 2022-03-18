/***************************************************************************
 *
 * $Id: StFttRawHit.cxx,v 2.1 2021/11/18 14:53:48 jdb Exp $
 *
 * Author: jdb, Nov 2021
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************/ 
#include "StFttRawHit.h"
#include <cmath>


StFttRawHit::StFttRawHit() 
: mSector(0),
mRDO(0),
mFEB(0),
mVMM(0),
mChannel(0),
mADC(0),
mBCID(0),
mTB(-32000),
mPlane(255),
mQuadrant(kFttUnknownQuadrant),
mRow(255),
mStrip(255),
mOrientation(kFttUnknownOrientation)
{ /*noop*/ }

StFttRawHit::StFttRawHit(   UChar_t mSector, UChar_t mRDO, UChar_t mFEB, 
                            UChar_t mVMM, UChar_t mChannel, UShort_t mADC, 
                            UShort_t mBCID, Short_t mTB){
    setRaw( mSector, mRDO, mFEB, mVMM, mChannel, mADC, mBCID, mTB);
}

void StFttRawHit::setRaw(   UChar_t mSector, UChar_t mRDO, UChar_t mFEB, 
                            UChar_t mVMM, UChar_t mChannel, UShort_t mADC, 
                            UShort_t mBCID, Short_t mTB){
    this->mSector  = mSector;
    this->mRDO     = mRDO;
    this->mFEB     = mFEB;
    this->mVMM     = mVMM;
    this->mChannel = mChannel;
    this->mADC     = mADC;
    this->mBCID    = mBCID;
    this->mTB      = mTB;
}

void StFttRawHit::setMapping(   UChar_t mPlane, UChar_t mQuadrant, 
                                UChar_t mRow, UChar_t mStrip, UChar_t mOrientation ){
    this->mPlane       = mPlane;
    this->mQuadrant    = mQuadrant;
    this->mRow         = mRow;
    this->mStrip       = mStrip;
    this->mOrientation = mOrientation;
}



ostream&
operator<<( ostream &os, const StFttRawHit& rh )
{
    os << " StFttRawHit( "    << endl;
    os << "\tmSector = "      << (int)rh.sector()      << endl;
    os << "\tmRDO = "         << (int)rh.rdo()         << endl;
    os << "\tmFEB = "         << (int)rh.feb()         << endl;
    os << "\tmVMM = "         << (int)rh.vmm()         << endl;
    os << "\tmChannel = "     << (int)rh.channel()     << endl;
    os << "\tmADC = "         << (int)rh.adc()         << endl;
    os << "\tmBCID = "        << (int)rh.bcid()        << endl;
    os << "\tmTB = "          << (int)rh.tb()          << endl;
    os << "\tmPlane = "       << (int)rh.plane()       << endl;
    os << "\tmQuadrant = "    << (int)rh.quadrant()    << endl;
    os << "\tmRow = "         << (int)rh.row()         << endl;
    os << "\tmStrip = "       << (int)rh.strip()       << endl;
    os << "\tmOrientation = " << (int)rh.orientation() << " ) " << endl;


    return os;
}