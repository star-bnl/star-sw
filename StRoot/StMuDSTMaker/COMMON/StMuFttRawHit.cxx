/***************************************************************************
 *
 * $Id: StMuFttRawHit.cxx
 *
 * Author: jdb, Nov 2021
 ***************************************************************************
 *
 * Description: 
 *
 ***************************************************************************/ 
#include "StMuFttRawHit.h"
#include "StEvent/StFttRawHit.h"
#include <cmath>
#include <iostream>


StMuFttRawHit::StMuFttRawHit() 
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

StMuFttRawHit::StMuFttRawHit( StFttRawHit * stHit ){
    set( stHit );
} // ctor from StEvent

StMuFttRawHit::StMuFttRawHit(   UChar_t mSector, UChar_t mRDO, UChar_t mFEB, 
                            UChar_t mVMM, UChar_t mChannel, UShort_t mADC, 
                            UShort_t mBCID, Short_t mTB, Short_t mBCIDDelta){
    setRaw( mSector, mRDO, mFEB, mVMM, mChannel, mADC, mBCID, mTB, mBCIDDelta);
} // ctor setRaw

void StMuFttRawHit::setRaw(   UChar_t mSector, UChar_t mRDO, UChar_t mFEB, 
                            UChar_t mVMM, UChar_t mChannel, UShort_t mADC, 
                            UShort_t mBCID, Short_t mTB, Short_t mBCIDDelta){
    this->mSector    = mSector;
    this->mRDO       = mRDO;
    this->mFEB       = mFEB;
    this->mVMM       = mVMM;
    this->mChannel   = mChannel;
    this->mADC       = mADC;
    this->mBCID      = mBCID;
    this->mTB        = mTB;
    this->mBCIDDelta = mBCIDDelta;
} // setRaw

void StMuFttRawHit::setMapping(   UChar_t mPlane, UChar_t mQuadrant, 
                                UChar_t mRow, UChar_t mStrip, UChar_t mOrientation ){
    this->mPlane       = mPlane;
    this->mQuadrant    = mQuadrant;
    this->mRow         = mRow;
    this->mStrip       = mStrip;
    this->mOrientation = mOrientation;
} // setMapping

void StMuFttRawHit::set( StFttRawHit * stHit ){
    setRaw( stHit->sector(), stHit->rdo(), stHit->feb(), stHit->vmm(), stHit->channel(), stHit->adc(), stHit->bcid(), stHit->tb(), stHit->dbcid());
    setMapping( stHit->plane(), stHit->quadrant(), stHit->row(), stHit->strip(), stHit->orientation() );
} // set from StEvent object



ostream&
operator<<( ostream &os, const StMuFttRawHit& rh )
{
    using namespace std;
    os << " StMuFttRawHit( "    << endl;
    os << "\tmSector = "      << (int)rh.sector()      << endl;
    os << "\tmRDO = "         << (int)rh.rdo()         << endl;
    os << "\tmFEB = "         << (int)rh.feb()         << endl;
    os << "\tmVMM = "         << (int)rh.vmm()         << endl;
    os << "\tmChannel = "     << (int)rh.channel()     << endl;
    os << "\tmADC = "         << (int)rh.adc()         << endl;
    os << "\tmBCID = "        << (int)rh.bcid()        << endl;
    os << "\tmBCIDDelta = "   << (int)rh.dbcid()       << endl;
    os << "\tmTB = "          << (int)rh.tb()          << endl;
    os << "\tmPlane = "       << (int)rh.plane()       << endl;
    os << "\tmQuadrant = "    << (int)rh.quadrant()    << endl;
    os << "\tmRow = "         << (int)rh.row()         << endl;
    os << "\tmStrip = "       << (int)rh.strip()       << endl;
    os << "\tmOrientation = " << (int)rh.orientation() << " ) " << endl;
    return os;
} // operator<< ostream