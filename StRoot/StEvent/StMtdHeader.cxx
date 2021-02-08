/***************************************************************************
 *
 * $Id: StMtdHeader.cxx,v 2.2 2015/04/24 17:51:00 ullrich Exp $
 *
 * Author: Frank Geurts, April 25, 2011
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StMtdHeader.cxx,v $
 * Revision 2.2  2015/04/24 17:51:00  ullrich
 * Added data member mTpcSectorMask and mShouldHaveRejectEvent incl. access fcts.
 *
 * Revision 2.1  2011/04/25 21:24:02  ullrich
 * Initial Revision.
 *
 **************************************************************************/
#include "StMtdHeader.h"

ClassImp(StMtdHeader)

StMtdHeader::StMtdHeader() {
    for(int i=0; i<MAXFIBER; i++) {
        mFiberHeader[i] = -1;
        mFiberTriggerWord[i] = 0;
        mTriggerTime[i] = 0;
    }
    mShouldHaveRejectEvent = -1;
    mTpcSectorMask = 0;
}

StMtdHeader::~StMtdHeader() {
    /* no op */
}

short StMtdHeader::fiberHeader(int fiberId) const {
    return mFiberHeader[fiberId];
}

unsigned int StMtdHeader::fiberTriggerWord(int fiberId) const {
    return mFiberTriggerWord[fiberId];
}


unsigned int StMtdHeader::triggerTime(int fiberId) const {
    return mTriggerTime[fiberId];
}

int StMtdHeader::shouldHaveRejectEvent() const {
    return mShouldHaveRejectEvent;
}

unsigned int StMtdHeader::tpcSectorMask() const {
    return mTpcSectorMask;
}

void StMtdHeader::setFiberHeader(int fiberId, short val) {
    mFiberHeader[fiberId] = val;
}

void StMtdHeader::setFiberTriggerWord(int fiberId, unsigned int val) {
    mFiberTriggerWord[fiberId] = val;
}

void StMtdHeader::setTriggerTime(unsigned int tdc, int fiberId) {
    mTriggerTime[fiberId] = tdc;
}

void StMtdHeader::setShouldHaveRejectEvent(int reject) {
    mShouldHaveRejectEvent = reject;
}

void StMtdHeader::setTpcSectorMask(unsigned int mask) {
    mTpcSectorMask = mask;
}
