/***************************************************************************
 *
 * $Id: StMtdRawHit.cxx,v 2.1 2011/04/25 21:24:02 ullrich Exp $
 *
 * Author: Frank Geurts, April 25, 2011
 ***************************************************************************
 *
 * Description: TOF raw hits from daq
 *
 ***************************************************************************
 *
 * $Log: StMtdRawHit.cxx,v $
 * Revision 2.1  2011/04/25 21:24:02  ullrich
 * Initial Revision.
 *
 **************************************************************************/

#include "StMtdRawHit.h"

static const char rcsid[] = "$Id: StMtdRawHit.cxx,v 2.1 2011/04/25 21:24:02 ullrich Exp $";

ClassImp(StMtdRawHit)

StMtdRawHit::StMtdRawHit() {
    mFlag = 0;
    mBackLeg = 0;
    mChannel = 0;
    mTdc = 0;
}

StMtdRawHit::StMtdRawHit(char iflag, unsigned char backleg,
                         unsigned char channel, unsigned int rawTdc) {
    mFlag = iflag;
    mBackLeg = backleg;
    mChannel = channel;
    mTdc = rawTdc;
}

StMtdRawHit::~StMtdRawHit() { /* noop */ }

int StMtdRawHit::operator==(const StMtdRawHit& p) const {
    return (p.mFlag == mFlag &&
            p.mBackLeg == mBackLeg &&
            p.mChannel == mChannel &&
            p.mTdc == mTdc );
}

int StMtdRawHit::operator!=(const StMtdRawHit& p) const {
    return !(*this == p);  // use operator==()
}

ostream& operator<<(ostream &os, const StMtdRawHit& hit) {
    os << " Flag " << hit.flag() << endl
       << " BackLeg " << hit.backleg() << endl
       << " Channel " << hit.channel() << endl
       << " Tdc " << hit.tdc() << endl ;
    return os;
}
