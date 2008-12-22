/***************************************************************************
 *
 * $Id: StBTofRawHit.cxx,v 2.1 2008/12/22 20:31:00 ullrich Exp $
 *
 * Author: Xin Dong, Nov 2008
 ***************************************************************************
 *
 * Description: TOF raw hits from daq
 *
 ***************************************************************************
 *
 * $Log: StBTofRawHit.cxx,v $
 * Revision 2.1  2008/12/22 20:31:00  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/

#include "StBTofRawHit.h"

static const char rcsid[] = "$Id: StBTofRawHit.cxx,v 2.1 2008/12/22 20:31:00 ullrich Exp $";

ClassImp(StBTofRawHit)

StBTofRawHit::StBTofRawHit()
{
    mLeTeFlag = 0;
    mTray = 0;
    mChannel = 0;
    mTdc = 0;
 }

StBTofRawHit::StBTofRawHit(unsigned char iflag, unsigned char tray,
          	       unsigned char channel, unsigned int rawTdc)
{
    mLeTeFlag = iflag;
    mTray = tray;
    mChannel = channel;
    mTdc = rawTdc;
}

StBTofRawHit::~StBTofRawHit() { /* noop */ }
    
int StBTofRawHit::operator==(const StBTofRawHit& p) const
{
    return (p.mLeTeFlag == mLeTeFlag &&
	    p.mTray == mTray &&
            p.mChannel == mChannel &&
	    p.mTdc == mTdc );
}

int StBTofRawHit::operator!=(const StBTofRawHit& p) const
{
    return !(*this == p);  // use operator==()
}

ostream&
operator<<(ostream &os, const StBTofRawHit& hit)
{
    os << " Le/Te " << hit.leteFlag() << endl
       << " Tray " << hit.tray() << endl
       << " Channel " << hit.channel() << endl
       << " Tdc " << hit.tdc() << endl ;
    return os;
}
