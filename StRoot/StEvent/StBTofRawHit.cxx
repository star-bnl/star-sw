/***************************************************************************
 *
 * $Id: StBTofRawHit.cxx,v 2.2 2009/01/15 00:48:10 ullrich Exp $
 *
 * Author: Xin Dong, Nov 2008
 ***************************************************************************
 *
 * Description: TOF raw hits from daq
 *
 ***************************************************************************
 *
 * $Log: StBTofRawHit.cxx,v $
 * Revision 2.2  2009/01/15 00:48:10  ullrich
 * mLeTeFlag changed to mFlag, tray(), module(), cell() now return int.
 *
 * Revision 2.1  2008/12/22 20:31:00  ullrich
 * Initial Revision.
 *
 *
 **************************************************************************/

#include "StBTofRawHit.h"

static const char rcsid[] = "$Id: StBTofRawHit.cxx,v 2.2 2009/01/15 00:48:10 ullrich Exp $";

ClassImp(StBTofRawHit)

StBTofRawHit::StBTofRawHit()
{
    mFlag = 0;
    mTray = 0;
    mChannel = 0;
    mTdc = 0;
 }

StBTofRawHit::StBTofRawHit(char iflag, unsigned char tray,
          	       unsigned char channel, unsigned int rawTdc)
{
    mFlag = iflag;
    mTray = tray;
    mChannel = channel;
    mTdc = rawTdc;
}

StBTofRawHit::~StBTofRawHit() { /* noop */ }
    
int StBTofRawHit::operator==(const StBTofRawHit& p) const
{
    return (p.mFlag == mFlag &&
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
    os << " Flag " << hit.flag() << endl
       << " Tray " << hit.tray() << endl
       << " Channel " << hit.channel() << endl
       << " Tdc " << hit.tdc() << endl ;
    return os;
}
