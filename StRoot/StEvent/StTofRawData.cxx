/***************************************************************************
 *
 * $Id: StTofRawData.cxx,v 2.1 2005/04/11 22:35:25 calderon Exp $
 *
 * Author: Xin Dong, Feb 2005
 ***************************************************************************
 *
 * Description: TOF raw hits (from Run 5)
 *
 ***************************************************************************
 *
 * $Log: StTofRawData.cxx,v $
 * Revision 2.1  2005/04/11 22:35:25  calderon
 * Tof Classes for Run 5.  Modifications and additions from Xin to
 * take care of new TOF daq and electronics.  Added StTofRawData and
 * modified containers and includes.
 *
 *
 **************************************************************************/

#include "StTofRawData.h"

static const char rcsid[] = "$Id: StTofRawData.cxx,v 2.1 2005/04/11 22:35:25 calderon Exp $";

ClassImp(StTofRawData)

StTofRawData::StTofRawData()
  : mLeTeFlag(0), mChannel(0), mTdc(0), mQuality(0)
{ /* noop */ }

StTofRawData::StTofRawData(unsigned short iflag, unsigned short channel,
			   unsigned int rawTdc, unsigned short quality)
  : mLeTeFlag(iflag), mChannel(channel), mTdc(rawTdc), mQuality(quality)
{ /* noop */ }

StTofRawData::~StTofRawData() { /* noop */ }
    
int StTofRawData::operator==(const StTofRawData& p) const
{
    return (p.mLeTeFlag == mLeTeFlag &&
            p.mChannel == mChannel && p.mTdc == mTdc &&
	    p.mQuality == mQuality);
}

int StTofRawData::operator!=(const StTofRawData& p) const
{
    return !(*this == p);  // use operator==()
}
/*
ostream& operator<<(ostream& os, const StTofRawData& d)
{
  return (os << "StTofRawData::> "<< " flag=" << d.flag()
	     << ", channel=" << d.channel()
	     << ", tdc=" << d.tdc());
}
*/
