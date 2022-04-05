/***************************************************************************
 *
 * $Id: StTofRawData.cxx,v 2.3 2008/02/26 20:01:57 ullrich Exp $
 *
 * Author: Xin Dong, Feb 2005
 ***************************************************************************
 *
 * Description: TOF raw hits (from Run 5)
 *
 ***************************************************************************
 *
 * $Log: StTofRawData.cxx,v $
 * Revision 2.3  2008/02/26 20:01:57  ullrich
 * Added new member mTriggerrime and related methods.
 *
 * Revision 2.2  2007/11/21 00:28:31  ullrich
 * Added new data member mTray plus access functions. New overloaded constructor.
 *
 * Revision 2.1  2005/04/11 22:35:25  calderon
 * Tof Classes for Run 5.  Modifications and additions from Xin to
 * take care of new TOF daq and electronics.  Added StTofRawData and
 * modified containers and includes.
 *
 *
 **************************************************************************/

#include "StTofRawData.h"

static const char rcsid[] = "$Id: StTofRawData.cxx,v 2.3 2008/02/26 20:01:57 ullrich Exp $";

ClassImp(StTofRawData)

StTofRawData::StTofRawData()
{
    mLeTeFlag = 0;
    mTray = 0;
    mChannel = 0;
    mTdc = 0;
    mTriggertime = 0;
    mQuality = 0;
 }

StTofRawData::StTofRawData(unsigned short iflag,
		       unsigned short channel,
		       unsigned int rawTdc,
		       unsigned short quality)
{
    mLeTeFlag = iflag;
    mTray = 0;
    mChannel = channel;
    mTdc = rawTdc;
    mTriggertime = 0;
    mQuality = quality;
}

StTofRawData::StTofRawData(unsigned short iflag,
		       unsigned short tray,
		       unsigned short channel,
		       unsigned int rawTdc,
		       unsigned short quality)
{
    mLeTeFlag = iflag;
    mTray = tray;
    mChannel = channel;
    mTdc = rawTdc;
    mTriggertime = 0;
    mQuality = quality;
 }

StTofRawData::StTofRawData(unsigned short iflag,
		       unsigned short tray,
		       unsigned short channel,
	                 unsigned int rawTdc,
		       unsigned int triggertime,
		       unsigned short quality)
{
    mLeTeFlag = iflag;
    mTray = tray;
    mChannel = channel;
    mTdc = rawTdc;
    mTriggertime = triggertime;
    mQuality = quality;
 }

StTofRawData::~StTofRawData() { /* noop */ }
    
int StTofRawData::operator==(const StTofRawData& p) const
{
    return (p.mLeTeFlag == mLeTeFlag &&
	  p.mTray == mTray &&
            p.mChannel == mChannel &&
	  p.mTdc == mTdc && 
 	  p.mQuality == mQuality &&
	  p.mTriggertime == mTriggertime);
}

int StTofRawData::operator!=(const StTofRawData& p) const
{
    return !(*this == p);  // use operator==()
}
