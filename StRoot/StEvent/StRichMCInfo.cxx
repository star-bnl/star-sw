/***************************************************************************
 *
 * $Id: StRichMCInfo.cxx,v 2.1 2000/05/22 21:44:35 ullrich Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichMCInfo.cxx,v $
 * Revision 2.1  2000/05/22 21:44:35  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StRichMCInfo.h"

static const char rcsid[] = "$Id: StRichMCInfo.cxx,v 2.1 2000/05/22 21:44:35 ullrich Exp $";

ClassImp(StRichMCInfo)

StRichMCInfo::StRichMCInfo()
    :  mId(-1), mGid(-1), mTrackp(-1), mCharge(-1), mProcess(-1)
{ /* noop */ }

StRichMCInfo::StRichMCInfo(Long_t id, Long_t gid,  Long_t trk,
			   Float_t q, Long_t pro)
    : mId(id), mGid(gid), mTrackp(trk), mCharge(q), mProcess(pro)
{ /* noop */ }

StRichMCInfo::~StRichMCInfo() { /* noop */ }
    
Int_t
StRichMCInfo::operator==(const StRichMCInfo& p) const
{
    return (p.id()      == mId     &&
	    p.gid()     == mGid    &&
	    p.trackp()  == mTrackp &&
	    p.charge()  == mCharge &&
	    p.process() == mProcess);
}

Int_t
StRichMCInfo::operator!=(const StRichMCInfo& p) const
{
    return !(*this == p);  // use operator==()
}
