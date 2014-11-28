/***************************************************************************
 *
 * $Id: StRichMCInfo.cxx,v 2.2 2001/04/05 04:00:53 ullrich Exp $
 *
 * Author: Brian Lasiuk, May 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichMCInfo.cxx,v $
 * Revision 2.2  2001/04/05 04:00:53  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  2000/05/22 21:44:35  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StRichMCInfo.h"

static const char rcsid[] = "$Id: StRichMCInfo.cxx,v 2.2 2001/04/05 04:00:53 ullrich Exp $";

ClassImp(StRichMCInfo)

StRichMCInfo::StRichMCInfo()
    :  mId(-1), mGid(-1), mTrackp(-1), mCharge(-1), mProcess(-1)
{ /* noop */ }

StRichMCInfo::StRichMCInfo(int id, int gid,  int trk,
                           float q, int pro)
    : mId(id), mGid(gid), mTrackp(trk), mCharge(q), mProcess(pro)
{ /* noop */ }

StRichMCInfo::~StRichMCInfo() { /* noop */ }
    
int
StRichMCInfo::operator==(const StRichMCInfo& p) const
{
    return (p.id()      == mId     &&
            p.gid()     == mGid    &&
            p.trackp()  == mTrackp &&
            p.charge()  == mCharge &&
            p.process() == mProcess);
}

int
StRichMCInfo::operator!=(const StRichMCInfo& p) const
{
    return !(*this == p);  // use operator==()
}
