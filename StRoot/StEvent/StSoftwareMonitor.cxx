/***************************************************************************
 *
 * $Id: StSoftwareMonitor.cxx,v 2.7 2009/11/23 16:34:07 fisyak Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSoftwareMonitor.cxx,v $
 * Revision 2.7  2009/11/23 16:34:07  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
 * Revision 2.6  2002/11/26 02:19:11  perev
 * StEventMaker ITTF modif
 *
 * Revision 2.5  2001/04/05 04:00:54  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.4  2000/12/08 03:53:41  ullrich
 * Prepared hooks for ToF.
 *
 * Revision 2.3  1999/11/05 11:36:00  ullrich
 * Added non-const version of methods
 *
 * Revision 2.2  1999/10/28 22:26:33  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:09  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StSoftwareMonitor.h"
#include "StFtpcSoftwareMonitor.h"
#include "StRichSoftwareMonitor.h"

static const char rcsid[] = "$Id: StSoftwareMonitor.cxx,v 2.7 2009/11/23 16:34:07 fisyak Exp $";

ClassImp(StSoftwareMonitor)

StSoftwareMonitor::StSoftwareMonitor()
{
    mFtpcMonitor = 0;
    mRichMonitor = 0;
}

StSoftwareMonitor::StSoftwareMonitor(const dst_mon_soft_ftpc_st* ftpcMon,
                                     const dst_mon_soft_rich_st* richMon)
{
    if (ftpcMon)
        mFtpcMonitor = new StFtpcSoftwareMonitor(*ftpcMon);
    else
        mFtpcMonitor = 0;

    if (richMon)
        mRichMonitor = new StRichSoftwareMonitor(*richMon);
    else
        mRichMonitor = 0;

}
void StSoftwareMonitor::setFtpcSoftwareMonitor  (dst_mon_soft_ftpc_st *tab)
{if (tab) mFtpcMonitor   = new StFtpcSoftwareMonitor                 (*tab);}
void StSoftwareMonitor::setRichSoftwareMonitor  (dst_mon_soft_rich_st *tab)
{if (tab) mRichMonitor   = new StRichSoftwareMonitor                 (*tab);}

StSoftwareMonitor::~StSoftwareMonitor()
{
    delete mFtpcMonitor;   mFtpcMonitor = 0;
    delete mRichMonitor;   mRichMonitor = 0;
}

StSoftwareMonitor::StSoftwareMonitor(const StSoftwareMonitor& mon)
{
    if (mon.mFtpcMonitor)
        mFtpcMonitor = new StFtpcSoftwareMonitor(*(mon.mFtpcMonitor));
    else
        mFtpcMonitor = 0;
    if (mon.mRichMonitor)
        mRichMonitor = new StRichSoftwareMonitor(*(mon.mRichMonitor));
    else
        mRichMonitor = 0;
};

StSoftwareMonitor&
StSoftwareMonitor::operator=(const StSoftwareMonitor& mon)
{
    if (this != &mon) {
        delete mFtpcMonitor;   mFtpcMonitor = 0;
        delete mRichMonitor;   mRichMonitor = 0;
        if (mon.mFtpcMonitor)
            mFtpcMonitor = new StFtpcSoftwareMonitor(*(mon.mFtpcMonitor));
        else
            mFtpcMonitor = 0;
        if (mon.mRichMonitor)
            mRichMonitor = new StRichSoftwareMonitor(*(mon.mRichMonitor));
        else
            mRichMonitor = 0;
    }
    return *this;
}


const StFtpcSoftwareMonitor*
StSoftwareMonitor::ftpc() const { return mFtpcMonitor; }

const StRichSoftwareMonitor*
StSoftwareMonitor::rich() const { return mRichMonitor; }


StFtpcSoftwareMonitor*
StSoftwareMonitor::ftpc() { return mFtpcMonitor; }

StRichSoftwareMonitor*
StSoftwareMonitor::rich() { return mRichMonitor; }

void
StSoftwareMonitor::setFtpcSoftwareMonitor(StFtpcSoftwareMonitor* val)
{
    if (mFtpcMonitor) delete mFtpcMonitor;
    mFtpcMonitor = val;
}
  
void
StSoftwareMonitor::setRichSoftwareMonitor(StRichSoftwareMonitor* val)
{
    if (mRichMonitor) delete mRichMonitor;
    mRichMonitor = val;
}
