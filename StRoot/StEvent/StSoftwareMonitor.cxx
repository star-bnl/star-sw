/***************************************************************************
 *
 * $Id: StSoftwareMonitor.cxx,v 2.3 1999/11/05 11:36:00 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSoftwareMonitor.cxx,v $
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
#include "StTpcSoftwareMonitor.h"
#include "StSvtSoftwareMonitor.h"
#include "StFtpcSoftwareMonitor.h"
#include "StEmcSoftwareMonitor.h"
#include "StRichSoftwareMonitor.h"
#include "StCtbSoftwareMonitor.h"
#include "StGlobalSoftwareMonitor.h"
#include "StL3SoftwareMonitor.h"
#include "tables/St_dst_mon_soft_tpc_Table.h"
#include "tables/St_dst_mon_soft_svt_Table.h"
#include "tables/St_dst_mon_soft_ftpc_Table.h"
#include "tables/St_dst_mon_soft_emc_Table.h"
#include "tables/St_dst_mon_soft_ctb_Table.h"
#include "tables/St_dst_mon_soft_rich_Table.h"
#include "tables/St_dst_mon_soft_glob_Table.h"
#include "tables/St_dst_mon_soft_l3_Table.h"

static const char rcsid[] = "$Id: StSoftwareMonitor.cxx,v 2.3 1999/11/05 11:36:00 ullrich Exp $";

ClassImp(StSoftwareMonitor)

StSoftwareMonitor::StSoftwareMonitor()
{
    mTpcMonitor = 0;
    mSvtMonitor = 0;
    mFtpcMonitor = 0;
    mEmcMonitor = 0;
    mRichMonitor = 0;
    mCtbMonitor = 0;
    mGlobalMonitor = 0;
    mL3Monitor = 0;
}

StSoftwareMonitor::StSoftwareMonitor(const dst_mon_soft_tpc_st* tpcMon,
                                     const dst_mon_soft_svt_st* svtMon,
                                     const dst_mon_soft_ftpc_st* ftpcMon,
                                     const dst_mon_soft_emc_st* emcMon,
                                     const dst_mon_soft_ctb_st* ctbMon,
                                     const dst_mon_soft_rich_st* richMon,
                                     const dst_mon_soft_glob_st* globalMon,
                                     const dst_mon_soft_l3_st* l3Mon)
{
    if (tpcMon)
        mTpcMonitor = new StTpcSoftwareMonitor(*tpcMon);
    else
        mTpcMonitor = 0;
    
    if (svtMon)
        mSvtMonitor = new StSvtSoftwareMonitor(*svtMon);
    else
        mSvtMonitor = 0;

    if (ftpcMon)
        mFtpcMonitor = new StFtpcSoftwareMonitor(*ftpcMon);
    else
        mFtpcMonitor = 0;

    if (emcMon)
        mEmcMonitor = new StEmcSoftwareMonitor(*emcMon);
    else
        mEmcMonitor = 0;

    if (richMon)
        mRichMonitor = new StRichSoftwareMonitor(*richMon);
    else
        mRichMonitor = 0;

    if (ctbMon)
        mCtbMonitor = new StCtbSoftwareMonitor(*ctbMon);
    else
        mCtbMonitor = 0;
    
    if (globalMon)
        mGlobalMonitor = new StGlobalSoftwareMonitor(*globalMon);
    else
        mGlobalMonitor = 0;

    if (l3Mon)
        mL3Monitor = new StL3SoftwareMonitor(*l3Mon);
    else
        mL3Monitor = 0;
}

StSoftwareMonitor::~StSoftwareMonitor()
{
    delete mTpcMonitor;    mTpcMonitor = 0;
    delete mSvtMonitor;    mSvtMonitor = 0;
    delete mFtpcMonitor;   mFtpcMonitor = 0;
    delete mEmcMonitor;    mEmcMonitor = 0;
    delete mRichMonitor;   mRichMonitor = 0;
    delete mCtbMonitor;    mCtbMonitor = 0;
    delete mGlobalMonitor; mGlobalMonitor = 0;
    delete mL3Monitor;     mL3Monitor = 0;
}

StSoftwareMonitor::StSoftwareMonitor(const StSoftwareMonitor& mon)
{
    if (mon.mTpcMonitor)
        mTpcMonitor = new StTpcSoftwareMonitor(*(mon.mTpcMonitor));
    else
        mTpcMonitor = 0;
    if (mon.mSvtMonitor)
        mSvtMonitor = new StSvtSoftwareMonitor(*(mon.mSvtMonitor));
    else
        mSvtMonitor = 0;
    if (mon.mFtpcMonitor)
        mFtpcMonitor = new StFtpcSoftwareMonitor(*(mon.mFtpcMonitor));
    else
        mFtpcMonitor = 0;
    if (mon.mEmcMonitor)
        mEmcMonitor = new StEmcSoftwareMonitor(*(mon.mEmcMonitor));
    else
        mEmcMonitor = 0;
    if (mon.mRichMonitor)
        mRichMonitor = new StRichSoftwareMonitor(*(mon.mRichMonitor));
    else
        mRichMonitor = 0;
    if (mon.mCtbMonitor)
        mCtbMonitor = new StCtbSoftwareMonitor(*(mon.mCtbMonitor));
    else
        mCtbMonitor = 0;
    if (mon.mGlobalMonitor)
        mGlobalMonitor = new StGlobalSoftwareMonitor(*(mon.mGlobalMonitor));
    else
        mGlobalMonitor = 0;
    if (mon.mL3Monitor)
        mL3Monitor = new StL3SoftwareMonitor(*(mon.mL3Monitor));
    else
        mL3Monitor = 0;
};

StSoftwareMonitor&
StSoftwareMonitor::operator=(const StSoftwareMonitor& mon)
{
    if (this != &mon) {
        delete mTpcMonitor;    mTpcMonitor = 0;
        delete mSvtMonitor;    mSvtMonitor = 0;
        delete mFtpcMonitor;   mFtpcMonitor = 0;
        delete mEmcMonitor;    mEmcMonitor = 0;
        delete mRichMonitor;   mRichMonitor = 0;
        delete mCtbMonitor;    mCtbMonitor = 0;
        delete mGlobalMonitor; mGlobalMonitor = 0;
        delete mL3Monitor;     mL3Monitor = 0;
        if (mon.mTpcMonitor)
            mTpcMonitor = new StTpcSoftwareMonitor(*(mon.mTpcMonitor));
        else
            mTpcMonitor = 0;
        if (mon.mSvtMonitor)
            mSvtMonitor = new StSvtSoftwareMonitor(*(mon.mSvtMonitor));
        else
            mSvtMonitor = 0;
        if (mon.mFtpcMonitor)
            mFtpcMonitor = new StFtpcSoftwareMonitor(*(mon.mFtpcMonitor));
        else
            mFtpcMonitor = 0;
        if (mon.mEmcMonitor)
            mEmcMonitor = new StEmcSoftwareMonitor(*(mon.mEmcMonitor));
        else
            mEmcMonitor = 0;
        if (mon.mRichMonitor)
            mRichMonitor = new StRichSoftwareMonitor(*(mon.mRichMonitor));
        else
            mRichMonitor = 0;
        if (mon.mCtbMonitor)
            mCtbMonitor = new StCtbSoftwareMonitor(*(mon.mCtbMonitor));
        else
            mCtbMonitor = 0;
        if (mon.mGlobalMonitor)
            mGlobalMonitor = new StGlobalSoftwareMonitor(*(mon.mGlobalMonitor));
        else
            mGlobalMonitor = 0;
        if (mon.mL3Monitor)
            mL3Monitor = new StL3SoftwareMonitor(*(mon.mL3Monitor));
        else
            mL3Monitor = 0;
    }
    return *this;
}

const StTpcSoftwareMonitor*
StSoftwareMonitor::tpc() const { return mTpcMonitor; }

const StSvtSoftwareMonitor*
StSoftwareMonitor::svt() const { return mSvtMonitor; }

const StFtpcSoftwareMonitor*
StSoftwareMonitor::ftpc() const { return mFtpcMonitor; }

const StEmcSoftwareMonitor*
StSoftwareMonitor::emc() const { return mEmcMonitor; }

const StRichSoftwareMonitor*
StSoftwareMonitor::rich() const { return mRichMonitor; }

const StCtbSoftwareMonitor*
StSoftwareMonitor::ctb() const { return mCtbMonitor; }

const StGlobalSoftwareMonitor*
StSoftwareMonitor::global() const { return mGlobalMonitor; }

const StL3SoftwareMonitor*
StSoftwareMonitor::l3() const { return mL3Monitor; }

StTpcSoftwareMonitor*
StSoftwareMonitor::tpc() { return mTpcMonitor; }

StSvtSoftwareMonitor*
StSoftwareMonitor::svt() { return mSvtMonitor; }

StFtpcSoftwareMonitor*
StSoftwareMonitor::ftpc() { return mFtpcMonitor; }

StEmcSoftwareMonitor*
StSoftwareMonitor::emc() { return mEmcMonitor; }

StRichSoftwareMonitor*
StSoftwareMonitor::rich() { return mRichMonitor; }

StCtbSoftwareMonitor*
StSoftwareMonitor::ctb() { return mCtbMonitor; }

StGlobalSoftwareMonitor*
StSoftwareMonitor::global() { return mGlobalMonitor; }

StL3SoftwareMonitor*
StSoftwareMonitor::l3() { return mL3Monitor; }

void
StSoftwareMonitor::setTpcSoftwareMonitor(StTpcSoftwareMonitor* val)
{
    if (mTpcMonitor) delete mTpcMonitor;
    mTpcMonitor = val;
}

void
StSoftwareMonitor::setSvtSoftwareMonitor(StSvtSoftwareMonitor* val)
{
    if (mSvtMonitor) delete mSvtMonitor;
    mSvtMonitor = val;
}

void
StSoftwareMonitor::setFtpcSoftwareMonitor(StFtpcSoftwareMonitor* val)
{
    if (mFtpcMonitor) delete mFtpcMonitor;
    mFtpcMonitor = val;
}
  
void
StSoftwareMonitor::setEmcSoftwareMonitor(StEmcSoftwareMonitor* val)
{
    if (mEmcMonitor) delete mEmcMonitor;
    mEmcMonitor = val;
}
      
void
StSoftwareMonitor::setRichSoftwareMonitor(StRichSoftwareMonitor* val)
{
    if (mRichMonitor) delete mRichMonitor;
    mRichMonitor = val;
}

void
StSoftwareMonitor::setCtbSoftwareMonitor(StCtbSoftwareMonitor* val)
{
    if (mCtbMonitor) delete mCtbMonitor;
    mCtbMonitor = val;
}

void
StSoftwareMonitor::setGlobalSoftwareMonitor(StGlobalSoftwareMonitor* val)
{
    if (mGlobalMonitor) delete mGlobalMonitor;
    mGlobalMonitor = val;
}

void
StSoftwareMonitor::setL3SoftwareMonitor(StL3SoftwareMonitor* val)
{
    if (mL3Monitor) delete mL3Monitor;
    mL3Monitor = val;
}
