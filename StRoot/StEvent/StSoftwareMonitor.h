/***************************************************************************
 *
 * $Id: StSoftwareMonitor.h,v 2.2 1999/11/05 11:36:04 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSoftwareMonitor.h,v $
 * Revision 2.2  1999/11/05 11:36:04  ullrich
 * Added non-const version of methods
 *
 * Revision 2.1  1999/10/13 19:43:40  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSoftwareMonitor_hh
#define StSoftwareMonitor_hh

#include "StObject.h"

class StTpcSoftwareMonitor;
class StSvtSoftwareMonitor;
class StFtpcSoftwareMonitor;
class StEmcSoftwareMonitor;
class StRichSoftwareMonitor;
class StCtbSoftwareMonitor;
class StGlobalSoftwareMonitor;
class StL3SoftwareMonitor;
class dst_mon_soft_tpc_st;
class dst_mon_soft_svt_st;
class dst_mon_soft_ftpc_st;
class dst_mon_soft_emc_st;
class dst_mon_soft_ctb_st;
class dst_mon_soft_rich_st;
class dst_mon_soft_glob_st;
class dst_mon_soft_l3_st;

class StSoftwareMonitor : public StObject {
public:
    StSoftwareMonitor();
    StSoftwareMonitor(const dst_mon_soft_tpc_st*,
                      const dst_mon_soft_svt_st*,
                      const dst_mon_soft_ftpc_st*,
                      const dst_mon_soft_emc_st*,
                      const dst_mon_soft_ctb_st*,
                      const dst_mon_soft_rich_st*,
                      const dst_mon_soft_glob_st*,
                      const dst_mon_soft_l3_st*);
    StSoftwareMonitor& operator=(const StSoftwareMonitor&);
    StSoftwareMonitor(const StSoftwareMonitor&);
    virtual ~StSoftwareMonitor();
                      
    StTpcSoftwareMonitor*          tpc();
    const StTpcSoftwareMonitor*    tpc() const;
    StSvtSoftwareMonitor*          svt();
    const StSvtSoftwareMonitor*    svt() const;
    StFtpcSoftwareMonitor*         ftpc();
    const StFtpcSoftwareMonitor*   ftpc() const;
    StEmcSoftwareMonitor*          emc();
    const StEmcSoftwareMonitor*    emc() const;
    StRichSoftwareMonitor*         rich();
    const StRichSoftwareMonitor*   rich() const;
    StCtbSoftwareMonitor*          ctb();
    const StCtbSoftwareMonitor*    ctb() const;
    StGlobalSoftwareMonitor*       global();
    const StGlobalSoftwareMonitor* global() const;
    StL3SoftwareMonitor*           l3();
    const StL3SoftwareMonitor*     l3() const;

    void setTpcSoftwareMonitor(StTpcSoftwareMonitor*);
    void setSvtSoftwareMonitor(StSvtSoftwareMonitor*);
    void setFtpcSoftwareMonitor(StFtpcSoftwareMonitor*);
    void setEmcSoftwareMonitor(StEmcSoftwareMonitor*);
    void setRichSoftwareMonitor(StRichSoftwareMonitor*);
    void setCtbSoftwareMonitor(StCtbSoftwareMonitor*);
    void setGlobalSoftwareMonitor(StGlobalSoftwareMonitor*);
    void setL3SoftwareMonitor(StL3SoftwareMonitor*);
    
protected:
    StTpcSoftwareMonitor    *mTpcMonitor;
    StSvtSoftwareMonitor    *mSvtMonitor;
    StFtpcSoftwareMonitor   *mFtpcMonitor;
    StEmcSoftwareMonitor    *mEmcMonitor;
    StRichSoftwareMonitor   *mRichMonitor;
    StCtbSoftwareMonitor    *mCtbMonitor;
    StGlobalSoftwareMonitor *mGlobalMonitor;
    StL3SoftwareMonitor     *mL3Monitor;
    
    ClassDef(StSoftwareMonitor,1)
};


#endif
