/*!
 * \class StSoftwareMonitor 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StSoftwareMonitor.h,v 2.5 2002/02/22 22:56:50 jeromel Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSoftwareMonitor.h,v $
 * Revision 2.5  2002/02/22 22:56:50  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.4  2001/04/05 04:00:42  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.3  2000/12/08 03:53:41  ullrich
 * Prepared hooks for ToF.
 *
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
class StTofSoftwareMonitor;
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
    StTofSoftwareMonitor*          tof();
    const StTofSoftwareMonitor*    tof() const;

    void setTpcSoftwareMonitor(StTpcSoftwareMonitor*);
    void setSvtSoftwareMonitor(StSvtSoftwareMonitor*);
    void setFtpcSoftwareMonitor(StFtpcSoftwareMonitor*);
    void setEmcSoftwareMonitor(StEmcSoftwareMonitor*);
    void setRichSoftwareMonitor(StRichSoftwareMonitor*);
    void setCtbSoftwareMonitor(StCtbSoftwareMonitor*);
    void setGlobalSoftwareMonitor(StGlobalSoftwareMonitor*);
    void setL3SoftwareMonitor(StL3SoftwareMonitor*);
    void setTofSoftwareMonitor(StTofSoftwareMonitor*);
    
protected:
    StTpcSoftwareMonitor    *mTpcMonitor;
    StSvtSoftwareMonitor    *mSvtMonitor;
    StFtpcSoftwareMonitor   *mFtpcMonitor;
    StEmcSoftwareMonitor    *mEmcMonitor;
    StRichSoftwareMonitor   *mRichMonitor;
    StCtbSoftwareMonitor    *mCtbMonitor;
    StGlobalSoftwareMonitor *mGlobalMonitor;
    StL3SoftwareMonitor     *mL3Monitor;
    StTofSoftwareMonitor    *mTofMonitor;
    
    ClassDef(StSoftwareMonitor,1)
};
#endif
