/***************************************************************************
 *
 * $Id: StCtbSoftwareMonitor.h,v 2.1 1999/10/13 19:42:53 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbSoftwareMonitor.h,v $
 * Revision 2.1  1999/10/13 19:42:53  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StCtbSoftwareMonitor_hh
#define StCtbSoftwareMonitor_hh

#include "StObject.h"
class dst_mon_soft_ctb_st;

class StCtbSoftwareMonitor : public StObject {
public:
    StCtbSoftwareMonitor();
    StCtbSoftwareMonitor(const dst_mon_soft_ctb_st&);
    // StCtbSoftwareMonitor(const StCtbSoftwareMonitor&);            use default
    // StCtbSoftwareMonitor& operator=(const StCtbSoftwareMonitor&); use default
    ~StCtbSoftwareMonitor();
    
    Long_t  mult_ctb_tot;

    ClassDef(StCtbSoftwareMonitor,1)
};
#endif
