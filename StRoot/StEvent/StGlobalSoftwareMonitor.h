/***************************************************************************
 *
 * $Id: StGlobalSoftwareMonitor.h,v 2.1 1999/10/13 19:43:17 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StGlobalSoftwareMonitor.h,v $
 * Revision 2.1  1999/10/13 19:43:17  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StGlobalSoftwareMonitor_hh
#define StGlobalSoftwareMonitor_hh

#include "StObject.h"
class dst_mon_soft_glob_st;

class StGlobalSoftwareMonitor : public StObject {
public:
    StGlobalSoftwareMonitor();
    StGlobalSoftwareMonitor(const dst_mon_soft_glob_st&);
    // StGlobalSoftwareMonitor(const StGlobalSoftwareMonitor&);            use default
    // StGlobalSoftwareMonitor& operator=(const StGlobalSoftwareMonitor&); use default
    ~StGlobalSoftwareMonitor();

    Long_t  n_trk_match[2];
    Long_t  prim_vrtx_ntrk;
    Float_t prim_vrtx_cov[6];
    Float_t prim_vrtx_chisq;

    ClassDef(StGlobalSoftwareMonitor,1)
};
#endif
