/***************************************************************************
 *
 * $Id: StFtpcSoftwareMonitor.h,v 2.1 1999/10/13 19:43:13 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcSoftwareMonitor.h,v $
 * Revision 2.1  1999/10/13 19:43:13  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StFtpcSoftwareMonitor_hh
#define StFtpcSoftwareMonitor_hh

#include "StObject.h"
class dst_mon_soft_ftpc_st;

class StFtpcSoftwareMonitor : public StObject {
public:
    StFtpcSoftwareMonitor();
    StFtpcSoftwareMonitor(const dst_mon_soft_ftpc_st&);
    // StFtpcSoftwareMonitor(const StFtpcSoftwareMonitor&);            use default
    // StFtpcSoftwareMonitor& operator=(const StFtpcSoftwareMonitor&); use default
    ~StFtpcSoftwareMonitor();

    Long_t  n_clus_ftpc[2];
    Long_t  n_pts_ftpc[2];
    Long_t  n_trk_ftpc[2];
    Float_t chrg_ftpc_tot[2];
    Float_t hit_frac_ftpc[2];
    Float_t avg_trkL_ftpc[2];
    Float_t res_pad_ftpc[2];
    Float_t res_drf_ftpc[2];

    ClassDef(StFtpcSoftwareMonitor,1)
};
#endif
