/***************************************************************************
 *
 * $Id: StTpcSoftwareMonitor.h,v 2.1 1999/10/13 19:44:09 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcSoftwareMonitor.h,v $
 * Revision 2.1  1999/10/13 19:44:09  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTpcSoftwareMonitor_hh
#define StTpcSoftwareMonitor_hh

#include "StObject.h"
class dst_mon_soft_tpc_st;

class StTpcSoftwareMonitor : public StObject {
public:
    StTpcSoftwareMonitor();
    StTpcSoftwareMonitor(const dst_mon_soft_tpc_st&);
    // StTpcSoftwareMonitor(const StTpcSoftwareMonitor&);            use default
    // StTpcSoftwareMonitor& operator=(const StTpcSoftwareMonitor&); use default
    ~StTpcSoftwareMonitor();

    Long_t  n_clus_tpc_tot;
    Long_t  n_clus_tpc_in[24];
    Long_t  n_clus_tpc_out[24];
    Long_t  n_pts_tpc_tot;
    Long_t  n_pts_tpc_in[24];
    Long_t  n_pts_tpc_out[24];
    Long_t  n_trk_tpc[2];
    Float_t chrg_tpc_drift[10];
    Float_t chrg_tpc_tot;
    Float_t chrg_tpc_in[24];
    Float_t chrg_tpc_out[24];
    Float_t hit_frac_tpc[2];
    Float_t avg_trkL_tpc[2];
    Float_t res_pad_tpc[2];
    Float_t res_drf_tpc[2];

    ClassDef(StTpcSoftwareMonitor,1)
};
#endif
