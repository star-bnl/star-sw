/*!
 * \class StTpcSoftwareMonitor 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StTpcSoftwareMonitor.h,v 2.3 2002/02/22 22:56:52 jeromel Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcSoftwareMonitor.h,v $
 * Revision 2.3  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:45  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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

    Int_t   n_clus_tpc_tot;
    Int_t   n_clus_tpc_in[24];
    Int_t   n_clus_tpc_out[24];
    Int_t   n_pts_tpc_tot;
    Int_t   n_pts_tpc_in[24];
    Int_t   n_pts_tpc_out[24];
    Int_t   n_trk_tpc[2];
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
