/*!
 * \class StFtpcSoftwareMonitor 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StFtpcSoftwareMonitor.h,v 2.3 2002/02/22 22:56:48 jeromel Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcSoftwareMonitor.h,v $
 * Revision 2.3  2002/02/22 22:56:48  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:37  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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

    Int_t    n_clus_ftpc[2];
    Int_t    n_pts_ftpc[2];
    Int_t    n_trk_ftpc[2];
    Float_t  chrg_ftpc_tot[2];
    Float_t  hit_frac_ftpc[2];
    Float_t  avg_trkL_ftpc[2];
    Float_t  res_pad_ftpc[2];
    Float_t  res_drf_ftpc[2];

    ClassDef(StFtpcSoftwareMonitor,1)
};
#endif
