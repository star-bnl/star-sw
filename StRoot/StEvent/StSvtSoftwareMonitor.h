/*!
 * \class StSvtSoftwareMonitor 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StSvtSoftwareMonitor.h,v 2.3 2002/02/22 22:56:51 jeromel Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtSoftwareMonitor.h,v $
 * Revision 2.3  2002/02/22 22:56:51  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:43  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
 * Revision 2.1  1999/10/13 19:43:51  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StSvtSoftwareMonitor_hh
#define StSvtSoftwareMonitor_hh

#include "StObject.h"
class dst_mon_soft_svt_st;

class StSvtSoftwareMonitor : public StObject {
public:
    StSvtSoftwareMonitor();
    StSvtSoftwareMonitor(const dst_mon_soft_svt_st&);
    // StSvtSoftwareMonitor(const StSvtSoftwareMonitor&);            use default
    // StSvtSoftwareMonitor& operator=(const StSvtSoftwareMonitor&); use default
    ~StSvtSoftwareMonitor();

    Int_t    n_clus_svt[4];
    Int_t    n_pts_svt[4];
    Int_t    n_trk_svt;
    Float_t  chrg_svt_tot[4];
    Float_t  hit_frac_svt[4];
    Float_t  avg_trkL_svt;
    Float_t  res_pad_svt;
    Float_t  res_drf_svt;

    ClassDef(StSvtSoftwareMonitor,1)
};
#endif
