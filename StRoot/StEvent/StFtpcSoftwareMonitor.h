/*!
 * \class StFtpcSoftwareMonitor 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StFtpcSoftwareMonitor.h,v 2.4 2009/11/23 16:34:06 fisyak Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcSoftwareMonitor.h,v $
 * Revision 2.4  2009/11/23 16:34:06  fisyak
 * Cleanup, remove dependence on dst tables, clean up software monitors
 *
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
#ifndef DST_MON_SOFT_FTPC_H
#define DST_MON_SOFT_FTPC_H
struct dst_mon_soft_ftpc_st {
  int n_clus_ftpc[2]; /* Tot. # clus in FTPC, east/west       */
  int n_pts_ftpc[2]; /* Tot. # space pts in FTPC, east/west  */
  int n_trk_ftpc[2]; /* Total # tracks in FTPC east/west     */
  float chrg_ftpc_tot[2]; /* Tot. charge dep. in FTPC, east/west  */
  float hit_frac_ftpc[2]; /* Frac. hits used in FTPC, east/west   */
  float avg_trkL_ftpc[2]; /* -OR- Avg. # pts assigned             */
  float res_pad_ftpc[2]; /* Avg. residual, pad direction,FTPC E/W*/
  float res_drf_ftpc[2]; /* Avg. resid., drift direction,FTPC E/W*/
};
#endif /* DST_MON_SOFT_FTPC_H */


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
