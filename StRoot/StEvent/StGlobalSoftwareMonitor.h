/*!
 * \class StGlobalSoftwareMonitor 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StGlobalSoftwareMonitor.h,v 2.3 2002/02/22 22:56:48 jeromel Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StGlobalSoftwareMonitor.h,v $
 * Revision 2.3  2002/02/22 22:56:48  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:37  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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

    Int_t   n_trk_match[2];
    Int_t   prim_vrtx_ntrk;
    Float_t prim_vrtx_cov[6];
    Float_t prim_vrtx_chisq;

    ClassDef(StGlobalSoftwareMonitor,1)
};
#endif
