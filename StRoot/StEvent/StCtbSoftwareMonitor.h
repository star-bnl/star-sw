/*!
 * \class StCtbSoftwareMonitor 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StCtbSoftwareMonitor.h,v 2.3 2002/02/22 22:56:46 jeromel Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbSoftwareMonitor.h,v $
 * Revision 2.3  2002/02/22 22:56:46  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:24  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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
    
    Int_t  mult_ctb_tot;

    ClassDef(StCtbSoftwareMonitor,1)
};
#endif
