/*!
 * \class StEmcSoftwareMonitor 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StEmcSoftwareMonitor.h,v 2.2 2002/02/22 22:56:47 jeromel Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StEmcSoftwareMonitor.h,v $
 * Revision 2.2  2002/02/22 22:56:47  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.1  1999/10/13 19:43:00  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StEmcSoftwareMonitor_hh
#define StEmcSoftwareMonitor_hh

#include "StObject.h"
class dst_mon_soft_emc_st;

class StEmcSoftwareMonitor : public StObject {
public:
    StEmcSoftwareMonitor();
    StEmcSoftwareMonitor(const dst_mon_soft_emc_st&);
    // StEmcSoftwareMonitor(const StEmcSoftwareMonitor&);            use default
    // StEmcSoftwareMonitor& operator=(const StEmcSoftwareMonitor&); use default
    ~StEmcSoftwareMonitor();
    
    Float_t energy_emc;

    ClassDef(StEmcSoftwareMonitor,1)
};
#endif
