/***************************************************************************
 *
 * $Id: StTofSoftwareMonitor.h,v 2.1 2000/12/08 03:52:42 ullrich Exp $
 *
 * Author: Thomas Ullrich, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofSoftwareMonitor.h,v $
 * Revision 2.1  2000/12/08 03:52:42  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StTofSoftwareMonitor_hh
#define StTofSoftwareMonitor_hh

#include "StObject.h"

class StTofSoftwareMonitor : public StObject {
public:
    StTofSoftwareMonitor();
    // StTofSoftwareMonitor(const StTofSoftwareMonitor&);            use default
    // StTofSoftwareMonitor& operator=(const StTofSoftwareMonitor&); use default
    ~StTofSoftwareMonitor();

private:
    
    ClassDef(StTofSoftwareMonitor,1)
};
#endif
