/*!
 * \class StTofSoftwareMonitor 
 * \author Thomas Ullrich, Dec 2000
 */
/***************************************************************************
 *
 * $Id: StTofSoftwareMonitor.h,v 2.2 2002/02/22 22:56:52 jeromel Exp $
 *
 * Author: Thomas Ullrich, Dec 2000
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTofSoftwareMonitor.h,v $
 * Revision 2.2  2002/02/22 22:56:52  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
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
