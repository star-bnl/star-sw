/***************************************************************************
 *
 * $Id: StRichSoftwareMonitor.h,v 2.1 1999/10/13 19:43:38 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StRichSoftwareMonitor.h,v $
 * Revision 2.1  1999/10/13 19:43:38  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:43:38  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StRichSoftwareMonitor_hh
#define StRichSoftwareMonitor_hh

#include "StObject.h"
class dst_mon_soft_rich_st;

class StRichSoftwareMonitor : public StObject {
public:
    StRichSoftwareMonitor();
    StRichSoftwareMonitor(const dst_mon_soft_rich_st&);
    // StRichSoftwareMonitor(const StRichSoftwareMonitor&);            use default
    // StRichSoftwareMonitor& operator=(const StRichSoftwareMonitor&); use default
    ~StRichSoftwareMonitor();
    
    Long_t  mult_rich_tot;

    ClassDef(StRichSoftwareMonitor,1)
};
#endif
