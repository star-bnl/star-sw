/***************************************************************************
 *
 * $Id: StL3SoftwareMonitor.h,v 2.1 1999/10/13 19:43:24 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL3SoftwareMonitor.h,v $
 * Revision 2.1  1999/10/13 19:43:24  ullrich
 * Initial Revision
 *
 **************************************************************************/
#ifndef StL3SoftwareMonitor_hh
#define StL3SoftwareMonitor_hh

#include "StObject.h"
class dst_mon_soft_l3_st;

class StL3SoftwareMonitor : public StObject {
public:
    StL3SoftwareMonitor();
    StL3SoftwareMonitor(const dst_mon_soft_l3_st&);
    // StL3SoftwareMonitor(const StL3SoftwareMonitor&);            use default
    // StL3SoftwareMonitor& operator=(const StL3SoftwareMonitor&); use default
    ~StL3SoftwareMonitor();

    Long_t  id_algorithm;
    Long_t  id_hardware;
    Short_t triggermask;
    Long_t  nTotalHits ;
    Long_t  nTotalTracks;
    Long_t  nTotalPrimaryTracks;
    Short_t processorId[24] ;
    Float_t vertex[3][24];
    Short_t id_param[24];
    Long_t  nHits[24];
    Long_t  nTracks[24];
    Long_t  nPrimaryTracks[24];
    Float_t cpuTime[24];

    ClassDef(StL3SoftwareMonitor,1)
};
#endif
