/*!
 * \class StL3SoftwareMonitor 
 * \author Thomas Ullrich, July 1999
 */
/***************************************************************************
 *
 * $Id: StL3SoftwareMonitor.h,v 2.3 2002/02/22 22:56:49 jeromel Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL3SoftwareMonitor.h,v $
 * Revision 2.3  2002/02/22 22:56:49  jeromel
 * Doxygen basic documentation in all header files. None of this is required
 * for QM production.
 *
 * Revision 2.2  2001/04/05 04:00:38  ullrich
 * Replaced all (U)Long_t by (U)Int_t and all redundant ROOT typedefs.
 *
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

    Int_t    id_algorithm;
    Int_t    id_hardware;
    Short_t  triggermask;
    Int_t    nTotalHits ;
    Int_t    nTotalTracks;
    Int_t    nTotalPrimaryTracks;
    Short_t  processorId[24] ;
    Float_t  vertex[3][24];
    Short_t  id_param[24];
    Int_t    nHits[24];
    Int_t    nTracks[24];
    Int_t    nPrimaryTracks[24];
    Float_t  cpuTime[24];

    ClassDef(StL3SoftwareMonitor,1)
};
#endif
