/***************************************************************************
 *
 * $Id: StL3SoftwareMonitor.cxx,v 2.1 1999/10/13 19:44:54 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StL3SoftwareMonitor.cxx,v $
 * Revision 2.1  1999/10/13 19:44:54  ullrich
 * Initial Revision
 *
 * Revision 2.1  1999/10/13 19:44:54  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include "StL3SoftwareMonitor.h"
#include "tables/dst_mon_soft_l3.h"

static const char rcsid[] = "$Id: StL3SoftwareMonitor.cxx,v 2.1 1999/10/13 19:44:54 ullrich Exp $";

ClassImp(StL3SoftwareMonitor)

StL3SoftwareMonitor::StL3SoftwareMonitor()
{
    id_algorithm = 0;
    id_hardware  = 0;
    triggermask  = 0;
    nTotalHits   = 0;
    nTotalTracks = 0;
    nTotalPrimaryTracks = 0;

    for (int i=0; i<24; i++) {
        id_param[i]       = 0;
        nHits[i]          = 0;
        nTracks[i]        = 0;
        nPrimaryTracks[i] = 0;
        cpuTime[i]        = 0;
        processorId[i]    = 0;
        for (int k=0; k<3; k++) vertex[k][i] = 0;
    }
}

StL3SoftwareMonitor::StL3SoftwareMonitor(const dst_mon_soft_l3_st& l3)
{
    id_algorithm = l3.id_algorithm;
    id_hardware  = l3.id_hardware;
    
    triggermask  = l3.triggermask;
    nTotalHits   = l3.nTotalHits;
    nTotalTracks = l3.nTotalTracks;
    nTotalPrimaryTracks = l3.nTotalPrimaryTracks;
    
    for (int i=0; i<24; i++) {
        processorId[i]    = l3.processorId[i];
        id_param[i]       = l3.id_param[i];
        nHits[i]          = l3.nHits[i];
        nTracks[i]        = l3.nTracks[i];
        nPrimaryTracks[i] = l3.nPrimaryTracks[i];
        cpuTime[i]        = l3.cpuTime[i];
        for (int k=0; k<3; k++) vertex[k][i] = l3.vertex[k][i];
    }
}

StL3SoftwareMonitor::~StL3SoftwareMonitor() {/* noop */}
