/***************************************************************************
 *
 * $Id: StGlobalSoftwareMonitor.cxx,v 2.2 1999/10/28 22:25:33 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StGlobalSoftwareMonitor.cxx,v $
 * Revision 2.2  1999/10/28 22:25:33  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.2  1999/10/28 22:25:33  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:47  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include "StGlobalSoftwareMonitor.h"
#include "tables/St_dst_mon_soft_glob_Table.h"

static const char rcsid[] = "$Id: StGlobalSoftwareMonitor.cxx,v 2.2 1999/10/28 22:25:33 ullrich Exp $";

ClassImp(StGlobalSoftwareMonitor)

StGlobalSoftwareMonitor::StGlobalSoftwareMonitor()
{
    fill_n(n_trk_match, 2, 0);
    prim_vrtx_ntrk = 0;
    fill_n(prim_vrtx_cov, 6, 0);
    prim_vrtx_chisq = 0;
}

StGlobalSoftwareMonitor::StGlobalSoftwareMonitor(const dst_mon_soft_glob_st& mon)
{
    copy(mon.n_trk_match+0, mon.n_trk_match+2, n_trk_match);
    prim_vrtx_ntrk = mon.prim_vrtx_ntrk;
    copy(mon.prim_vrtx_cov+0, mon.prim_vrtx_cov+6, prim_vrtx_cov);
    prim_vrtx_chisq = mon.prim_vrtx_chisq;
}

StGlobalSoftwareMonitor::~StGlobalSoftwareMonitor() {/* noop */}

