/***************************************************************************
 *
 * $Id: StSvtSoftwareMonitor.cxx,v 2.2 1999/10/28 22:26:55 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StSvtSoftwareMonitor.cxx,v $
 * Revision 2.2  1999/10/28 22:26:55  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.2  1999/10/28 22:26:55  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:20  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include "StSvtSoftwareMonitor.h"
#include "tables/St_dst_mon_soft_svt_Table.h"

static const char rcsid[] = "$Id: StSvtSoftwareMonitor.cxx,v 2.2 1999/10/28 22:26:55 ullrich Exp $";

ClassImp(StSvtSoftwareMonitor)

StSvtSoftwareMonitor::StSvtSoftwareMonitor()
{
    fill_n(n_clus_svt, 4, 0);
    fill_n(n_pts_svt, 4, 0);
    n_trk_svt = 0;
    fill_n(chrg_svt_tot, 4, 0);
    fill_n(hit_frac_svt, 4, 0);
    avg_trkL_svt = 0;
    res_pad_svt = 0;
    res_drf_svt = 0;
}

StSvtSoftwareMonitor::StSvtSoftwareMonitor(const dst_mon_soft_svt_st& mon)
{
    copy(mon.n_clus_svt+0, mon.n_clus_svt+4, n_clus_svt);
    copy(mon.n_pts_svt+0, mon.n_pts_svt+4, n_pts_svt);
    n_trk_svt = mon.n_trk_svt;
    copy(mon.chrg_svt_tot+0, mon.chrg_svt_tot+4, chrg_svt_tot);
    copy(mon.hit_frac_svt+0, mon.hit_frac_svt+4, hit_frac_svt);
    avg_trkL_svt = mon.avg_trkL_svt;
    res_pad_svt = mon.res_pad_svt;
    res_drf_svt = mon.res_drf_svt;
}

StSvtSoftwareMonitor::~StSvtSoftwareMonitor() {/* noop */}
