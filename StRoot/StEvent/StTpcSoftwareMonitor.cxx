/***************************************************************************
 *
 * $Id: StTpcSoftwareMonitor.cxx,v 2.3 1999/12/21 15:09:16 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StTpcSoftwareMonitor.cxx,v $
 * Revision 2.3  1999/12/21 15:09:16  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.2  1999/10/28 22:27:18  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:45:37  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include "StTpcSoftwareMonitor.h"
#include "tables/St_dst_mon_soft_tpc_Table.h"
#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
using std::copy;
#endif

static const char rcsid[] = "$Id: StTpcSoftwareMonitor.cxx,v 2.3 1999/12/21 15:09:16 ullrich Exp $";

ClassImp(StTpcSoftwareMonitor)

StTpcSoftwareMonitor::StTpcSoftwareMonitor()
{
    n_clus_tpc_tot = 0;
    fill_n(n_clus_tpc_in, 24, 0);
    fill_n(n_clus_tpc_out, 24, 0);
    n_pts_tpc_tot = 0;
    fill_n(n_pts_tpc_in, 24, 0);
    fill_n(n_pts_tpc_out, 24, 0);
    fill_n(n_trk_tpc, 2, 0);
    fill_n(chrg_tpc_drift, 10, 0);
    chrg_tpc_tot = 0;
    fill_n(chrg_tpc_in, 24, 0);
    fill_n(chrg_tpc_out, 24, 0);
    fill_n(hit_frac_tpc, 2, 0);
    fill_n(avg_trkL_tpc, 2, 0);
    fill_n(res_pad_tpc, 2, 0);
    fill_n(res_drf_tpc, 2, 0);
}

StTpcSoftwareMonitor::StTpcSoftwareMonitor(const dst_mon_soft_tpc_st& mon)
{
    n_clus_tpc_tot = mon.n_clus_tpc_tot;
    copy(mon.n_clus_tpc_in+0, mon.n_clus_tpc_in+24, n_clus_tpc_in);
    copy(mon.n_clus_tpc_out+0, mon.n_clus_tpc_out+24, n_clus_tpc_out);
    n_pts_tpc_tot = mon.n_pts_tpc_tot;
    copy(mon.n_pts_tpc_in+0, mon.n_pts_tpc_in+24, n_pts_tpc_in);
    copy(mon.n_pts_tpc_out+0, mon.n_pts_tpc_out+24, n_pts_tpc_out);
    copy(mon.n_trk_tpc+0, mon.n_trk_tpc+2, n_trk_tpc);
    copy(mon.chrg_tpc_drift+0, mon.chrg_tpc_drift+10, chrg_tpc_drift);
    chrg_tpc_tot = mon.chrg_tpc_tot;
    copy(mon.chrg_tpc_in+0, mon.chrg_tpc_in+24, chrg_tpc_in);
    copy(mon.chrg_tpc_out+0, mon.chrg_tpc_out+24, chrg_tpc_out);
    copy(mon.hit_frac_tpc+0, mon.hit_frac_tpc+2, hit_frac_tpc);
    copy(mon.avg_trkL_tpc+0, mon.avg_trkL_tpc+2, avg_trkL_tpc);
    copy(mon.res_pad_tpc+0, mon.res_pad_tpc+2, res_pad_tpc);
    copy(mon.res_drf_tpc+0, mon.res_drf_tpc+2, res_drf_tpc);
}

StTpcSoftwareMonitor::~StTpcSoftwareMonitor() {/* noop */}
