/***************************************************************************
 *
 * $Id: StFtpcSoftwareMonitor.cxx,v 2.3 1999/12/21 15:08:52 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StFtpcSoftwareMonitor.cxx,v $
 * Revision 2.3  1999/12/21 15:08:52  ullrich
 * Modified to cope with new compiler version on Sun (CC5.0).
 *
 * Revision 2.2  1999/10/28 22:25:27  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:44  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include "StFtpcSoftwareMonitor.h"
#include "tables/St_dst_mon_soft_ftpc_Table.h"
#if !defined(ST_NO_NAMESPACES)
using std::fill_n;
using std::copy;
#endif

static const char rcsid[] = "$Id: StFtpcSoftwareMonitor.cxx,v 2.3 1999/12/21 15:08:52 ullrich Exp $";

ClassImp(StFtpcSoftwareMonitor)

StFtpcSoftwareMonitor::StFtpcSoftwareMonitor()
{
    fill_n(n_clus_ftpc, 2, 0);
    fill_n(n_pts_ftpc, 2, 0);
    fill_n(n_trk_ftpc, 2, 0);
    fill_n(chrg_ftpc_tot, 2, 0);
    fill_n(hit_frac_ftpc, 2, 0);
    fill_n(avg_trkL_ftpc, 2, 0);
    fill_n(res_pad_ftpc, 2, 0);
    fill_n(res_drf_ftpc, 2, 0);
}

StFtpcSoftwareMonitor::StFtpcSoftwareMonitor(const dst_mon_soft_ftpc_st& mon)
{
    copy(mon.n_clus_ftpc+0, mon.n_clus_ftpc+2, n_clus_ftpc);
    copy(mon.n_pts_ftpc+0, mon.n_pts_ftpc+2, n_pts_ftpc);
    copy(mon.n_trk_ftpc+0, mon.n_trk_ftpc+2, n_trk_ftpc);
    copy(mon.chrg_ftpc_tot+0, mon.chrg_ftpc_tot+2, chrg_ftpc_tot);
    copy(mon.hit_frac_ftpc+0, mon.hit_frac_ftpc+2, hit_frac_ftpc);
    copy(mon.avg_trkL_ftpc+0, mon.avg_trkL_ftpc+2, avg_trkL_ftpc);
    copy(mon.res_pad_ftpc+0, mon.res_pad_ftpc+2, res_pad_ftpc);
    copy(mon.res_drf_ftpc+0, mon.res_drf_ftpc+2, res_drf_ftpc);
}

StFtpcSoftwareMonitor::~StFtpcSoftwareMonitor() {/* noop */}
