/***************************************************************************
 *
 * $Id: StCtbSoftwareMonitor.cxx,v 2.2 1999/10/28 22:24:55 ullrich Exp $
 *
 * Author: Thomas Ullrich, July 1999
 ***************************************************************************
 *
 * Description:
 *
 ***************************************************************************
 *
 * $Log: StCtbSoftwareMonitor.cxx,v $
 * Revision 2.2  1999/10/28 22:24:55  ullrich
 * Adapted new StArray version. First version to compile on Linux and Sun.
 *
 * Revision 2.1  1999/10/13 19:44:26  ullrich
 * Initial Revision
 *
 **************************************************************************/
#include <algorithm>
#include "StCtbSoftwareMonitor.h"
#include "tables/St_dst_mon_soft_ctb_Table.h"

static const char rcsid[] = "$Id: StCtbSoftwareMonitor.cxx,v 2.2 1999/10/28 22:24:55 ullrich Exp $";

ClassImp(StCtbSoftwareMonitor)

StCtbSoftwareMonitor::StCtbSoftwareMonitor()
{
    mult_ctb_tot = 0;
}

StCtbSoftwareMonitor::StCtbSoftwareMonitor(const dst_mon_soft_ctb_st& mon)
{
    mult_ctb_tot = mon.mult_ctb_tot;
}

StCtbSoftwareMonitor::~StCtbSoftwareMonitor() {/* noop */}
